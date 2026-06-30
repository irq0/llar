(ns llar.update
  (:require
   [clojure.tools.logging :as log]
   [java-time.api :as time]
   [mount.core :refer [defstate]]
   [slingshot.slingshot :refer [throw+ try+]]
   [llar.metrics :as metrics]
   [llar.config :as config]
   [llar.converter :as converter]
   [iapetos.core :as prometheus]
   [llar.fetch :as fetch]
   [llar.postproc :as proc]
   [llar.rc :as rc]
   [llar.sched :refer [defsched] :as sched]
   [llar.store :as store :refer [store-items!]]
   [llar.digest :as digest]
   [llar.persistency :as persistency]))

;;;; Update - Combines fetch and persistency with additional state management
;;;; Source state is managed in the core/state atom.

(defonce prom-registry
  (-> metrics/prom-registry
      (prometheus/register
       (prometheus/gauge :llar/source-state
                         {:description "Current source state. Exactly one status should be 1 per source."
                          :labels [:source :status]})
       (prometheus/gauge :llar/source-last-attempt-unixtime
                         {:description "Last time a source update was attempted."
                          :labels [:source]})
       (prometheus/gauge :llar/source-last-success-unixtime
                         {:description "Last time a source update succeeded."
                          :labels [:source]})
       (prometheus/gauge :llar/source-last-failure-unixtime
                         {:description "Last time a source update failed."
                          :labels [:source]})
       (prometheus/gauge :llar/source-last-duration-seconds
                         {:description "Duration of the last completed source update."
                          :labels [:source]})
       (prometheus/gauge :llar/source-last-items
                         {:description "Item counts from the last completed source update."
                          :labels [:source :stage]})
       (prometheus/gauge :llar/source-in-progress
                         {:description "Whether a source update is currently in progress."
                          :labels [:source]})
       (prometheus/gauge :llar/source-update-start-unixtime
                         {:description "Last time a source update started."
                          :labels [:source]})
       (prometheus/gauge :llar/source-last-failure-info
                         {:description "Classification of the latest source failure. Value is 1 for the active classification."
                          :labels [:source :source_type :status :step :reason_class]})
       (prometheus/counter :llar/update-attempts-total
                           {:description "Completed source update attempts."
                            :labels [:source_type :result :step :reason_class]})
       (prometheus/counter :llar/update-failures-total
                           {:description "Completed failed source update attempts."
                            :labels [:source_type :step :reason_class]})
       (prometheus/histogram :llar/update-duration-seconds
                             {:description "Source update duration by bounded fleet labels."
                              :labels [:source_type :result]
                              :buckets [0.5 1 2.5 5 10 30 60 120 300 600 1800]})
       (prometheus/counter :llar/items-processed-total
                           {:description "Items seen at update stages."
                            :labels [:source_type :stage :result]})
       (prometheus/gauge :llar/sources-total
                         {:description "Number of sources by current status."
                          :labels [:status]})
       (prometheus/gauge :llar/sources-in-progress-total
                         {:description "Number of source updates currently in progress."})
       (prometheus/gauge :llar/sources-stuck-total
                         {:description "Number of source updates running longer than the stuck threshold."}))))

(def +available-states+
  [:ok :new :temp-fail :perm-fail :bug :updating])

(def ^:private +item-stages+
  [:fetched :processed :stored])

(def ^:private +stuck-threshold+
  (time/minutes 20))

(def ^:private +failure-states+
  #{:temp-fail :perm-fail :bug})

(defn- update-step [state]
  (let [step (get-in state [:last-exception :data :update-step])]
    (case step
      :postproc :postproc
      :store :store
      (if (+failure-states+ (:status state)) :fetch :none))))

(defn- reason-class [state]
  (if (+failure-states+ (:status state))
    (metrics/reason-class-from-data (get-in state [:last-exception :data]))
    :none))

(defn- failure-info-labels [k feed state]
  (when (+failure-states+ (:status state))
    {:source (name k)
     :source_type (metrics/source-type-label (:src feed))
     :status (metrics/label-value (:status state))
     :step (metrics/label-value (update-step state))
     :reason_class (metrics/label-value (reason-class state))}))

(defstate state
  :start (atom {}))

(defn get-current-state []
  (into {}
        (map (fn [[k v]]
               (if (instance? clojure.lang.Atom v)
                 [k @v]
                 [k v]))
             @state)))

(def src-state-template
  "New sources start with this template"
  {:key nil
   :last-successful-fetch-ts nil
   :last-attempt-ts nil
   :forced-update? false
   :status :new
   :last-exception nil
   :retry-count 0})

(defn- calc-last-duration [state]
  (if (nil? (:last-attempt-ts state))
    nil
    (time/duration (:last-attempt-ts state) (time/zoned-date-time))))

(defn- make-next-state
  ([state ok-status extra-state]
   (let [now (time/zoned-date-time)
         duration (calc-last-duration state)]
     (merge state
            {:last-finished-ts now
             :last-successful-fetch-ts now
             :last-duration duration
             :status ok-status
             :last-exception nil
             :retry-count 0}
            extra-state)))
  ([state next-status next-retry-count last-exception]
   (let [now (time/zoned-date-time)
         duration (calc-last-duration state)]
     (merge state
            {:last-finished-ts now
             :last-duration duration
             :status next-status
             :last-exception (converter/make-exception-context-from-slingshot-throw-context last-exception)
             :retry-count next-retry-count}))))

(defn- update-feed!
  "Update feed. Return new state"
  [k & {:keys [skip-proc skip-store overwrite?]
        :or {skip-proc false
             skip-store false
             overwrite? false}}]

  (let [feed (config/get-source k)
        state (get @state k)
        {:keys [src]} feed
        retry-count (or (get-in feed [:state :retry-count]) 0)]
    (try+
     (let [fetched (fetch/fetch feed (:fetch-meta state))
           processed (try
                       (if-not skip-proc
                         (proc/process feed state fetched)
                         fetched)
                       (catch clojure.lang.ExceptionInfo ex
                         (throw+ (merge (ex-data ex)
                                        {:fetched (map #(select-keys % [:summary :meta])
                                                       fetched)
                                         :feed feed
                                         :update-step :postproc
                                         :skip skip-proc})
                                 "postproc failed" ex)))

           dbks (try+
                 (if-not skip-store
                   (store-items! processed :overwrite? overwrite?)
                   processed)
                 (catch Object _
                   (throw+ {:type ::store-error
                            :feed feed
                            :fetched fetched
                            :processed processed
                            :update-step :store
                            :skip skip-store})))]

       (log/infof "update %s: fetched: %d, after processing: %d, new in db: %d (skip-proc: %s, skip-store: %s, fetch-meta: %s)"
                  (str src) (count fetched) (count processed) (count dbks)
                  skip-proc skip-store  (meta fetched))

       (make-next-state state :ok {:stats {:fetched (count fetched)
                                           :processed (count processed)
                                           :db (count dbks)}
                                   :fetch-meta (meta fetched)}))

     (catch [:type :llar.http/server-error-retry-later] _
       (make-next-state state :temp-fail (inc retry-count) &throw-context))

     (catch [:type :llar.http/client-error-retry-later] _
       (make-next-state state :temp-fail (inc retry-count) &throw-context))

     (catch [:type :llar.postproc/postproc-temp-fail] _
       (make-next-state state :temp-fail (inc retry-count) &throw-context))

     (catch [:type :llar.http/request-error] _
       (make-next-state state :perm-fail 0 &throw-context))

     (catch [:type :llar.fetch.feed/rome-failure] _
       (make-next-state state :perm-fail 0 &throw-context))

     (catch [:type :llar.http/unexpected-error] _
       (make-next-state state :bug 0 &throw-context))

     (catch [:clojure.spec.alpha/failure :assertion-failed] x
       (log/error "spec assertion failed. BUG!" x)
       (make-next-state state :bug 0 &throw-context))

     (catch java.lang.OutOfMemoryError _ex
       (log/warn (:throwable &throw-context) "Out of memory! Adjust resource limits? (-> temp fail) " (str src))
       (make-next-state state :temp-fail (inc retry-count) &throw-context))

     (catch java.io.IOException ex
       (log/warn (:throwable &throw-context) "IOException for" (str src))
       (if (re-find #"error=11" (ex-message (:cause ex)))
         (make-next-state state :temp-fail (inc retry-count) &throw-context)
         (make-next-state state :bug 0 &throw-context)))

     (catch java.net.ConnectException _
       (log/warn (:throwable &throw-context) "Connection error (-> temp-fail) for" (str src))
       (make-next-state state :temp-fail (inc retry-count) &throw-context))

     (catch java.lang.AssertionError _
       (log/error (:throwable &throw-context) "ASSERTION! BUG! (spec?)" (str src))
       (make-next-state state :bug 0 &throw-context))

     (catch Object _
       (log/error (:throwable &throw-context) "Unexpected error (-> bug) for " (str src) src)
       (make-next-state state :bug 0 &throw-context)))))

(defn set-status!
  "Set feed's status"
  [k new-status]
  (let [src (get @state k)]
    (when-not (instance? clojure.lang.Atom src)
      (swap! state assoc-in [k :status] new-status)
      new-status)))

(defn reset-all-failed!
  "Reset all feed states to :new"
  []
  (doseq [[k _v] (config/get-sources)]
    (set-status! k :new)))

(defn- stuck? [state]
  (and (= :updating (:status state))
       (:last-attempt-ts state)
       (time/before? (:last-attempt-ts state)
                     (time/minus (time/zoned-date-time) +stuck-threshold+))))

(defn- observe-state-summary-metrics []
  (let [states (vals (get-current-state))
        states-by-status (group-by :status states)
        in-progress (count (get states-by-status :updating []))
        stuck (count (filter stuck? states))]
    (doseq [status +available-states+]
      (prometheus/set prom-registry :llar/sources-total
                      {:status (metrics/label-value status)}
                      (count (get states-by-status status []))))
    (prometheus/set prom-registry :llar/sources-in-progress-total in-progress)
    (prometheus/set prom-registry :llar/sources-stuck-total stuck)))

(defn- observe-source-state! [k state]
  (let [source (name k)]
    (doseq [status +available-states+]
      (prometheus/set prom-registry :llar/source-state
                      {:source source
                       :status (metrics/label-value status)}
                      (if (= status (:status state)) 1 0)))
    (prometheus/set prom-registry :llar/source-in-progress
                    {:source source}
                    (if (= :updating (:status state)) 1 0))
    (when-let [ts (:last-attempt-ts state)]
      (prometheus/set prom-registry :llar/source-last-attempt-unixtime
                      {:source source}
                      (/ (time/to-millis-from-epoch ts) 1000.0)))
    (when-let [ts (:last-successful-fetch-ts state)]
      (prometheus/set prom-registry :llar/source-last-success-unixtime
                      {:source source}
                      (/ (time/to-millis-from-epoch ts) 1000.0)))
    (when-let [dur (:last-duration state)]
      (prometheus/set prom-registry :llar/source-last-duration-seconds
                      {:source source}
                      (/ (.toMillis dur) 1000.0)))
    (doseq [stage +item-stages+
            :let [metric-stage (if (= stage :stored) :stored stage)
                  stat-key (if (= stage :stored) :db stage)
                  value (get-in state [:stats stat-key])]]
      (when (some? value)
        (prometheus/set prom-registry :llar/source-last-items
                        {:source source
                         :stage (metrics/label-value metric-stage)}
                        value)))
    (when (+failure-states+ (:status state))
      (when-let [ts (:last-finished-ts state)]
        (prometheus/set prom-registry :llar/source-last-failure-unixtime
                        {:source source}
                        (/ (time/to-millis-from-epoch ts) 1000.0))))))

(defn- observe-update-start! [k state]
  (let [source (name k)]
    (prometheus/set prom-registry :llar/source-in-progress
                    {:source source}
                    1)
    (when-let [ts (:last-attempt-ts state)]
      (prometheus/set prom-registry :llar/source-update-start-unixtime
                      {:source source}
                      (/ (time/to-millis-from-epoch ts) 1000.0))
      (prometheus/set prom-registry :llar/source-last-attempt-unixtime
                      {:source source}
                      (/ (time/to-millis-from-epoch ts) 1000.0))))
  (observe-source-state! k state)
  (observe-state-summary-metrics))

(defn- observe-failure-info! [k feed previous-state new-state]
  (when-let [labels (failure-info-labels k feed previous-state)]
    (prometheus/set prom-registry :llar/source-last-failure-info labels 0))
  (when-let [labels (failure-info-labels k feed new-state)]
    (prometheus/set prom-registry :llar/source-last-failure-info labels 1)))

(defn- observe-update-complete! [k feed previous-state new-state]
  (let [source-type (metrics/source-type-label (:src feed))
        result (metrics/label-value (:status new-state))
        step (metrics/label-value (update-step new-state))
        reason (metrics/label-value (reason-class new-state))
        duration (some-> (:last-duration new-state) .toMillis (/ 1000.0))]
    (observe-source-state! k new-state)
    (observe-failure-info! k feed previous-state new-state)
    (prometheus/inc prom-registry :llar/update-attempts-total
                    {:source_type source-type
                     :result result
                     :step step
                     :reason_class reason})
    (when duration
      (prometheus/observe prom-registry :llar/update-duration-seconds
                          {:source_type source-type
                           :result result}
                          duration))
    (doseq [[stage value] [[:fetched (get-in new-state [:stats :fetched])]
                           [:processed (get-in new-state [:stats :processed])]
                           [:stored (get-in new-state [:stats :db])]]
            :when (some? value)]
      (prometheus/inc prom-registry :llar/items-processed-total
                      {:source_type source-type
                       :stage (metrics/label-value stage)
                       :result result}
                      value))
    (when (+failure-states+ (:status new-state))
      (prometheus/inc prom-registry :llar/update-failures-total
                      {:source_type source-type
                       :step step
                       :reason_class reason}))
    (observe-state-summary-metrics)))

;;; Update API

(defn update!
  "Update feed by id"
  [k & {:keys [force]
        :as args}]

  (when (nil? (config/get-source k))
    (throw+ {:type ::unknown-source-key :key k :known-keys (keys (config/get-sources))}))

  (when-not (satisfies? fetch/FetchSource (:src (config/get-source k)))
    (let [src (config/get-source k)]
      (throw+ {:type ::source-not-fetchable
               :key k
               :src (:src src)
               :src-type (type (:src src))})))

  (when-not (contains? @state k)
    (swap! state assoc k (assoc src-state-template :key k)))

    ;; don't update the same feed in parallel
    ;; push force update flag into state to make it accessible to ItemProcessor
  (swap! state update k assoc :forced-update? force)
  (let [cur-state (get @state k)
        cur-status (:status cur-state)]
    (condp = cur-status
      :new
      (log/debug "updating new feed: " k)
      :ok
      (log/debug "updating working feed: " k)
      :temp-fail
      (log/debug "temporary failing feed %d/%d: %s"
                 (:retry-count cur-state) (rc/rc [:update :max-retry]) k)
      :perm-fail
      (log/debug "skipping perm fail feed: " k)

      :bug
      (log/debug "skipping feed that triggered a bug: " k)

      :updating
      (log/debug "update already running: " k)

      (log/debugf "unknown status \"%s\": %s" cur-status k))

    (when force
      (log/debugf "force updating %s feed %s" cur-status k))

    (let [update? (or force
                      (#{:ok :new} cur-status)
                      (and (= cur-status :temp-fail)
                           (< (:retry-count cur-state) (rc/rc [:update :max-retry]))))]
      (if update?
        (do
          (swap! state update k assoc :status :updating)
          (swap! state update k assoc :last-attempt-ts (time/zoned-date-time))
          (observe-update-start! k (get @state k))
          (let [kw-args (mapcat identity (dissoc args :force))
                new-state (apply update-feed! k kw-args)
                new-status (:status new-state)]
            (log/debugf "[%s] State: %s -> %s " k
                        cur-status new-status)
            (swap! state (fn [current]
                           (assoc current k new-state)))
            (observe-update-complete! k (config/get-source k) cur-state new-state)
            new-status))
        (do
          (observe-source-state! k cur-state)
          (observe-state-summary-metrics)
          cur-status)))))

(defn updateable-sources []
  (into {} (filter #(satisfies? fetch/FetchSource (:src (val %))) (config/get-sources))))

(defn update-some! [keys & args]
  (doall
   (pmap #(apply update! (key %) args)
         (filter #(contains? (set keys) (key %)) (updateable-sources)))))

(defn update-all! [& args]
  (doall
   (pmap #(apply update! (key %) args) (updateable-sources))))

(defn update-matching! [re & args]
  (doall
   (pmap #(apply update! (key %) args)
         (filter #(re-find re (name (key %))) (updateable-sources)))))

(defn update-tagged! [tag & args]
  (doall
   (pmap #(apply update! (key %) args)
         (filter #(contains? (:tags (val %)) tag) (updateable-sources)))))

(defn update-failed! [& args]
  (let [failed-source-keys (map key (filter (fn [[_k v]]
                                              (contains? #{:perm-fail :temp-fail} (:status v)))
                                            @state))
        failed-sources (select-keys (updateable-sources) failed-source-keys)]
    (doall (pmap #(apply update! (key %) args) failed-sources))))

(defn update-bugged! [& args]
  (let [failed-source-keys (map key (filter (fn [[_k v]]
                                              (contains? #{:bug} (:status v)))
                                            @state))
        failed-sources (select-keys (updateable-sources) failed-source-keys)]
    (doall
     (map #(apply update! (key %) args) failed-sources))))

(defn update-unfetched! [& args]
  (let [sources-and-state (merge-with merge (updateable-sources) @state)
        unfetched-keys (map key (filter (fn [[_k v]] (nil? (:status v))) sources-and-state))
        result (doall (pmap #(apply update! % args) unfetched-keys))]
    result))

(defn- throwable-error [throwable]
  {:message (ex-message throwable)
   :data (ex-data throwable)
   :class (some-> throwable class str)})

(defn- autoread-config-error [sched-name reason detail sched]
  {:name sched-name
   :status :error
   :reason reason
   :period (:period sched)
   :source-count 0
   :sources []
   :error detail})

(defn- validate-autoread-sched [sched-name {:keys [period pred] :as sched}]
  (cond
    (nil? period)
    (autoread-config-error sched-name :invalid-period "missing period" sched)

    (not (ifn? pred))
    (autoread-config-error sched-name :invalid-predicate "predicate is not callable" sched)

    :else
    (try
      (time/minus (time/zoned-date-time) period)
      nil
      (catch Throwable t
        (autoread-config-error sched-name :invalid-period (throwable-error t) sched)))))

(defn remove-unread-for-autoread-sched! [sched-name {:keys [period pred predicate]}]
  (let [sched (cond-> {:period period :pred pred}
                (some? predicate) (assoc :predicate predicate))
        predicate-result (when (some? predicate)
                           {:predicate predicate})]
    (or (validate-autoread-sched sched-name sched)
        (let [matched-keys (atom [])]
          (try+
           (let [sources (updateable-sources)
                 filtered (filter pred sources)
                 keys (mapv first filtered)]
             (reset! matched-keys keys)
             (log/infof "remove-unread-tags: %s keys to untag if older then %s: %s"
                        sched-name period keys)
             (if (seq keys)
               (let [result (persistency/remove-unread-for-items-of-source-older-then!
                             store/backend-db
                             keys
                             (time/minus (time/zoned-date-time) period))]
                 (merge
                  {:name sched-name
                   :status :ok
                   :period period
                   :source-count (count keys)
                   :sources keys
                   :updated-items (count result)}
                  predicate-result))
               (merge
                {:name sched-name
                 :status :skipped
                 :reason :no-matching-sources
                 :period period
                 :source-count 0
                 :sources []}
                predicate-result)))
           (catch Object _
             (let [throwable (:throwable &throw-context)
                   keys @matched-keys]
               (log/error throwable "remove-unread-tags: autoread rule failed" sched-name)
               (merge
                {:name sched-name
                 :status :error
                 :period period
                 :source-count (count keys)
                 :sources keys
                 :error (throwable-error throwable)}
                predicate-result))))))))

(defn remove-unread-tags-status [autoread-results digest-result]
  (if (or (some #(= :error (:status %)) autoread-results)
          (= :error (:status digest-result)))
    :error
    :ok))

(defsched remove-unread-tags :early-morning
  (let [autoread-results
        (doall
         (for [[sched-name sched] (config/get-autoread-scheds)]
           (remove-unread-for-autoread-sched! sched-name sched)))
        ;; Digest: issue-windowed autoread. Keep the most recent
        ;; keep-unread-issues issues :unread; clear :unread on older issues.
        digest-result
        (when (rc/rc [:digest :enabled?])
          (let [keep-recent (rc/rc [:digest :keep-unread-issues])
                stale (digest/issues-outside-window keep-recent)]
            (try+
             (log/infof "remove-unread-tags: digest issues outside latest %d to clear: %s"
                        keep-recent stale)
             (let [results (doall
                            (for [i stale]
                              (persistency/remove-unread-for-items-with-tag!
                               store/backend-db
                               (digest/issue-tag i))))]
               {:status :ok
                :keep-recent keep-recent
                :cleared-issue-count (count stale)
                :cleared-issues stale
                :updated-items (reduce + (map count results))})
             (catch Object _
               (let [throwable (:throwable &throw-context)]
                 (log/error throwable "remove-unread-tags: digest cleanup failed")
                 {:status :error
                  :keep-recent keep-recent
                  :cleared-issue-count (count stale)
                  :cleared-issues stale
                  :error (throwable-error throwable)})))))]
    {:status (remove-unread-tags-status autoread-results digest-result)
     :autoread autoread-results
     :digest digest-result}))

(defmacro defsched-feed-by-filter
  "Schedule updates for all fetchable sources matching a predicate."
  {:llar.config/kind :construct
   :llar.config/form "(sched-fetch SCHED-NAME CHIME-TIMES PREDICATE)"
   :llar.config/order 30
   :llar.config/keys ["PREDICATE can use the source predicate bindings"
                      "CHIME-TIMES can be a canned schedule keyword or a chime time sequence"]
   :llar.config/example "(sched-fetch my-feeds :now-and-hourly\n  (some #{:my-feed-group} $TAGS))"}
  [sched-name chime-times pred]
  (let [source-key (gensym "source-key")
        source (gensym "source")]
    `(defstate ~sched-name
       :start (let [schedule# (sched/make-schedule
                               {:key (keyword '~sched-name)
                                :mount-state ~(str "#'" (ns-name *ns*) "/" sched-name)
                                :sched-name (str '~sched-name)
                                :chime-times (when (keyword? ~chime-times) ~chime-times)
                                :sched-type :update-feed-by-filter
                                :pred (quote ~pred)
                                :run-fn (fn []
                                          (let [sources# (updateable-sources)
                                                filtered# (filter (fn [[~source-key ~source]]
                                                                    (let [~@(config/source-predicate-let-bindings source-key source)]
                                                                      ~pred))
                                                                  sources#)
                                                keys# (mapv first filtered#)
                                                results# (doall (pmap update! keys#))]
                                            (log/infof "Scheduled feed update %s: %s"
                                                       '~sched-name (vec (interleave keys# results#)))
                                            {:keys keys#
                                             :results results#
                                             :count (count keys#)}))})
                    started# (sched/start-schedule! schedule# ~chime-times)]
                started#)
       :stop (.close ~sched-name))))
