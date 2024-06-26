(ns llar.update
  (:require
   [clojure.tools.logging :as log]
   [java-time.api :as time]
   [mount.core :refer [defstate]]
   [slingshot.slingshot :refer [throw+ try+]]
   [chime.core :as chime]
   [llar.metrics :as metrics]
   [llar.appconfig :as appconfig]
   [llar.config :as config]
   [llar.converter :as converter]
   [iapetos.core :as prometheus]
   [llar.fetch :as fetch]
   [llar.postproc :as proc]
   [llar.sched :refer [defsched] :as sched]
   [llar.store :as store :refer [store-items!]]
   [llar.persistency :as persistency]))

;;;; Update - Combines fetch and persistency with additional state management
;;;; Source state is managed in the core/state atom.

(defonce prom-registry
  (-> metrics/prom-registry
      (prometheus/subsystem "update")
      (prometheus/register
       (prometheus/gauge :llar/update-duration-millis
                         {:description "Time it took to fetch and process a source"
                          :labels [:source]})
       (prometheus/gauge :llar/statuses
                         {:description "Number of fetched sources for each status"
                          :labels [:status]})
       (prometheus/gauge :llar/last-success-unixtime
                         {:description "Last time the source was successfully fetched"
                          :labels [:source]}))))

(def +available-states+
  [:ok :new :temp-fail :perm-fail :bug :updating])

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
                                         :skip skip-proc}))))

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

(defn- observe-state-summary-metrics []
  (let [states (group-by :status (vals (get-current-state)))]
    (doseq [status +available-states+]
      (prometheus/observe prom-registry :llar/statuses
                          {:status (name status)}
                          (count (get states status []))))))

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
                 (:retry-count cur-state) (appconfig/update-max-retry) k)
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
                           (< (:retry-count cur-state) (appconfig/update-max-retry))))]
      (if update?
        (do
          (swap! state update k assoc :status :updating)
          (swap! state update k assoc :last-attempt-ts (time/zoned-date-time))
          (let [kw-args (mapcat identity (dissoc args :force))
                new-state (apply update-feed! k kw-args)
                new-status (:status new-state)]
            (when-let [dur (:last-duration new-state)]
              (prometheus/observe prom-registry
                                  :llar/update-duration-millis
                                  {:source (str k)}
                                  (.toMillis dur)))
            (when-let [success-ts (:last-successful-fetch-ts new-state)]
              (prometheus/observe prom-registry :llar/last-success-unixtime
                                  {:source (str k)}
                                  (/ (time/to-millis-from-epoch success-ts) 1000)))
            (log/debugf "[%s] State: %s -> %s " k
                        cur-status new-status)
            (swap! state (fn [current]
                           (assoc current k new-state)))
            (observe-state-summary-metrics)
            new-status))
        cur-status))))

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

(defsched remove-unread-tags :early-morning
  (doseq [[sched-name {:keys [period pred]}] (config/get-autoread-scheds)
          :let [sources (updateable-sources)
                filtered (filter pred sources)
                keys (mapv first filtered)]]
    (log/infof "remove-unread-tags: %s keys to untag if older then %s: %s"
               sched-name period keys)
    (persistency/remove-unread-for-items-of-source-older-then!
     store/backend-db
     keys
     (time/minus (time/zoned-date-time) period))))

(defmacro defsched-feed-by-filter [sched-name chime-times pred]
  `(defstate ~sched-name
     :start (vary-meta (chime/chime-at
                        (sched/resolve-chime-times ~chime-times)
                        (fn [~'$TIME]
                          (prometheus/set-to-current-time  ~'llar.metrics/prom-registry
                                                           :llar-sched/last-run
                                                           {:schedule (str '~sched-name)})
                          (metrics/with-log-exec-time-named ~sched-name
                            (let [sources# (updateable-sources)
                                  filtered# (filter (fn [[k# source#]]
                                                      (let [~'$KEY k#
                                                            ~'$SRC (:src source#)
                                                            ~'$TAGS (:tags source#)]
                                                        ~pred))
                                                    sources#)
                                  keys# (mapv first filtered#)
                                  result# (pmap update! keys#)]
                              (log/infof "Scheduled feed update %s: %s"
                                         '~sched-name (vec (interleave keys# result#)))))))
                       merge
                       {:sched-name (str '~sched-name)
                        :chime-times (when (keyword? ~chime-times) ~chime-times)
                        :sched-type :update-feed-by-filter
                        :pred (quote ~pred)})
     :stop (.close ~sched-name)))
