(ns u1f596.update
  (:require
   [u1f596.config :as config]
   [u1f596.appconfig :as appconfig]
   [u1f596.fetch :as fetch]
   [u1f596.store :refer [store-items!]]
   [u1f596.postproc :as proc]
   [u1f596.converter :as converter]
   [java-time :as time]
   [clojure.edn :as edn]
   [slingshot.slingshot :refer [throw+ try+]]
   [taoensso.timbre :as log]
   [mount.core :refer [defstate]]
   [clojure.java.io :as io]
   [nio2.core :as nio2]))

;;;; Update - Combines fetch and persistency with additional state management
;;;; Source state is managed in the core/state atom.

(defn startup-read-state []
  (let [state-dir (appconfig/state-dir)
        state-file (.resolve state-dir "state.edn")
        backup-file (.resolve state-dir
                              (str "u1f596_state.edn."
                                   (time/format :iso-instant (time/zoned-date-time))))]
    (log/info "Using state file" state-file)
    (when (nio2/exists? state-file)
      (log/info "State file exists. Creating backup copy in " backup-file)
      (nio2/copy-file state-file backup-file))
    (try+
     (converter/read-edn-state (slurp state-file))
     (catch java.lang.RuntimeException _
       (log/warn "Failed to read state file. Starting with clean state")
       {}))))

(defstate state
  :start (atom (startup-read-state))
  :stop (spit (.resolve (appconfig/state-dir) "state.edn") (converter/print-state @state)))

(defn get-current-state []
  (into {}
        (map (fn [[k v]]
               (if (instance? clojure.lang.Atom v)
                 [k @v]
                 [k v]))
             @state)))

(defn sources-merge-in-state [sources]
  (merge-with merge sources (get-current-state)))

(def src-state-template
  "New sources start with this template"
  {:key nil
   :last-successful-fetch-ts nil
   :last-attempt-ts nil
   :forced-update? false
   :status :new
   :last-exception nil
   :retry-count 0})

(defn- make-next-state [state next-status next-retry-count last-exception]
  (let [now (time/zoned-date-time)]
        (merge state
               {:last-attempt-ts now
                :status next-status
                :last-exception last-exception
                :retry-count next-retry-count})))

(defn- update-feed!
  "Update feed. Return new state"
  [k & {:keys [skip-proc skip-store overwrite?]
        :or {skip-proc false
             skip-store false
             overwrite? false}}]

  (let [feed (config/get-source k)
        state (get @state k)
        now (time/zoned-date-time)
        {:keys [src]} feed
        retry-count (or (get-in feed [:state :retry-count]) 0)]
    (try+
     (let [fetched (fetch/fetch feed)
           processed (try+
                      (if-not skip-proc
                        (proc/process feed state fetched)
                        fetched)
                      (catch Object _
                        (throw+ {:type ::proc-error
                                 :fetched (map #(select-keys %
                                                             [:summary :meta])
                                               fetched)
                                 :feed feed
                                 :skip skip-proc})))

           dbks (try+
                 (if-not skip-store
                   (store-items! processed :overwrite? overwrite?)
                   processed)
                 (catch Object _
                   (throw+ {:type ::store-error
                            :feed feed
                            :fetched fetched
                            :processed processed
                            :skip skip-store})))]

       (log/infof "Updated %s: fetched: %d, after processing: %d, new in db: %d (skip-proc: %s, skip-store: %s)"
                  (str src) (count fetched) (count processed) (count dbks)
                  skip-proc skip-store)

       (-> (make-next-state state :ok 0 nil)
           (assoc :last-successful-fetch-ts (time/zoned-date-time))))

     (catch [:type :u1f596.http/server-error-retry-later] _
       (make-next-state state :temp-fail (inc retry-count) &throw-context))

     (catch [:type :u1f596.http/request-error] _
       (make-next-state state :perm-fail 0 &throw-context))

     (catch [:type :u1f596.http/unexpected-error] _
       (make-next-state state :bug 0 &throw-context))

     (catch java.lang.OutOfMemoryError ex
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
  (doseq [[k v] (config/get-sources)]
    (set-status! k :new)))

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
      (log/debug "Updating new feed: " k)
      :ok
      (log/debug "Updating working feed: " k)
      :temp-fail
      (log/debug "Temporary failing feed %d/%d: %s"
                 (:retry-count cur-state) (appconfig/update-max-retry) k)
      :perm-fail
      (log/debug "Skipping perm fail feed: " k)

      :bug
      (log/debug "Skipping feed that triggerd a bug: " k)

      (log/debugf "Unknown status \"%s\": %s" cur-status k))

    (when force
      (log/debugf "Force updating %s feed %s" cur-status k))

    (let [update? (or force
                      (#{:ok :new} cur-status)
                      (and (= cur-status :temp-fail)
                           (< (:retry-count cur-state) (appconfig/update-max-retry))))]
      (if update?
        (do
          (swap! state update k assoc :status :updating)
          (let [kw-args (mapcat identity (dissoc args :force))
                new-state (apply update-feed! k kw-args)
                new-status (:status new-state)]
            (log/debugf "[%s] State: %s -> %s " k
                        cur-status new-status)
            (swap! state (fn [current]
                           (assoc current k new-state)))
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
  (let [failed-source-keys (map key (filter (fn [[k v]]
                                              (contains? #{:perm-fail :temp-fail} (:status v ) ))
                                            @state))
        failed-sources (select-keys (updateable-sources) failed-source-keys)]
    (doall (pmap #(apply update! (key %) args) failed-sources))))


(defn update-bugged! [& args]
  (let [failed-source-keys (map key (filter (fn [[k v]]
                                              (contains? #{:bug} (:status v ) ))
                                            @state))
        failed-sources (select-keys (updateable-sources) failed-source-keys)]
    (doall
     (map #(apply update! (key %) args) failed-sources))))

(defn update-unfetched! [& args]
  (let [sources-and-state (merge-with merge (updateable-sources) @state)
        unfetched-keys (map key (filter (fn [[k v]] (nil? (:status v))) sources-and-state))
        result (doall (pmap #(apply update! % args) unfetched-keys))]
    result))
