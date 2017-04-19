(ns infowarss.update
  (:require
   [infowarss.core :refer [*srcs* *state* *update-max-retires*]]
   [infowarss.fetch :as fetch]
   [infowarss.persistency :refer [store-items! duplicate?]]
   [infowarss.postproc :as proc]
   [clj-time.core :as time]
   [slingshot.slingshot :refer [throw+ try+]]
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]))


(def src-state-template
  {:last-successful-fetch-ts nil
   :last-attempt-ts nil
   :status :new
   :update-lock (Object.)
   :last-exception nil
   :retry-count 0})

(defn- update-feed!
  "Update feed. Return new state"
  [k & {:keys [skip-proc skip-store]
           :or {skip-proc false skip-store false}}]

  (let [feed (get @*srcs* k)
        state (get @*state* k)
        now (time/now)
        {:keys [src]} feed]
    (try+
      (let [fetched (fetch/fetch feed)
            processed (if-not skip-proc
                        (proc/process feed fetched)
                        fetched)
            dbks (if-not skip-store
                   (store-items! processed)
                   processed)]

        (log/infof "Updating %s: fetched: %d, after processing: %d, new in db: %d (skip-proc: %s, skip-store: %s)"
          (str src) (count fetched) (count processed) (count dbks)
          skip-proc skip-store)

        (merge state
          {:last-attempt-ts now
           :last-successful-fetch-ts now
           :status :ok
           :retry-count 0}))

      (catch [:type ::server-error-retry-later] _
        (merge state
          {:last-attempt-ts now
           :status :temp-fail
           :last-exception &throw-context
           :retry-count (inc (get-in feed [:state :retry-count]))}))

      (catch [:type ::request-error] _
        (merge state
          {:last-attempt-ts now
           :status :perm-fail
           :last-exception &throw-context
           :retry-count 0}))

      (catch [:type ::unexpected-error] _
        (merge state
          {:last-attempt-ts now
           :status :perm-fail
           :last-exception &throw-context
           :retry-count 0}))

      (catch java.net.ConnectException _
        (log/error "Connection error: "  &throw-context)
        (merge state
          {:last-attempt-ts now
           :status :temp-fail
           :last-exception &throw-context
           :retry-count 0}))

      (catch Object _
        (log/error "Unexpected error: "  &throw-context)
        (merge state
          {:last-attempt-ts now
           :status :perm-fail
           :last-exception &throw-context
           :retry-count 0})))))

(defn set-status! [k new-status]
  (let [src (get @*state* k)]
    (when (contains? src k)
      (swap! *state* (fn [current]
                      (assoc-in current [k :status] new-status)))
      new-status)))

(defn reset-all-failed! []
  (doseq [[k v] @*srcs*]
    (set-status! k :new)))




(defn update!
  "Update feed by id (see: *srcs*)"
  [k & {:keys [force skip-proc skip-store]
        :as args}]


  (when-not (contains? @*state* k)
    (swap! *state* assoc k src-state-template))

    ;; don't update the same feed in parallel
  (locking (get-in @*state* [k :update-lock])
    (let [cur-state (get @*state* k)
          cur-status (:status cur-state)]

      (condp = cur-status
        :new
        (log/info "Updating new feed: " k)
        :ok
        (log/info "Updating working feed: " k)
        :temp-fail
        (log/info "Temporary failing feed %d/%d: %s"
          (:retry-count cur-state) *update-max-retires* k)
        :perm-fail
        (log/info "Skipping perm fail feed: " k)
        (log/infof "Unknown status \"%s\": %s" cur-status k))

      (when force
        (log/infof "Force updating %s feed %s" cur-status k))

      (when (or
              force
              (#{:ok :new} cur-status)
              (and
                (= cur-status :temp-fail)
                (< (:retry-count cur-state) *update-max-retires*)))
        (let [kw-args (mapcat identity (dissoc args :force))
              new-state (apply update-feed! k kw-args)
              new-status (:status new-state)]
          (log/infof "[%s] State: %s -> %s " k
            cur-status new-status)
          (swap! *state* (fn [current]
                           (assoc current k new-state)))
          new-status)))))

(defn update-all! [& args]
  (doall
    (for [[k v] @*srcs*]
      (apply update! k args))))
