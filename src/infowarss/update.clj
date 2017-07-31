(ns infowarss.update
  (:require
   [infowarss.core :refer [*srcs* config]]
   [infowarss.fetch :as fetch]
   [infowarss.persistency :refer [store-items! duplicate?]]
   [infowarss.postproc :as proc]
   [infowarss.converter :as converter]
   [clj-time.core :as time]
   [slingshot.slingshot :refer [throw+ try+]]
   [taoensso.timbre :as log]
   [hara.io.scheduler :as sched]
   [mount.core :refer [defstate]]
   [clojure.java.io :as io]
   [hara.time.joda]
   [taoensso.timbre.appenders.core :as appenders]))

;;;; Update - Combines fetch and persistency with additional state management
;;;; Source state is managed in the core/state atom.

(defstate state
  :start (atom (converter/read-edn-string (slurp (io/resource "state.edn"))))
  :stop (spit (io/resource "state.edn") (prn-str @state)))

(def src-state-template
  "New sources start with this template"
  {:key nil
   :last-successful-fetch-ts nil
   :last-attempt-ts nil
   :status :new
   :update-lock (Object.)
   :last-exception nil
   :retry-count 0})

(defn- update-feed!
  "Update feed. Return new state"
  [k & {:keys [skip-proc skip-store overwrite?]
        :or {skip-proc false
             skip-store false
             overwrite? false}}]

  (let [feed (get *srcs* k)
        state (get @state k)
        now (time/now)
        {:keys [src]} feed]
    (try+
      (let [fetched (fetch/fetch feed)
            processed (if-not skip-proc
                        (proc/process feed state fetched)
                        fetched)
            dbks (if-not skip-store
                   (store-items! processed :overwrite? overwrite?)
                   processed)]

        (log/infof "Updating %s: fetched: %d, after processing: %d, new in db: %d (skip-proc: %s, skip-store: %s)"
          (str src) (count fetched) (count processed) (count dbks)
          skip-proc skip-store)

        (merge state
          {:last-attempt-ts now
           :last-successful-fetch-ts now
           :status :ok
           :retry-count 0}))

      (catch [:type :infowarss.fetch/server-error-retry-later] _
        (merge state
          {:last-attempt-ts now
           :status :temp-fail
           :last-exception &throw-context
           :retry-count (inc (get-in feed [:state :retry-count]))}))

      (catch [:type :infowarss.fetch/request-error] _
        (merge state
          {:last-attempt-ts now
           :status :perm-fail
           :last-exception &throw-context
           :retry-count 0}))

      (catch [:type :infowarss.fetch/unexpected-error] _
        (merge state
          {:last-attempt-ts now
           :status :perm-fail
           :last-exception &throw-context
           :retry-count 0}))

      (catch java.net.ConnectException _
        (log/warn &throw-context "Connection error (-> temp-fail) for" (str src))
        (merge state
          {:last-attempt-ts now
           :status :temp-fail
           :last-exception &throw-context
           :retry-count 0}))

      (catch Object _
        (log/error &throw-context "Unexpected error (-> perm-fail) for " (str src))
        (merge state
          {:last-attempt-ts now
           :status :perm-fail
           :last-exception &throw-context
           :retry-count 0})))))

(defn set-status!
  "Set feed's status"
  [k new-status]
  (let [src (get @state k)]
    (when (contains? src k)
      (swap! state (fn [current]
                      (assoc-in current [k :status] new-status)))
      new-status)))

(defn reset-all-failed!
  "Reset all feed states to :new"
  []
  (doseq [[k v] *srcs*]
    (set-status! k :new)))

;;; Update API

(defn update!
  "Update feed by id (see: *srcs*)"
  [k & {:keys [force skip-proc skip-store overwrite?]
        :as args}]

  (when (nil? (get *srcs* k))
    (throw+ {:type ::unknown-source-key :key k :known-keys (keys *srcs*)}))

  (when-not (satisfies? fetch/FetchSource (get-in *srcs* [k :src]))
    (throw+ {:type ::source-not-fetchable :key k}))

  (when-not (contains? @state k)
    (swap! state assoc k (assoc src-state-template :key k)))

    ;; don't update the same feed in parallel
  (locking (get-in @state [k :update-lock])
    (let [cur-state (get @state k)
          cur-status (:status cur-state)]

      (condp = cur-status
        :new
        (log/debug "Updating new feed: " k)
        :ok
        (log/debug "Updating working feed: " k)
        :temp-fail
        (log/debug "Temporary failing feed %d/%d: %s"
          (:retry-count cur-state) (:update-max-retires config) k)
        :perm-fail
        (log/debug "Skipping perm fail feed: " k)
        (log/debugf "Unknown status \"%s\": %s" cur-status k))

      (when force
        (log/debugf "Force updating %s feed %s" cur-status k))

      (if (or
              force
              (#{:ok :new} cur-status)
              (and
                (= cur-status :temp-fail)
                (< (:retry-count cur-state) (:update-max-retires config))))
        (let [kw-args (mapcat identity (dissoc args :force))
              new-state (apply update-feed! k kw-args)
              new-status (:status new-state)]
          (log/debugf "[%s] State: %s -> %s " k
            cur-status new-status)
          (swap! state (fn [current]
                           (assoc current k new-state)))
          new-status)
        cur-status))))

(defn update-all! [& args]
  (doall
    (for [[k v] *srcs*
          :when (satisfies? fetch/FetchSource (:src v))]
      (apply update! k args))))

(defn update-matching! [re & args]
  (doall
    (for [[k v] *srcs*
          :when (and (satisfies? fetch/FetchSource (:src v))
                  (re-find re (name k)))]
      (apply update! k args))))
