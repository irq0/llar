(ns infowarss.update
  (:require
   [infowarss.config :as config]
   [infowarss.fetch :as fetch]
   [infowarss.persistency :refer [store-items!]]
   [infowarss.postproc :as proc]
   [infowarss.converter :as converter]
   [java-time :as time]
   [clojure.edn :as edn]
   [slingshot.slingshot :refer [throw+ try+]]
   [taoensso.timbre :as log]
   [mount.core :refer [defstate]]
   [clojure.java.io :as io]))

;;;; Update - Combines fetch and persistency with additional state management
;;;; Source state is managed in the core/state atom.

(def config (edn/read-string (slurp (io/resource "config.edn"))))

(defn startup-read-state []
  (let [res (io/resource "state.edn")
        backup (io/file (str "/tmp/infowarss_state.edn." (time/format :iso-instant (time/zoned-date-time))))]
    (log/info "Reading feed state file. Creating backup in " backup)
    (io/copy (io/file (.getFile res)) backup)
    (try+
     (converter/read-edn-state (slurp res))
     (catch java.lang.RuntimeException _
       (log/warn "Failed to read state file. Starting with clean state")
       {}))))

(defstate state
  :start (atom (startup-read-state))
  :stop (spit (io/resource "state.edn") (converter/print-state @state)))

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

(defn- update-feed!
  "Update feed. Return new state"
  [k & {:keys [skip-proc skip-store overwrite?]
        :or {skip-proc false
             skip-store false
             overwrite? false}}]

  (let [feed (get config/*srcs* k)
        state (get @state k)
        now (time/zoned-date-time)
        {:keys [src]} feed]
    (try+
     (let [fetched (try+
                    (fetch/fetch feed)
                    (catch Object _
                      (throw+ {:type ::fetch-error
                               :feed feed})))

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
       (log/warn (:throwable &throw-context) "Connection error (-> temp-fail) for" (str src))
       (merge state
              {:last-attempt-ts now
               :status :temp-fail
               :last-exception &throw-context
               :retry-count 0}))

     (catch Object _
       (log/error (:throwable &throw-context) "Unexpected error (-> perm-fail) for " (str src) src)
       (merge state
              {:last-attempt-ts now
               :status :perm-fail
               :last-exception &throw-context
               :retry-count 0})))))

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
  (doseq [[k v] config/*srcs*]
    (set-status! k :new)))

;;; Update API

(defn update!
  "Update feed by id (see: *srcs*)"
  [k & {:keys [force]
        :as args}]

  (when (nil? (get config/*srcs* k))
    (throw+ {:type ::unknown-source-key :key k :known-keys (keys config/*srcs*)}))

  (when-not (satisfies? fetch/FetchSource (get-in config/*srcs* [k :src]))
    (throw+ {:type ::source-not-fetchable
             :key k
             :src (get-in config/*srcs* [k :src])
             :src-type (type (get-in config/*srcs* [k :src]))}))

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
      cur-status)))

(defn update-all! [& args]
  (doall
   (for [[k v] (shuffle (vec config/*srcs*))
         :when (satisfies? fetch/FetchSource (:src v))]
     (apply update! k args))))

(defn update-matching! [re & args]
  (doall
   (for [[k v] config/*srcs*
         :when (and (satisfies? fetch/FetchSource (:src v))
                    (re-find re (name k)))]
     (apply update! k args))))
