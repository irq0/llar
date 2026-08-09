(ns llar.bookmark-capture
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [llar.appconfig :as appconfig]
   [llar.db.bookmark-capture :as capture-db]
   [llar.fetch :as fetch]
   [llar.fetch.bookmark :as bookmark]
   [llar.item :as item]
   [llar.metrics :as metrics]
   [llar.persistency :as persistency]
   [llar.pool :as pool]
   [llar.postproc :as proc]
   [llar.privacy :as privacy]
   [llar.sched :refer [defsched]]
   [llar.store :as store]
   [llar.update :as update]
   [llar.work :as work])
  (:import
   [java.net URI]))

(def ^:private lease-seconds 1200)
(def ^:private max-attempts 5)
(def ^:private retry-delay-seconds [60 300 1800 7200])

(defn normalize-url [url]
  (let [url (some-> url str string/trim)]
    (when-not (and (not (string/blank? url))
                   (privacy/external-http-url? url))
      (throw (ex-info "Capture URL must be an absolute HTTP or HTTPS URL"
                      {:type ::invalid-url :url url})))
    (let [normalized (privacy/strip-tracking-params url)
          parsed (URI. normalized)]
      (when (or (nil? (.getHost parsed))
                (string/blank? (.getHost parsed)))
        (throw (ex-info "Capture URL must contain a host"
                        {:type ::invalid-url :url url})))
      normalized)))

(defn enqueue!
  ([db url title submitted-by]
   (when-not (or (nil? title) (string? title))
     (throw (ex-info "Capture title must be a string"
                     {:type ::invalid-title})))
   (when (and (string? url) (> (count url) 8192))
     (throw (ex-info "Capture URL is too long"
                     {:type ::invalid-url :max-length 8192})))
   (let [url (normalize-url url)]
     (when (and title (> (count title) 1000))
       (throw (ex-info "Capture title is too long"
                       {:type ::invalid-title :max-length 1000})))
     (capture-db/enqueue!
      db {:url url
          :url-fingerprint (fetch/make-item-hash url)
          :title (some-> title string/trim not-empty)
          :submitted-by submitted-by}))))

(defn enqueue-outcome [capture]
  (cond
    (:inserted capture) :queued
    (#{:pending :processing} (:status capture)) :already-queued
    (= :complete (:status capture)) :already-saved
    :else :needs-attention))

(def outcome-messages
  {:queued "Saved to Llar for processing"
   :already-queued "This URL is already queued"
   :already-saved "This URL is already saved"
   :needs-attention "A previous capture failed; retry it from the dashboard"})

(defn- stored-item-id [results]
  (let [results (vec results)
        item-id (get-in results [0 :item :id])]
    (when-not (and (= 1 (count results)) (pos-int? item-id))
      (throw (ex-info "Bookmark store did not return exactly one item ID"
                      {:type ::store-result-invalid
                       :result-count (count results)
                       :results results})))
    item-id))

(defn- store-items! [db items]
  (->> items
       (map #(persistency/store-item! db (persistency/to-couch %)
                                      {:overwrite? true}))
       (remove nil?)
       doall))

(defn- ensure-saved-unread! [db item-id]
  (persistency/transition-item-state! db item-id :save)
  (persistency/transition-item-state! db item-id :mark-unread)
  item-id)

(defn- item-with-content [db item-id]
  (when-let [stored (and item-id (persistency/get-item-by-id db item-id))]
    (when (item/best-content stored) stored)))

(defn- ensure-raw-item! [db capture]
  (if (and (:item-id capture)
           (persistency/get-item-by-id db (:item-id capture)))
    (ensure-saved-unread! db (:item-id capture))
    (let [raw (bookmark/make-raw-bookmark
               (:url capture) (:title capture) (:created-ts capture))
          item-id (stored-item-id (store-items! db [raw]))]
      (ensure-saved-unread! db item-id)
      (when-not (capture-db/set-item! db (:id capture) (:lease-version capture) item-id)
        (throw (ex-info "Bookmark capture lost its processing lease before item linkage"
                        {:type ::lease-lost :capture-id (:id capture)})))
      item-id)))

(defn- enrich! [db capture]
  (let [feed (bookmark/make-readability-bookmark-feed
              (:url capture) (:created-ts capture))
        state (assoc update/src-state-template :key :bookmark)
        fetched (fetch/fetch feed)
        processed (vec (proc/process feed state fetched))]
    (when-not (= 1 (count processed))
      (throw (ex-info "Bookmark extraction did not produce exactly one item"
                      {:type ::extraction-result-invalid
                       :capture-id (:id capture)
                       :fetched-count (count fetched)
                       :processed-count (count processed)})))
    (stored-item-id (store-items! db processed))))

(defn- retry-delay [attempt-count]
  (nth retry-delay-seconds
       (min (dec attempt-count) (dec (count retry-delay-seconds)))))

(defn- failure-class [throwable]
  (metrics/label-value (metrics/exception-reason-class throwable)))

(defn- error-message [throwable]
  (let [message (or (ex-message throwable) (str throwable))]
    (subs message 0 (min 4000 (count message)))))

(defn- record-failure! [db capture throwable]
  (let [class (failure-class throwable)
        message (error-message throwable)]
    (if (>= (:attempt-count capture) max-attempts)
      (capture-db/fail! db (:id capture) (:lease-version capture) class message)
      (capture-db/reschedule! db (:id capture) (:lease-version capture)
                              (retry-delay (:attempt-count capture))
                              class message))))

(defn process-capture!
  "Process one already-claimed capture. Safe to repeat after a lost completion update."
  [db capture]
  (work/with-work {:kind :bookmark :source (:submitted-by capture) :stage :raw-store}
    (try
      (let [item-id (ensure-raw-item! db capture)]
        (if (item-with-content db item-id)
          (log/infof "bookmark capture %d already has enriched content" (:id capture))
          (do
            (work/stage! :fetch)
            (let [enriched-id (enrich! db capture)]
              (when-not (= item-id enriched-id)
                (throw (ex-info "Bookmark enrichment changed item identity"
                                {:type ::item-identity-changed
                                 :capture-id (:id capture)
                                 :raw-item-id item-id
                                 :enriched-item-id enriched-id}))))))
        (work/stage! :complete)
        (or (capture-db/complete! db (:id capture) (:lease-version capture) item-id)
            (throw (ex-info "Bookmark capture lost its processing lease before completion"
                            {:type ::lease-lost :capture-id (:id capture)}))))
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw e))
      (catch Throwable throwable
        (log/warn throwable "bookmark capture failed" (select-keys capture [:id :url :attempt-count]))
        (record-failure! db capture throwable)))))

(defn process-next! [db]
  (when-let [capture (capture-db/claim-next! db lease-seconds)]
    (process-capture! db capture)))

(defn run-scheduled! []
  (pool/call-on pool/source-pool #(process-next! store/backend-db)))

(defn- configured-schedule []
  (or (appconfig/capture :schedule) :now-and-every-minute))

(defsched bookmark-capture-scheduler (configured-schedule)
  (when (appconfig/capture)
    (run-scheduled!)))
