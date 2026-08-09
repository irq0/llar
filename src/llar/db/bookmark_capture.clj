(ns llar.db.bookmark-capture
  (:require
   [clojure.set :as set]
   [llar.db.sql :as sql]
   [next.jdbc :as jdbc]))

(def ^:private capture-keys
  {:url_fingerprint :url-fingerprint
   :attempt_count :attempt-count
   :next_attempt_ts :next-attempt-ts
   :lease_expires_ts :lease-expires-ts
   :lease_version :lease-version
   :item_id :item-id
   :submitted_by :submitted-by
   :failure_class :failure-class
   :last_error :last-error
   :created_ts :created-ts
   :updated_ts :updated-ts
   :last_attempt_ts :last-attempt-ts
   :completed_ts :completed-ts})

(defn- normalize-capture [capture]
  (some-> capture
          (set/rename-keys capture-keys)
          (update :status keyword)))

(defn enqueue!
  "Insert a durable capture without mutating an existing URL capture."
  [db capture]
  (jdbc/with-transaction [tx (:datasource db)]
    (normalize-capture
     (or (sql/insert-bookmark-capture tx capture)
         (sql/get-bookmark-capture-by-fingerprint
          tx (select-keys capture [:url-fingerprint]))))))

(defn by-id [db id]
  (normalize-capture (sql/get-bookmark-capture db {:id id})))

(defn claim-next! [db lease-seconds]
  (normalize-capture
   (sql/claim-next-bookmark-capture db {:lease-seconds lease-seconds})))

(defn set-item! [db id lease-version item-id]
  (normalize-capture
   (sql/set-bookmark-capture-item db {:id id
                                      :lease-version lease-version
                                      :item-id item-id})))

(defn complete! [db id lease-version item-id]
  (normalize-capture
   (sql/complete-bookmark-capture db {:id id
                                      :lease-version lease-version
                                      :item-id item-id})))

(defn reschedule! [db id lease-version delay-seconds failure-class last-error]
  (normalize-capture
   (sql/reschedule-bookmark-capture
    db {:id id
        :lease-version lease-version
        :delay-seconds delay-seconds
        :failure-class failure-class
        :last-error last-error})))

(defn fail! [db id lease-version failure-class last-error]
  (normalize-capture
   (sql/fail-bookmark-capture
    db {:id id
        :lease-version lease-version
        :failure-class failure-class
        :last-error last-error})))

(defn retry! [db id]
  (normalize-capture (sql/retry-bookmark-capture db {:id id})))

(defn dismiss! [db id]
  (normalize-capture (sql/dismiss-bookmark-capture db {:id id})))

(defn list-captures [db limit]
  (mapv normalize-capture (sql/list-bookmark-captures db {:limit limit})))

(defn operational-counts [db]
  (sql/bookmark-capture-operational-counts db))

(defn oldest-ready [db]
  (:ready_since (sql/bookmark-capture-oldest-ready db)))

(defn dashboard-counts [db]
  (some-> (sql/bookmark-capture-dashboard-counts db)
          (set/rename-keys {:retry_wait :retry-wait})))
