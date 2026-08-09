(ns llar.metrics.bookmark-capture
  "Scrape-time health metrics for the durable bookmark capture queue."
  (:require
   [clojure.tools.logging :as log]
   [iapetos.registry :as registry]
   [llar.db.bookmark-capture :as capture-db]
   [llar.metrics :as metrics]
   [llar.metrics.collector :as collector]
   [llar.store :as store]))

(def ^:private items-metric "llar_bookmark_queue_items")
(def ^:private oldest-metric "llar_bookmark_queue_oldest_ready_unixtime")

(defn database-snapshot []
  (when (instance? llar.db.core.PostgresqlDataStore store/backend-db)
    {:counts (capture-db/operational-counts store/backend-db)
     :oldest-ready (capture-db/oldest-ready store/backend-db)}))

(defn- epoch-second [timestamp]
  (cond
    (instance? java.time.ZonedDateTime timestamp)
    (.toEpochSecond ^java.time.ZonedDateTime timestamp)

    (instance? java.time.OffsetDateTime timestamp)
    (.toEpochSecond ^java.time.OffsetDateTime timestamp)

    (instance? java.time.Instant timestamp)
    (.getEpochSecond ^java.time.Instant timestamp)

    :else nil))

(defn- families [sample-fn]
  (try
    (when-let [{:keys [counts oldest-ready]} (sample-fn)]
      [(collector/family
        items-metric collector/gauge
        "Bookmark captures by actionable operational state."
        (map (fn [{:keys [state count]}]
               (collector/sample items-metric ["state"] [state] count))
             counts))
       (when-let [epoch (epoch-second oldest-ready)]
         (collector/family
          oldest-metric collector/gauge
          "Unix time when the oldest currently ready bookmark capture became runnable."
          [(collector/sample oldest-metric [] [] epoch)]))])
    (catch Throwable throwable
      (log/warn throwable "bookmark capture metric sampling failed; omitting queue metrics")
      [])))

(defn make-collector [sample-fn]
  (collector/sampling-collector #(families sample-fn)))

(defonce collector
  (doto (make-collector database-snapshot)
    (.register (registry/raw metrics/prom-registry))))
