(ns llar.metrics.resources
  "Sampling Prometheus collector for bounded resources.

  Executors, throttles and connection pools register a `sample-fn` here. The collector
  calls them from `collect`, i.e. at scrape time, so values are always live and there is
  no polling loop to keep in sync.

  Wait times deliberately do not live here: they cannot be sampled from a snapshot and
  must be observed at acquire time by the resource itself."
  (:require
   [clojure.tools.logging :as log]
   [iapetos.registry :as registry]
   [llar.metrics :as metrics]
   [llar.metrics.collector :as collector]))

(def ^:private +label-names+ ["resource" "kind"])

(def ^:private +families+
  "Measure key -> metric family. Order determines /metrics output order."
  [{:measure :in-use
    :metric "llar_resource_in_use"
    :type collector/gauge
    :help "Currently occupied slots of a bounded resource."}
   {:measure :limit
    :metric "llar_resource_limit"
    :type collector/gauge
    :help "Configured capacity of a bounded resource."}
   {:measure :queued
    :metric "llar_resource_queued"
    :type collector/gauge
    :help "Work waiting for a bounded resource to free up."}
   {:measure :completed
    :metric "llar_resource_completed_total"
    :type collector/counter
    :help "Work units a bounded resource has completed."}])

(defn make-resources
  "A registry of bounded resources, independent of the global one."
  []
  (atom {}))

(defn register!
  "Register `resource-name` so every scrape calls `sample-fn`.

  `sample-fn` returns a map of measure -> number, using any of :in-use, :limit,
  :queued and :completed. Measures it omits are not emitted."
  [resources* resource-name kind sample-fn]
  (swap! resources* assoc resource-name {:kind kind :sample-fn sample-fn})
  resource-name)

(defn unregister! [resources* resource-name]
  (swap! resources* dissoc resource-name)
  resource-name)

(defn- sample-resource [resource-name {:keys [kind sample-fn]}]
  (try
    {:resource (metrics/label-value resource-name)
     :kind (metrics/label-value kind)
     :measures (sample-fn)}
    (catch Throwable t
      ;; A single broken sampler must never take down /metrics.
      (log/warnf t "resource sampler failed, omitting from scrape: %s" resource-name)
      nil)))

(defn sample-all
  "Sample every registered resource, dropping any whose sampler threw."
  [resources*]
  (into [] (keep (fn [[resource-name resource]]
                   (sample-resource resource-name resource)))
        @resources*))

(defn- family [{:keys [measure metric type help]} samples]
  (collector/family
   metric type help
   (map (fn [{:keys [resource kind measures]}]
          (when-let [value (get measures measure)]
            (collector/sample metric +label-names+ [resource kind] value)))
        samples)))

(defn make-collector
  "A Prometheus collector that samples `resources*` on every scrape."
  [resources*]
  (collector/sampling-collector
   (fn []
     (let [samples (sample-all resources*)]
       (map #(family % samples) +families+)))))

;; The app-wide registry. Pools and throttles register themselves here at start, and
;; `collector` publishes them on /metrics for as long as they stay registered.
(defonce resources
  (make-resources))

(defonce collector
  (doto (make-collector resources)
    (.register (registry/raw metrics/prom-registry))))
