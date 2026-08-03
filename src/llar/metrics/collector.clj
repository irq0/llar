(ns llar.metrics.collector
  "Building blocks for Prometheus collectors that sample at scrape time.

  Sampling means no polling loop and no staleness: the value is computed when /metrics
  is read. These live in one place mainly so the `collect` arity trap is encoded once -
  see `sampling-collector`."
  (:import
   [io.prometheus.client Collector Collector$Type
    Collector$MetricFamilySamples Collector$MetricFamilySamples$Sample]))

(def gauge Collector$Type/GAUGE)
(def counter Collector$Type/COUNTER)

(defn sample
  "One sample of `metric`. `label-names` and `label-values` must line up."
  [metric label-names label-values value]
  (Collector$MetricFamilySamples$Sample. metric label-names label-values (double value)))

(defn family
  "A metric family, or nil when there is nothing to report - an empty family is noise.

  Note simpleclient strips a `_total` suffix from COUNTER *family* names while keeping
  it on the samples, so the family name and the exported line can differ."
  [metric type help samples]
  (let [samples (vec (remove nil? samples))]
    (when (seq samples)
      (Collector$MetricFamilySamples. metric type help samples))))

(defn sampling-collector
  "A Collector that calls `collect-fn` on every scrape. `collect-fn` returns a seq of
  families; nils are dropped so callers can build them unconditionally.

  Both `collect` arities have to be implemented. `proxy` shadows every arity of a method
  name, and CollectorRegistry always enumerates through collect(Predicate) - so
  implementing only collect() turns every scrape of /metrics into an ArityException."
  ^Collector [collect-fn]
  (proxy [Collector] []
    (collect
      ([]
       (into [] (remove nil?) (collect-fn)))
      ([sample-name-filter]
       (let [families (into [] (remove nil?) (collect-fn))]
         (if (nil? sample-name-filter)
           families
           (into []
                 (keep (fn [^Collector$MetricFamilySamples fam]
                         (.filter fam sample-name-filter)))
                 families)))))))
