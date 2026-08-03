(ns llar.metrics.resources-test
  (:require
   [clojure.test :refer [deftest is]]
   [iapetos.registry :as registry]
   [llar.metrics :as metrics]
   [llar.metrics.resources :as uut])
  (:import
   [io.prometheus.client CollectorRegistry]
   [io.prometheus.client.exporter.common TextFormat]
   [java.io StringWriter]))

(defn- collect->data
  "Flatten collect() output into {sample-name #{[resource kind value]}} so tests can
  compare against plain Clojure data instead of building Sample objects.

  Keyed by sample name rather than family name: simpleclient strips the _total suffix
  from COUNTER family names but keeps it on the samples, and the sample name is what
  ends up in /metrics."
  [families]
  (reduce (fn [acc family]
            (reduce (fn [acc sample]
                      (update acc (.-name sample) (fnil conj #{})
                              (conj (vec (.-labelValues sample)) (.-value sample))))
                    acc
                    (.-samples family)))
          {}
          families))

(deftest collect-emits-a-sample-per-registered-measure
  (let [resources (uut/make-resources)
        collector (uut/make-collector resources)]
    (uut/register! resources :item-postproc :executor
                   (constantly {:in-use 3 :limit 8 :queued 2 :completed 100}))
    (let [data (collect->data (.collect collector))]
      (is (= #{["item_postproc" "executor" 3.0]} (get data "llar_resource_in_use")))
      (is (= #{["item_postproc" "executor" 8.0]} (get data "llar_resource_limit")))
      (is (= #{["item_postproc" "executor" 2.0]} (get data "llar_resource_queued")))
      (is (= #{["item_postproc" "executor" 100.0]}
             (get data "llar_resource_completed_total"))))))

(deftest collect-emits-every-registered-resource-into-one-family
  (let [resources (uut/make-resources)
        collector (uut/make-collector resources)]
    (uut/register! resources :source-update :executor (constantly {:in-use 1 :limit 4}))
    (uut/register! resources :streaming :throttle (constantly {:in-use 1 :limit 1}))
    (is (= #{["source_update" "executor" 1.0]
             ["streaming" "throttle" 1.0]}
           (get (collect->data (.collect collector)) "llar_resource_in_use")))))

(deftest throwing-sample-fn-does-not-break-the-scrape
  (let [resources (uut/make-resources)
        collector (uut/make-collector resources)]
    (uut/register! resources :healthy :executor (constantly {:in-use 1 :limit 2}))
    (uut/register! resources :broken :throttle (fn [] (throw (ex-info "boom" {}))))
    (let [data (collect->data (.collect collector))]
      (is (= #{["healthy" "executor" 1.0]} (get data "llar_resource_in_use")))
      (is (= #{["healthy" "executor" 2.0]} (get data "llar_resource_limit"))))))

(deftest measures-a-resource-does-not-report-are-omitted
  (let [resources (uut/make-resources)
        collector (uut/make-collector resources)]
    ;; a throttle has no completed count - the family must not appear with a bogus zero
    (uut/register! resources :streaming :throttle (constantly {:in-use 1 :limit 1 :queued 4}))
    (let [data (collect->data (.collect collector))]
      (is (= #{["streaming" "throttle" 4.0]} (get data "llar_resource_queued")))
      (is (nil? (get data "llar_resource_completed_total"))))))

(deftest collector-survives-a-real-registry-scrape
  ;; CollectorRegistry enumerates via collect(Predicate), a second arity that `proxy`
  ;; shadows into an ArityException when only collect() is implemented. Calling
  ;; .collect directly in the tests above does not exercise that path; /metrics does.
  (let [resources (uut/make-resources)
        registry (CollectorRegistry.)]
    (.register registry (uut/make-collector resources))
    (uut/register! resources :scraped :executor (constantly {:in-use 2 :limit 4}))
    (is (= 2.0 (.getSampleValue registry "llar_resource_in_use"
                                (into-array String ["resource" "kind"])
                                (into-array String ["scraped" "executor"]))))
    (let [out (StringWriter.)]
      (TextFormat/write004 out (.metricFamilySamples registry))
      (is (re-find #"llar_resource_in_use\{resource=\"scraped\",kind=\"executor\",?\} 2\.0"
                   (str out))))))

(deftest default-collector-is-wired-into-the-app-registry
  ;; the defonce wiring in llar.metrics.resources is what puts these on /metrics at all;
  ;; every other test here uses an isolated registry and would pass without it
  (try
    (uut/register! uut/resources ::wiring-check :executor (constantly {:in-use 7 :limit 9}))
    (is (= 7.0 (.getSampleValue (registry/raw metrics/prom-registry)
                                "llar_resource_in_use"
                                (into-array String ["resource" "kind"])
                                (into-array String ["wiring_check" "executor"]))))
    (finally
      (uut/unregister! uut/resources ::wiring-check))))

(deftest unregistered-resources-stop-being-emitted
  (let [resources (uut/make-resources)
        collector (uut/make-collector resources)]
    (uut/register! resources :transient :executor (constantly {:in-use 1 :limit 2}))
    (uut/unregister! resources :transient)
    (is (empty? (collect->data (.collect collector))))))
