(ns llar.work-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [iapetos.export :as export]
   [iapetos.registry :as registry]
   [java-time.api :as time]
   [llar.metrics :as metrics]
   [llar.pool :as pool]
   [llar.work :as uut]))

(use-fixtures :each
  (fn [f]
    (uut/reset-work!)
    (f)
    (uut/reset-work!)))

(defn- work-gauge [metric label-names label-values]
  (.getSampleValue (registry/raw metrics/prom-registry)
                   metric
                   (into-array String label-names)
                   (into-array String label-values)))

(deftest work-is-visible-while-running-and-gone-afterwards
  (uut/with-work {:kind :source :source :my-feed :stage :fetch}
    (let [entries (uut/in-flight)
          entry (first entries)]
      (is (= 1 (count entries)))
      (is (= :source (:kind entry)))
      (is (= :my-feed (:source entry)))
      (is (= :fetch (:stage entry)))
      (is (string? (:thread entry)))))
  (is (empty? (uut/in-flight))))

(deftest work-is-unregistered-even-when-the-body-throws
  (is (thrown? clojure.lang.ExceptionInfo
               (uut/with-work {:kind :source :source :boom}
                 (throw (ex-info "boom" {})))))
  (is (empty? (uut/in-flight)) "a throwing body must not leak a registry entry"))

(deftest with-work-returns-the-body-value
  (is (= :the-value (uut/with-work {:kind :item} :the-value))))

(deftest stage-and-waiting-on-update-the-current-unit
  (uut/with-work {:kind :source :source :s :stage :fetch}
    (uut/stage! :store)
    (is (= :store (:stage (first (uut/in-flight)))))
    (uut/waiting-on! :av-download)
    (is (= :av-download (:waiting-on (first (uut/in-flight)))))
    (uut/waiting-on! nil)
    (is (nil? (:waiting-on (first (uut/in-flight)))))))

(deftest nested-work-tracks-both-units
  (uut/with-work {:kind :source :source :s :stage :postproc}
    (uut/with-work {:kind :item :source :s :stage :postproc}
      (is (= [:source :item] (mapv :kind (uut/in-flight)))))
    (is (= [:source] (mapv :kind (uut/in-flight))))))

(deftest in-flight-is-ordered-oldest-first
  ;; a straggler at the top of the table is the entire point of the detail view
  (let [t0 (time/zoned-date-time 2026 8 3 12 0 0 0 "UTC")]
    (with-redefs [time/zoned-date-time (constantly t0)]
      (uut/register! {:kind :source :source :old :stage :fetch}))
    (with-redefs [time/zoned-date-time (constantly (time/plus t0 (time/minutes 5)))]
      (uut/register! {:kind :source :source :recent :stage :fetch}))
    (with-redefs [time/zoned-date-time (constantly (time/plus t0 (time/minutes 6)))]
      (is (= [:old :recent] (mapv :source (uut/in-flight))))
      (is (= [360.0 60.0] (mapv :age-seconds (uut/in-flight)))))))

(deftest aggregates-are-exported-per-kind-and-stage
  (uut/with-work {:kind :source :source :s :stage :fetch}
    (uut/with-work {:kind :item :source :s :stage :postproc}
      (is (= 1.0 (work-gauge "llar_work_in_flight" ["kind" "stage"] ["source" "fetch"])))
      (is (= 1.0 (work-gauge "llar_work_in_flight" ["kind" "stage"] ["item" "postproc"])))
      (is (some? (work-gauge "llar_work_oldest_age_seconds" ["kind"] ["source"]))))))

(deftest exported-metrics-carry-no-per-source-labels
  ;; per-source-per-item cardinality is exactly what prometheus is built to reject:
  ;; the detail stays in the atom, only aggregates get exported
  (uut/with-work {:kind :source :source :a-very-specific-feed-name :stage :fetch}
    (let [text (export/text-format metrics/prom-registry)]
      (is (re-find #"llar_work_in_flight" text))
      (is (not (re-find #"a_very_specific_feed_name" text))))))

(deftest work-id-is-conveyed-into-pool-tasks
  ;; the pools dispatch through bound-fn*, so an item task inherits its source's work id
  ;; and can self-report what it is blocked on without threading an id through every call
  (let [pool (pool/make-pool :work-conveyance 2)]
    (try
      (uut/with-work {:kind :source :source :s :stage :postproc}
        (is (= [true true]
               (pool/pmap-failfast pool (fn [_] (some? uut/*work-id*)) (range 2)))))
      (finally
        (pool/shutdown! pool)))))
