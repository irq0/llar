(ns llar.metrics.bookmark-capture-test
  (:require
   [clojure.test :refer [deftest is]]
   [llar.metrics.bookmark-capture :as uut]))

(defn- collect->data [families]
  (reduce (fn [acc family]
            (reduce (fn [acc sample]
                      (update acc (.-name sample) (fnil conj #{})
                              (conj (vec (.-labelValues sample)) (.-value sample))))
                    acc
                    (.-samples family)))
          {}
          families))

(deftest collector-emits-only-the-actionable-queue-signals
  (let [oldest (java.time.ZonedDateTime/of
                2026 8 9 12 0 0 0 java.time.ZoneOffset/UTC)
        collector (uut/make-collector
                   (constantly
                    {:counts [{:state "ready" :count 2}
                              {:state "processing" :count 1}
                              {:state "retry_wait" :count 3}
                              {:state "failed" :count 4}]
                     :oldest-ready oldest}))
        data (collect->data (.collect collector))]
    (is (= #{["ready" 2.0]
             ["processing" 1.0]
             ["retry_wait" 3.0]
             ["failed" 4.0]}
           (get data "llar_bookmark_queue_items")))
    (is (= #{[(double (.toEpochSecond oldest))]}
           (get data "llar_bookmark_queue_oldest_ready_unixtime")))
    (is (= #{"llar_bookmark_queue_items"
             "llar_bookmark_queue_oldest_ready_unixtime"}
           (set (keys data))))))

(deftest collector-omits-oldest-when-no-work-is-ready
  (let [collector (uut/make-collector
                   (constantly {:counts [{:state "ready" :count 0}]
                                :oldest-ready nil}))
        data (collect->data (.collect collector))]
    (is (= #{["ready" 0.0]} (get data "llar_bookmark_queue_items")))
    (is (nil? (get data "llar_bookmark_queue_oldest_ready_unixtime")))))

(deftest sampling-failure-never-breaks-all-metrics
  (let [collector (uut/make-collector #(throw (Exception. "database unavailable")))]
    (is (empty? (.collect collector)))))
