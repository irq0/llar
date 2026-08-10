(ns llar.value-inspector-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [hiccup2.core :as h]
   [java-time.api :as time]
   [llar.value-inspector :as inspector]))

(defn- map-entry [node printed-key]
  (some #(when (= printed-key (get-in % [:key :printed])) %)
        (:children node)))

(defn- display-node-count [node]
  (if-not node
    0
    (+ 1
       (display-node-count (:metadata node))
       (if (= "map" (:kind node))
         (reduce + 0 (map (fn [{:keys [key value]}]
                            (+ (display-node-count key)
                               (display-node-count value)))
                          (:children node)))
         (reduce + 0 (map display-node-count (:children node)))))))

(defn- printed-character-count [node]
  (if-not node
    0
    (+ (count (or (:printed node) ""))
       (printed-character-count (:metadata node))
       (if (= "map" (:kind node))
         (reduce + 0 (map (fn [{:keys [key value]}]
                            (+ (printed-character-count key)
                               (printed-character-count value)))
                          (:children node)))
         (reduce + 0 (map printed-character-count (:children node)))))))

(deftest presentation-preserves-clojure-forms-and-jvm-types
  (let [node (inspector/present {:keyword :value
                                 "keyword" 'value
                                 :vector [1 2]
                                 :list '(1 2)
                                 :set #{1 2}})]
    (testing "map keys remain typed values rather than JSON property names"
      (is (= "clojure.lang.Keyword"
             (get-in (map-entry node ":keyword") [:key :runtime-type])))
      (is (= "java.lang.String"
             (get-in (map-entry node "\"keyword\"") [:key :runtime-type]))))
    (testing "collection delimiters and concrete implementations are retained"
      (is (= ["[" "]"]
             ((juxt :open :close) (:value (map-entry node ":vector")))))
      (is (= ["(" ")"]
             ((juxt :open :close) (:value (map-entry node ":list")))))
      (is (= ["#{" "}"]
             ((juxt :open :close) (:value (map-entry node ":set")))))
      (is (= "clojure.lang.PersistentVector"
             (get-in (map-entry node ":vector") [:value :runtime-type]))))))

(deftest presentation-uses-readable-known-java-forms
  (let [instant (time/zoned-date-time 2026 8 10 12 30 0 0 "Europe/Berlin")
        node (inspector/present instant)]
    (is (= "zoned-date-time" (:semantic-type node)))
    (is (= "java.time.ZonedDateTime" (:runtime-type node)))
    (is (= "#java.time/zoned-date-time \"2026-08-10T12:30+02:00[Europe/Berlin]\""
           (:printed node)))))

(deftest compact-inspector-emits-a-client-variant
  (let [html (str (h/html (inspector/value-inspector
                           [[:value "Value" {:answer 42}]]
                           {:variant :compact :max-nodes 10})))]
    (is (re-find #"data-clojure-inspector-variant=\"compact\"" html))
    (is (not (re-find #"\"variant\"" html)))))

(deftest presentation-bounds-lazy-and-deep-values
  (let [lazy-node (inspector/present (iterate inc 0) {:max-children 3})
        deep-node (inspector/present {:a {:b {:c 1}}} {:max-depth 2})]
    (is (false? (:count-known lazy-node)))
    (is (= 3 (count (:children lazy-node))))
    (is (true? (:truncated lazy-node)))
    (is (= "depth"
           (get-in deep-node [:children 0 :value :children 0 :value
                              :truncation-reason])))))

(deftest presentation-enforces-global-node-and-character-budgets
  (testing "collection traversal stops at the shared node budget"
    (let [node (inspector/present (vec (range 100)) {:max-nodes 5})]
      (is (= 5 (display-node-count node)))
      (is (true? (:truncated node)))
      (is (= "nodes" (:truncation-reason node)))))
  (testing "printed scalar text shares one hard character budget"
    (let [node (inspector/present ["abcdefgh" "ijklmnop"]
                                  {:max-total-printed-length 7})]
      (is (<= (printed-character-count node) 7))
      (is (= "characters" (get-in node [:children 0 :truncation-reason])))
      (is (= "characters" (get-in node [:children 1 :truncation-reason]))))))

(deftest payload-shares-its-budgets-across-roots
  (let [payload (inspector/payload [[:one "One" (vec (range 20))]
                                    [:two "Two" (vec (range 20))]]
                                   {:max-nodes 8})]
    (is (= 8 (reduce + (map (comp display-node-count :node) (:roots payload)))))))

(deftest inspector-payload-is-safe-to-embed-in-html
  (let [rendered (str (h/html
                       (inspector/value-inspector
                        [[:value "Value" {:html "</script><script>alert(1)</script>"}]])))]
    (is (not (re-find #"</script><script>" rendered)))
    (is (re-find #"clojure-value-inspector-payload" rendered))
    (is (re-find #"java.lang.String" rendered))))
