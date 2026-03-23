(ns llar.db.ranking-test
  "Integration tests for ranked sorting, sort orders, and pagination."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [java-time.api :as time]
   [llar.db.test-fixtures :refer [*test-db* with-test-db-fixture with-clean-db-fixture
                                  create-test-tag
                                  create-test-item]]
   [llar.persistency :as persistency]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set]))

(use-fixtures :once with-test-db-fixture)
(use-fixtures :each with-clean-db-fixture)

(defn- refresh-source-stats! [db]
  (jdbc/execute! db ["REFRESH MATERIALIZED VIEW source_stats"]))

(defn- get-source-stats-raw [db]
  (jdbc/execute! db ["SELECT * FROM source_stats"]
                 {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps}))

;; ---------------------------------------------------------------------------
;; Test 1: source_stats materialized view
;; ---------------------------------------------------------------------------

(deftest test-source-stats-materialized-view
  (testing "source_stats computes items_per_day correctly for high-freq vs low-freq sources"
    (let [now (time/zoned-date-time)]
      ;; high-freq: 20 items spread over 45 days (~2 days apart)
      (doseq [i (range 20)]
        (create-test-item *test-db*
                          :src-name "high-freq"
                          :hash (str "hf-" i)
                          :title (str "High Freq " i)
                          :ts (time/minus now (time/days (* i 2)))))
      ;; low-freq: 2 items spread over 45 days
      (doseq [i (range 2)]
        (create-test-item *test-db*
                          :src-name "low-freq"
                          :hash (str "lf-" i)
                          :title (str "Low Freq " i)
                          :ts (time/minus now (time/days (* i 45)))))
      (refresh-source-stats! *test-db*)
      (let [stats (get-source-stats-raw *test-db*)
            by-source (into {} (map (fn [row] [(:source_id row) row]) stats))
            ;; Lookup source IDs by querying items
            hf-source-id (:source_id (first (jdbc/execute! *test-db*
                                                           ["SELECT source_id FROM items INNER JOIN sources ON items.source_id = sources.id WHERE sources.key = 'test-high-freq' LIMIT 1"]
                                                           {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps})))
            lf-source-id (:source_id (first (jdbc/execute! *test-db*
                                                           ["SELECT source_id FROM items INNER JOIN sources ON items.source_id = sources.id WHERE sources.key = 'test-low-freq' LIMIT 1"]
                                                           {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps})))
            hf-ipd (:items_per_day (get by-source hf-source-id))
            lf-ipd (:items_per_day (get by-source lf-source-id))]
        (is (some? hf-ipd) "high-freq source should have items_per_day")
        (is (some? lf-ipd) "low-freq source should have items_per_day")
        (is (pos? hf-ipd) "high-freq items_per_day should be positive")
        (is (pos? lf-ipd) "low-freq items_per_day should be positive")
        (is (> hf-ipd lf-ipd) "high-freq should have higher items_per_day than low-freq")))))

;; ---------------------------------------------------------------------------
;; Test 2: sort-order :newest
;; ---------------------------------------------------------------------------

(deftest test-sort-order-newest
  (testing "Items returned in descending timestamp order with :newest sort"
    (let [now (time/zoned-date-time)]
      (doseq [i (range 5)]
        (create-test-item *test-db*
                          :src-name "newest-sort"
                          :hash (str "newest-" i)
                          :title (str "Item " i)
                          :ts (time/minus now (time/hours i))))
      (let [results (persistency/get-items-recent *test-db* {:sort-order :newest :limit 10})]
        (is (= 5 (count results)))
        (is (= "Item 0" (:title (first results))) "Most recent item should be first")
        (is (= "Item 4" (:title (last results))) "Oldest item should be last")
        ;; Verify descending order
        (is (every? true?
                    (map (fn [a b] (time/after? (:ts a) (:ts b)))
                         results (rest results)))
            "Items should be in descending ts order")))))

;; ---------------------------------------------------------------------------
;; Test 3: sort-order :oldest
;; ---------------------------------------------------------------------------

(deftest test-sort-order-oldest
  (testing "Items returned in ascending timestamp order with :oldest sort"
    (let [now (time/zoned-date-time)]
      (doseq [i (range 5)]
        (create-test-item *test-db*
                          :src-name "oldest-sort"
                          :hash (str "oldest-" i)
                          :title (str "Item " i)
                          :ts (time/minus now (time/hours i))))
      (let [results (persistency/get-items-recent *test-db* {:sort-order :oldest :limit 10})]
        (is (= 5 (count results)))
        (is (= "Item 4" (:title (first results))) "Oldest item should be first")
        (is (= "Item 0" (:title (last results))) "Most recent item should be last")
        ;; Verify ascending order
        (is (every? true?
                    (map (fn [a b] (time/before? (:ts a) (:ts b)))
                         results (rest results)))
            "Items should be in ascending ts order")))))

;; ---------------------------------------------------------------------------
;; Test 4: ranked sort with rarity boost
;; ---------------------------------------------------------------------------

(deftest test-sort-order-ranked-rarity-boost
  (testing "Ranked sort boosts items from rare sources"
    (let [now (time/zoned-date-time)]
      ;; freq source: 20 items, 1 hour apart, starting from 20 hours ago
      (doseq [i (range 20)]
        (create-test-item *test-db*
                          :src-name "freq"
                          :hash (str "freq-bg-" i)
                          :title (str "Freq Background " i)
                          :ts (time/minus now (time/hours (+ 1 i)))))
      ;; rare source: 2 items, one 45 days ago and one 30 days ago
      (create-test-item *test-db*
                        :src-name "rare"
                        :hash "rare-bg-1"
                        :title "Rare Background 1"
                        :ts (time/minus now (time/days 45)))
      (create-test-item *test-db*
                        :src-name "rare"
                        :hash "rare-bg-2"
                        :title "Rare Background 2"
                        :ts (time/minus now (time/days 30)))
      (refresh-source-stats! *test-db*)
      ;; Now insert one new item from each source at the same timestamp
      (let [same-ts (time/minus now (time/minutes 1))]
        (create-test-item *test-db*
                          :src-name "freq"
                          :hash "freq-new"
                          :title "Freq New"
                          :ts same-ts)
        (create-test-item *test-db*
                          :src-name "rare"
                          :hash "rare-new"
                          :title "Rare New"
                          :ts same-ts))
      (let [results (persistency/get-items-recent *test-db*
                                                  {:sort-order :ranked
                                                   :ranked true
                                                   :highlight-boost 48.0
                                                   :rarity-cap 168.0
                                                   :limit 2})
            top-titles (mapv :title results)]
        (is (= 2 (count results)))
        (is (= "Rare New" (first top-titles))
            "Item from rare source should rank first due to rarity boost")))))

;; ---------------------------------------------------------------------------
;; Test 5: ranked sort with highlight boost
;; ---------------------------------------------------------------------------

(deftest test-sort-order-ranked-highlight-boost
  (testing "Ranked sort boosts highlighted items"
    (let [now (time/zoned-date-time)]
      (create-test-tag *test-db* :highlight)
      ;; Two items from the same source, same timestamp
      (create-test-item *test-db*
                        :src-name "hl-src"
                        :hash "highlighted-item"
                        :title "Highlighted Item"
                        :ts now
                        :tags #{:highlight})
      (create-test-item *test-db*
                        :src-name "hl-src"
                        :hash "normal-item"
                        :title "Normal Item"
                        :ts now
                        :tags #{})
      (refresh-source-stats! *test-db*)
      (let [results (persistency/get-items-recent *test-db*
                                                  {:sort-order :ranked
                                                   :ranked true
                                                   :highlight-boost 48.0
                                                   :rarity-cap 168.0
                                                   :limit 2})]
        (is (= 2 (count results)))
        (is (= "Highlighted Item" (:title (first results)))
            "Highlighted item should rank first due to 48h boost")))))

;; ---------------------------------------------------------------------------
;; Test 6: ranked sort with combined boosts
;; ---------------------------------------------------------------------------

(deftest test-sort-order-ranked-combined-boosts
  (testing "Combined rarity + highlight boosts outweigh age difference"
    (let [now (time/zoned-date-time)]
      (create-test-tag *test-db* :highlight)
      ;; freq source: 20 items to establish high frequency
      (doseq [i (range 20)]
        (create-test-item *test-db*
                          :src-name "freq-cb"
                          :hash (str "freq-cb-bg-" i)
                          :title (str "Freq CB BG " i)
                          :ts (time/minus now (time/hours (+ 1 i)))))
      ;; rare source: 2 items to establish low frequency
      (create-test-item *test-db*
                        :src-name "rare-cb"
                        :hash "rare-cb-bg-1"
                        :title "Rare CB BG 1"
                        :ts (time/minus now (time/days 45)))
      (create-test-item *test-db*
                        :src-name "rare-cb"
                        :hash "rare-cb-bg-2"
                        :title "Rare CB BG 2"
                        :ts (time/minus now (time/days 30)))
      (refresh-source-stats! *test-db*)
      ;; Highlighted item from rare source, 3 days old
      (create-test-item *test-db*
                        :src-name "rare-cb"
                        :hash "rare-highlighted"
                        :title "Rare Highlighted"
                        :ts (time/minus now (time/days 3))
                        :tags #{:highlight})
      ;; Non-highlighted item from freq source, right now
      (create-test-item *test-db*
                        :src-name "freq-cb"
                        :hash "freq-now"
                        :title "Freq Now"
                        :ts now
                        :tags #{})
      (let [results (persistency/get-items-recent *test-db*
                                                  {:sort-order :ranked
                                                   :ranked true
                                                   :highlight-boost 48.0
                                                   :rarity-cap 168.0
                                                   :limit 2})
            top-titles (mapv :title results)]
        (is (= 2 (count results)))
        ;; Rare+highlighted item: age ~72h, rarity boost ~168h, highlight boost 48h
        ;; effective_age = 72 - 48 - 168 = -144 (very negative, ranks high)
        ;; Freq item: age ~0h, rarity boost ~1h, highlight boost 0h
        ;; effective_age = 0 - 0 - ~1 = ~-1
        ;; So rare+highlighted should rank first
        (is (= "Rare Highlighted" (first top-titles))
            "Rare+highlighted item should rank higher despite being 3 days older")))))

;; ---------------------------------------------------------------------------
;; Test 7: pagination cursor with :newest
;; ---------------------------------------------------------------------------

(deftest test-pagination-cursor-newest
  (testing "Cursor-based pagination with :newest sort"
    (let [now (time/zoned-date-time)]
      (doseq [i (range 10)]
        (create-test-item *test-db*
                          :src-name "page-newest"
                          :hash (str "pn-" i)
                          :title (str "Item " i)
                          :ts (time/minus now (time/hours i))))
      (let [page1 (persistency/get-items-recent *test-db*
                                                {:sort-order :newest :limit 5})
            cursor-item (last page1)
            page2 (persistency/get-items-recent *test-db*
                                                {:sort-order :newest
                                                 :limit 5
                                                 :before {:ts (:ts cursor-item)
                                                          :id (:id cursor-item)}})]
        (is (= 5 (count page1)) "Page 1 should have 5 items")
        (is (= 5 (count page2)) "Page 2 should have 5 items")
        (let [page1-ids (set (map :id page1))
              page2-ids (set (map :id page2))]
          (is (empty? (clojure.set/intersection page1-ids page2-ids))
              "No ID overlap between pages"))))))

;; ---------------------------------------------------------------------------
;; Test 8: pagination cursor with :oldest
;; ---------------------------------------------------------------------------

(deftest test-pagination-cursor-oldest
  (testing "Cursor-based pagination with :oldest sort"
    (let [now (time/zoned-date-time)]
      (doseq [i (range 10)]
        (create-test-item *test-db*
                          :src-name "page-oldest"
                          :hash (str "po-" i)
                          :title (str "Item " i)
                          :ts (time/minus now (time/hours i))))
      (let [page1 (persistency/get-items-recent *test-db*
                                                {:sort-order :oldest :limit 5})
            cursor-item (last page1)
            page2 (persistency/get-items-recent *test-db*
                                                {:sort-order :oldest
                                                 :limit 5
                                                 :before {:ts (:ts cursor-item)
                                                          :id (:id cursor-item)}})]
        (is (= 5 (count page1)) "Page 1 should have 5 items")
        (is (= 5 (count page2)) "Page 2 should have 5 items")
        (let [page1-ids (set (map :id page1))
              page2-ids (set (map :id page2))]
          (is (empty? (clojure.set/intersection page1-ids page2-ids))
              "No ID overlap between pages"))
        ;; Page 1 should have the 5 oldest items (Item 9 through Item 5)
        (is (= "Item 9" (:title (first page1))) "Page 1 first item should be oldest")
        ;; Page 2 should have the 5 newest items (Item 4 through Item 0)
        (is (= "Item 4" (:title (first page2))) "Page 2 first item should continue from cursor")))))

;; ---------------------------------------------------------------------------
;; Test 9: offset-based pagination with :ranked
;; ---------------------------------------------------------------------------

(deftest test-pagination-offset-ranked
  (testing "Offset-based pagination with ranked sort"
    (let [now (time/zoned-date-time)]
      ;; source A: 15 items to establish frequency
      (doseq [i (range 15)]
        (create-test-item *test-db*
                          :src-name "offset-a"
                          :hash (str "oa-" i)
                          :title (str "A Item " i)
                          :ts (time/minus now (time/hours i))))
      ;; source B: 5 items
      (doseq [i (range 5)]
        (create-test-item *test-db*
                          :src-name "offset-b"
                          :hash (str "ob-" i)
                          :title (str "B Item " i)
                          :ts (time/minus now (time/hours (* i 3)))))
      (refresh-source-stats! *test-db*)
      (let [page1 (persistency/get-items-recent *test-db*
                                                {:sort-order :ranked
                                                 :ranked true
                                                 :highlight-boost 48.0
                                                 :rarity-cap 168.0
                                                 :limit 5
                                                 :offset 0})
            page2 (persistency/get-items-recent *test-db*
                                                {:sort-order :ranked
                                                 :ranked true
                                                 :highlight-boost 48.0
                                                 :rarity-cap 168.0
                                                 :limit 5
                                                 :offset 5})]
        (is (= 5 (count page1)) "Page 1 should have 5 items")
        (is (= 5 (count page2)) "Page 2 should have 5 items")
        (let [page1-ids (set (map :id page1))
              page2-ids (set (map :id page2))]
          (is (empty? (clojure.set/intersection page1-ids page2-ids))
              "No ID overlap between pages"))))))

;; ---------------------------------------------------------------------------
;; Test 10: ranked with empty source_stats
;; ---------------------------------------------------------------------------

(deftest test-ranked-empty-source-stats
  (testing "Ranked sort works even when source_stats has no data for sources"
    (let [now (time/zoned-date-time)]
      ;; Create items but do NOT refresh source_stats
      (doseq [i (range 3)]
        (create-test-item *test-db*
                          :src-name "no-stats"
                          :hash (str "ns-" i)
                          :title (str "No Stats " i)
                          :ts (time/minus now (time/hours i))))
      ;; Refresh to get an empty view (no items existed before the view was first populated,
      ;; and these items' source won't be in source_stats since we're refreshing with these items
      ;; already present - but the COALESCE handles NULL items_per_day from left join)
      ;; Actually, to test the NULL case properly, we need to NOT refresh at all.
      ;; The materialized view was populated during migration with no data, so it's empty.
      (let [results (persistency/get-items-recent *test-db*
                                                  {:sort-order :ranked
                                                   :ranked true
                                                   :highlight-boost 48.0
                                                   :rarity-cap 168.0
                                                   :limit 5})]
        (is (= 3 (count results)) "Should return all 3 items despite empty source_stats")
        (is (every? some? (map :title results)) "All items should have titles")))))

;; ---------------------------------------------------------------------------
;; Test 11: ranked next-item via offset
;; ---------------------------------------------------------------------------

(deftest test-ranked-next-item-via-offset
  (testing "Offset retrieval matches position in full ranked list"
    (let [now (time/zoned-date-time)]
      ;; source A: 10 items
      (doseq [i (range 10)]
        (create-test-item *test-db*
                          :src-name "next-a"
                          :hash (str "na-" i)
                          :title (str "Next A " i)
                          :ts (time/minus now (time/hours i))))
      ;; source B: 3 items (rarer)
      (doseq [i (range 3)]
        (create-test-item *test-db*
                          :src-name "next-b"
                          :hash (str "nb-" i)
                          :title (str "Next B " i)
                          :ts (time/minus now (time/hours (* i 5)))))
      (refresh-source-stats! *test-db*)
      (let [full-list (persistency/get-items-recent *test-db*
                                                    {:sort-order :ranked
                                                     :ranked true
                                                     :highlight-boost 48.0
                                                     :rarity-cap 168.0
                                                     :limit 13})
            single (persistency/get-items-recent *test-db*
                                                 {:sort-order :ranked
                                                  :ranked true
                                                  :highlight-boost 48.0
                                                  :rarity-cap 168.0
                                                  :limit 1
                                                  :offset 4})]
        (is (= 13 (count full-list)) "Full list should have 13 items")
        (is (= 1 (count single)) "Single result should have 1 item")
        (is (= (:id (nth full-list 4)) (:id (first single)))
            "Offset 4 should match item at position 4 (0-indexed) in full list")))))

;; ---------------------------------------------------------------------------
;; Test 12: default sort order (backward compat)
;; ---------------------------------------------------------------------------

(deftest test-default-sort-order-backward-compat
  (testing "No :sort-order key defaults to newest-first (backward compatible)"
    (let [now (time/zoned-date-time)]
      (doseq [i (range 5)]
        (create-test-item *test-db*
                          :src-name "default-sort"
                          :hash (str "ds-" i)
                          :title (str "Item " i)
                          :ts (time/minus now (time/hours i))))
      (let [results (persistency/get-items-recent *test-db* {:limit 5})]
        (is (= 5 (count results)))
        (is (= "Item 0" (:title (first results))) "Most recent item should be first")
        (is (= "Item 4" (:title (last results))) "Oldest item should be last")
        ;; Verify descending order
        (is (every? true?
                    (map (fn [a b] (time/after? (:ts a) (:ts b)))
                         results (rest results)))
            "Items should be in descending ts order (default = newest)")))))
