(ns llar.db.query-test
  "Integration tests for item queries and stats."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [java-time.api :as time]
   [clojure.string :as string]
   [clojure.set :as set]
   [llar.db.test-fixtures :refer [*test-db* with-test-db-fixture with-clean-db-fixture
                                  create-test-tag
                                  create-test-item create-test-item-data]]
   [llar.persistency :as persistency]))

(use-fixtures :once with-test-db-fixture)
(use-fixtures :each with-clean-db-fixture)

(deftest test-get-items-recent-basic
  (testing "Get recent items without filters"
    (let [now (time/zoned-date-time)]
      (doseq [i (range 5)]
        (create-test-item *test-db*
                          :src-name "get-items-recent-basic"
                          :hash (str "recent-" i)
                          :title (str "Item " i)
                          :ts (time/minus now (time/hours i))))
      (let [results (persistency/get-items-recent *test-db* {:limit 10})]
        (is (= 5 (count results)) "Should return all 5 items")
        (is (= "Item 0" (:title (first results))) "Most recent item should be first")))))

(deftest test-get-items-recent-with-limit
  (testing "Limit parameter restricts number of results"
    (doseq [i (range 10)]
      (create-test-item *test-db* :src-name "items-recent-with-limit" :hash (str "limit-" i) :title (str "Item " i)))
    (let [results (persistency/get-items-recent *test-db* {:limit 3})]
      (is (= 3 (count results)) "Should return exactly 3 items"))))

(deftest test-get-items-recent-with-tag-filter
  (testing "Filter items by tag"
    (create-test-item *test-db* :src-name "recent-tag-filter" :hash "unread-1" :title "Unread Item 1" :tags #{:unread})
    (create-test-item *test-db* :src-name "recent-tag-filter" :hash "unread-2" :title "Unread Item 2" :tags #{:unread})
    (create-test-item *test-db* :src-name "recent-tag-filter" :hash "pinned-1" :title "Pinned Item" :tags #{:pinned})
    (create-test-item *test-db* :src-name "recent-tag-filter" :hash "no-tags" :title "No Tags Item" :tags #{})
    (let [results (persistency/get-items-recent *test-db* {:limit 10 :with-tag :unread})]
      (is (= 2 (count results)) "Should return 2 unread items")
      (is (every? #(string/starts-with? (:title %) "Unread") results)
          "All results should be unread items"))))

(deftest test-get-items-recent-with-type-filter
  (testing "Filter items by type"
    (create-test-item *test-db* :src-name "tweet-source" :hash "tweet-1" :title "Tweet 1")
    (create-test-item *test-db* :src-name "link-source" :hash "link-1" :title "Link 1")
    (create-test-item *test-db* :src-name "bookmark-source" :hash "bookmark-1" :title "Bookmark 1")
    (create-test-item *test-db* :src-name "another-tweet-source" :hash "tweet-2" :title "Tweet 2")
    (let [results (persistency/get-items-recent *test-db* {:limit 10 :with-type :item-type/link})]
      (is (= 4 (count results)) "Should return all items (all are links by default)")
      (is (every? #(= :item-type/link (:type %)) results)
          "All results should be links"))))

(deftest test-get-items-recent-with-simple-filter
  (testing "Simple filter for unread items"
    (create-test-item *test-db* :src-name "recent-with-simple-filter" :hash "unread-1" :title "Unread 1" :tags #{:unread})
    (create-test-item *test-db* :src-name "recent-with-simple-filter" :hash "unread-2" :title "Unread 2" :tags #{:unread})
    (create-test-item *test-db* :src-name "recent-with-simple-filter" :hash "read-1" :title "Read 1" :tags #{})
    (let [results (persistency/get-items-recent *test-db* {:limit 10 :simple-filter :unread})]
      (is (= 2 (count results)) "Should return only unread items"))))

(deftest test-source-discovery-and-actionable-counts
  (testing "Discover source membership without calculating historical totals"
    (create-test-item *test-db* :src-name "counts-one" :hash "one-unread"
                      :tags #{:digest-issue-1 :unread})
    (create-test-item *test-db* :src-name "counts-one" :hash "one-read"
                      :tags #{:digest-issue-1})
    (create-test-item *test-db* :src-name "counts-two" :hash "two-unread"
                      :tags #{:digest-issue-1 :unread})
    (create-test-item *test-db* :src-name "counts-other" :hash "other-unread"
                      :tags #{:unread})
    (let [sources (persistency/get-sources *test-db* {})
          one-id (get-in sources [:test-counts-one :id])
          two-id (get-in sources [:test-counts-two :id])
          other-id (get-in sources [:test-counts-other :id])
          source-ids [one-id two-id other-id]]
      (is (every? some? source-ids))
      (is (= (set [one-id two-id])
             (persistency/get-source-ids-with-tag
              *test-db* source-ids :digest-issue-1)))
      (is (= (zipmap [one-id two-id] [1 1])
             (persistency/get-source-item-counts
              *test-db*
              {:source-ids source-ids
               :simple-filter :unread
               :with-tag :digest-issue-1})))
      (is (= (zipmap source-ids [1 1 1])
             (persistency/get-source-item-counts
              *test-db*
              {:source-ids source-ids
               :simple-filter :unread})))
      (is (= #{} (persistency/get-source-ids-with-tag
                  *test-db* [] :digest-issue-1)))
      (is (= {} (persistency/get-source-item-counts
                 *test-db* {:source-ids [] :simple-filter :unread}))))))

(deftest test-get-items-recent-pagination
  (testing "Pagination with :before parameter"
    (let [now (time/zoned-date-time)]
      (doseq [i (range 10)]
        (create-test-item *test-db*
                          :src-name "pagination-test"
                          :hash (str "page-" i)
                          :title (str "Item " i)
                          :ts (time/minus now (time/hours i))))
      (let [page1 (persistency/get-items-recent *test-db* {:limit 4})]
        (is (= 4 (count page1)) "First page should have 2 items")
        (is (= "Item 0" (:title (first page1))))
        (is (= "Item 1" (:title (second page1))))
        (is (= "Item 2" (:title (nth page1 2))))
        (is (= "Item 3" (:title (nth page1 3))))
        (let [cursor-item (nth page1 3)
              page2 (persistency/get-items-recent *test-db*
                                                  {:limit 3
                                                   :before {:ts (:ts cursor-item)
                                                            :id (:id cursor-item)}})]
          (is (= 3 (count page2)) "Second page should have 3 items")
          (is (= "Item 4" (:title (first page2))) "Second page should start with Item 4")
          (is (= "Item 5" (:title (second page2))) "Second page should have Item 5"))))))

(deftest test-get-items-recent-source-subset-pagination
  (testing "Merge bounded per-source streams without scanning unrelated items"
    (let [now (time/zoned-date-time)]
      (create-test-item *test-db* :src-name "selected-a" :hash "selected-a-new"
                        :title "A new" :ts now)
      (create-test-item *test-db* :src-name "selected-b" :hash "selected-b-new"
                        :title "B new" :ts (time/minus now (time/hours 1)))
      (create-test-item *test-db* :src-name "selected-a" :hash "selected-a-old"
                        :title "A old" :ts (time/minus now (time/hours 2)))
      (create-test-item *test-db* :src-name "selected-b" :hash "selected-b-old"
                        :title "B old" :ts (time/minus now (time/hours 3)))
      (create-test-item *test-db* :src-name "not-selected" :hash "not-selected-newest"
                        :title "Unrelated newest" :ts (time/plus now (time/hours 1)))
      (let [sources (persistency/get-sources *test-db* {})
            source-ids [(get-in sources [:test-selected-a :id])
                        (get-in sources [:test-selected-b :id])]
            page1 (persistency/get-items-recent
                   *test-db* {:with-source-ids source-ids :limit 2})
            page2 (persistency/get-items-recent
                   *test-db* {:with-source-ids source-ids
                              :before (select-keys (last page1) [:ts :id])
                              :limit 2})
            oldest (persistency/get-items-recent
                    *test-db* {:with-source-ids source-ids
                               :sort-order :oldest
                               :limit 2})]
        (is (= ["A new" "B new"] (mapv :title page1)))
        (is (= ["A old" "B old"] (mapv :title page2)))
        (is (= ["B old" "A old"] (mapv :title oldest)))
        (is (empty? (persistency/get-items-recent
                     *test-db* {:with-source-ids [] :limit 2})))))))

(deftest test-get-items-recent-tag-pagination-with-tied-timestamps
  (testing "GIN-first tag pages seek by timestamp and ID without gaps or overlap"
    (let [same-ts (time/zoned-date-time)]
      (doseq [i (range 8)]
        (create-test-item *test-db*
                          :src-name "tag-pagination"
                          :hash (str "tag-page-" i)
                          :title (str "Tagged " i)
                          :ts same-ts
                          :tags #{:pagination-tag}))
      (create-test-item *test-db*
                        :src-name "tag-pagination"
                        :hash "tag-page-unrelated"
                        :title "Unrelated"
                        :ts same-ts)
      (let [page1 (persistency/get-items-recent
                   *test-db*
                   {:sort-order :newest :with-tag :pagination-tag :limit 3})
            cursor (last page1)
            page2 (persistency/get-items-recent
                   *test-db*
                   {:sort-order :newest
                    :with-tag :pagination-tag
                    :limit 3
                    :before (select-keys cursor [:ts :id])})]
        (is (= 3 (count page1)))
        (is (= 3 (count page2)))
        (is (empty? (set/intersection (set (map :id page1))
                                      (set (map :id page2)))))
        (is (every? #(some #{"pagination-tag"} (:tags %))
                    (concat page1 page2)))
        (is (apply > (map :id (concat page1 page2))))))))

(deftest test-get-items-recent-with-data
  (testing "Include item data when :with-data? is true"
    (let [id (:id (create-test-item *test-db*
                                    :src-name "data-test"
                                    :hash "data-item"
                                    :title "Item with data"
                                    :entry {:url "https://example.com"}))]
      (tap> id)
      (create-test-item-data *test-db* :item-id id :text "foo"))

    ;; Query without data
    (let [results-no-data (persistency/get-items-recent *test-db* {:limit 10 :with-data? false})]
      (is (nil? (:text (first results-no-data))) "Should not include data"))

    ;; Query with data
    (let [results-with-data (persistency/get-items-recent *test-db* {:limit 10 :with-data? true})
          item-data (:text (first results-with-data))]
      (is (some? item-data) "Should include data"))))

(deftest test-get-items-recent-with-bounded-preview-descriptions
  (testing "Hydrate only bounded plain-text descriptions after limiting the batch"
    (let [id (:id (create-test-item *test-db*
                                    :src-name "preview-description-test"
                                    :hash "preview-description-item"
                                    :title "Item with preview description"
                                    :entry {:url "https://example.com"}))
          long-description (apply str (repeat 700 "x"))]
      (create-test-item-data *test-db*
                             :item-id id
                             :type :item-data-type/description
                             :text long-description)
      (create-test-item-data *test-db*
                             :item-id id
                             :type :item-data-type/content
                             :text "Full content must stay out of previews")
      (let [item (first (persistency/get-items-recent
                         *test-db*
                         {:limit 1 :with-preview-data? true}))]
        (is (= 512 (count (get-in item [:data :description "text/plain"]))))
        (is (nil? (get-in item [:data :content])))
        (is (nil? (:text item)))))))

(deftest test-get-item-by-id
  (testing "Retrieve single item by ID with data"
    (let [id (:id (create-test-item *test-db*
                                    :src-name "id-test"
                                    :hash "id-test"
                                    :title "Test Item By ID"
                                    :entry {:url "https://example.com"}))]
      (create-test-item-data *test-db* :item-id id :type :item-data-type/content :text "Plain content")
      (create-test-item-data *test-db* :item-id id :type :item-data-type/description :text "A description")

      ;; Retrieve by ID
      (let [retrieved (persistency/get-item-by-id *test-db* id)]
        (is (= id (:id retrieved)) "Should have correct ID")
        (is (= "Test Item By ID" (:title retrieved)) "Should have correct title")
        (is (some? (:data retrieved)) "Should include data")

        ;; Verify data structure
        (let [data (:data retrieved)]
          (is (contains? data :content) "Should have content data")
          (is (contains? data :description) "Should have description data")
          (is (= "Plain content" (get-in data [:content "text/plain"]))
              "Content should match")
          (is (= "A description" (get-in data [:description "text/plain"]))
              "Description should match"))))))

(deftest test-get-items-by-tag
  (testing "Get all items with a specific tag"
    (create-test-tag *test-db* :wubly)
    (create-test-tag *test-db* :doubly)
    (create-test-item *test-db* :src-name "tag-test" :hash "important-1" :title "Important 1" :tags #{:wubly})
    (create-test-item *test-db* :src-name "tag-test" :hash "important-2" :title "Important 2" :tags #{:wubly})
    (create-test-item *test-db* :src-name "tag-test" :hash "not-important" :title "Not Important" :tags #{:doubly})

    ;; Query by tag
    (let [results (persistency/get-items-by-tag *test-db* :wubly)]
      (is (= 2 (count results)) "Should return 2 important items")
      (is (every? #(string/starts-with? (:title %) "Important") results)
          "All results should be important items"))))

(deftest test-get-table-row-counts
  (testing "Get row counts for all tables"
    (create-test-item *test-db* :src-name "count-test" :hash "count-1" :title "Item 1")
    (create-test-item *test-db* :src-name "count-test" :hash "count-2" :title "Item 2")

    (let [counts (persistency/get-table-row-counts *test-db*)]
      (is (map? (first counts)) "Should return sequence of maps")
      (is (some #(and (= "items" (:table %)) (= 2 (:count %))) counts)
          "Should have 2 items")
      (is (some #(and (= "sources" (:table %)) (>= (:count %) 1)) counts)
          "Should have at least 1 source"))))

(deftest test-get-type-stats
  (testing "Get statistics grouped by item type"
    (create-test-item *test-db* :src-name "link-source" :hash "link-1" :title "Link 1")
    (create-test-item *test-db* :src-name "link-source" :hash "link-2" :title "Link 2")
    (create-test-item *test-db* :src-name "tweet-source" :hash "tweet-1" :title "Tweet 1")

    (let [stats (persistency/get-type-stats *test-db*)]
      (is (seq stats) "Should return type statistics")
      ;; Should have stats for link type (all items are links by default)
      (is (some #(= :item-type/link (:type %)) stats)
          "Should have link type stats"))))

(deftest test-get-tags
  (testing "Get all unique tags from database"
    (create-test-tag *test-db* :custom-tag)
    (create-test-tag *test-db* :pinned)

    (let [tags (persistency/get-tags *test-db*)]
      (is (set? tags) "Should return a set")
      (is (contains? tags :unread) "Should contain unread tag")
      (is (contains? tags :pinned) "Should contain pinned tag")
      (is (contains? tags :custom-tag) "Should contain custom tag"))))

(deftest test-combined-filters
  (testing "Combine multiple filters"
    (create-test-item *test-db* :src-name "combined-1" :hash "match" :title "Match" :tags #{:unread})
    (create-test-item *test-db* :src-name "combined-2" :hash "wrong-tag" :title "Wrong Tag" :tags #{:pinned})
    (create-test-item *test-db* :src-name "combined-3" :hash "no-tag" :title "No Tag" :tags #{})

    ;; Query with both type and tag filters
    (let [results (persistency/get-items-recent *test-db*
                                                {:limit 10
                                                 :with-type :item-type/link
                                                 :with-tag :unread})]
      (is (= 1 (count results)) "Should return only 1 item matching both filters")
      (is (= "Match" (:title (first results))) "Should return the matching item"))))
