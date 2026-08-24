(ns llar.db.gems-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [java-time.api :as time]
   [llar.db.gems]
   [llar.db.modify]
   [llar.db.search]
   [llar.db.test-fixtures :refer [*test-db* create-test-item create-test-item-data
                                  with-clean-db-fixture with-test-db-fixture]]
   [llar.events :as events]
   [llar.persistency :as persistency]))

(use-fixtures :once with-test-db-fixture)
(use-fixtures :each with-clean-db-fixture)

(deftest gem-facets-and-browsing-use-live-archive-membership
  (create-test-item *test-db* :hash "gem-clojure" :src-name "one"
                    :title "Clojure Gem" :tags #{:archive :clojure})
  (create-test-item *test-db* :hash "gem-untagged" :src-name "two"
                    :title "Untagged Gem" :tags #{:archive})
  (create-test-item *test-db* :hash "not-a-gem" :src-name "three"
                    :title "Ordinary Item" :tags #{:clojure})
  (let [facets (persistency/get-gem-facets *test-db* {})]
    (is (= 2 (:total facets)))
    (is (= 1 (:topic-count facets)))
    (is (= #{["clojure" 1] ["__untagged__" 1]}
           (set (map (juxt :value :count) (:tags facets)))))
    (is (= #{"test-one" "test-two"}
           (set (map :value (:sources facets))))))
  (is (= ["Clojure Gem"]
         (mapv :title (:items (persistency/get-gem-items
                               *test-db* {:tag "clojure"})))))
  (is (= ["Untagged Gem"]
         (mapv :title (:items (persistency/get-gem-items
                               *test-db* {:tag "__untagged__"}))))))

(deftest gem-search-membership-updates-without-refreshing-the-text-index
  (let [gem-id (:id (create-test-item *test-db* :hash "searchable-gem"
                                      :title "Searchable Gem" :tags #{:archive :research}))
        other-id (:id (create-test-item *test-db* :hash "searchable-other"
                                        :title "Searchable Other" :tags #{:research}))]
    (create-test-item-data *test-db* :item-id gem-id :text "quokka knowledge")
    (create-test-item-data *test-db* :item-id other-id :text "quokka distraction")
    (persistency/update-index! *test-db*)
    (is (= [gem-id]
           (mapv :id (persistency/search *test-db* "quokka"
                                         {:archived-only? true
                                          :with-tag "research"}))))
    (persistency/transition-item-state! *test-db* gem-id :unarchive)
    (is (empty? (persistency/search *test-db* "quokka"
                                    {:archived-only? true})))))

(deftest search-index-splits-languages-and-counts-only-on-request
  (let [english-id (:id (create-test-item *test-db*
                                          :hash "search-language-english"
                                          :title "Quokka English"
                                          :entry {:url "https://example.com/en"
                                                  :language "en"}))
        german-id (:id (create-test-item *test-db*
                                         :hash "search-language-german"
                                         :title "Quokka Deutsch"
                                         :entry {:url "https://example.com/de"
                                                 :language "de"}))
        default-id (:id (create-test-item *test-db*
                                          :hash "search-language-default"
                                          :title "Quokka Default"))]
    (persistency/update-index! *test-db*)
    (let [rows (persistency/search *test-db* "quokka" {:limit 10})
          counted-page (persistency/search *test-db* "quokka"
                                           {:limit 1
                                            :offset 1
                                            :with-total-count? true})]
      (is (= #{english-id german-id default-id} (set (map :id rows))))
      (is (every? #(not (contains? % :total-count)) rows))
      (is (= 1 (count counted-page)))
      (is (= 3 (:total-count (first counted-page)))))))

(deftest rediscovery-prefers-items-without-recorded-history
  (let [old-id (:id (create-test-item *test-db* :hash "old-gem"
                                      :src-name "old" :tags #{:archive}))
        fresh-id (:id (create-test-item *test-db* :hash "fresh-gem"
                                        :src-name "fresh" :tags #{:archive}))
        [offer] (persistency/record-results-offered!
                 *test-db* [old-id]
                 (events/context :related :related-generated
                   {:feature "gems" :kind "rediscover"}))]
    (persistency/record-impressions! *test-db* [(:id offer)])
    (let [tomorrow (time/plus (time/zoned-date-time) (time/days 1))
          items (persistency/get-gem-rediscovery-candidates
                 *test-db* {:day-cutoff tomorrow
                            :day-key "test-day"
                            :candidate-limit 10
                            :candidate-offset 0
                            :limit 5})]
      (is (= fresh-id (:id (first items))))
      (is (= old-id (:id (second items))))
      (is (some? (:last-resurfaced (second items)))))))
