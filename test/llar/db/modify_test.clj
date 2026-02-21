(ns llar.db.modify-test
  "Integration tests for item storage and tag operations."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [java-time.api :as time]
   [llar.db.test-fixtures :refer [*test-db* with-test-db-fixture with-clean-db-fixture
                                  create-test-item create-test-tag create-test-item-data]]
   [llar.db.modify]  ; Load protocol implementations
   [llar.persistency :as persistency]
   [next.jdbc :as jdbc]))

(use-fixtures :once with-test-db-fixture)
(use-fixtures :each with-clean-db-fixture)

(deftest test-store-new-item
  (testing "Store a new item with basic fields"
    (let [item-result (create-test-item *test-db* :hash "unique-hash-1" :title "New Item")]
      (is (some? item-result) "Should return result for new item")
      (is (number? (:id item-result)) "Should have item ID")
      (is (clojure.string/includes? (:hash item-result) "SHA-256:") "Hash should have SHA-256 prefix"))))

(deftest test-store-duplicate-item
  (testing "Storing duplicate item (same hash) should be skipped"
    (let [result1 (create-test-item *test-db* :hash "duplicate-hash" :title "First Item")
          result2 (create-test-item *test-db* :hash "duplicate-hash" :title "Second Item")
          full-hash (:hash result1)]
      (is (some? result1) "First item should be stored")
      (is (nil? result2) "Duplicate item should be skipped (return nil)")

      ;; Verify only one item exists in database
      (let [items (jdbc/execute! *test-db*
                                 ["SELECT * FROM items WHERE hash = ?" full-hash]
                                 {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps})]
        (is (= 1 (count items)) "Should have exactly one item with this hash")
        (is (= "First Item" (:title (first items))) "Should keep the first item's data")))))

(deftest test-store-with-overwrite
  (testing "Storing with overwrite flag should update existing item"
    (let [result1 (create-test-item *test-db* :hash "overwrite-hash" :title "Original Title" :overwrite? false)
          full-hash (:hash result1)
          result2 (create-test-item *test-db* :hash "overwrite-hash" :title "Updated Title" :overwrite? true)]
      (is (some? result1) "First item should be stored")
      (is (some? result2) "Second item should overwrite")

      ;; Verify the item was updated
      (let [items (jdbc/execute! *test-db*
                                 ["SELECT * FROM items WHERE hash = ?" full-hash]
                                 {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps})]
        (is (= 1 (count items)) "Should have exactly one item")
        (is (= "Updated Title" (:title (first items))) "Title should be updated")))))

(deftest test-store-item-with-tags
  (testing "Store item with tags creates tag associations"
    (create-test-tag *test-db* :important)

    (let [result (create-test-item *test-db* :hash "tagged-item" :tags #{:unread :important})
          full-hash (:hash result)]
      (is (some? result) "Item should be stored")

      ;; Verify tags are associated
      (let [stored-item (first (jdbc/execute! *test-db*
                                              ["SELECT * FROM items WHERE hash = ?" full-hash]
                                              {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps}))
            tag-ids (vec (:tagi stored-item))]
        (is (= 2 (count tag-ids)) "Should have 2 tag IDs")
        (is (every? number? tag-ids) "Tag IDs should be numbers")))))

(deftest test-store-item-with-content-data
  (testing "Store item with text content via create-test-item-data"
    (let [item-id (:id (create-test-item *test-db* :hash "content-item" :title "Content Item"))]
      (is (some? item-id) "Item should be stored")

      ;; Add content data
      (create-test-item-data *test-db* :item-id item-id :type :item-data-type/content
                             :mime-type "text/plain" :text "This is plain text content")
      (create-test-item-data *test-db* :item-id item-id :type :item-data-type/content
                             :mime-type "text/html" :text "<p>This is HTML content</p>")

      ;; Verify item_data entries
      (let [data-rows (jdbc/execute! *test-db*
                                     ["SELECT * FROM item_data WHERE item_id = ?" item-id]
                                     {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps})]
        (is (= 2 (count data-rows)) "Should have 2 data rows (plain + html)")
        (is (every? #(= "content" (:type %)) data-rows)
            "All should be content type")
        (is (some #(= "text/plain" (:mime_type %)) data-rows)
            "Should have plain text entry")
        (is (some #(= "text/html" (:mime_type %)) data-rows)
            "Should have HTML entry")))))

(deftest test-store-item-with-descriptions
  (testing "Store item with description data"
    (let [item-id (:id (create-test-item *test-db* :hash "desc-item" :title "Description Item"))]
      (is (some? item-id) "Item should be stored")

      ;; Add description data
      (create-test-item-data *test-db* :item-id item-id :type :item-data-type/description
                             :mime-type "text/plain" :text "A brief description")

      ;; Verify description stored correctly
      (let [data-rows (jdbc/execute! *test-db*
                                     ["SELECT * FROM item_data WHERE item_id = ? AND type = ?"
                                      item-id :item-data-type/description]
                                     {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps})]
        (is (= 1 (count data-rows)) "Should have 1 description row")
        (is (= "A brief description" (:text (first data-rows)))
            "Description text should match")))))

(deftest test-store-creates-source-if-missing
  (testing "create-test-item creates source if it doesn't exist"
    ;; create-test-item automatically creates source with "test-" prefix
    (let [result (create-test-item *test-db* :hash "auto-source-item" :src-name "new-source")]
      (is (some? result) "Item should be stored")

      ;; Verify source was created (with test- prefix)
      (let [sources (jdbc/execute! *test-db*
                                   ["SELECT * FROM sources WHERE name = ?" "test-new-source"]
                                   {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps})]
        (is (= 1 (count sources)) "Source should be created automatically")
        (is (= "test-new-source" (:name (first sources))) "Source name should have test- prefix")))))

(deftest test-item-set-tags
  (testing "item-set-tags! replaces item's tags"
    (create-test-tag *test-db* :pinned)
    (create-test-tag *test-db* :archived)
    (let [item-id (:id (create-test-item *test-db* :hash "tag-test-item" :tags #{:unread}))
          ids (persistency/item-set-tags! *test-db* item-id [:pinned :archived])]
      (tap> ids)
      (is (= 3 (count ids)) "Should have 3 tags")
      (is (every? integer? ids) "Returns tag ids")

        ;; Verify tags in database
      (let [stored-item (first (jdbc/execute! *test-db*
                                              ["SELECT * FROM items WHERE id = ?" item-id]
                                              {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps}))]
        (is (= 3 (count (:tagi stored-item))) "Should have 3 tag IDs")))))

(deftest test-item-remove-tags
  (testing "item-remove-tags! removes specific tags from item"
    (create-test-tag *test-db* :pinned)
    (create-test-tag *test-db* :important)

    ;; Create item with multiple tags
    (let [item-id (:id (create-test-item *test-db* :hash "remove-tag-item" :tags #{:unread :pinned :important}))]

      ;; Remove one tag
      (let [remaining-tags (persistency/item-remove-tags! *test-db* item-id [:unread])]
        (is (= 2 (count remaining-tags)) "Should have 2 tags remaining")
        (is (not (some #(= :unread %) remaining-tags)) "unread tag should be removed")))))

(deftest test-remove-unread-for-source-older-than
  (testing "remove-unread-for-items-of-source-older-then! removes unread tags from old items"
    ;; Create items with different timestamps
    (let [old-ts (time/minus (time/zoned-date-time) (time/days 10))
          new-ts (time/zoned-date-time)
          cutoff-ts (time/minus (time/zoned-date-time) (time/days 5))

      ;; Old item with unread tag
          old-id (:id (create-test-item *test-db*
                                        :hash "old-unread-item"
                                        :src-name "old-source"
                                        :tags #{:unread}
                                        :ts old-ts))

        ;; New item with unread tag (should keep it)
          new-id (:id (create-test-item *test-db*
                                        :hash "new-unread-item"
                                        :src-name "old-source"
                                        :tags #{:unread}
                                        :ts new-ts))]

      ;; Get source key to pass to remove function
      (let [source-key (-> (jdbc/execute! *test-db*
                                          ["SELECT key FROM sources WHERE name = ?" "test-old-source"]
                                          {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps})
                           first
                           :key
                           keyword)]

          ;; Remove unread from old items
        (persistency/remove-unread-for-items-of-source-older-then!
         *test-db* [source-key] cutoff-ts)

          ;; Check old item lost unread tag
        (let [old-stored (first (jdbc/execute! *test-db*
                                               ["SELECT * FROM items WHERE id = ?" old-id]
                                               {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps}))
              new-stored (first (jdbc/execute! *test-db*
                                               ["SELECT * FROM items WHERE id = ?" new-id]
                                               {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps}))]
          (is (empty? (:tagi old-stored)) "Old item should have no tags")
          (is (= 1 (count (:tagi new-stored))) "New item should still have unread tag"))))))

(deftest test-transaction-rollback
  (testing "Items and item_data table stay consistent"
    ;; Store an item successfully
    (let [result1 (create-test-item *test-db* :hash "rollback-test-1")]
      (is (some? result1) "First item should store successfully")

      ;; Store another item with item data
      (let [item2-id (:id (create-test-item *test-db* :hash "rollback-test-2"))]
        (is (some? item2-id) "Second item should store")

        ;; Add some item data
        (create-test-item-data *test-db* :item-id item2-id :text "content")

        ;; Verify referential integrity: every item_data entry has a valid item_id
        (let [orphaned (jdbc/execute! *test-db*
                                      ["SELECT * FROM item_data WHERE item_id NOT IN (SELECT id FROM items)"]
                                      {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps})]
          (is (empty? orphaned) "Should have no orphaned item_data entries"))))))
