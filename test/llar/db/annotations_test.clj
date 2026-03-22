(ns llar.db.annotations-test
  "Integration tests for annotation CRUD and auto-tagging."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [llar.db.test-fixtures :refer [*test-db* with-test-db-fixture with-clean-db-fixture
                                  create-test-item]]
   [llar.db.annotations]
   [llar.persistency :as persistency]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set]))

(use-fixtures :once with-test-db-fixture)
(use-fixtures :each with-clean-db-fixture)

(defn- get-item-tags [db item-id]
  (let [item (first (jdbc/execute! db
                                   ["SELECT tagi FROM items WHERE id = ?" item-id]
                                   {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps}))]
    (:tagi item)))

(defn- tag-name-for-id [db tag-id]
  (:tag (first (jdbc/execute! db
                              ["SELECT tag FROM tags WHERE id = ?" tag-id]
                              {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps}))))

(defn- item-has-tag? [db item-id tag-name]
  (let [tag-ids (get-item-tags db item-id)]
    (some #(= tag-name (tag-name-for-id db %)) tag-ids)))

(deftest test-create-text-highlight
  (testing "Create a text highlight annotation (selector present, body nil)"
    (let [item-id (:id (create-test-item *test-db*))
          selector {:type "TextQuoteSelector"
                    :exact "highlighted text"
                    :prefix "some prefix context "
                    :suffix " some suffix context"}
          ann (persistency/create-annotation! *test-db* item-id selector nil)]
      (is (some? ann) "Should return created annotation")
      (is (number? (:id ann)) "Should have an ID")
      (is (= item-id (:item_id ann)) "Should reference correct item")
      (is (some? (:selector ann)) "Should have selector")
      (is (nil? (:body ann)) "Body should be nil for highlight"))))

(deftest test-create-item-note
  (testing "Create an item-level note (selector nil, body present)"
    (let [item-id (:id (create-test-item *test-db*))
          ann (persistency/create-annotation! *test-db* item-id nil "This is a note")]
      (is (some? ann) "Should return created annotation")
      (is (nil? (:selector ann)) "Selector should be nil for note")
      (is (= "This is a note" (:body ann)) "Body should match"))))

(deftest test-get-annotations
  (testing "Retrieve annotations for an item"
    (let [item-id (:id (create-test-item *test-db*))
          selector {:type "TextQuoteSelector" :exact "test"}
          _ (persistency/create-annotation! *test-db* item-id selector nil)
          _ (persistency/create-annotation! *test-db* item-id nil "a note")
          anns (persistency/get-annotations *test-db* item-id)]
      (is (= 2 (count anns)) "Should have 2 annotations")
      (is (some #(some? (:selector %)) anns) "Should have a highlight")
      (is (some #(= "a note" (:body %)) anns) "Should have a note"))))

(deftest test-delete-annotation
  (testing "Delete an annotation by ID"
    (let [item-id (:id (create-test-item *test-db*))
          ann (persistency/create-annotation! *test-db* item-id nil "to delete")
          result (persistency/delete-annotation! *test-db* (:id ann))]
      (is (= (:id ann) (:deleted result)) "Should return deleted ID")
      (is (= item-id (:item-id result)) "Should return item-id")
      (is (empty? (persistency/get-annotations *test-db* item-id))
          "Annotations should be empty after delete"))))

(deftest test-auto-tag-on-first-annotation
  (testing "First annotation auto-tags item with :has-annotations"
    (let [item-id (:id (create-test-item *test-db*))]
      (is (not (item-has-tag? *test-db* item-id "has-annotations"))
          "Should not have tag before annotation")
      (persistency/create-annotation! *test-db* item-id nil "first note")
      (is (item-has-tag? *test-db* item-id "has-annotations")
          "Should have :has-annotations tag after first annotation"))))

(deftest test-auto-untag-on-last-delete
  (testing "Deleting last annotation removes :has-annotations tag"
    (let [item-id (:id (create-test-item *test-db*))
          ann1 (persistency/create-annotation! *test-db* item-id nil "note 1")
          ann2 (persistency/create-annotation! *test-db* item-id nil "note 2")]
      (is (item-has-tag? *test-db* item-id "has-annotations")
          "Should have tag with annotations")
      (persistency/delete-annotation! *test-db* (:id ann1))
      (is (item-has-tag? *test-db* item-id "has-annotations")
          "Should still have tag with one annotation remaining")
      (persistency/delete-annotation! *test-db* (:id ann2))
      (is (not (item-has-tag? *test-db* item-id "has-annotations"))
          "Should lose tag after all annotations deleted"))))
