(ns llar.db.core-test
  "Tests for type conversions between Clojure and PostgreSQL types."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [llar.db.test-fixtures :refer [*test-db* with-test-db-fixture with-clean-db-fixture
                                  create-test-source]]
   [llar.db.core]
   [next.jdbc :as jdbc]))

(use-fixtures :once with-test-db-fixture)
(use-fixtures :each with-clean-db-fixture)

(deftest test-keyword-enum-conversion
  (testing "Clojure keywords convert to PostgreSQL ENUMs and back"
    (let [source (create-test-source *test-db*
                                      :key "enum-test"
                                      :name "Enum Test"
                                      :type :item-type/tweet)]
      (is (= :item-type/tweet (:type source))
          "Enum value should round-trip as keyword with namespace")))

  (testing "Different enum types"
    (let [link-source (create-test-source *test-db*
                                           :key "link-source"
                                           :type :item-type/link)
          feed-source (create-test-source *test-db*
                                           :key "feed-source"
                                           :type :item-type/feed)
          mail-source (create-test-source *test-db*
                                           :key "mail-source"
                                           :type :item-type/mail)]
      (is (= :item-type/link (:type link-source)))
      (is (= :item-type/feed (:type feed-source)))
      (is (= :item-type/mail (:type mail-source))))))

(deftest test-jsonb-conversion
  (testing "Clojure maps convert to JSONB and back"
    (let [test-data {:foo "bar"
                     :nested {:key "value"
                              :number 42}
                     :array [1 2 3]}
          source (create-test-source *test-db*
                                      :key "jsonb-test"
                                      :data test-data)]
      (is (= test-data (:data source))
          "JSONB should round-trip with nested structures intact")))

  (testing "Empty map"
    (let [source (create-test-source *test-db*
                                      :key "empty-map"
                                      :data {})]
      (is (= {} (:data source))
          "Empty map should round-trip correctly")))

  (testing "Map with various types"
    (let [test-data {:string "text"
                     :number 123
                     :float 45.67
                     :boolean true
                     :null nil
                     :array ["a" "b" "c"]
                     :nested-map {:inner "value"}}
          source (create-test-source *test-db*
                                      :key "complex-jsonb"
                                      :data test-data)]
      (is (= test-data (:data source))
          "Complex JSONB structures should preserve all types"))))

(deftest test-text-array-conversion
  (testing "Clojure vectors convert to PostgreSQL text arrays and back"
    ;; Create source first for foreign key
    (create-test-source *test-db* :key "array-source")
    (let [test-urls ["https://example.com" "https://test.com"]
          result (first
                  (jdbc/execute! *test-db*
                                 ["INSERT INTO items (hash, title, type, source_id, ts, author, tagi, entry, nlp_nwords, nlp_urls, nlp_names, nlp_nouns, nlp_verbs, nlp_top)
                                   VALUES (?, ?, ?, ?, NOW(), ?, ?, ?, ?, ?, ?, ?, ?, ?)
                                   RETURNING *"
                                  "array-test-hash"
                                  "Array Test"
                                  :item-type/link
                                  1
                                  "Test Author"
                                  [1 2]  ; Non-empty integer array for tagi
                                  {:url "https://example.com"}
                                  0
                                  test-urls
                                  ["Name1" "Name2"]
                                  ["noun1" "noun2"]
                                  ["verb1" "verb2"]
                                  {}]
                                 {:return-keys true
                                  :builder-fn next.jdbc.result-set/as-unqualified-lower-maps}))]
      (is (= test-urls (:nlp_urls result))
          "Text array should round-trip as vector")
      (is (= ["Name1" "Name2"] (:nlp_names result)))
      (is (= ["noun1" "noun2"] (:nlp_nouns result)))
      (is (= ["verb1" "verb2"] (:nlp_verbs result)))))

  (testing "Empty array"
    (create-test-source *test-db* :key "empty-array-source")
    (let [result (first
                  (jdbc/execute! *test-db*
                                 ["INSERT INTO items (hash, title, type, source_id, ts, author, tagi, entry, nlp_nwords, nlp_urls, nlp_names, nlp_nouns, nlp_verbs, nlp_top)
                                   VALUES (?, ?, ?, ?, NOW(), ?, ?, ?, ?, ?, ?, ?, ?, ?)
                                   RETURNING *"
                                  "empty-array-hash"
                                  "Empty Array Test"
                                  :item-type/link
                                  1
                                  "Test Author"
                                  [1]  ; Non-empty integer array for tagi
                                  {:url "https://example.com"}
                                  0
                                  []  ; Empty text arrays for nlp fields
                                  []
                                  []
                                  []
                                  {}]
                                 {:return-keys true
                                  :builder-fn next.jdbc.result-set/as-unqualified-lower-maps}))]
      (is (= [] (:nlp_urls result))
          "Empty array should round-trip correctly"))))

(deftest test-integer-array-conversion
  (testing "Integer arrays (tagi field)"
    (create-test-source *test-db* :key "int-array-source")
    (let [tag-ids [1 2 3 4]
          result (first
                  (jdbc/execute! *test-db*
                                 ["INSERT INTO items (hash, title, type, source_id, ts, author, tagi, entry, nlp_nwords, nlp_urls, nlp_names, nlp_nouns, nlp_verbs, nlp_top)
                                   VALUES (?, ?, ?, ?, NOW(), ?, ?, ?, ?, ?, ?, ?, ?, ?)
                                   RETURNING *"
                                  "int-array-hash"
                                  "Int Array Test"
                                  :item-type/link
                                  1
                                  "Test Author"
                                  tag-ids
                                  {:url "https://example.com"}
                                  0
                                  []
                                  []
                                  []
                                  []
                                  {}]
                                 {:return-keys true
                                  :builder-fn next.jdbc.result-set/as-unqualified-lower-maps}))]
      (is (= tag-ids (vec (:tagi result)))
          "Integer array should round-trip correctly"))))

(deftest test-timestamp-conversion
  (testing "ZonedDateTime converts to timestamp and back"
    (let [source (create-test-source *test-db* :key "timestamp-test")]
      (is (instance? java.time.ZonedDateTime (:created_ts source))
          "Timestamp should be read as ZonedDateTime")
      (is (instance? java.time.ZonedDateTime (:updated_ts source))
          "Timestamp should be read as ZonedDateTime"))))

(deftest test-null-handling
  (testing "Null values in JSONB"
    (let [source (create-test-source *test-db*
                                      :key "null-in-jsonb"
                                      :data {:key nil})]
      (is (= {:key nil} (:data source))
          "Null values in JSONB should be preserved")))

  (testing "NULL database value for optional fields"
    ;; The 'data' field in sources is nullable (hstore/jsonb)
    (let [rows (jdbc/execute! *test-db*
                              ["INSERT INTO sources (key, name, type) VALUES (?, ?, ?) RETURNING *"
                               "null-data-source"
                               "Null Data Source"
                               :item-type/link]
                              {:return-keys true
                               :builder-fn next.jdbc.result-set/as-unqualified-lower-maps})
          source (first rows)]
      (is (nil? (:data source))
          "NULL database value should be read as nil"))))
