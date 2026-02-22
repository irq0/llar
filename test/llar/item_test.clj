(ns llar.item-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is are testing]]
   [java-time.api :as time]
   [llar.fetch :as fetch]
   [llar.item]
   [llar.specs]
   [llar.src :as src]))

(def example-src (src/feed "https://example.com/feed.xml"))

(defn make-meta []
  (fetch/make-meta example-src))

(deftest make-meta-test
  (testing "make-meta returns a valid metadata map"
    (let [m (make-meta)]
      (is (s/valid? :irq0/item-metadata m))
      (is (= example-src (:source m)))
      (is (string? (:source-name m)))
      (is (= :unknown (:source-key m)))
      (is (instance? java.time.ZonedDateTime (:fetch-ts m)))
      (is (set? (:tags m)))
      (is (= 2 (:version m)))))
  (testing "make-meta tags are empty by default"
    (is (empty? (:tags (make-meta))))))

(deftest make-item-hash-test
  (testing "hash matches expected format"
    (let [h (fetch/make-item-hash "foo" "bar")]
      (is (s/valid? :irq0/item-hash h))
      (is (re-matches llar.item/item-hash-regex h))))
  (testing "same inputs produce same hash"
    (is (= (fetch/make-item-hash "a" "b")
           (fetch/make-item-hash "a" "b"))))
  (testing "different inputs produce different hashes"
    (is (not= (fetch/make-item-hash "a")
              (fetch/make-item-hash "b"))))
  (testing "empty string produces valid hash"
    (is (s/valid? :irq0/item-hash (fetch/make-item-hash "")))))

(deftest item-metadata-spec-test
  (testing "valid metadata passes spec"
    (is (s/valid? :irq0/item-metadata
                  {:source example-src
                   :source-name "test"
                   :source-key :test
                   :fetch-ts (time/zoned-date-time)
                   :tags #{}
                   :version 2})))
  (testing "missing required key fails spec"
    (is (not (s/valid? :irq0/item-metadata
                       {:source example-src
                        :source-name "test"
                        :fetch-ts (time/zoned-date-time)
                        :tags #{}
                        :version 2}))))
  (testing "tags must be a set"
    (is (not (s/valid? :irq0/item-metadata
                       {:source example-src
                        :source-name "test"
                        :source-key :test
                        :fetch-ts (time/zoned-date-time)
                        :tags [:unread]
                        :version 2})))))

(deftest item-summary-spec-test
  (testing "valid summary passes spec"
    (is (s/valid? :irq0/item-summary
                  {:ts (time/zoned-date-time)
                   :title "Test Title"})))
  (testing "missing ts fails spec"
    (is (not (s/valid? :irq0/item-summary {:title "Test Title"}))))
  (testing "missing title fails spec"
    (is (not (s/valid? :irq0/item-summary {:ts (time/zoned-date-time)})))))

(deftest item-hash-spec-test
  (testing "valid hashes"
    (are [h] (s/valid? :irq0/item-hash h)
      "SHA-256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      "SHA-256:a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"))
  (testing "invalid hashes"
    (are [h] (not (s/valid? :irq0/item-hash h))
      "sha-256:abc"
      "SHA-256:toolong-ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
      "MD5:abc"
      ""
      nil)))
