(ns llar.tags-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llar.tags :as tags]))

(deftest test-normalize-tag
  (testing "basic lowercase"
    (is (= "mytag" (tags/normalize-tag :MyTag)))
    (is (= "mytag" (tags/normalize-tag "MyTag")))
    (is (= "mytag" (tags/normalize-tag "MYTAG"))))

  (testing "already lowercase"
    (is (= "unread" (tags/normalize-tag :unread)))
    (is (= "has-video" (tags/normalize-tag :has-video))))

  (testing "whitespace handling"
    (is (= "my-tag" (tags/normalize-tag "  my tag  ")))
    (is (= "a-b-c" (tags/normalize-tag "a  b  c")))
    (is (= "trimmed" (tags/normalize-tag "  trimmed  "))))

  (testing "special characters stripped"
    (is (= "tag" (tags/normalize-tag "tag!")))
    (is (= "tag" (tags/normalize-tag "<tag>")))
    (is (= "mytag" (tags/normalize-tag "my.tag")))
    (is (= "mytag" (tags/normalize-tag "my@#$tag"))))

  (testing "unicode letters preserved"
    (is (= "über" (tags/normalize-tag "Über")))
    (is (= "café" (tags/normalize-tag "Café")))
    (is (= "naïve" (tags/normalize-tag "Naïve")))
    (is (= "日本語" (tags/normalize-tag "日本語"))))

  (testing "hyphens preserved and collapsed"
    (is (= "my-tag" (tags/normalize-tag "my--tag")))
    (is (= "tag" (tags/normalize-tag "-tag-")))
    (is (= "a-b" (tags/normalize-tag "a---b"))))

  (testing "nil for empty/blank/nil"
    (is (nil? (tags/normalize-tag "")))
    (is (nil? (tags/normalize-tag "   ")))
    (is (nil? (tags/normalize-tag "!!!")))
    (is (nil? (tags/normalize-tag nil)))
    (is (nil? (tags/normalize-tag "  !@#  "))))

  (testing "digits allowed"
    (is (= "tag123" (tags/normalize-tag "tag123")))
    (is (= "2024" (tags/normalize-tag "2024"))))

  (testing "namespaced keywords use name part only"
    (is (= "bar" (tags/normalize-tag :foo/bar))))

  (testing "non-string non-keyword input"
    (is (= "42" (tags/normalize-tag 42)))))

(deftest test-normalize-tags
  (testing "filters nil results"
    (is (= ["good" "also-good"]
           (vec (tags/normalize-tags [:good "!!!" :Also-Good ""])))))

  (testing "empty input"
    (is (= [] (vec (tags/normalize-tags []))))))
