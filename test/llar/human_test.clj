(ns llar.human-test
  (:require
   [clojure.test :refer [are deftest is testing]]
   [java-time.api :as time]
   [llar.human :as uut]))

(deftest truncate-ellipsis-test
  (testing "short string returned as-is"
    (is (= "hello" (uut/truncate-ellipsis "hello" 10))))
  (testing "exact length string returned as-is"
    (is (= "hello" (uut/truncate-ellipsis "hello" 5))))
  (testing "string truncated with ellipsis"
    (is (= "hell…" (uut/truncate-ellipsis "hello world" 5))))
  (testing "empty string"
    (is (= "" (uut/truncate-ellipsis "" 5))))
  (testing "len 1 produces just ellipsis"
    (is (= "…" (uut/truncate-ellipsis "hello" 1)))))

(deftest truncate-test
  (testing "short string returned as-is"
    (is (= "hello" (uut/truncate "hello" 10))))
  (testing "exact length returned as-is"
    (is (= "hello" (uut/truncate "hello" 5))))
  (testing "string truncated without ellipsis"
    (is (= "hello" (uut/truncate "hello world" 5))))
  (testing "empty string"
    (is (= "" (uut/truncate "" 5)))))

(deftest format-duration-test
  (testing "numeric seconds"
    (are [expected seconds] (= expected (uut/format-duration seconds))
      "500 ms" 0.5
      "12.5 s" 12.5
      "2 m 5 s" 125
      "2 h 3 m" 7380))
  (testing "java duration"
    (is (= "1 m 30 s" (uut/format-duration (time/seconds 90))))))

(deftest filesize-test
  (testing "bytes"
    (is (= "1.00 B" (uut/filesize 1)))
    (is (= "999.00 B" (uut/filesize 999))))
  (testing "kilobytes"
    (is (= "1.00KB" (uut/filesize 1024)))
    (is (= "1.50KB" (uut/filesize 1536))))
  (testing "megabytes"
    (is (= "1.00MB" (uut/filesize (* 1024 1024)))))
  (testing "gigabytes"
    (is (= "1.00GB" (uut/filesize (* 1024 1024 1024))))))

(deftest datetime-ago-short-test
  (testing "less than 1 hour"
    (let [ts (time/minus (time/zoned-date-time) (time/minutes 30))]
      (is (= "<1h" (uut/datetime-ago-short ts)))))
  (testing "a few hours ago"
    (let [ts (time/minus (time/zoned-date-time) (time/hours 3))]
      (is (= "3h" (uut/datetime-ago-short ts)))))
  (testing "multiple days ago (uses period)"
    (let [ts (time/minus (time/zoned-date-time) (time/days 5))]
      (is (string? (uut/datetime-ago-short ts)))
      (is (seq (uut/datetime-ago-short ts))))))

(deftest datetime-ago-does-not-render-negative-future-durations
  (is (= "just now"
         (uut/datetime-ago (time/plus (time/zoned-date-time) (time/hours 2))))))

(deftest host-identifier-test
  (testing "standard domain"
    (are [expected url] (= expected (uut/host-identifier url))
      "www.example.com" "https://www.example.com/path"
      "sub.example.com" "http://sub.example.com/"
      "example.org" "https://example.org/"))
  (testing "IP address or unparsable falls back to host string"
    (is (string? (uut/host-identifier "http://192.168.1.1/")))))
