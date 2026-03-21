(ns llar.podcast-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.data.xml :as xml]
   [java-time.api :as time]
   [llar.apis.podcast :as podcast-api]
   [llar.podcast :as uut])
  (:import
   [java.time ZonedDateTime]))

(deftest test-format-duration
  (testing "formats seconds as HH:MM:SS"
    (is (= "01:30:00" (podcast-api/format-duration 5400)))
    (is (= "00:05:23" (podcast-api/format-duration 323)))
    (is (= "00:00:00" (podcast-api/format-duration 0)))
    (is (= "00:00:01" (podcast-api/format-duration 1)))
    (is (= "10:00:00" (podcast-api/format-duration 36000))))
  (testing "returns nil for nil input"
    (is (nil? (podcast-api/format-duration nil)))))

(deftest test-format-rfc2822
  (testing "formats ZonedDateTime as RFC 2822"
    (let [zdt (time/zoned-date-time 2024 3 15 12 30 0 0 "UTC")
          result (podcast-api/format-rfc2822 zdt)]
      (is (string? result))
      (is (re-find #"Fri, 15 Mar 2024 12:30:00" result))))
  (testing "returns nil for nil input"
    (is (nil? (podcast-api/format-rfc2822 nil)))))

(deftest test-downloadable-media-url
  (testing "YouTube URLs are downloadable"
    (is (uut/downloadable-media-url? "https://www.youtube.com/watch?v=abc123"))
    (is (uut/downloadable-media-url? "https://youtu.be/abc123"))
    (is (uut/downloadable-media-url? "https://youtube.com/embed/abc123")))
  (testing "direct media URLs are downloadable"
    (is (uut/downloadable-media-url? "https://example.com/file.mp3"))
    (is (uut/downloadable-media-url? "https://example.com/file.mp4"))
    (is (uut/downloadable-media-url? "https://example.com/file.m4a"))
    (is (uut/downloadable-media-url? "https://example.com/file.ogg"))
    (is (uut/downloadable-media-url? "https://example.com/file.webm")))
  (testing "regular URLs are not downloadable"
    (is (not (uut/downloadable-media-url? "https://example.com/article")))
    (is (not (uut/downloadable-media-url? "https://example.com/page.html")))
    (is (not (uut/downloadable-media-url? nil)))))

(deftest test-media-type
  (testing "YouTube URLs classified as video"
    (is (= :video (uut/media-type "https://youtube.com/watch?v=abc")))
    (is (= :video (uut/media-type "https://youtu.be/abc123"))))
  (testing "audio file URLs classified as audio"
    (is (= :audio (uut/media-type "https://example.com/file.mp3")))
    (is (= :audio (uut/media-type "https://example.com/file.m4a")))
    (is (= :audio (uut/media-type "https://example.com/file.ogg"))))
  (testing "video file URLs classified as video"
    (is (= :video (uut/media-type "https://example.com/file.mp4")))
    (is (= :video (uut/media-type "https://example.com/file.webm"))))
  (testing "non-media URLs return nil"
    (is (nil? (uut/media-type "https://example.com/article")))
    (is (nil? (uut/media-type nil)))))

(deftest test-wrap-token-auth
  (let [handler (fn [_] {:status 200 :body "ok"})
        wrapped (podcast-api/wrap-token-auth handler)]
    (testing "valid token passes through"
      (with-redefs [llar.appconfig/credentials (fn [k] (when (= k :podcast-token) "secret123"))]
        (let [response (wrapped {:params {"token" "secret123"}})]
          (is (= 200 (:status response))))))
    (testing "wrong token returns 403"
      (with-redefs [llar.appconfig/credentials (fn [k] (when (= k :podcast-token) "secret123"))]
        (let [response (wrapped {:params {"token" "wrong"}})]
          (is (= 403 (:status response))))))
    (testing "missing token returns 403"
      (with-redefs [llar.appconfig/credentials (fn [k] (when (= k :podcast-token) "secret123"))]
        (let [response (wrapped {:params {}})]
          (is (= 403 (:status response))))))))
