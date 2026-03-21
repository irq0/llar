(ns llar.podcast-test
  (:require
   [cheshire.core :as json]
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

(deftest test-chapters->json
  (testing "converts yt-dlp chapters to Podcasting 2.0 JSON"
    (let [chapters [{:title "Intro" :start_time 0 :end_time 120}
                    {:title "Main Topic" :start_time 120 :end_time 600}]
          result (json/parse-string (#'podcast-api/chapters->json chapters) true)]
      (is (= "1.2.0" (:version result)))
      (is (= 2 (count (:chapters result))))
      (is (= 0 (get-in result [:chapters 0 :startTime])))
      (is (= "Intro" (get-in result [:chapters 0 :title])))
      (is (= 120 (get-in result [:chapters 0 :endTime])))))
  (testing "handles chapters without end_time"
    (let [chapters [{:title "Segment" :start_time 0}]
          result (json/parse-string (#'podcast-api/chapters->json chapters) true)]
      (is (= 1 (count (:chapters result))))
      (is (nil? (get-in result [:chapters 0 :endTime]))))))

(deftest test-format-description-html
  (testing "includes provenance header"
    (let [html (#'podcast-api/format-description-html "desc" :my-source "https://example.com/video")]
      (is (re-find #"<strong>Source:</strong> my-source" html))
      (is (re-find #"href=\"https://example.com/video\"" html))
      (is (re-find #"Original" html))))
  (testing "converts newlines to <br/>"
    (let [html (#'podcast-api/format-description-html "line1\nline2" :src nil)]
      (is (re-find #"line1<br/>" html))))
  (testing "escapes HTML entities"
    (let [html (#'podcast-api/format-description-html "a<b>c" :src nil)]
      (is (re-find #"a&lt;b&gt;c" html))))
  (testing "linkifies URLs"
    (let [html (#'podcast-api/format-description-html "visit https://example.com now" :src nil)]
      (is (re-find #"<a href=\"https://example.com\">https://example.com</a>" html))))
  (testing "handles nil source-key"
    (let [html (#'podcast-api/format-description-html "desc" nil nil)]
      (is (not (re-find #"Source:" html)))))
  (testing "handles blank description"
    (let [html (#'podcast-api/format-description-html "" :src nil)]
      (is (string? html)))))
