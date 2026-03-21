(ns llar.fetch.streaming-test
  (:require
   [llar.fetch.streaming :as uut]
   [llar.postproc :as postproc]
   [llar.persistency :as persistency]
   [llar.fetch :as fetch]
   [llar.src :as src]
   [llar.specs]
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is testing]]))

(s/check-asserts true)

(deftest format-duration-test
  (testing "formats seconds into h:mm:ss"
    (is (= "1:30:00" (#'uut/format-duration 5400)))
    (is (= "0:30" (#'uut/format-duration 30)))
    (is (= "10:05" (#'uut/format-duration 605)))
    (is (= "2:00:00" (#'uut/format-duration 7200))))
  (testing "returns nil for zero or negative"
    (is (nil? (#'uut/format-duration 0)))
    (is (nil? (#'uut/format-duration -1)))
    (is (nil? (#'uut/format-duration nil)))))

(deftest streaming-channel-source-test
  (testing "creates StreamingChannel source"
    (let [src (src/streaming-channel "https://www.youtube.com/@veritasium")]
      (is (src/streaming-channel? src))
      (is (src/source? src))
      (is (= 30 (get-in src [:args :max-results])))))
  (testing "accepts custom max-results"
    (let [src (src/streaming-channel "https://www.youtube.com/@veritasium" :max-results 10)]
      (is (= 10 (get-in src [:args :max-results]))))))

(deftest streaming-html-summary-test
  (testing "generates HTML with channel and duration"
    (let [html (#'uut/streaming-html-summary "Video Title" "Channel" 600 "http://thumb.jpg" "http://example.com/watch")]
      (is (string? html))
      (is (.contains html "Channel"))
      (is (.contains html "10:00"))
      (is (.contains html "http://thumb.jpg"))))
  (testing "generates HTML without duration"
    (let [html (#'uut/streaming-html-summary "Title" "Ch" nil nil "http://url")]
      (is (string? html))
      (is (.contains html "Ch"))
      (is (not (.contains html "Duration"))))))

(deftest make-streaming-item-test
  (testing "creates valid StreamingItem"
    (let [meta {:source (src/streaming-channel "https://www.youtube.com/@test")
                :source-name "[StreamingChannel: https://www.youtube.com/@test]"
                :source-key :test
                :fetch-ts (java.time.ZonedDateTime/now)
                :tags #{}
                :version 2}
          summary {:ts (java.time.ZonedDateTime/now) :title "Test"}
          hash (fetch/make-item-hash "test-id")
          entry {:url "https://youtube.com/watch?v=abc" :title "Test"}
          item (uut/make-streaming-item meta summary hash entry)]
      (is (instance? llar.fetch.streaming.StreamingItem item))
      (is (= "Test" (get-in item [:summary :title]))))))

(defn- make-test-item [url]
  (let [meta {:source (src/streaming-channel "https://www.youtube.com/@test")
              :source-name "[StreamingChannel: test]"
              :source-key :test
              :fetch-ts (java.time.ZonedDateTime/now)
              :tags #{}
              :version 2}
        summary {:ts (java.time.ZonedDateTime/now) :title "Test"}
        hash (fetch/make-item-hash url)
        entry {:url url :title "Test" :authors ["Test"] :contents {}}]
    (uut/make-streaming-item meta summary hash entry)))

(deftest post-process-tagging-test
  (testing "YouTube URL gets :has-video tag"
    (let [item (make-test-item "https://www.youtube.com/watch?v=abc123")
          processed (postproc/post-process-item item nil nil)]
      (is (contains? (get-in processed [:meta :tags]) :has-video))))
  (testing "youtu.be URL gets :has-video tag"
    (let [item (make-test-item "https://youtu.be/abc123")
          processed (postproc/post-process-item item nil nil)]
      (is (contains? (get-in processed [:meta :tags]) :has-video))))
  (testing "SoundCloud URL gets :has-audio tag"
    (let [item (make-test-item "https://soundcloud.com/artist/track")
          processed (postproc/post-process-item item nil nil)]
      (is (contains? (get-in processed [:meta :tags]) :has-audio))))
  (testing "Bandcamp URL gets :has-audio tag"
    (let [item (make-test-item "https://artist.bandcamp.com/track/song")
          processed (postproc/post-process-item item nil nil)]
      (is (contains? (get-in processed [:meta :tags]) :has-audio))))
  (testing "media.ccc.de URL gets :has-video tag"
    (let [item (make-test-item "https://media.ccc.de/v/37c3-12345")
          processed (postproc/post-process-item item nil nil)]
      (is (contains? (get-in processed [:meta :tags]) :has-video))))
  (testing "regular URL gets no media tags"
    (let [item (make-test-item "https://example.com/article")
          processed (postproc/post-process-item item nil nil)]
      (is (not (contains? (get-in processed [:meta :tags]) :has-video)))
      (is (not (contains? (get-in processed [:meta :tags]) :has-audio))))))

(deftest to-couch-test
  (testing "converts StreamingItem to couch format"
    (let [item (make-test-item "https://www.youtube.com/watch?v=abc")
          couched (persistency/to-couch item)]
      (is (= :link (:type couched)))
      (is (nil? (get-in couched [:meta :source :args]))))))
