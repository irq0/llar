(ns llar.media-library-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [java-time.api :as time]
   [llar.apis.media-library :as media-api]
   [llar.blobstore :as blobstore]
   [llar.media-library :as uut]
   [llar.podcast :as podcast]))

(deftest virtual-layout
  (let [entries [{:item-id 1
                  :filename "First-1.mp4"
                  :source-directory "source-a"
                  :source-title "Source A Channel"
                  :year "2026"
                  :month "08"
                  :blob-hash "hash-1"
                  :mime-type "video/mp4"}
                 {:item-id 2
                  :filename "Second-2.m4a"
                  :source-directory "source-b"
                  :year "2026"
                  :month "07"
                  :blob-hash "hash-2"
                  :mime-type "audio/mp4"}]]
    (with-redefs [blobstore/get-blob-metadata
                  (fn [hash] {:size (if (= hash "hash-1") 100 200)})]
      (testing "root exposes source and date views"
        (is (= [["By-Source"] ["By-Date"]]
               (mapv :path (:children (uut/directory [] entries))))))
      (testing "source view exposes source folders and media"
        (is (= #{["By-Source" "source-a"] ["By-Source" "source-b"]}
               (->> (:children (uut/directory ["By-Source"] entries))
                    (filter :collection?)
                    (map :path)
                    set)))
        (is (= "Source A Channel · source-a"
               (->> (:children (uut/directory ["By-Source"] entries))
                    (filter #(= ["By-Source" "source-a"] (:path %)))
                    first
                    :display-name)))
        (is (= [["By-Source" "source-a" "First-1.mp4"]]
               (->> (:children (uut/directory ["By-Source" "source-a"] entries))
                    (filter #(= :blob (:resource-kind %)))
                    (mapv :path))))
        (is (some #(= ["By-Source" "source-a" "folder.png"] (:path %))
                  (:children (uut/directory
                              ["By-Source" "source-a"] entries)))))
      (testing "date view stops at year and month before listing media"
        (is (= [["By-Date" "2026"]]
               (mapv :path (:children (uut/directory ["By-Date"] entries)))))
        (is (= #{["By-Date" "2026" "07"] ["By-Date" "2026" "08"]}
               (set (map :path (:children (uut/directory
                                           ["By-Date" "2026"] entries))))))
        (is (= [["By-Date" "2026" "08" "First-1.mp4"]
                ["By-Date" "2026" "08" "First-1.nfo"]]
               (mapv :path (:children (uut/directory
                                       ["By-Date" "2026" "08"] entries)))))))))

(deftest paths-and-stale-items
  (is (= "M%C3%BCnchen%20Videos/Tea%20%26%20Talks"
         (uut/encoded-path ["München Videos" "Tea & Talks"])))
  (is (= "München & Talks" (uut/safe-name "München & Talks" "fallback")))
  (let [entry {:item-id 1
               :filename "Missing-1.mp4"
               :source-directory "source-a"
               :year "2026"
               :month "08"
               :blob-hash "missing"
               :mime-type "video/mp4"}]
    (with-redefs [blobstore/get-blob-metadata
                  (fn [_] (throw (java.io.FileNotFoundException. "missing")))]
      (is (= ["folder.png"]
             (mapv :display-name
                   (:children (uut/directory
                               ["By-Source" "source-a"] [entry]))))))))

(deftest disambiguates-sanitized-source-names
  (with-redefs [podcast/read-podcast-index
                (constantly
                 {"hash-1" {:item-id 1 :source-key :source?name
                            :item-title "One" :mime-type "video/mp4"}
                  "hash-2" {:item-id 2 :source-key :source*name
                            :item-title "Two" :mime-type "video/mp4"}})]
    (let [names (mapv :source-directory (uut/entries))]
      (is (= 2 (count (distinct names))))
      (is (every? #(string/starts-with? % "source-name--") names)))))

(deftest reserves-favorite-artwork-paths
  (with-redefs [podcast/read-podcast-index
                (constantly
                 {"hash-1" {:item-id 1 :source-key :favorite.png
                            :item-title "One" :mime-type "video/mp4"}})]
    (is (string/starts-with? (:source-directory (first (uut/entries)))
                             "favorite.png--"))))

(deftest generated-navigation-artwork-is-stable-as-the-library-grows
  (let [favorite-etag (fn [entries]
                        (->> (:children (uut/directory ["By-Source"] entries))
                             (some #(when (= "favorite.png" (:display-name %)) %))
                             :etag))
        entry {:item-id 1
               :filename "First-1.mp4"
               :source-directory "source-a"
               :source-title "Source A"
               :source-key :source-a
               :blob-hash "hash-1"
               :mime-type "video/mp4"}
        second-entry (assoc entry
                            :item-id 2
                            :filename "Second-2.mp4"
                            :blob-hash "hash-2")]
    (is (= (favorite-etag [])
           (favorite-etag [entry second-entry]))
        "favorite artwork must not change between ranged client requests")
    (with-redefs [blobstore/get-blob-metadata
                  (constantly {:size 100 :mime-type "video/mp4"})]
      (let [folder-etag (fn [entries]
                          (->> (:children (uut/directory
                                           ["By-Source" "source-a"] entries))
                               (some #(when (= "folder.png" (:display-name %)) %))
                               :etag))]
        (is (= (folder-etag [entry])
               (folder-etag [entry second-entry]))
            "source artwork must not change between ranged client requests")))))

(deftest webdav-discovery
  (with-redefs [media-api/base-url (constantly "https://media.example")
                podcast/read-podcast-index (constantly {})]
    (let [response (media-api/app {:request-method :propfind
                                   :uri "/library/"
                                   :headers {"depth" "1"}})]
      (is (= 207 (:status response)))
      (is (= "1" (get-in response [:headers "DAV"])))
      (is (string/includes? (:body response) "By-Source"))
      (is (string/includes? (:body response) "By-Date"))
      (is (not (string/includes? (:body response) "token="))))
    (testing "advertised favorite artwork is retrievable"
      (let [response (media-api/app {:request-method :get
                                     :uri "/library/By-Source/favorite-atv.png"})]
        (is (= 200 (:status response)))
        (is (= "image/png" (get-in response [:headers "Content-Type"])))))
    (testing "known collections reject writes explicitly"
      (let [response (media-api/app {:request-method :put
                                     :uri "/library/"})]
        (is (= 405 (:status response)))
        (is (= "Method Not Allowed" (:body response)))))))

(deftest infuse-sidecars
  (let [completed-at (time/zoned-date-time 2026 8 22 10 0 0 0
                                           "Europe/Berlin")
        index {"media-hash" {:item-id 1
                             :item-title "First"
                             :source-title "Source A Channel"
                             :source-key :source-a
                             :mime-type "video/mp4"
                             :completed-at completed-at}}
        blobs {"media-hash" {:size 100
                             :mime-type "video/mp4"
                             :podcast-metadata
                             {:id "video-1"
                              :extractor "youtube"
                              :title "First & Best"
                              :description "A useful description."
                              :original_url "https://youtube.example/watch?v=1&list=all"
                              :channel "Source A Channel"
                              :upload_date "20260820"
                              :categories ["Technology"]
                              :tags ["Clojure"]
                              :transcript "WEBVTT\n\n00:00.000 --> 00:01.000\nHello"
                              :transcript-language "en-orig"
                              :transcript-format "vtt"
                              :transcript-mime-type "text/vtt"
                              :fanart-hash "fanart-hash"}}
               "fanart-hash" {:size 30 :mime-type "image/png"}}]
    (with-redefs [media-api/base-url (constantly "https://media.example")
                  podcast/read-podcast-index (constantly index)
                  blobstore/get-blob-metadata (fn [hash] (get blobs hash))]
      (let [listing (media-api/app {:request-method :propfind
                                    :uri "/library/By-Source/source-a/"
                                    :headers {"depth" "1"}})]
        (is (= 207 (:status listing)))
        (doseq [filename ["First-1.mp4"
                          "First-1.nfo"
                          "First-1-en.vtt"
                          "First-1-fanart.png"
                          "folder.png"]]
          (is (string/includes? (:body listing) filename)))
        (is (not (string/includes? (:body listing) "First-1.png")))
        (is (string/includes? (:body listing) "getlastmodified"))
        (is (string/includes? (:body listing)
                              "Sat, 22 Aug 2026 08:00:00 GMT"))
        (is (string/includes? (:body listing) "creationdate"))
        (is (string/includes? (:body listing) "\"media-hash\"")))
      (let [request {:uri "/library/By-Source/source-a/First-1-en.vtt"}
            transcript (media-api/app (assoc request :request-method :get))
            head (media-api/app (assoc request :request-method :head))]
        (is (= 200 (:status transcript)))
        (is (= "text/vtt" (get-in transcript [:headers "Content-Type"])))
        (is (= "WEBVTT\n\n00:00.000 --> 00:01.000\nHello"
               (slurp (:body transcript))))
        (is (= 200 (:status head)))
        (is (nil? (:body head)))
        (is (= (get-in transcript [:headers "Content-Length"])
               (get-in head [:headers "Content-Length"]))))

      (let [nfo (media-api/app {:request-method :get
                                :uri "/library/By-Source/source-a/First-1.nfo"})
            body (slurp (:body nfo))]
        (is (= 200 (:status nfo)))
        (is (= "application/xml; charset=utf-8"
               (get-in nfo [:headers "Content-Type"])))
        (is (string/includes? body "<title>First &amp; Best</title>"))
        (is (string/includes? body "A useful description."))
        (is (string/includes?
             body
             "Original: https://youtube.example/watch?v=1&amp;list=all"))
        (is (string/includes? body "<premiered>2026-08-20</premiered>"))))))
