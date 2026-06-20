(ns llar.digest-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [java-time.api :as time]
   [llar.appconfig :as appconfig]
   [llar.blobstore :as blobstore]
   [llar.digest :as uut]
   [llar.persistency :as persistency])
  (:import
   [java.io ByteArrayInputStream]
   [java.util.zip ZipFile]))

(deftest issue-tag-roundtrip-test
  (testing "issue-tag builds the expected keyword"
    (is (= :digest-issue-7 (uut/issue-tag 7))))
  (testing "parse-issue-number is the inverse"
    (is (= 7 (uut/parse-issue-number :digest-issue-7)))
    (is (= 42 (uut/parse-issue-number (uut/issue-tag 42)))))
  (testing "parse-issue-number ignores unrelated tags"
    (is (nil? (uut/parse-issue-number :digest)))
    (is (nil? (uut/parse-issue-number :saved)))
    (is (nil? (uut/parse-issue-number :unread)))))

(deftest issues-outside-window-test
  (with-redefs [persistency/get-tags
                (constantly #{:unread :saved :digest
                              :digest-issue-1 :digest-issue-2 :digest-issue-3})]
    (testing "keep latest 1: issues 1 and 2 are stale, 3 kept"
      (is (= #{1 2} (set (uut/issues-outside-window 1)))))
    (testing "keep latest 2: only issue 1 stale"
      (is (= #{1} (set (uut/issues-outside-window 2)))))
    (testing "keep more than exist: nothing stale"
      (is (empty? (uut/issues-outside-window 5)))))
  (with-redefs [persistency/get-tags (constantly #{:unread :saved :digest})]
    (testing "no issues yet: nothing stale"
      (is (empty? (uut/issues-outside-window 1))))))

(deftest llar-item-url-test
  (testing "nil when no reader base-url is configured"
    (with-redefs [appconfig/reader (constantly nil)]
      (is (nil? (uut/llar-item-url 5)))))
  (testing "builds an absolute reader url"
    (with-redefs [appconfig/reader (fn [_] "http://host:8023")]
      (is (= "http://host:8023/reader/group/default/none/source/all/item/by-id/5/"
             (uut/llar-item-url 5))))))

(defn- zip-entries
  "Return a map of entry-name -> entry-content-string for a zip/epub file."
  [file]
  (with-open [zf (ZipFile. ^java.io.File file)]
    (into {}
          (map (fn [e]
                 [(.getName e) (slurp (.getInputStream zf e))]))
          (enumeration-seq (.entries zf)))))

(def ^:private sample-items
  [{:id 1 :title "First Article" :author "Ada"
    :source-key :blog-a :ts (time/zoned-date-time 2026 6 1)
    :nwords 600 :top-words {"words" [["hello" 3]]}
    :entry {:url "https://example.com/1"}
    :data {:content {"text/html" "<p>Hello <b>world</b></p>"}}}
   {:id 2 :title "Second Article"
    :source-key :blog-b :ts (time/zoned-date-time 2026 6 2)
    :entry {:url "https://example.com/2"}
    :data {:description {"text/plain" "just a plain summary"}}}
   {:id 3 :title "Empty One" :entry {} :data {}}])

(deftest render-epub-test
  (let [f (uut/render-epub! 13 sample-items {:inline-images? false})]
    (try
      (testing "produces an existing, non-empty file"
        (is (.exists f))
        (is (pos? (.length f))))
      (let [entries (zip-entries f)
            opf (some (fn [[k v]] (when (string/ends-with? k ".opf") v)) entries)]
        (testing "valid epub container: mimetype is application/epub+zip"
          (is (= "application/epub+zip" (get entries "mimetype"))))
        (testing "metadata carries the issue title"
          (is (some? opf))
          (is (string/includes? opf "LLAR Digest #13")))
        (testing "one xhtml chapter per item"
          (is (= 3 (count (filter (fn [[k _]]
                                    (re-matches #".*chapter-\d+\.xhtml" k))
                                  entries)))))
        (testing "html content is carried through"
          (let [ch1 (some (fn [[k v]] (when (string/includes? k "chapter-1") v)) entries)]
            (is (string/includes? ch1 "Hello"))
            (is (string/includes? ch1 "First Article"))
            (is (string/includes? ch1 "https://example.com/1"))))
        (testing "header has absolute timestamp and reading-time estimate"
          (let [ch1 (some (fn [[k v]] (when (string/includes? k "chapter-1") v)) entries)]
            (is (string/includes? ch1 "2026-06-01 00:00"))
            (is (string/includes? ch1 "min read"))))
        (testing "plain-text body is wrapped, not dropped"
          (let [ch2 (some (fn [[k v]] (when (string/includes? k "chapter-2") v)) entries)]
            (is (string/includes? ch2 "just a plain summary"))))
        (testing "item with no content still renders a chapter"
          (let [ch3 (some (fn [[k v]] (when (string/includes? k "chapter-3") v)) entries)]
            (is (string/includes? ch3 "no content"))))
        (testing "cover page has title, date, article count and grouped TOC"
          (let [cover (some (fn [[k v]] (when (string/includes? k "digest-cover") v)) entries)]
            (is (some? cover))
            (is (string/includes? cover "LLAR Digest #13"))
            (is (string/includes? cover "3 articles"))
            (is (re-find #"\d{4}-\d{2}-\d{2}" cover))     ; generation date (today)
            (is (string/includes? cover "Contents"))
            (is (string/includes? cover "blog-a"))
            (is (string/includes? cover "blog-b"))
            (is (string/includes? cover "unknown"))       ; item without a source-key
            (is (string/includes? cover "chapter-1.xhtml")))))
      (finally (.delete f)))))

(def ^:private blob-hash-hex
  "a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c6d7e8f90")

(defn- chapter-1 [entries]
  (some (fn [[k v]] (when (string/includes? k "chapter-1") v)) entries))

(deftest blob-hash-test
  (testing "extracts the content hash from a root-relative blobstore src"
    (is (= blob-hash-hex (#'uut/blob-hash (str "/blob/" blob-hash-hex)))))
  (testing "also matches when a base url made the src absolute"
    (is (= blob-hash-hex (#'uut/blob-hash (str "https://reader.example/blob/" blob-hash-hex)))))
  (testing "nil for remote, data and blank srcs (never matched -> never downloaded)"
    (is (nil? (#'uut/blob-hash "https://example.com/cat.png")))
    (is (nil? (#'uut/blob-hash "data:image/png;base64,AAAA")))
    (is (nil? (#'uut/blob-hash "")))
    (is (nil? (#'uut/blob-hash nil)))))

(deftest inline-blobstore-images-test
  (let [img-bytes (.getBytes "fake-png-bytes" "UTF-8")
        items [{:id 1 :title "With Image" :source-key :blog-a
                :entry {:url "https://example.com/1"}
                :data {:content {"text/html"
                                 (str "<p>before <img src=\"/blob/" blob-hash-hex
                                      "\"/> after</p>")}}}]]
    (testing "blobstored <img> is embedded from the blobstore and rewritten to a local href"
      (with-redefs [blobstore/get-blob (fn [h]
                                         (is (= blob-hash-hex h) "looked up by the hash from src")
                                         {:data (ByteArrayInputStream. img-bytes)
                                          :mime-type "image/png"
                                          :size (count img-bytes)})]
        (let [f (uut/render-epub! 1 items {:inline-images? true})]
          (try
            (let [entries (zip-entries f)
                  ch1 (chapter-1 entries)
                  img-entry (some (fn [[k v]] (when (string/ends-with? k "images/img-1.png") v)) entries)]
              (is (some? img-entry)
                  "image embedded as an epub resource, extension from mime-type")
              (is (= "fake-png-bytes" img-entry)
                  "embedded bytes come straight from the blobstore")
              (is (string/includes? ch1 "src=\"images/img-1.png\"")
                  "chapter <img> rewritten to the embedded resource")
              (is (not (string/includes? ch1 "/blob/"))
                  "no dangling blobstore reference left behind"))
            (finally (.delete f))))))

    (testing "oversized blob is left as a /blob/ reference, not embedded"
      (with-redefs [blobstore/get-blob (fn [_]
                                         {:data (ByteArrayInputStream. img-bytes)
                                          :mime-type "image/png"
                                          :size (inc (* 5 1024 1024))})]
        (let [f (uut/render-epub! 1 items {:inline-images? true})]
          (try
            (let [ch1 (chapter-1 (zip-entries f))]
              (is (string/includes? ch1 (str "/blob/" blob-hash-hex)))
              (is (not (string/includes? ch1 "images/img-1"))))
            (finally (.delete f))))))

    (testing "non-blobstore <img> is never fetched and left untouched"
      (let [remote-items [{:id 1 :title "Remote Image" :source-key :blog-a
                           :entry {:url "https://example.com/1"}
                           :data {:content {"text/html"
                                            "<p><img src=\"https://example.com/cat.png\"/></p>"}}}]]
        (with-redefs [blobstore/get-blob (fn [_] (throw (ex-info "must not fetch non-blob images" {})))]
          (let [f (uut/render-epub! 1 remote-items {:inline-images? true})]
            (try
              (let [ch1 (chapter-1 (zip-entries f))]
                (is (string/includes? ch1 "https://example.com/cat.png"))
                (is (not (string/includes? ch1 "images/img-1"))))
              (finally (.delete f)))))))))

(deftest grouping-test
  (testing "items are grouped by source in first-seen order; chapters numbered globally"
    (let [items [{:id 1 :title "A1" :source-key :alpha :entry {} :data {:content {"text/html" "<p>a1</p>"}}}
                 {:id 2 :title "B1" :source-key :beta :entry {} :data {:content {"text/html" "<p>b1</p>"}}}
                 {:id 3 :title "A2" :source-key :alpha :entry {} :data {:content {"text/html" "<p>a2</p>"}}}]
          f (uut/render-epub! 1 items {:inline-images? false})]
      (try
        (let [entries (zip-entries f)
              cover (some (fn [[k v]] (when (string/includes? k "digest-cover") v)) entries)]
          (testing "one section divider per distinct source (2), not per item"
            (is (= 2 (count (filter (fn [[k _]] (re-matches #".*section-\d+\.xhtml" k)) entries)))))
          (testing "three chapters total"
            (is (= 3 (count (filter (fn [[k _]] (re-matches #".*chapter-\d+\.xhtml" k)) entries)))))
          (testing "alpha group lists both its articles together in the TOC"
            (let [alpha-idx (.indexOf cover "alpha")
                  beta-idx (.indexOf cover "beta")]
              (is (< alpha-idx beta-idx) "alpha appears before beta (first-seen order)")
              ;; both A1 and A2 are listed under alpha, before beta's heading
              (is (< (.indexOf cover "A1") beta-idx))
              (is (< (.indexOf cover "A2") beta-idx)))))
        (finally (.delete f))))))
