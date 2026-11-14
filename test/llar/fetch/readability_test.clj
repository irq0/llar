(ns llar.fetch.readability-test
  (:require
   [clojure.test :refer [deftest is]]
   [clojure.string :as string]
   [java-time.api :as time]
   [llar.analysis :as analysis]
   [llar.fetch :as fetch]
   [llar.fetch.readability :as readability]
   [llar.http :as http]
   [llar.src :as src]
   [llar.postproc :as postproc]))

(deftest post-process-tags-analyzed-video-urls
  (let [item (readability/->ReadabilityItem
              {:tags #{}}
              {}
              nil
              {:url "https://example.com/article"})
        processed (with-redefs [analysis/analyze-entry
                                (constantly
                                 {:language :en
                                  :nlp {:urls #{"https://www.youtube.com/watch?v=123"}}})]
                    (postproc/post-process-item item nil nil))]
    (is (contains? (get-in processed [:meta :tags]) :has-video))
    (is (= #{"https://www.youtube.com/watch?v=123"}
           (get-in processed [:entry :nlp :urls])))))

(deftest url-fetch-captures-safe-input-before-extraction
  (let [calls (atom [])
        page "<html><body>Unprocessed page</body></html>"
        snapshot "<html lang='en'><head><title>Page title</title></head><body class='article-page'><div class='article'><p>Complete source text</p><img src='/blob/site-image'><a href='/more'>More</a></div></body></html>"]
    (with-redefs [http/fetch (fn [& _] {:status :ok :body page :summary {:ts (time/zoned-date-time)}})
                  http/raw-readability (fn [html _]
                                         (swap! calls conj html)
                                         {:content "<p>Article</p>" :textContent "Article"
                                          :sourceSnapshot snapshot :title "Title"
                                          :sourceSnapshotUrl "https://cdn.example.com/posts/"})
                  http/blobify identity]
      (let [item (first (fetch/fetch-source (src/readability "https://example.com/story") {}))
            entry (:entry item)
            retained (get-in entry [:source-snapshot "text/html"])]
        (is (= [page] @calls))
        (is (string/includes? retained "Complete source text"))
        (is (string/includes? retained "<title>Page title</title>"))
        (is (string/includes? retained "<body class=\"article-page\">"))
        (is (string/includes? retained "https://cdn.example.com/blob/site-image"))
        (is (string/includes? retained "https://cdn.example.com/more"))
        (is (= "https://cdn.example.com/posts/" (:source-snapshot-url entry)))
        (is (not (contains? (:readability entry) :sourceSnapshot)))
        (is (not (string/includes? (get-in entry [:contents "text/html"]) "Complete source text")))))))
