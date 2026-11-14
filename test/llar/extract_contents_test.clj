(ns llar.extract-contents-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as string]
            [llar.analysis :as analysis]
            [llar.fetch :as fetch]
            [llar.fetchutils :as uut]
            [llar.postproc :as proc]
            [llar.http :as http]))

(def email
  {:hash "unchanged" :meta {:tags #{:highlight}}
   :entry {:authors ["Newsletter <newsletter@example.com>"]
           :contents {"text/html" "<div>Original email</div>" "text/plain" "Original email"}
           :nlp {:nwords 99}}})

(deftest configured-extraction-uses-received-html-and-keeps-original
  (let [calls (atom [])
        processor (uut/readability-contents :from :contents :selector ".body.markup")]
    (with-redefs [fetch/fetch-source (fn [& _] (throw (AssertionError. "Must not fetch the web")))
                  http/raw-readability (fn [html url options]
                                         (swap! calls conj [html url options])
                                         {:content "<p>Clean article</p>" :textContent "Clean article"
                                          :excerpt "Clean summary" :sourceSnapshot html})
                  analysis/analyze-entry (fn [entry]
                                           (is (= "Clean article" (get-in entry [:contents "text/plain"])))
                                           {:nlp {:nwords 2}})]
      (let [result (processor email)]
        (is (= (:hash email) (:hash result)))
        (is (= (:meta email) (:meta result)))
        (is (= (get-in email [:entry :contents]) (get-in result [:entry :source-snapshot])))
        (is (= {:nwords 2} (get-in result [:entry :nlp])))
        (is (= "Clean summary" (get-in result [:entry :descriptions "text/plain"])))
        (is (= result (processor result)))
        (is (= [["<div>Original email</div>" nil {:from :contents :selector ".body.markup"}]
                ["<div>Original email</div>" nil {:from :contents :selector ".body.markup"}]] @calls))))))

(deftest extraction-failure-and-missing-content-preserve-the-item
  (doseq [response [{} {:content "" :textContent ""}]]
    (with-redefs [http/raw-readability (fn [& _] response)]
      (is (= email ((uut/readability-contents :from :contents) email)))))
  (with-redefs [http/raw-readability (fn [& _] (throw (ex-info "Failed" {})))]
    (is (= email ((uut/readability-contents :from :contents) email))))
  (testing "text-only emails need no extraction"
    (let [item (update-in email [:entry :contents] dissoc "text/html")]
      (is (= item ((uut/readability-contents :from :contents) item)))))
  (testing "the default URL processor leaves URL-less email alone"
    (is (= email ((uut/readability-contents) email)))))

(deftest url-extraction-retains-the-page-and-reuses-it-on-reprocessing
  (let [fetches (atom 0)
        snapshot {"text/html" "<article>Full source page</article>"}
        incoming {:entry {:url "https://example.com/article"
                          :contents {"text/html" "<p>Feed teaser</p>"}}}
        processor (uut/readability-contents)]
    (with-redefs [fetch/fetch-source
                  (fn [& _]
                    (swap! fetches inc)
                    [{:entry {:contents {"text/html" "<p>Extracted article</p>" "text/plain" "Extracted article"}
                              :source-snapshot snapshot
                              :source-snapshot-url "https://example.com/article"}}])
                  proc/process-feedless-item (fn [_ item] item)
                  http/raw-readability
                  (fn [html url _]
                    (is (= (get snapshot "text/html") html))
                    (is (= "https://example.com/article" url))
                    {:content "<p>Re-extracted</p>" :textContent "Re-extracted"
                     :sourceSnapshot "<p>Must not replace the retained snapshot</p>"})
                  analysis/analyze-entry (constantly {:nlp {:nwords 1}})]
      (let [extracted (processor incoming)
            reprocessed (processor extracted)]
        (is (= snapshot (get-in extracted [:entry :source-snapshot])))
        (is (= "<p>Extracted article</p>" (get-in extracted [:entry :contents "text/html"])))
        (is (= snapshot (get-in reprocessed [:entry :source-snapshot])))
        (is (string/includes? (get-in reprocessed [:entry :contents "text/html"]) "<p>Re-extracted</p>"))
        (is (= 1 @fetches))
        (processor incoming)
        (is (= 2 @fetches) "A fresh item capture still fetches upstream")))))

(deftest stores-the-sanitized-snapshot-instead-of-the-raw-input
  (with-redefs [http/raw-readability (constantly {:content "<p>Article</p>" :textContent "Article"
                                                  :sourceSnapshot "<div>Safe source</div>"})
                analysis/analyze-entry (constantly {})]
    (is (= "<div>Safe source</div>"
           (get-in ((uut/readability-contents :from :contents) email)
                   [:entry :source-snapshot "text/html"])))))

(deftest removed-concatenation-option-is-rejected
  (is (thrown? AssertionError (uut/readability-contents :keep-orig? true))))

(deftest re-extracted-images-use-the-normal-blob-store
  (with-redefs [http/raw-readability
                (constantly {:content "<p>Article</p><img src='https://example.com/photo.jpg'>"
                             :textContent "Article" :sourceSnapshot "<div>Source</div>"})
                http/try-blobify-url! (constantly "/blob/stored-image")
                analysis/analyze-entry (constantly {})]
    (is (string/includes? (get-in ((uut/readability-contents :from :contents) email)
                                  [:entry :contents "text/html"])
                          "src=\"/blob/stored-image\""))))
