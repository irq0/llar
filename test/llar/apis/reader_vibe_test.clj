(ns llar.apis.reader-vibe-test
  (:require
   [clojure.test :refer [deftest is]]
   [hiccup2.core :as h]
   [java-time.api :as time]
   [llar.apis.reader :as reader]
   [llar.persistency :as persistency]
   [llar.vibe :as vibe]))

(deftest todays-vibe-renders-offer-provenance-and-hides-seen-by-default
  (let [now (time/zoned-date-time)
        snapshot {:run-id "run-1"
                  :generated-at now
                  :clusters [{:id 0 :representative-id 1
                              :source-count 2 :article-count 2 :unseen-count 1
                              :match-score 0.75
                              :latest-ts now :terms ["election"]
                              :items [{:id 1 :title "Election result" :source-key "a"
                                       :ts now :tags ["unread"]}
                                      {:id 2 :title "Second report" :source-key "b"
                                       :ts now :tags []}]}
                             {:id 1 :representative-id 3
                              :source-count 1 :article-count 1 :unseen-count 0
                              :latest-ts now :terms []
                              :items [{:id 3 :title "Fully seen" :source-key "c"
                                       :ts now :tags []}]}]}]
    (with-redefs [vibe/current-vibe (atom snapshot)
                  reader/frontend-db :db
                  persistency/record-results-offered!
                  (fn [_ items _]
                    (mapv (fn [position item]
                            {:id (+ 100 position) :item-id (:id item)})
                          (range 1 (inc (count items))) items))]
      (let [rendered (str (h/html (reader/tools-view-handler
                                   {:view :todays-vibe :request-params {}})))]
        (is (re-find #"Election result" rendered))
        (is (re-find #"reader-vibe-representative-source\">a</span>" rendered))
        (is (re-find #"data-vibe-cluster-id=\"0\"" rendered))
        (is (re-find #"75% lexical agreement" rendered))
        (is (re-find #"data-vibe-terms=\"election\"" rendered))
        (is (re-find #"Coverage</span>a · b" rendered))
        (is (re-find #"Story signals</span>election" rendered))
        (is (re-find #"1 alternate report" rendered))
        (is (re-find #"latest &lt;1h" rendered))
        (is (re-find #"data-offer-id=\"101\"" rendered))
        (is (re-find #"data-offer-id=\"102\"" rendered))
        (is (re-find #"action=\"/reader/tools/todays-vibe/seen\"" rendered))
        (is (re-find #"name=\"run-id\" type=\"hidden\" value=\"run-1\"" rendered))
        (is (re-find #"name=\"cluster-id\" type=\"hidden\" value=\"0\"" rendered))
        (is (re-find #"Mark every report in this story seen" rendered))
        (is (not (re-find #"Fully seen" rendered)))))))

(deftest todays-vibe-included-seen-story-has-status-without-dead-action
  (let [now (time/zoned-date-time)
        snapshot {:run-id "run-seen"
                  :generated-at now
                  :window-hours 24
                  :clusters [{:id 2 :representative-id 7
                              :source-count 1 :article-count 1 :unseen-count 0
                              :latest-ts now :terms []
                              :items [{:id 7 :title "Already read" :source-key "source"
                                       :ts now :tags []}]}]}]
    (with-redefs [vibe/current-vibe (atom snapshot)
                  reader/frontend-db :db
                  persistency/record-results-offered!
                  (fn [_ items _]
                    (mapv (fn [item] {:id 201 :item-id (:id item)}) items))]
      (let [rendered (str (h/html (reader/tools-view-handler
                                   {:view :todays-vibe
                                    :request-params {:include-seen "true"}})))]
        (is (re-find #"Already read" rendered))
        (is (re-find #"Fully seen" rendered))
        (is (re-find #"including fully seen" rendered))
        (is (re-find #"24h window" rendered))
        (is (not (re-find #"reader-vibe-seen-form" rendered)))))))

(deftest mark-story-seen-rejects-a-stale-vibe-snapshot
  (with-redefs [vibe/current-vibe (atom {:run-id "current-run"
                                         :clusters []})]
    (is (= {:status 409
            :body "This Vibe snapshot is stale; reload and try again."}
           (reader/reader-mark-story-seen "stale-run" 0)))))
