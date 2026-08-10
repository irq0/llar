(ns llar.apis.reader-related-test
  (:require
   [clojure.test :refer [deftest is]]
   [hiccup2.core :as h]
   [llar.apis.reader :as uut]
   [llar.db.search :as db-search]
   [llar.persistency :as persistency]))

(deftest related-button-is-a-compact-accessible-item-action
  (let [button (uut/related-button 42)]
    (is (= "btn reader-icon-button" (get-in button [1 :class])))
    (is (= "Find related items" (get-in button [1 :title])))
    (is (= "Find related items" (get-in button [1 :aria-label])))
    (is (= "/reader/item/by-id/42/related" (get-in button [1 :href])))
    (is (= "fas fa-project-diagram" (get-in button [2 1 :class])))))

(deftest external-link-button-preserves-optional-target
  (let [same-tab (uut/external-link-button "https://example.test")
        new-tab (uut/external-link-button "https://example.test" "_blank")]
    (is (= {:class "btn reader-icon-button"
            :title "Open original item"
            :aria-label "Open original item"
            :href "https://example.test"}
           (second same-tab)))
    (is (= "_blank" (get-in new-tab [1 :target])))
    (is (= "fas fa-external-link-alt" (get-in same-tab [2 1 :class])))))

(deftest item-toolbox-buttons-share-icon-button-markup
  (doseq [[button label icon-class]
          [[(uut/dump-button "/item/dump")
            "Inspect item"
            "fas fa-search"]
           [(uut/focus-button "/item/focus")
            "Show item HTML focus mode"
            "fas fa-expand"]
           [(uut/download-button "/item/download")
            "Open raw HTML content"
            "fas fa-remove-format"]]]
    (is (= "btn reader-icon-button" (get-in button [1 :class])))
    (is (= label (get-in button [1 :title])))
    (is (= label (get-in button [1 :aria-label])))
    (is (= icon-class (get-in button [2 1 :class])))))

(deftest related-route-renders-ranked-offer-links
  (let [now (java.time.ZonedDateTime/now)]
    (with-redefs [uut/frontend-db :db
                  db-search/related-items
                  (constantly {:item {:id 42 :title "Seed"}
                               :query "\"topic\""
                               :results [{:id 7 :title "Match" :key "source"
                                          :rank 0.25 :title-rank 0.1
                                          :relative-score 1.0
                                          :matched-terms ["topic" "system"]
                                          :ts now :headline "[[[topic]]] system"}]})
                  uut/gather-reader-index-data
                  (fn [_]
                    {:uri "/reader/item/by-id/42/related"
                     :group-name :default :group-item :none :source-key :all
                     :mode :show-item
                     :items [{:id 42 :title "Seed" :source-key "source" :tags []}]
                     :sources {} :active-sources [] :selected-sources []
                     :item-tags []})
                  persistency/record-results-offered!
                  (fn [& _] [{:id 99 :item-id 7}])]
      (let [rendered (str (uut/reader-related 42))]
        (is (re-find #"Related to “Seed”" rendered))
        (is (re-find #"id=\"top-nav\"" rendered))
        (is (re-find #"id=\"groupnav\"" rendered))
        (is (re-find #"data-offer-id=\"99\"" rendered))
        (is (re-find #"offer=99" rendered))
        (is (re-find #"100% of top" rendered))
        (is (re-find #">topic<" rendered))))))

(deftest show-item-breadcrumb-title-links-to-item-detail
  (let [rendered (str (h/html
                       (uut/nav-bar
                        {:uri "/reader/group/item-tags/saved/source/all/item/by-id/42"
                         :group-name :item-tags
                         :group-item :saved
                         :source-key :all
                         :mode :show-item
                         :selected-sources []
                         :items [{:id 42 :title "Selected item" :source-key "feed"
                                  :tags []}]})))]
    (is (re-find #"href=\"/reader/group/item-tags/saved/source/all/item/by-id/42\">Selected item</a>"
                 rendered))))
