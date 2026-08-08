(ns llar.apis.reader-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]
   [hiccup2.core :as h]
   [java-time.api :as time]
   [llar.apis.reader :as uut]
   [llar.db.search :as db-search]
   [llar.lab :as lab]
   [llar.persistency :as persistency]
   [llar.rc :as rc]
   [llar.vibe :as vibe]))

(deftest list-style-uses-rc-defaults
  (with-redefs [rc/rc (fn [path]
                        (when (= [:reader :default-list-view :blog] path)
                          :headlines))]
    (is (= :headlines (uut/get-list-style {:group-item :blog})))
    (is (= :gallery (uut/get-list-style {:group-item :blog
                                         :list-style :gallery})))))

(deftest youtube-preview-fills-ratio-container
  (let [rendered (str (uut/render-special-item-content
                       {:url "https://www.youtube.com/watch?v=abc123"
                        :entry {:thumbnail "/blob/youtube-thumbnail.jpg"}}
                       #{}))]
    (is (re-find #"class=\"youtube-preview-container\"" rendered))
    (is (re-find #"class=\"lazy-youtube\"" rendered))
    (is (re-find #"data-vid=\"abc123\"" rendered))
    (is (re-find #"alt=\"Play video on YouTube\"" rendered))))

(deftest youtube-player-sends-origin-referrer
  (let [javascript (slurp (io/resource "status/llar.js"))]
    (is (re-find #"youtube-nocookie\.com/embed/" javascript))
    (is (re-find #"referrerpolicy=\"strict-origin-when-cross-origin\"" javascript))))

(deftest intentional-reader-tags-use-explicit-transitions
  (let [calls (atom [])]
    (with-redefs [uut/frontend-db :db
                  persistency/item-set-tags!
                  (fn [_ id tags] (swap! calls conj [:set id tags]))
                  persistency/item-remove-tags!
                  (fn [_ id tags] (swap! calls conj [:remove id tags]))]
      (uut/reader-item-modify 42 :set :archive)
      (is (= [[:set 42 [:archive]]
              [:remove 42 [:saved :in-progress :unread]]]
             @calls)))))

(deftest item-view-tag-buttons-are-icon-only-with-tooltip-labels
  (let [button (uut/tag-button 42 {:tag :saved
                                   :is-set? false
                                   :icon-set "fas fa-star icon-is-set"
                                   :icon-unset "far fa-star"})]
    (is (= {:title "Save for later" :aria-label "Save for later"}
           (select-keys (second button) [:title :aria-label])))
    (is (= [[:i {:class "far fa-star"} "\u2009"]]
           (subvec button 2)))))

(deftest digest-tag-button-follows-runtime-configuration
  (with-redefs [rc/rc (fn [path] (= path [:digest :enabled?]))]
    (is (some #(= :digest (:tag %)) (uut/tag-buttons)))
    (is (= "Include in next digest"
           (get-in (uut/tag-button 42 (assoc uut/+digest-tag-button+ :is-set? false))
                   [1 :title])))
    (is (= "Remove from next digest"
           (get-in (uut/tag-button 42 (assoc uut/+digest-tag-button+ :is-set? true))
                   [1 :title]))))
  (with-redefs [rc/rc (constantly false)]
    (is (not-any? #(= :digest (:tag %)) (uut/tag-buttons)))))

(deftest todays-vibe-renders-offer-provenance-and-hides-seen-by-default
  (let [now (time/zoned-date-time)
        snapshot {:run-id "run-1"
                  :generated-at now
                  :clusters [{:id 0 :representative-id 1
                              :source-count 2 :article-count 2 :unseen-count 1
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
                  uut/frontend-db :db
                  persistency/record-results-offered!
                  (fn [_ items _]
                    (mapv (fn [position item]
                            {:id (+ 100 position) :item-id (:id item)})
                          (range 1 (inc (count items))) items))]
      (let [rendered (str (h/html (uut/tools-view-handler
                                   {:view :todays-vibe :request-params {}})))]
        (is (re-find #"Election result" rendered))
        (is (re-find #"data-offer-id=\"101\"" rendered))
        (is (re-find #"action=\"/reader/tools/todays-vibe/seen\"" rendered))
        (is (not (re-find #"Fully seen" rendered)))))))

(deftest item-detail-route-accepts-optional-offer-provenance
  (let [opened (atom [])
        modifications (atom [])
        request (fn [params]
                  (uut/app {:request-method :get
                            :uri "/reader/group/item-tags/digest-issue-4/source/all/item/by-id/24292"
                            :params params}))]
    (with-redefs [uut/frontend-db :db
                  uut/reader-index (fn [_] {:status 200 :body "item"})
                  uut/reader-item-modify
                  (fn [& args]
                    (swap! modifications conj args)
                    "")
                  persistency/record-item-opened!
                  (fn [& args] (swap! opened conj args))]
      (is (= 200 (:status (request {:mark "read"}))))
      (is (= 200 (:status (request {:mark "read" :offer "91"}))))
      (is (= [nil 91] (mapv last @opened)))
      (is (= 2 (count @modifications))))))

(deftest ranked-query-args-use-rc-ranking
  (with-redefs [rc/rc (fn [path]
                        (when (= [:reader :ranking] path)
                          {:highlight-boost-hours 6
                           :rarity-boost-cap-hours 12}))]
    (let [args (#'uut/build-items-query-args {:mode :list-items} :ranked)]
      (is (= 6 (:highlight-boost args)))
      (is (= 12 (:rarity-cap args)))
      (is (= :ranked (:sort-order args))))))

(deftest reading-queue-reasons
  (is (= [:saved]
         (#'uut/queue-item-reasons {:tags ["saved"]
                                    :type :item-type/link})))
  (is (= [:in-progress]
         (#'uut/queue-item-reasons {:tags ["in-progress"]
                                    :type :item-type/link})))
  (is (= [:unread-bookmark]
         (#'uut/queue-item-reasons {:tags ["unread"]
                                    :type :item-type/bookmark})))
  (is (= [:saved :unread-bookmark]
         (#'uut/queue-item-reasons {:tags ["saved" "unread"]
                                    :type :item-type/bookmark})))
  (is (= []
         (#'uut/queue-item-reasons {:tags ["unread" "highlight"]
                                    :type :item-type/link}))))

(deftest reading-queue-distinguishes-not-compiled-from-empty
  (with-redefs [lab/current-clustered-saved-items
                (atom lab/+saved-clusters-not-compiled+)]
    (let [rendered (pr-str (uut/tools-view-handler
                            {:view :saved-overview :request-params {}}))]
      (is (re-find #"has not been compiled yet" rendered))
      (is (not (re-find #"No saved" rendered)))))
  (with-redefs [lab/current-clustered-saved-items
                (atom {:clusters {} :last-update nil})]
    (let [rendered (pr-str (uut/tools-view-handler
                            {:view :saved-overview :request-params {}}))]
      (is (re-find #"No saved, in-progress" rendered))
      (is (not (re-find #"has not been compiled yet" rendered))))))

(deftest reading-queue-filters
  (let [saved {:tags ["saved"] :type :item-type/link}
        in-progress {:tags ["in-progress"] :type :item-type/link}
        unread-bookmark {:tags ["unread"] :type :item-type/bookmark}
        read-bookmark {:tags [] :type :item-type/bookmark}
        highlighted {:tags ["highlight"] :type :item-type/link}]
    (is (#'uut/queue-item-matches-filter? nil saved))
    (is (not (#'uut/queue-item-matches-filter? nil read-bookmark)))
    (is (not (#'uut/queue-item-matches-filter? nil highlighted)))
    (is (#'uut/queue-item-matches-filter? :saved saved))
    (is (#'uut/queue-item-matches-filter? :in-progress in-progress))
    (is (#'uut/queue-item-matches-filter? :unread-bookmarks unread-bookmark))
    (is (#'uut/queue-item-matches-filter? :unread unread-bookmark))
    (is (not (#'uut/queue-item-matches-filter? :saved unread-bookmark)))
    (is (not (#'uut/queue-item-matches-filter? :unread-bookmarks read-bookmark)))
    (is (not (#'uut/queue-item-matches-filter? :saved highlighted)))))

(deftest reading-queue-time-filter-buckets
  (is (= :under-5 (#'uut/queue-time-filter-for-minutes 4)))
  (is (= :5-15 (#'uut/queue-time-filter-for-minutes 5)))
  (is (= :15-30 (#'uut/queue-time-filter-for-minutes 15)))
  (is (= :30-60 (#'uut/queue-time-filter-for-minutes 30)))
  (is (= :60-plus (#'uut/queue-time-filter-for-minutes 60)))
  (is (= :60-plus (#'uut/queue-time-filter-for-minutes 61)))
  (is (nil? (#'uut/queue-time-filter-for-minutes nil))))

(defn- queue-item-with-reading-minutes [minutes]
  {:tags ["saved"]
   :type :item-type/link
   :nwords (* minutes 200)
   :top-words {"words" [["aa" 1]]}})

(deftest reading-queue-time-filters
  (let [under-5 (queue-item-with-reading-minutes 4)
        five-to-15 (queue-item-with-reading-minutes 5)
        fifteen-to-30 (queue-item-with-reading-minutes 15)
        thirty-to-60 (queue-item-with-reading-minutes 30)
        sixty-plus (queue-item-with-reading-minutes 60)
        unknown-time {:tags ["saved"] :type :item-type/link}]
    (is (#'uut/queue-item-matches-time-filter? nil unknown-time))
    (is (not (#'uut/queue-item-matches-time-filter? :under-5 unknown-time)))
    (is (#'uut/queue-item-matches-time-filter? :unknown unknown-time))
    (is (#'uut/queue-item-matches-time-filter? :under-5 under-5))
    (is (#'uut/queue-item-matches-time-filter? :5-15 five-to-15))
    (is (#'uut/queue-item-matches-time-filter? :15-30 fifteen-to-30))
    (is (#'uut/queue-item-matches-time-filter? :30-60 thirty-to-60))
    (is (#'uut/queue-item-matches-time-filter? :60-plus sixty-plus))
    (is (not (#'uut/queue-item-matches-time-filter? :under-5 five-to-15)))))

(deftest reading-queue-combined-filters
  (let [saved-short (queue-item-with-reading-minutes 4)
        in-progress-short (assoc saved-short :tags ["in-progress"])
        saved-long (queue-item-with-reading-minutes 60)
        unread-link (assoc saved-short :tags ["unread"])
        unread-bookmark (assoc saved-short :tags ["unread"] :type :item-type/bookmark)]
    (is (#'uut/queue-item-matches-filters? :saved :under-5 saved-short))
    (is (not (#'uut/queue-item-matches-filters? :saved :under-5 in-progress-short)))
    (is (not (#'uut/queue-item-matches-filters? :saved :under-5 saved-long)))
    (is (not (#'uut/queue-item-matches-filters? nil :under-5 unread-link)))
    (is (#'uut/queue-item-matches-filters? :unread-bookmarks :under-5 unread-bookmark))))

(deftest reading-queue-stats
  (is (= {:total 4
          :saved 2
          :in-progress 1
          :unread-bookmarks 1
          :unread 2}
         (#'uut/queue-stats [{:tags ["saved"] :type :item-type/link}
                             {:tags ["saved" "unread"] :type :item-type/link}
                             {:tags ["in-progress"] :type :item-type/link}
                             {:tags ["unread"] :type :item-type/bookmark}
                             {:tags [] :type :item-type/bookmark}]))))

(deftest reading-queue-time-stats
  (is (= {:total 6
          :under-5 1
          :5-15 1
          :15-30 1
          :30-60 1
          :60-plus 1}
         (#'uut/queue-time-stats [(queue-item-with-reading-minutes 4)
                                  (queue-item-with-reading-minutes 5)
                                  (queue-item-with-reading-minutes 15)
                                  (queue-item-with-reading-minutes 30)
                                  (queue-item-with-reading-minutes 60)
                                  {:tags ["saved"] :type :item-type/link}
                                  {:tags ["highlight"] :type :item-type/link
                                   :nwords 800
                                   :top-words {"words" [["aa" 1]]}}]))))

(deftest reading-queue-sql-sources-saved-in-progress-and-unread-bookmarks
  (let [sql (slurp (io/resource "sql/search.sql"))]
    (is (re-find #"tagi @@ '1'" sql))
    (is (re-find #"tagi @@ '2'" sql))
    (is (re-find #"items\.type = 'bookmark' and tagi @@ '0'" sql))))

(deftest search-syntax-normalization
  (is (= :web (db-search/normalize-search-syntax nil)))
  (is (= :web (db-search/normalize-search-syntax :unknown)))
  (is (= :web (db-search/normalize-search-syntax "web")))
  (is (= :plain (db-search/normalize-search-syntax "plain")))
  (is (= :phrase (db-search/normalize-search-syntax :phrase)))
  (is (= :advanced (db-search/normalize-search-syntax "advanced"))))

(deftest search-sql-supports-user-facing-and-advanced-modes
  (let [sql (slurp (io/resource "sql/search.sql"))]
    (is (re-find #"websearch_to_tsquery" sql))
    (is (re-find #"plainto_tsquery" sql))
    (is (re-find #"phraseto_tsquery" sql))
    (is (re-find #"to_tsquery" sql))
    (is (re-find #"ts_rank_cd" sql))
    (is (re-find #"ts_headline" sql))))

(deftest search-index-migration-adds-snippets-language-and-indexes
  (let [migration (slurp (io/resource "migrations/20260630000001-search-index-v2.up.sql"))]
    (is (re-find #"search_config" migration))
    (is (re-find #"headline_text" migration))
    (is (re-find #"USING GIN \(document\)" migration))
    (is (re-find #"item_data" migration))
    (is (re-find #"left\(COALESCE\(item_text\.search_text, ''\), 200000\)" migration))
    (is (re-find #"left\(concat_ws" migration))))

(deftest search-headline-renders-markers-as-mark-elements
  (is (= [:span "foo " [:mark "bar"] " baz"]
         (#'uut/render-search-headline "foo [[[bar]]] baz")))
  (is (= [:span [:mark "foo"] " and " [:mark "bar"]]
         (#'uut/render-search-headline "[[[foo]]] and [[[bar]]]"))))
