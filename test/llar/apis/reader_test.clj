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
                  persistency/transition-item-state!
                  (fn [_ id command]
                    (swap! calls conj [id command])
                    {:id id :type :item-type/link :tags []})]
      (uut/reader-item-modify 42 :set :archive)
      (uut/reader-item-modify 42 :set :research)
      (uut/reader-item-modify 42 :set :in-progress)
      (is (= [[42 :archive]
              [42 {:action :add-tag :tag :research}]
              [42 {:action :save-checkpoint :selector nil :progress 0.0}]]
             @calls)))))

(deftest done-control-is-available-in-headline-view
  (let [rendered (str (h/html (uut/headlines-list-items
                               {:group-name :default
                                :group-item :none
                                :source-key :all
                                :sources {}
                                :items [{:id 42
                                         :source-key "feed"
                                         :title "Queued headline"
                                         :ts (time/zoned-date-time)
                                         :tags ["saved"]
                                         :type :item-type/link
                                         :nwords 100
                                         :url "https://example.com"}]})))]
    (is (re-find #"btn-item-done" rendered))
    (is (re-find #"data-action=\"done\"" rendered))))

(deftest item-state-endpoint-rejects-non-semantic-actions
  (is (= {:status 400 :body {:error "Invalid item state action"}}
         (uut/reader-item-state 42 :add-tag))))

(deftest item-state-route-allows-action-specific-optional-parameters
  (let [calls (atom [])
        request (fn [params]
                  (uut/app {:request-method :post
                            :uri "/reader/item/by-id/42/state"
                            :params params}))]
    (with-redefs [uut/reader-item-state
                  (fn [& args]
                    (swap! calls conj args)
                    {:status 200 :body {}})]
      (is (= 200 (:status (request {:action "save"}))))
      (is (= 200 (:status (request {:action "save-checkpoint"
                                    :selector "selector"
                                    :progress "0.25"}))))
      (is (= [[42 :save nil nil nil]
              [42 :save-checkpoint nil "selector" "0.25"]]
             @calls)))))

(deftest item-state-endpoint-accepts-reading-checkpoints
  (let [command (atom nil)
        selector "{\"position\":{\"type\":\"TextPositionSelector\",\"start\":0,\"end\":4},\"quote\":{\"type\":\"TextQuoteSelector\",\"exact\":\"read\"}}"]
    (with-redefs [uut/frontend-db :db
                  persistency/transition-item-state!
                  (fn [_ id transition]
                    (reset! command transition)
                    {:id id :type :item-type/link :tags []
                     :checkpoint (:selector transition)})]
      (is (= 200 (:status (uut/reader-item-state
                           42 :save-checkpoint nil selector "0.25"))))
      (is (= :save-checkpoint (:action @command)))
      (is (= 0.25 (:progress @command)))
      (is (= "read" (get-in @command [:selector :quote :exact]))))))

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
                  uut/reader-item-state
                  (fn [& args]
                    (swap! modifications conj args)
                    "")
                  persistency/record-item-opened!
                  (fn [& args] (swap! opened conj args))]
      (is (= 200 (:status (request {:mark "read"}))))
      (is (= 200 (:status (request {:mark "read" :offer "91"}))))
      (is (= [nil 91] (mapv last @opened)))
      (is (= [[24292 :seen] [24292 :seen]] @modifications)))))

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
  (is (= [:continue-reading]
         (#'uut/queue-item-reasons {:tags []
                                    :checkpoint-progress 0.0
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

(deftest reading-queue-clusters-do-not-affect-continue-reading
  (with-redefs [lab/current-clustered-saved-items
                (atom lab/+saved-clusters-not-compiled+)
                uut/frontend-db :db
                persistency/get-reading-queue-items (fn [_ _] [])]
    (let [rendered (pr-str (uut/tools-view-handler
                            {:view :saved-overview :request-params {}}))]
      (is (re-find #"No saved, partially read" rendered))
      (is (re-find #"not yet" rendered))))
  (with-redefs [lab/current-clustered-saved-items
                (atom {:clusters {{:id 1 :words ["Distributed" "Systems"]}
                                  [{:id 7}]}
                       :last-update (time/zoned-date-time)})
                uut/frontend-db :db
                persistency/get-reading-queue-items
                (fn [_ _]
                  [{:id 7 :title "A queue item" :tags ["saved"]
                    :type :item-type/link :ts (time/zoned-date-time)
                    :nwords 100 :source-key "feed"
                    :url "https://example.com/item"}])]
    (let [rendered (pr-str (uut/tools-view-handler
                            {:view :saved-overview :request-params {}}))]
      (is (re-find #"Distributed" rendered))
      (is (re-find #"A queue item" rendered))))
  (with-redefs [uut/frontend-db :db
                persistency/get-reading-progress-items (fn [_ _] [])]
    (let [rendered (pr-str (uut/tools-view-handler
                            {:view :continue-reading :request-params {}}))]
      (is (re-find #"Nothing is in progress" rendered))
      (is (not (re-find #"cluster" rendered))))))

(deftest reading-queue-filters
  (let [saved {:tags ["saved"] :type :item-type/link}
        in-progress {:tags [] :checkpoint-progress 0.0 :type :item-type/link}
        unread-bookmark {:tags ["unread"] :type :item-type/bookmark}
        read-bookmark {:tags [] :type :item-type/bookmark}
        highlighted {:tags ["highlight"] :type :item-type/link}]
    (is (= [true false true true false false]
           (mapv (fn [[filter item]]
                   (#'uut/queue-item-matches-filter? filter item))
                 [[nil saved]
                  [nil highlighted]
                  [:continue-reading in-progress]
                  [:unread-bookmarks unread-bookmark]
                  [:unread-bookmarks read-bookmark]
                  [:saved unread-bookmark]])))))

(deftest reading-queue-time-filter-buckets
  (is (= [:under-5 :5-15 :15-30 :30-60 :60-plus nil]
         (mapv #'uut/queue-time-filter-for-minutes [4 5 15 30 60 nil]))))

(defn- queue-item-with-reading-minutes [minutes]
  {:tags ["saved"]
   :type :item-type/link
   :nwords (* minutes 200)
   :top-words {"words" [["aa" 1]]}})

(deftest reading-queue-combined-filters
  (let [saved-short (queue-item-with-reading-minutes 4)
        in-progress-short (assoc saved-short :tags [] :checkpoint-progress 0.0)
        saved-long (queue-item-with-reading-minutes 60)
        unread-bookmark (assoc saved-short :tags ["unread"] :type :item-type/bookmark)]
    (is (#'uut/queue-item-matches-filters? :saved :under-5 saved-short))
    (is (not (#'uut/queue-item-matches-filters? :saved :under-5 in-progress-short)))
    (is (not (#'uut/queue-item-matches-filters? :saved :under-5 saved-long)))
    (is (#'uut/queue-item-matches-filters? :unread-bookmarks :under-5 unread-bookmark))))

(deftest reading-queue-semantics-have-one-sql-definition
  (let [migration (slurp (io/resource "migrations/20260809000001-reading-progress.up.sql"))
        fever (slurp (io/resource "sql/fever.sql"))]
    (is (re-find #"CREATE VIEW reading_queue_items" migration))
    (is (re-find #"tagi @@ '1'" migration))
    (is (re-find #"reading_progress IS NOT NULL" migration))
    (is (re-find #"type = 'bookmark' AND tagi @@ '0'" migration))
    (is (re-find #"reading_queue_items" fever))))

(deftest search-syntax-normalization
  (is (= [:web :web :plain :phrase :advanced]
         (mapv db-search/normalize-search-syntax
               [nil :unknown "plain" :phrase "advanced"]))))

(deftest search-headline-renders-markers-as-mark-elements
  (is (= [:span "foo " [:mark "bar"] " baz"]
         (#'uut/render-search-headline "foo [[[bar]]] baz")))
  (is (= [:span [:mark "foo"] " and " [:mark "bar"]]
         (#'uut/render-search-headline "[[[foo]]] and [[[bar]]]"))))
