(ns llar.apis.reader-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [hiccup2.core :as h]
   [java-time.api :as time]
   [llar.apis.reader :as uut]
   [llar.appconfig :as appconfig]
   [llar.bookmark-capture :as bookmark-capture]
   [llar.db.query :as db-query]
   [llar.db.search :as db-search]
   [llar.db.sql :as sql]
   [llar.lab :as lab]
   [llar.persistency :as persistency]
   [llar.rc :as rc]
   [llar.update :as update]
   [llar.vibe :as vibe]))

(deftest reader-bookmark-form-uses-the-durable-capture-queue
  (let [call (atom nil)]
    (with-redefs [bookmark-capture/enqueue!
                  (fn [db url title submitted-by]
                    (reset! call [db url title submitted-by])
                    {:id 7 :status :pending :inserted true})]
      (let [response (uut/app {:request-method :post
                               :uri "/reader/bookmark/add"
                               :params {:url "https://example.com/story"
                                        :type "readability-bookmark"}})]
        (is (= 201 (:status response)))
        (is (= :queued (get-in response [:body :result])))
        (is (= "https://example.com/story" (second @call)))
        (is (= "reader" (nth @call 3)))))))

(deftest list-style-uses-rc-defaults
  (with-redefs [rc/rc (fn [path]
                        (when (= [:reader :default-list-view :blog] path)
                          :headlines))]
    (is (= :headlines (uut/get-list-style {:group-item :blog})))
    (is (= :gallery (uut/get-list-style {:group-item :blog
                                         :list-style :gallery})))))

(deftest list-style-navigation-shows-the-effective-view
  (with-redefs [rc/rc (fn [path]
                        (when (= [:reader :default-list-view :blog] path)
                          :headlines))]
    (let [rendered (str (h/html (uut/group-nav
                                 {:mode :list-items
                                  :uri "/reader/group/default/blog/source/all/items"
                                  :group-name :default
                                  :group-item :blog
                                  :sources {}
                                  :item-tags []})))]
      (is (re-find #"Use Configured Default" rendered))
      (is (re-find #"(?s)id=\"view-style-select\".*?nav-link active.*?list-style=headlines.*?Headlines"
                   rendered)))))

(deftest gallery-distinguishes-image-zoom-from-text-only-items
  (is (false? (#'uut/usable-image-url? " SELF ")))
  (is (false? (#'uut/usable-image-url? "default")))
  (is (true? (#'uut/usable-image-url? "https://example.com/image.jpg")))
  (let [base-item {:id 42
                   :source-key "feed"
                   :title "A picture"
                   :ts (time/zoned-date-time)
                   :tags []
                   :url "https://example.com/story"}
        without-image (str (h/html (uut/gallery-list-item
                                    {} "/reader/source/feed" base-item)))
        with-image (str (h/html (uut/gallery-list-item
                                 {} "/reader/source/feed"
                                 (assoc base-item :entry
                                        {:thumbnail "https://example.com/image.jpg"}))))]
    (is (not (re-find #"reader-gallery-image-trigger|<img" without-image)))
    (is (string/includes? without-image "reader-gallery-card is-text-only"))
    (is (string/includes? without-image "reader-gallery-text-lead"))
    (is (string/includes? without-image
                          "reader-gallery-signal\">example.com</span>"))
    (is (string/includes? with-image "reader-gallery-card has-image"))
    (is (string/includes? with-image "reader-gallery-image-trigger"))
    (is (string/includes? with-image "reader-gallery-media-fallback"))
    (is (string/includes? with-image
                          "data-error-reveal=\".reader-gallery-media-fallback\""))
    (is (string/includes? with-image
                          "reader-gallery-card-image reader-defensive-image is-cover"))
    (is (string/includes? with-image "reader-gallery-zoom-cue"))
    (is (re-find #"alt=\"Preview for A picture\"" with-image))
    (is (string/includes? with-image "reader-gallery-meta"))
    (is (string/includes? with-image "reader-gallery-action-buttons"))
    (is (string/includes? with-image "btn-state-saved"))
    (is (string/includes? with-image "btn-item-done"))
    (is (string/includes? with-image "compact-item-more-menu-42"))))

(deftest gallery-prefers-direct-photos-and-preserves-grid-order
  (let [item {:id 43
              :source-key "photos"
              :title "Direct photo"
              :ts (time/zoned-date-time)
              :tags ["unread"]
              :url "https://example.com/story"
              :entry {:entities {:photos ["https://example.com/photo.jpg"]}
                      :thumbnail "https://example.com/thumb.jpg"}}
        card (str (h/html (uut/gallery-list-item
                           {} "/reader/source/photos" item)))
        grid (str (h/html (uut/gallery-list-items
                           {:group-name :default
                            :group-item :none
                            :source-key :all
                            :items [item (assoc item :id 44 :title "Second")]})))]
    (is (string/includes? card "src=\"https://example.com/photo.jpg\""))
    (is (not (string/includes? card "thumb.jpg")))
    (is (string/includes? card
                          "reader-gallery-card-image reader-defensive-image is-contain"))
    (is (string/includes? card "data-unread=\"true\""))
    (is (string/includes? grid
                          "row-cols-1 row-cols-sm-2 row-cols-xl-3"))
    (is (< (string/index-of grid "Direct photo")
           (string/index-of grid "Second")))))

(deftest gallery-youtube-items-lazy-load-inline-instead-of-opening-an-image-modal
  (let [rendered (str
                  (h/html
                   (uut/gallery-list-item
                    {}
                    "/reader/source/youtube"
                    {:id 45
                     :source-key "youtube"
                     :title "Watch this"
                     :ts (time/zoned-date-time)
                     :tags ["has-video" "unread"]
                     :url "https://www.youtube.com/watch?v=abc123"
                     :entry {:thumbnail "/blob/youtube-thumbnail.jpg"
                             :duration 605}})))]
    (is (string/includes? rendered
                          "lazy-youtube-trigger reader-gallery-video-trigger"))
    (is (string/includes? rendered "data-vid=\"abc123\""))
    (is (string/includes? rendered
                          "data-target=\"youtube-container-45-abc123\""))
    (is (string/includes? rendered "reader-lazy-video-cue"))
    (is (string/includes? rendered "aria-label=\"Play video on YouTube\""))
    (is (not (string/includes? rendered "reader-gallery-modal")))
    (is (not (string/includes? rendered "reader-gallery-zoom-cue")))
    (is (string/includes? rendered "Video duration: 10:05"))))

(deftest broken-gallery-images-reveal-their-stable-frame-fallback
  (let [javascript (slurp (io/resource "status/llar.js"))]
    (is (string/includes? javascript "data-error-reveal"))
    (is (string/includes? javascript "revealTarget.removeAttribute(\"hidden\")"))))

(deftest youtube-preview-fills-ratio-container
  (let [rendered (str (uut/render-special-item-content
                       {:url "https://www.youtube.com/watch?v=abc123"
                        :entry {:thumbnail "/blob/youtube-thumbnail.jpg"}}
                       #{}))]
    (is (re-find #"class=\"youtube-preview-container\"" rendered))
    (is (re-find #"class=\"lazy-youtube-trigger" rendered))
    (is (re-find #"class=\"lazy-youtube reader-defensive-image\"" rendered))
    (is (re-find #"data-vid=\"abc123\"" rendered))
    (is (re-find #"alt=\"Play video on YouTube\"" rendered))))

(deftest youtube-list-preview-is-compact-and-removes-a-broken-ratio
  (let [rendered (str (uut/render-special-item-content
                       {:url "https://www.youtube.com/watch?v=abc123"
                        :entry {:thumbnail "/blob/youtube-thumbnail.jpg"}}
                       #{:llar.apis.reader/compact-media}))]
    (is (re-find #"reader-list-video-preview" rendered))
    (is (re-find #"reader-defensive-image" rendered))
    (is (re-find #"data-error-remove=\"\.ratio\"" rendered))))

(deftest reader-uses-video-duration-instead-of-description-reading-time
  (with-redefs [appconfig/credentials (constantly nil)
                rc/rc (constantly nil)]
    (let [base-item {:id 42
                     :source-key "youtube"
                     :title "A long video"
                     :url "https://www.youtube.com/watch?v=abc123"
                     :ts (time/zoned-date-time)
                     :tags ["has-video"]
                     :nwords 20
                     :top-words {"words" [["description" 3]]}
                     :entry {:language "en" :duration 605}
                     :data {:content {"text/plain" "Short description"}}}
          article (str (h/html (uut/main-show-item {:items [base-item]})))
          list-item (str (h/html (uut/main-list-item
                                  {:sources {:youtube {:options #{}}}}
                                  "/reader/source/youtube"
                                  base-item)))]
      (is (re-find #"fa-play-circle.*10:05 video" article))
      (is (not (string/includes? article "20 words")))
      (is (not (string/includes? article "min read")))
      (is (re-find #"fa-play-circle.*11 min video" list-item)))))

(deftest preview-renders-a-bounded-semantic-fingerprint
  (let [item {:id 42
              :source-key "feed"
              :title "Signals worth scanning"
              :url "https://example.com/article"
              :ts (time/zoned-date-time)
              :tags ["unread" "my-label"]
              :top-words {"words" [["security" 12]
                                   ["architecture" 10]
                                   ["agents" 9]
                                   ["systems" 8]
                                   ["platform" 7]
                                   ["protocols" 6]
                                   ["overflowterm" 5]
                                   ["hiddencontext" 4]]}
              :names ["Linus Torvalds" "Grace Hopper" "Ada Lovelace" "Barbara Liskov"]
              :urls ["https://example.com/related-path"
                     "https://github.com/example/project"
                     "https://en.wikipedia.org/wiki/Computer_security"
                     "https://news.ycombinator.com/item?id=42"
                     "https://clojure.org/reference/reader"]
              :entry {}}
        fingerprint (#'uut/preview-fingerprint item)
        rendered (str (h/html (uut/main-list-item
                               {:sources {:feed {:options #{}}}}
                               "/reader/source/feed"
                               item)))]
    (is (= ["security" "architecture" "agents" "systems" "platform" "protocols"]
           (mapv :label (:terms fingerprint))))
    (is (= [:strong :strong :regular :regular :regular :quiet]
           (mapv :weight (:terms fingerprint))))
    (is (= ["Linus Torvalds" "Grace Hopper" "Ada Lovelace"]
           (mapv :label (:entities fingerprint))))
    (is (= 2 (count (:references fingerprint))))
    (is (= 2 (count (:more-terms fingerprint))))
    (is (= 1 (count (:more-entities fingerprint))))
    (is (= 2 (count (:more-references fingerprint))))
    (is (string/includes? rendered "class=\"reader-preview-fingerprint\""))
    (is (string/includes? rendered "aria-label=\"Extracted signals\""))
    (is (not (string/includes? rendered ">Terms<")))
    (is (not (string/includes? rendered ">Entities<")))
    (is (not (string/includes? rendered ">References<")))
    (is (string/includes? rendered "<details class=\"reader-preview-more-signals\">"))
    (is (string/includes? rendered ">+5</summary>"))
    (is (string/includes? rendered "2 terms · 1 entity · 2 references"))
    (is (string/includes? rendered "overflowterm"))
    (is (not (string/includes? rendered "https://example.com/related-path")))
    (is (not (string/includes? rendered "word-cloud")))
    (is (string/includes? rendered "my-label"))))

(deftest preview-combines-a-restrained-description-with-the-fingerprint
  (let [base-item {:id 42
                   :source-key "feed"
                   :title "Description source"
                   :url "https://example.com/article"
                   :ts (time/zoned-date-time)
                   :tags []
                   :entry {}
                   :data {:description {"text/plain" "  Configured\n\n description  "}}}
        description-rendered (str
                              (h/html
                               (uut/main-list-item
                                {:sources {:feed {:options #{:main-list-use-description}}}}
                                "/reader/source/feed"
                                (assoc base-item
                                       :top-words {"words" [["security" 10]]}))))
        empty-rendered (str
                        (h/html
                         (uut/main-list-item
                          {:sources {:feed {:options #{}}}}
                          "/reader/source/feed"
                          base-item)))]
    (is (string/includes? description-rendered "Configured description"))
    (is (string/includes? description-rendered "reader-preview-description"))
    (is (string/includes? description-rendered "reader-preview-fingerprint"))
    (is (not (string/includes? empty-rendered "reader-preview-fingerprint")))
    (is (not (string/includes? empty-rendered "No extracted signals")))))

(deftest preview-description-is-bounded-and-omits-title-duplicates
  (let [long-description (apply str (repeat 400 "x"))
        bounded (#'uut/preview-description
                 {:title "Different title"
                  :data {:description {"text/plain" long-description}}})]
    (is (= 360 (count bounded)))
    (is (string/ends-with? bounded "…"))
    (is (nil? (#'uut/preview-description
               {:title "  SAME title "
                :data {:description {"text/plain" "Same\n title"}}})))
    (is (nil? (#'uut/preview-description
               {:title "No description"
                :data {:description {"text/plain" " \n "}}})))))

(deftest preview-description-hydration-is-bounded-to-the-selected-item-ids
  (let [query-args (atom nil)]
    (with-redefs [sql/get-item-preview-descriptions
                  (fn [_ args]
                    (reset! query-args args)
                    ;; Unquoted PostgreSQL identifiers historically arrived
                    ;; with underscores; hydration accepts that row shape too.
                    [{:item_id 2 :description "Second item summary"}])]
      (let [items (#'db-query/attach-preview-descriptions
                   :db
                   [{:id 1 :title "First"}
                    {:id 2 :title "Second"}])]
        (is (= {:item-ids [1 2] :max-characters 512} @query-args))
        (is (nil? (:data (first items))))
        (is (= "Second item summary"
               (get-in (second items) [:data :description "text/plain"])))))))

(deftest preview-description-hydration-fails-open
  (let [items [{:id 1 :title "First"}
               {:id 2 :title "Second"}]]
    (with-redefs [sql/get-item-preview-descriptions
                  (fn [& _]
                    (throw (java.sql.SQLException. "preview query unavailable")))]
      (is (= items
             (#'db-query/attach-preview-descriptions-best-effort :db items))))))

(deftest reader-hides-false-reading-time-when-video-duration-is-missing
  (with-redefs [appconfig/credentials (constantly nil)
                rc/rc (constantly nil)]
    (let [rendered (str
                    (h/html
                     (uut/main-show-item
                      {:items [{:id 42
                                :source-key "youtube"
                                :title "Unknown length"
                                :url "https://www.youtube.com/watch?v=abc123"
                                :ts (time/zoned-date-time)
                                :tags ["has-video"]
                                :nwords 200
                                :entry {:language "en"}
                                :data {:content {"text/plain" "Description"}}}]})))]
      (is (not (string/includes? rendered "min read")))
      (is (not (string/includes? rendered "200 words"))))))

(deftest reddit-item-content-does-not-repeat-its-heading
  (let [rendered (str (h/html (uut/main-show-item
                               {:uri "/reader/item/by-id/42"
                                :items [{:id 42
                                         :source-key "reddit-games"
                                         :title "Repeated title"
                                         :url "https://reddit.example/post"
                                         :entry {:language "en"
                                                 :score 42
                                                 :comments-url "https://reddit.example/comments"}
                                         :tags []
                                         :nwords 10
                                         :data {:description
                                                {"text/html" "<h1>Repeated title</h1><p>Body</p>"}}}]})))
        css (slurp (io/resource "status/llar.css"))]
    (is (re-find #"item-content-body hyphenate item-content-reddit" rendered))
    (is (re-find #"class=\"summary\"><div><p>.*Subreddit:.*r/games.*Score:.*42"
                 rendered))
    (is (not (re-find #"<h1>Repeated title</h1>" rendered)))
    (is (re-find #"\.item-content-reddit > h1:first-child \{[^}]*display: none"
                 css))))

(deftest list-navbar-keeps-the-read-and-update-toolbar-visible
  (with-redefs [rc/rc (constantly nil)]
    (let [rendered (str (h/html (uut/nav-bar
                                 {:mode :list-items
                                  :uri "/reader/group/default/none/source/all/items"
                                  :group-name :default
                                  :group-item :none
                                  :source-key :all
                                  :sources []
                                  :selected-sources []
                                  :source-update-context {:source-count 12
                                                          :estimated-duration-label "30.0 s"}
                                  :items [{:id 42 :title "An item" :tags []}]})))]
      (is (re-find #"reader-navbar-path breadcrumb path" rendered))
      (is (not (re-find #"col-12 form-control-dark breadcrumb" rendered)))
      (is (not (re-find #"navbar-dark|bg-dark" rendered)))
      (is (re-find #"reader-navbar-context" rendered))
      (is (re-find #"reader-navbar-actions" rendered))
      (is (not (re-find #"navbar-list|navbar-brand[^\"]*col-md" rendered)))
      (is (re-find #"btn-mark-view-read" rendered))
      (is (re-find #"btn-update-sources-in-view" rendered))
      (is (re-find #"btn-update-sources-in-view[^>]*>\s*<i[^>]*fa-download" rendered))
      (is (re-find #"btn-reload-current-view" rendered))
      (is (re-find #"btn-next-batch" rendered))
      (is (re-find #"title=\"Read all in view\"" rendered))
      (is (not (string/includes? rendered "~30.0 s")))
      (is (not (string/includes? rendered "12 sources")))
      (is (not (re-find #"btn-outline-secondary" rendered))))))

(deftest source-update-estimate-stays-out-of-the-source-sidebar
  (let [rendered (str (h/html
                       (uut/source-nav
                        {:mode :list-items
                         :group-name :source-tag
                         :group-item :all
                         :source-key :all
                         :active-sources []
                         :source-update-context {:estimated-duration-label "30.0 s"}})))]
    (is (not (string/includes? rendered "reader-source-update-estimate")))
    (is (not (string/includes? rendered "Update usually ~30.0 s")))))

(deftest list-footer-keeps-only-useful-snapshot-and-scroll-policy-context
  (with-redefs [rc/rc (constantly nil)]
    (let [ts (time/zoned-date-time 2026 8 11 12 0 0 0 "Z")
          snapshot-ts (time/zoned-date-time 2026 8 11 14 32 0 0 "Z")
          x {:uri "/reader/group/default/all/source/all/items"
             :items [{:id 42 :ts ts}]
             :list-style :preview
             :sort-order :newest
             :filter :unread
             :snapshot-ts snapshot-ts
             :source-update-context {:last-fetch {:last-success ts
                                                  :stats {:fetched 18 :db 3}}}
             :selected-sources [{:options #{:mark-read-on-view}}
                                {:options #{}}]
             :range-recent {:id 42 :ts ts}
             :range-before {:id 42 :ts ts}
             :has-more? true}
          footer (str (h/html (#'uut/batch-footer x)))
          final-footer (str (h/html (#'uut/batch-footer (assoc x :has-more? false))))]
      (is (string/includes? footer "Snapshot 2026-08-11 14:32"))
      (is (string/includes? footer "1 item"))
      (is (string/includes? footer "Newest First"))
      (is (string/includes? footer "unread"))
      (is (string/includes? footer "Last fetch "))
      (is (string/includes? footer ": 18 fetched · 3 new"))
      (is (string/includes? footer "Mark on scroll: 1 of 2 sources"))
      (is (not (string/includes? footer "Preview")))
      (is (not (string/includes? footer "End of this finite batch")))
      (is (string/includes? footer "id=\"reader-list-lifecycle-status\""))
      (is (string/includes? footer "aria-live=\"polite\""))
      (is (string/includes? footer "Next batch"))
      (is (not (string/includes? final-footer "Next batch"))))))

(deftest source-update-estimate-requires-coverage-and-respects-concurrency
  (let [sources [{:key :one} {:key :two} {:key :three}]
        state {:one {:last-duration (time/seconds 10)}
               :two {:last-duration (time/seconds 20)}
               :three {:last-duration (time/seconds 30)}}]
    (with-redefs [update/updateable-sources (constantly {:one {} :two {} :three {}})
                  update/get-current-state (constantly state)
                  rc/rc (constantly 2)]
      (let [context (#'uut/source-update-view-context sources)]
        (is (= 3 (:source-count context)))
        (is (= 3 (:duration-sample-count context)))
        (is (= 30000 (:estimated-duration-ms context)))
        (is (= "30.0 s" (:estimated-duration-label context)))))
    (with-redefs [update/updateable-sources (constantly {:one {} :two {} :three {}})
                  update/get-current-state (constantly (dissoc state :three))
                  rc/rc (constantly 2)]
      (is (nil? (:estimated-duration-ms
                 (#'uut/source-update-view-context sources)))))))

(deftest item-navbar-keeps-only-the-persistent-reading-actions
  (with-redefs [rc/rc (constantly nil)]
    (let [rendered (str (h/html (uut/nav-bar
                                 {:mode :show-item
                                  :uri "/reader/group/default/none/source/all/item/by-id/42"
                                  :group-name :default
                                  :group-item :none
                                  :source-key :all
                                  :selected-sources []
                                  :items [{:id 42 :source-key "feed" :title "Current"
                                           :tags ["saved" "archive" "podcast"]}
                                          {:id 43 :source-key "feed" :title "Next"
                                           :tags []}]})))]
      (is (re-find #"btn-state-saved" rendered))
      (is (re-find #"btn-item-done" rendered))
      (is (re-find #"title=\"Show item HTML focus mode\"" rendered))
      (is (re-find #"btn-annotation-mode" rendered))
      (is (re-find #"id=\"btn-next-item\"[^>]*>\s*<i[^>]*fa-step-forward" rendered))
      (is (not (re-find #"fa-arrow-down|btn-state-archived|item-tag-toggle" rendered))))))

(deftest mobile-navbar-opens-destination-drawers-without-duplicating-the-navbar
  (with-redefs [rc/rc (constantly nil)]
    (let [rendered (str (h/html (uut/nav-bar
                                 {:mode :list-items
                                  :uri "/reader/group/default/all/source/all/items"
                                  :group-name :default
                                  :group-item :all
                                  :source-key :all
                                  :selected-sources []
                                  :items [{:id 42 :source-key "feed" :title "Current" :tags []}]
                                  :has-more? true})))
          tool-rendered (str (h/html (uut/nav-bar
                                      {:mode :tools
                                       :view :search
                                       :uri "/reader/tools/search"
                                       :group-name :default
                                       :group-item :all
                                       :source-key :all
                                       :items []})))]
      (is (re-find #"reader-mobile-navigation-toggle[^>]*data-bs-target=\"#groupnav\"" rendered))
      (is (re-find #"reader-mobile-sources-toggle[^>]*data-bs-target=\"#sourcenav\"" rendered))
      (is (not (string/includes? rendered "data-bs-target=\"#navbar\"")))
      (is (string/includes? rendered "navbar-collapse d-none d-md-flex"))
      (is (= 1 (count (re-seq #"btn-reload-current-view" rendered))))
      (is (not (string/includes? tool-rendered "reader-mobile-actions"))))))

(deftest reader-landmarks-support-keyboard-navigation
  (with-redefs [rc/rc (constantly nil)]
    (let [skip-link (str (h/html (#'uut/reader-skip-link)))
          main (str (h/html (uut/main-view {:mode :unknown})))
          group-navigation (str (h/html (uut/group-nav
                                         {:mode :list-items
                                          :uri "/reader/group/default/all/source/all/items"
                                          :group-name :default
                                          :group-item :all
                                          :filter :unread
                                          :item-tags []
                                          :sources {}})))
          source-navigation (str (h/html (uut/source-nav
                                          {:mode :list-items
                                           :group-name :default
                                           :group-item :all
                                           :source-key :one
                                           :active-sources [{:key :one
                                                             :title "One"
                                                             :item-tags {:all 1}}]})))]
      (is (string/includes? skip-link "href=\"#reader-main\""))
      (is (string/includes? main "id=\"reader-main\""))
      (is (string/includes? main "tabindex=\"-1\""))
      (is (string/includes? group-navigation "aria-label=\"Reader destinations\""))
      (is (re-find #"aria-current=\"page\"[^>]*>.*unread" group-navigation))
      (is (string/includes? source-navigation "aria-label=\"Sources in current view\""))
      (is (re-find #"aria-current=\"page\"" source-navigation)))))

(deftest annotation-mode-renders-a-readable-explicit-workspace
  (with-redefs [appconfig/credentials (constantly nil)
                rc/rc (constantly nil)]
    (let [article (str (h/html (uut/main-show-item
                                {:items [{:id 42
                                          :url "https://example.com"
                                          :tags []
                                          :entry {:language "en"}
                                          :data {:description
                                                 {"text/html" "<p>Annotate me</p>"}}}]})))
          button (str (h/html (uut/annotation-button)))
          javascript (slurp (io/resource "status/llar.js"))]
      (is (string/includes? button "aria-pressed=\"false\""))
      (is (string/includes? article "aria-label=\"Annotation tools\""))
      (is (string/includes? article "id=\"annotation-mode-status\""))
      (is (string/includes? article "aria-live=\"polite\""))
      (is (string/includes? article "btn-close-annotation-mode"))
      (is (string/includes? article "reader-annotation-notes"))
      (is (string/includes? article "placeholder=\"Write an item note\""))
      (is (string/includes? javascript "selectionchange.annotation"))
      (is (string/includes? javascript "reader-annotation-delete"))
      (is (string/includes? javascript "unavailable highlight"))
      (is (not (string/includes? javascript "click.annotation-delete"))))))

(deftest tool-breadcrumb-names-the-current-tool-instead-of-all
  (with-redefs [rc/rc (constantly nil)]
    (doseq [[view title icon-class] [[:saved-overview "Reading Queue" "fas fa-project-diagram"]
                                     [:continue-reading "Continue Reading" "fas fa-map-marker-alt"]
                                     [:todays-vibe "Today’s Vibe" "fas fa-fire"]
                                     [:gems "Gems" "fas fa-gem"]
                                     [:search "Search" "fas fa-search"]]]
      (let [rendered (str (h/html (uut/nav-bar
                                   {:mode :tools
                                    :view view
                                    :uri (str "/reader/tools/" (name view))
                                    :group-name :default
                                    :group-item :all
                                    :source-key :all
                                    :items []})))]
        (is (string/includes? rendered title))
        (is (string/includes? rendered icon-class))
        (is (re-find #"breadcrumb-item active" rendered))
        (is (not (re-find #">all</a>" rendered)))))))

(deftest tool-workbench-keeps-context-compact-and-consistent
  (doseq [[view title purpose] [[:saved-overview "Reading Queue" "Review items you saved or paused"]
                                [:continue-reading "Continue Reading" "Resume items with a saved reading position"]
                                [:todays-vibe "Today’s Vibe" "Scan recent stories grouped across sources"]
                                [:gems "Gems" "Find something you kept"]
                                [:search "Search" "Search the stored archive"]]]
    (let [rendered (str (h/html (#'uut/tool-workbench {:view view} [:div "Tool body"])))]
      (is (string/includes? rendered "reader-tool-workbench"))
      (is (string/includes? rendered "reader-tool-purpose"))
      (is (string/includes? rendered title))
      (is (string/includes? rendered purpose))
      (is (= 1 (count (re-seq #"<h1" rendered)))))))

(deftest tool-navigation-keeps-destinations-and-excludes-list-only-controls
  (with-redefs [rc/rc (fn [path]
                        (when (= path [:reader :favorites])
                          [[:hackernews :source-tag]
                           [:in-progress :item-tags]]))]
    (let [rendered (str (h/html (uut/group-nav {:mode :tools
                                                :view :search
                                                :group-name :default
                                                :group-item :all
                                                :item-tags [:archive :saved]
                                                :sources {:one {:tags #{:hackernews :tech}
                                                                :type :feed}}})))]
      (is (string/includes? rendered "id=\"groupnav\""))
      (is (string/includes? rendered "hackernews"))
      (is (string/includes? rendered "aria-current=\"page\""))
      (is (string/includes? rendered "Reading Queue"))
      (is (string/includes? rendered "Continue Reading"))
      (is (not (string/includes? rendered "view-style-select")))
      (is (not (string/includes? rendered "sort-order-select")))
      (is (not (string/includes? rendered ">all<")))
      (is (not (string/includes? rendered ">unread<")))
      (is (not (string/includes? rendered ">today<")))
      (is (string/includes? rendered "Item Tags"))
      (is (string/includes? rendered "/reader/group/item-tags/archive/source/all/items"))
      (is (string/includes? rendered "Source Tags"))
      (is (string/includes? rendered "/reader/group/source-tag/tech/source/all/items"))
      (is (string/includes? rendered " Type</span>"))
      (is (string/includes? rendered "/reader/group/type/feed/source/all/items"))
      (is (not (string/includes? rendered ">in-progress<"))))))

(deftest related-breadcrumb-extends-the-originating-item-path
  (with-redefs [rc/rc (constantly nil)]
    (let [rendered (str (h/html (uut/nav-bar
                                 {:mode :show-item
                                  :breadcrumb-suffix {:icon "fas fa-project-diagram"
                                                      :label "related"}
                                  :uri "/reader/group/default/blog/source/all/item/by-id/42/related"
                                  :group-name :default
                                  :group-item :blog
                                  :source-key :all
                                  :selected-sources []
                                  :items [{:id 42
                                           :source-key "feed"
                                           :title "An item"
                                           :tags []}]})))]
      (is (re-find #"(?s)>blog</a>.*?>feed</a>.*?>An item</a>.*?fa-project-diagram.*?related"
                   rendered))
      (is (re-find #"breadcrumb-item active" rendered)))))

(deftest inspect-breadcrumb-extends-the-originating-item-path
  (with-redefs [rc/rc (constantly nil)]
    (let [rendered (str (h/html (uut/nav-bar
                                 {:mode :dump-item
                                  :uri "/reader/group/default/blog/source/all/item/by-id/42/dump"
                                  :group-name :default
                                  :group-item :blog
                                  :source-key :all
                                  :selected-sources []
                                  :items [{:id 42
                                           :source-key "feed"
                                           :title "An item"
                                           :tags []}]})))]
      (is (re-find #"(?s)>blog</a>.*?>feed</a>.*?>An item</a>.*?fa-search.*?Inspect"
                   rendered))
      (is (re-find #"breadcrumb-item active" rendered)))))

(deftest related-button-preserves-the-reader-path
  (let [rendered (str (h/html (uut/related-button
                               {:group-name :item-tags
                                :group-item :saved
                                :source-key :feed
                                :list-style :headlines}
                               42)))]
    (is (re-find #"/reader/group/item-tags/saved/source/feed/item/by-id/42/related\?list-style=headlines"
                 rendered))))

(deftest related-route-passes-the-originating-reader-path
  (let [call (atom nil)]
    (with-redefs [uut/reader-related
                  (fn [& args]
                    (reset! call args)
                    {:status 200 :body "related"})]
      (let [response (uut/app {:request-method :get
                               :uri "/reader/group/item-tags/saved/source/feed/item/by-id/42/related"
                               :params {:list-style "headlines"}})]
        (is (= 200 (:status response)))
        (is (= 42 (first @call)))
        (is (= {:group-name :item-tags
                :group-item :saved
                :source-key :feed
                :list-style :headlines}
               (select-keys (second @call)
                            [:group-name :group-item :source-key :list-style])))))))

(deftest reader-bootstrap-primary-is-the-llar-orange
  (let [css (slurp (io/resource "status/llar.css"))]
    (is (re-find #"--llar-primary: #f2711c" css))
    (is (re-find #"--llar-primary-control: #ff9a57" css))
    (is (re-find #"::highlight\(llar-annotation\) \{[^}]*var\(--llar-annotation-bg\)"
                 css))
    (is (re-find #"\.llar-reader ::selection \{[^}]*var\(--llar-primary-control\)"
                 css))
    (is (re-find #"\.reader-mode-show-item #item-content-body-container"
                 css))
    (is (re-find #"\.reader-mode-focus-item #item-content-body-container \{[^}]*var\(--llar-space-3\)"
                 css))
    (is (re-find #"\.reading-checkpoint-control\.is-active \{[^}]*var\(--bs-primary\)"
                 css))
    (is (re-find #"\.checkpoint-resume-target \{[^}]*var\(--llar-primary-rgb\)"
                 css))))

(deftest reader-design-language-has-scoped-light-and-dark-tokens
  (let [css (slurp (io/resource "status/llar.css"))]
    (is (re-find #"\.llar-reader \{[^}]*--llar-canvas: #f5f5f3" css))
    (is (re-find #"\.llar-reader\[data-bs-theme=\"dark\"\] \{" css))
    (is (re-find #"@media \(prefers-color-scheme: dark\)" css))
    (is (re-find #"\.llar-reader:not\(\[data-bs-theme=\"light\"\]\)" css))
    (is (re-find #"--llar-font-reading: \"IBM Plex Serif\"" css))
    (is (re-find #"--llar-reading-measure: 60ch" css))))

(deftest youtube-player-sends-origin-referrer
  (let [javascript (slurp (io/resource "status/llar.js"))]
    (is (re-find #"\$\(\"\.lazy-youtube-trigger\"\)" javascript))
    (is (re-find #"youtube-nocookie\.com/embed/" javascript))
    (is (string/includes? javascript "?controls=1&fs=1&playsinline=1"))
    (is (string/includes? javascript "encrypted-media; fullscreen; gyroscope"))
    (is (string/includes? javascript "allowfullscreen"))
    (is (re-find #"referrerpolicy=\"strict-origin-when-cross-origin\"" javascript))))

(deftest item-tags-use-the-state-endpoint
  (let [calls (atom [])]
    (with-redefs [uut/frontend-db :db
                  persistency/transition-item-state!
                  (fn [_ id command]
                    (swap! calls conj [id command])
                    {:id id :type :item-type/link :tags []})]
      (is (= 200 (:status (uut/reader-item-state
                           42 :add-tag :research nil nil))))
      (is (= 200 (:status (uut/reader-item-state
                           42 :remove-tag :research nil nil))))
      (is (= [[42 {:action :add-tag :tag :research}]
              [42 {:action :remove-tag :tag :research}]]
             @calls)))))

(deftest headline-view-is-a-compact-scan-table-with-restrained-actions
  (let [rendered (str (h/html (uut/headlines-list-items
                               {:group-name :default
                                :group-item :none
                                :source-key :all
                                :sources {:feed {:title "Example Feed"
                                                 :url "https://feed.example.org/rss"}}
                                :items [{:id 42
                                         :source-key "feed"
                                         :title "Queued headline"
                                         :ts (time/zoned-date-time)
                                         :tags ["unread" "saved"]
                                         :type :item-type/link
                                         :nwords 100
                                         :url "https://example.com"}]})))]
    (is (re-find #"class=\"reader-headlines-table\"" rendered))
    (is (re-find #"aria-label=\"Headlines in the current snapshot\"" rendered))
    (is (re-find #"class=\"reader-headline-title-column\"" rendered))
    (is (re-find #"class=\"reader-headline-source-column\"" rendered))
    (is (re-find #"class=\"reader-headline-row\"[^>]*data-unread=\"true\"" rendered))
    (is (re-find #"class=\"reader-headline-link\"[^>]*>Queued headline</a>" rendered))
    (is (re-find #"reader-headline-source-key\">feed</span>" rendered))
    (is (re-find #"reader-headline-host\"> → example.com</span>" rendered))
    (is (re-find #"class=\"reader-headline-consumption\"[^>]*>1m</td>" rendered))
    (is (re-find #"btn-item-done" rendered))
    (is (re-find #"data-action-set=\"done\"" rendered))
    (is (re-find #"data-action-unset=\"mark-unread\"" rendered))
    (is (re-find #"id=\"compact-item-more-menu-42\"" rendered))
    (is (string/includes? rendered "Original item"))
    (is (string/includes? rendered "Related items"))
    (is (string/includes? rendered "Focus mode"))
    (is (string/includes? rendered "Save for later"))
    (is (not (string/includes? rendered "table-responsive")))
    (is (not (string/includes? rendered "fa-ellipsis-v")))
    (is (not (re-find #"btn-tag-unread" rendered)))
    (is (not (re-find #"direct-tag-buttons" rendered)))))

(deftest annotated-items-have-a-dedicated-cue-in-every-list-style
  (let [item {:id 42
              :source-key "feed"
              :title "Annotated item"
              :ts (time/zoned-date-time)
              :tags ["unread" "has-annotations" "research"]
              :url "https://example.com/article"}
        context {:group-name :default
                 :group-item :all
                 :source-key :all
                 :sources {:feed {:title "Example Feed"
                                  :options #{}}}}
        link-prefix "/reader/group/default/all/source/all"
        preview (str (h/html (uut/main-list-item context link-prefix item)))
        headlines (str (h/html (uut/headlines-list-items
                                (assoc context :items [item]))))
        gallery (str (h/html (uut/gallery-list-item context link-prefix item)))
        javascript (slurp (io/resource "status/llar.js"))]
    (doseq [rendered [preview headlines gallery]]
      (is (= 1 (count (re-seq #"reader-annotation-cue" rendered))))
      (is (string/includes? rendered "title=\"Open annotations\""))
      (is (string/includes? rendered "annotations=open"))
      (is (string/includes? rendered "mark=read")))
    (is (not (string/includes? preview ">has-annotations<")))
    (is (string/includes? preview ">research<"))
    (is (string/includes? javascript
                          "params.get(\"annotations\") !== \"open\""))
    (is (string/includes? javascript
                          "$(openRequestedAnnotations)"))))

(deftest unannotated-items-do-not-render-the-annotation-cue
  (let [item {:id 42
              :source-key "feed"
              :title "Plain item"
              :ts (time/zoned-date-time)
              :tags ["research"]
              :url "https://example.com/article"}
        rendered (str (h/html (uut/main-list-item
                               {:sources {:feed {:options #{}}}}
                               "/reader/source/feed"
                               item)))]
    (is (not (string/includes? rendered "reader-annotation-cue")))))

(deftest done-control-shows-confirmed-read-state
  (let [unread (str (h/html (uut/done-button {:id 41 :tags ["unread"]})))
        read (str (h/html (uut/done-button {:id 42 :tags []})))]
    (is (re-find #"data-is-set=\"false\"" unread))
    (is (re-find #"title=\"Done reading\"" unread))
    (is (not (re-find #"<i class=\"fas fa-check-circle icon-is-set\"" unread)))
    (is (re-find #"data-is-set=\"true\"" read))
    (is (re-find #"title=\"Mark unread\"" read))
    (is (re-find #"fa-check-circle icon-is-set" read))))

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

(deftest reader-separates-the-article-from-viewport-reading-controls
  (with-redefs [appconfig/credentials (constantly nil)
                rc/rc (constantly nil)
                uut/nav-bar (constantly nil)
                uut/group-nav (constantly nil)
                uut/source-nav (constantly nil)]
    (let [item {:id 42
                :url "https://example.com"
                :data {:description
                       {"text/html" "<p>One</p><p>Two</p>"}}
                :entry {:language "en"}
                :tags []
                :nwords 2
                :type :item-type/link
                :checkpoint-selector {:position {:start 4 :end 8}}
                :checkpoint-progress 0.25}
          article (str (h/html (uut/main-show-item
                                {:uri "/reader/item/by-id/42"
                                 :items [item]})))
          overlay (str (h/html (uut/reading-viewport-overlay item)))
          focus-shell (str (#'uut/render-reader-shell
                            {:mode :focus-item :items [item]}
                            [:main "article"]
                            "Example"))
          show-shell (str (#'uut/render-reader-shell
                           {:mode :show-item :items [item]}
                           [:main "article"]
                           "Example"))]
      (is (re-find #"class=\"reading-surface\"" article))
      (is (re-find #"class=\"reader-item-toolbar\"" article))
      (is (re-find #"class=\"reader-item-meta\"" article))
      (is (re-find #"class=\"reader-item-document-actions\"" article))
      (is (re-find #"id=\"item-more-menu\"" article))
      (is (re-find #">Inspect item</span>" article))
      (is (re-find #">Raw extracted HTML</span>" article))
      (is (string/includes? article "Representation · Summary HTML"))
      (is (not (re-find #"item-data-select|Reading Time Estimate" article)))
      (is (not (re-find #"reading-(?:step|checkpoint)-rail" article)))
      (is (re-find #"class=\"reading-viewport-overlay\"" overlay))
      (is (re-find #"class=\"reading-step-rail\"" overlay))
      (is (re-find #"class=\"reading-structure-rail\"" overlay))
      (is (re-find #"class=\"reading-structure-landmarks\"" overlay))
      (is (re-find #"class=\"reading-checkpoint-rail\"" overlay))
      (is (re-find #"class=\"reading-checkpoint-tools\""
                   overlay))
      (is (re-find #"reading-checkpoint-resume btn-resume-checkpoint" overlay))
      (is (re-find #"reading-checkpoint-control reading-checkpoint-save"
                   overlay))
      (is (not (re-find #"btn-outline-secondary|btn-secondary" overlay)))
      (is (not (re-find #"(?:btn|outline)-warning" overlay)))
      (is (re-find #"<body class=\"llar-reader reader-mode-focus-item\"" focus-shell))
      (is (re-find #"class=\"reading-viewport-overlay\"" focus-shell))
      (is (re-find #"<body class=\"llar-reader reader-mode-show-item\"" show-shell))
      (is (re-find #"class=\"reading-viewport-overlay\"" show-shell)))))

(deftest reader-shell-escapes-content-derived-html
  (with-redefs [uut/nav-bar (constantly nil)
                uut/group-nav (constantly nil)
                uut/source-nav (constantly nil)]
    (let [title "<template>: The Content Template element"
          shell (#'uut/render-reader-shell
                 {:mode :list-items :items [{:id 42 :title title}]}
                 [:main [:h4 title]]
                 title)]
      (is (string/starts-with? shell "<!DOCTYPE html>"))
      (is (re-find #"<h4>&lt;template&gt;: The Content Template element</h4>"
                   shell))
      (is (not (string/includes? shell "<template>")))
      (is (re-find #"<script src=\"/static/llar.js\?v=reader-h01-02\"></script></body></html>$"
                   shell)))))

(deftest reader-loads-the-current-jquery-runtime
  (let [footer (apply str (map #(str (h/html %)) (uut/html-footer)))]
    (is (string/includes? footer "/static/jquery/jquery.min.js?v=4.0.0"))))

(deftest item-inspector-leads-with-provenance-signals-and-representations
  (let [ts (time/zoned-date-time 2026 8 11 12 0 0 0 "Europe/Berlin")
        rendered (str
                  (h/html
                   (uut/dump-item
                    {:uri "/reader/group/default/all/source/all/item/by-id/42/dump"
                     :group-name :default
                     :group-item :all
                     :source-key :all
                     :sources {:feed {:title "Example Feed"
                                      :proc {:pre [identity] :post [identity identity]}
                                      :options #{:main-list-use-description}
                                      :tags #{:news}}}
                     :inspect-details
                     {:source-state {:status :ok
                                     :last-successful-fetch-ts ts
                                     :last-duration (java.time.Duration/ofSeconds 2)
                                     :stats {:fetched 20 :processed 18 :db 3}}
                      :annotations [{:id 7 :created-ts ts}]
                      :events [{:event-type :result-offered
                                :recorded-at ts
                                :position 4
                                :surface :related
                                :trigger :related-generated
                                :data {:score 0.75}}
                               {:event-type :impression
                                :recorded-at ts
                                :surface :related
                                :trigger :viewport-dwell}
                               {:event-type :item-opened
                                :recorded-at ts
                                :surface :item-detail
                                :trigger :item-rendered}]}
                     :items [{:id 42
                              :title "Inspectable item"
                              :url "https://example.test/post"
                              :source-key "feed"
                              :author "Ada"
                              :ts ts
                              :checkpoint-progress 0.4
                              :checkpoint-updated-ts ts
                              :nwords 800
                              :names ["Ada" "Berlin"]
                              :nouns ["reader"]
                              :verbs ["inspect"]
                              :urls ["https://clojure.org"]
                              :top-words {"words" [["interface" 4]]}
                              :tags ["unread" "saved" "research"]
                              :entry {:language "en"}
                              :data {:content {"text/html" "<p>Article</p>"
                                               "text/plain" "Article"}
                                     :description {"text/plain" "Summary"}}}]})))]
    (is (string/includes? rendered "Item identity and provenance"))
    (is (string/includes? rendered "Example Feed"))
    (is (string/includes? rendered "Original host"))
    (is (string/includes? rendered "example.test"))
    (is (string/includes? rendered "Unread · Saved"))
    (is (string/includes? rendered "research"))
    (is (string/includes? rendered "Automatic health checks"))
    (is (string/includes? rendered "No obvious problems found"))
    (is (string/includes? rendered "Source and processing context"))
    (is (string/includes? rendered "1 pre · 2 post"))
    (is (string/includes? rendered "20 fetched · 18 processed · 3 new"))
    (is (string/includes? rendered "Extracted signals"))
    (is (string/includes? rendered "Ada, Berlin"))
    (is (string/includes? rendered "interface, reader, inspect"))
    (is (string/includes? rendered "Available representations"))
    (is (string/includes? rendered "Extracted HTML"))
    (is (string/includes? rendered "Extracted text"))
    (is (string/includes? rendered "Summary text"))
    (is (string/includes? rendered "Reader workflow history"))
    (is (string/includes? rendered "Seen in viewport"))
    (is (string/includes? rendered "Annotation added"))
    (is (string/includes? rendered "Reading place updated"))
    (is (string/includes? rendered "position 4 · related · related-generated · score 0.750"))
    (is (string/includes? rendered "data=content&amp;content-type=text%2Fhtml"))
    (is (string/includes? rendered "Clojure data and render context"))
    (is (string/includes? rendered "data-clojure-value-inspector"))
    (is (= "🔍 Inspect — Inspectable item"
           (uut/short-page-headline {:mode :dump-item
                                     :items [{:title "Inspectable item"}]})))))

(deftest item-inspector-surfaces-derived-health-warnings
  (let [rendered (str
                  (h/html
                   (uut/dump-item
                    {:sources {}
                     :items [{:id 43
                              :title "Repeated title"
                              :source-key "retired-feed"
                              :ts (time/plus (time/zoned-date-time) (time/days 1))
                              :nwords -1
                              :tags []
                              :entry {}
                              :data {:description {"text/html" ""
                                                   "text/plain" "Summary"}}}]})))]
    (is (string/includes? rendered "Original URL is missing"))
    (is (string/includes? rendered "Empty representation"))
    (is (string/includes? rendered "Summary fallback"))
    (is (string/includes? rendered "Item timestamp is in the future"))
    (is (string/includes? rendered "Word analysis is unavailable"))
    (is (string/includes? rendered "Language is not stored"))
    (is (string/includes? rendered "Source is not currently configured"))))

(deftest reading-navigation-has-one-mode-aware-forward-path
  (let [javascript (slurp (io/resource "status/llar.js"))]
    (is (re-find #"function advanceReadingBlock\(\)" javascript))
    (is (re-find #"function readingUsesHorizontalColumns" javascript))
    (is (re-find #"function readingAxisMetrics" javascript))
    (is (re-find #"querySelectorAll\(\"h1, h2, h3, h4, hr\"\)" javascript))
    (is (re-find #"function readingStructurePriority" javascript))
    (is (re-find #"var segmentCount = 24" javascript))
    (is (re-find #"elements\.slice\(segmentStart, segmentEnd\)" javascript))
    (is (not (re-find #"\.slice\(0, 24\)" javascript)))
    (is (re-find #"Section break before" javascript))
    (is (re-find #"is-heading-\" \+ element\.tagName\.substring\(1\)" javascript))
    (is (re-find #"function rebuildReadingLandmarks" javascript))
    (is (re-find #"function updateNearestReadingLandmark" javascript))
    (is (re-find #"metrics\.start \+ metrics\.extent / 3" javascript))
    (is (re-find #"Math\.abs\(landmark\.progress - readingFocus\)" javascript))
    (is (re-find #"readingPrefersReducedMotion\(\) \? \"auto\" : \"smooth\""
                 javascript))
    (is (re-find #"new ResizeObserver" javascript))
    (is (re-find #"new MutationObserver" javascript))
    (is (not (re-find #"viewport-(?:bottom|pivot)" javascript)))))

(deftest reading-checkpoint-controls-flash-and-use-the-bottom-icon-hud
  (let [javascript (slurp (io/resource "status/llar.js"))
        css (slurp (io/resource "status/llar.css"))]
    (is (re-find #"flashReadingLocation\(container, checkpoint\.selector\)"
                 javascript))
    (is (re-find #"function checkpointRange\(container, selector\)" javascript))
    (is (re-find #"bottom: calc\(0\.75rem \+ env\(safe-area-inset-bottom\)\)"
                 css))
    (is (re-find #"\.reading-step-rail \{[^}]*left: 0;" css))
    (is (re-find #"\.reading-checkpoint-rail \{[^}]*right: 0;" css))
    (is (re-find #"\.reader-mode-show-item \.reading-structure-rail \{[^}]*right: 16\.6667%"
                 css))
    (is (re-find #"\.reading-structure-landmark\.is-heading-1 \{[^}]*1rem"
                 css))
    (is (re-find #"\.reading-structure-landmark\.is-heading-4 \{[^}]*0\.4rem"
                 css))
    (is (re-find #"\.reading-structure-landmark\.is-divider::before \{[^}]*width: 0\.3rem"
                 css))
    (is (re-find #"@media \(prefers-reduced-motion: reduce\) \{[^}]*\.reading-structure-landmark"
                 css))
    (is (re-find #"\.reading-checkpoint-control \{[^}]*border: 0;"
                 css))))

(deftest item-view-state-buttons-are-icon-only-with-tooltip-labels
  (let [button (uut/state-button 42 (assoc (first uut/+state-buttons+)
                                           :is-set? false))]
    (is (= {:title "Save for later" :aria-label "Save for later"}
           (select-keys (second button) [:title :aria-label])))
    (is (string/includes? (get-in button [1 :class]) "reader-icon-button"))
    (is (= [[:i {:class "far fa-star" :aria-hidden "true"}]]
           (subvec button 2)))))

(deftest reader-state-updates-do-not-prune-or-reload-lists
  (let [javascript (slurp (io/resource "status/llar.js"))]
    (is (not (re-find #"itemNoLongerMatchesView|data-item-root" javascript)))
    (is (not (re-find #"location\\.(?:reload|replace)" javascript)))
    (is (not (re-find #"direct-tag-buttons|ajax-toggle|btn-tag-unread" javascript)))
    (is (re-find #"requestItemState\(item\.data\(\"id\"\), \"seen\"\)"
                 javascript))))

(deftest list-lifecycle-keeps-source-checks-manual-and-batch-read-reversible
  (let [javascript (slurp (io/resource "status/llar.js"))]
    (is (string/includes? javascript
                          "This snapshot will stay in place while the check runs."))
    (is (string/includes? javascript "Open updated snapshot"))
    (is (re-find #"setSourceUpdateReady\(itemsUrl\)" javascript))
    (is (not (string/includes? javascript "fa-hourglass-half")))
    (is (string/includes? javascript "fa-sync-alt icon-is-set"))
    (is (not (string/includes? javascript ".text(\"Refresh ready\")")))
    (is (not (string/includes? javascript "fa-spinner fa-spin")))
    (is (string/includes? javascript "reader-lifecycle-dots"))
    (is (string/includes? javascript "reader-source-update-dots"))
    (is (string/includes? javascript ".removeClass(\"active\")"))
    (is (string/includes? javascript "document.activeElement === this"))
    (is (string/includes? javascript "reader-lifecycle-sync"))
    (is (string/includes? javascript "Recent source runs suggest about "))
    (is (string/includes? javascript "No timing information yet."))
    (is (re-find #"runItemStateBatch\(ids, \"seen\"" javascript))
    (is (re-find #"runItemStateBatch\(ids, \"mark-unread\"" javascript))
    (is (not (string/includes? javascript
                               ".btn-update-sources-in-view\").popover")))))

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

(deftest gems-landing-renders-rediscovery-and-records-metadata-provenance
  (let [now (time/zoned-date-time)
        context (atom nil)
        item {:id 42
              :title "A keeper"
              :author "Ada"
              :source-key "example"
              :ts now
              :type :item-type/link
              :nwords 800
              :tags ["archive" "clojure"]
              :entry {}
              :last-resurfaced (time/minus now (time/days 5))
              :last-opened (time/minus now (time/days 2))
              :data {:description {"text/plain" "Worth finding again."}}}]
    (with-redefs [uut/frontend-db :db
                  persistency/get-gem-facets
                  (constantly {:total 1 :topic-count 1 :source-count 1
                               :tags [{:value "clojure" :count 1}]
                               :sources [{:value "example" :count 1}]})
                  persistency/get-gem-rediscovery-candidates
                  (fn [_ _] [item])
                  persistency/record-results-offered!
                  (fn [_ items event-context]
                    (reset! context event-context)
                    (mapv (fn [offered] {:id 91 :item-id (:id offered)}) items))]
      (let [rendered (str (h/html (uut/tools-view-handler
                                   {:view :gems :request-params {}})))]
        (is (re-find #"Rediscover" rendered))
        (is (re-find #"A keeper" rendered))
        (is (re-find #"data-offer-id=\"91\"" rendered))
        (is (re-find #"aria-label=\"Find related gems\"" rendered))
        (is (re-find #"clojure" rendered))
        (is (re-find #"Resurfaced 5d ago · Opened 2d ago" rendered))
        (is (= :related (:surface @context)))
        (is (= :related-generated (:trigger @context)))
        (is (= "gems" (get-in @context [:metadata :feature])))
        (is (= "rediscover" (get-in @context [:metadata :kind])))))))

(deftest gems-search-is-live-archive-scoped
  (let [seen (atom nil)
        now (time/zoned-date-time)]
    (with-redefs [uut/frontend-db :db
                  persistency/get-gem-facets
                  (constantly {:total 3 :topic-count 1 :source-count 1
                               :tags [] :sources []})
                  persistency/search
                  (fn [_ query options]
                    (reset! seen [query options])
                    [{:id 7 :title "Found gem" :key "source"
                      :ts now :type :item-type/link :nwords 100
                      :tags ["archive" "research"]
                      :headline "A [[[matching]]] passage"
                      :total-count 1}])]
      (let [rendered (str (h/html (uut/tools-view-handler
                                   {:view :gems
                                    :request-params {:query "matching"
                                                     :tag "research"
                                                     :source "source"}})))]
        (is (= "matching" (first @seen)))
        (is (true? (get-in @seen [1 :archived-only?])))
        (is (= "research" (get-in @seen [1 :with-tag])))
        (is (= "source" (get-in @seen [1 :with-source-key])))
        (is (re-find #"Found gem" rendered))
        (is (re-find #"<mark>matching</mark>" rendered))
        (is (re-find #"Topic: research" rendered))
        (is (re-find #"aria-current=\"page\"[^>]*>Relevance" rendered))
        (is (re-find #"aria-label=\"Clear Topic filter\"" rendered))
        (is (re-find #"aria-label=\"Clear Source filter\"" rendered))))))

(deftest gems-browse-empty-state-reflects-active-filters
  (with-redefs [uut/frontend-db :db
                persistency/get-gem-facets
                (constantly {:total 2 :topic-count 1 :source-count 1
                             :tags [] :sources []})
                persistency/get-gem-items
                (constantly {:total 0 :items []})]
    (let [rendered (str (h/html (uut/tools-view-handler
                                 {:view :gems
                                  :request-params {:tag "missing"
                                                   :browse "true"}})))]
      (is (re-find #"No gems match the active filters" rendered))
      (is (re-find #"aria-label=\"Clear Topic filter\"" rendered)))))

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
      (is (= (inc uut/+max-items+) (:limit args)))
      (is (= :ranked (:sort-order args))))))

(deftest only-preview-list-batches-request-description-data
  (with-redefs [rc/rc (constantly nil)]
    (is (true? (:with-preview-data?
                (#'uut/build-items-query-args
                 {:mode :list-items :list-style :preview}
                 :newest))))
    (is (false? (:with-preview-data?
                 (#'uut/build-items-query-args
                  {:mode :list-items :list-style :headlines}
                  :newest))))
    (is (false? (:with-preview-data?
                 (#'uut/build-items-query-args
                  {:mode :show-item :list-style :preview}
                  :newest))))))

(deftest reading-queue-reasons
  (is (= [:saved]
         (#'uut/queue-item-reasons {:tags ["saved"]
                                    :type :item-type/link})))
  (is (= [:continue-reading]
         (#'uut/queue-item-reasons {:tags []
                                    :checkpoint-progress 0.0
                                    :type :item-type/link})))
  (is (= [:saved]
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
      (is (re-find #"No saved or partially read" rendered))
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

(deftest reading-queue-extends-the-regular-preview-item
  (let [now (time/zoned-date-time)
        description (apply str (repeat 80 "A useful queue description. "))
        item {:id 42
              :title "A saved item"
              :author "Reader"
              :source-key "feed"
              :ts now
              :type :item-type/link
              :url "https://example.com/story"
              :entry {:thumbnail "https://example.com/image.png"
                      :comments-url "https://example.com/comments"}
              :tags ["saved" "unread"]
              :checkpoint-progress 0.42
              :nwords 1200
              :data {:description {"text/plain" description}}}
        x (#'uut/queue-item-context {:request-params {}} item)
        rendered (str (h/html (uut/main-list-item
                               x
                               (#'uut/queue-item-link-prefix item)
                               item
                               {:extra-class "reader-queue-preview"
                                :metadata-before (#'uut/queue-preview-metadata item)
                                :render-description #'uut/render-queue-description})))]
    (is (string/includes? rendered "feed-item reader-queue-preview"))
    (is (string/includes? rendered "reader-preview-body"))
    (is (string/includes? rendered "item-preview-small float-end"))
    (is (string/includes? rendered "alt=\"Preview for A saved item\""))
    (is (not (string/includes? rendered "reader-queue-open")))
    (is (string/includes? rendered "Saved place 42%"))
    (is (string/includes? rendered "reader-queue-excerpt"))
    (is (string/includes? rendered "reader-queue-description-more"))
    (is (string/includes? rendered "<summary>More</summary>"))
    (is (string/includes? rendered "comments"))
    (is (string/includes? rendered "reader-list-item-toolbar"))
    (is (string/includes? rendered "reader-list-item-utility-actions"))
    (is (string/includes? rendered "reader-list-item-tags"))
    (is (string/includes? rendered "btn-state-saved"))
    (is (string/includes? rendered "btn-item-done"))))

(deftest reading-queue-no-title-and-no-image-remain-compact
  (let [item {:id 7
              :title ""
              :source-key "feed"
              :ts (time/zoned-date-time)
              :type :item-type/link
              :url "https://example.com/story"
              :entry {:thumbnail "default"}
              :tags ["saved"]
              :nwords 200}
        x (#'uut/queue-item-context {:request-params {}} item)
        rendered (str (h/html (uut/main-list-item
                               x
                               (#'uut/queue-item-link-prefix item)
                               item
                               {:extra-class "reader-queue-preview"
                                :metadata-before (#'uut/queue-preview-metadata item)
                                :render-description #'uut/render-queue-description})))]
    (is (string/includes? rendered "(no title)"))
    (is (not (string/includes? rendered "reader-queue-open")))
    (is (not (string/includes? rendered "<img")))
    (is (not (string/includes? rendered "reader-queue-description")))))

(deftest reading-queue-cluster-heading-explains-scope-and-time
  (let [items [{:type :item-type/link :tags ["saved"] :nwords 400}
               {:type :item-type/link :tags ["saved"] :nwords 800}
               {:type :item-type/link :tags ["saved"]}]
        rendered (str (h/html (#'uut/queue-cluster-heading
                               {:id 1 :words ["software" "performance"]}
                               items)))]
    (is (string/includes? rendered ">Related by<"))
    (is (string/includes? rendered "reader-queue-cluster-terms"))
    (is (string/includes? rendered "software · performance"))
    (is (string/includes? rendered "3 items"))
    (is (string/includes? rendered "≈ 9 min for 2 of 3"))))

(deftest reading-queue-cluster-index-links-visible-clusters
  (let [clusters [[{:id 10 :words ["distributed" "systems"]}
                   [{:type :item-type/link :tags ["saved"] :nwords 400}]]
                  [{:id :unclustered :words ["Unclustered"]}
                   [{:type :item-type/link :tags ["saved"]}]]]
        rendered (str (h/html (#'uut/queue-cluster-index clusters)))]
    (is (string/includes? rendered "Jump to cluster"))
    (is (not (string/includes? rendered "<details")))
    (is (string/includes? rendered "reader-queue-cluster-index-count\">2"))
    (is (string/includes? rendered "href=\"#queue-cluster-10\""))
    (is (string/includes? rendered "distributed · systems"))
    (is (string/includes? rendered "href=\"#queue-cluster-unclustered\""))
    (is (string/includes? rendered "Unclustered"))))

(deftest reading-queue-cluster-index-stays-hidden-for-one-cluster
  (is (nil? (#'uut/queue-cluster-index
             [[{:id 1 :words ["only"]}
               [{:type :item-type/link :tags ["saved"]}]]]))))

(deftest reading-queue-and-continue-reading-keep-distinct-presentations
  (let [item {:id 7
              :title "Resume later"
              :source-key "feed"
              :ts (time/zoned-date-time)
              :type :item-type/link
              :url "https://example.com/story"
              :tags []
              :checkpoint-progress 0.2
              :checkpoint-updated-ts (time/zoned-date-time)
              :nwords 200}
        queue (str (h/html (#'uut/render-reading-queue
                            {:request-params {}}
                            {:clusters {{:id 1 :words ["topic"]} [item]}
                             :offset 0
                             :page-size 100
                             :has-more? false})))
        continue (str (h/html (#'uut/render-reading-queue
                               {:request-params {}}
                               {:items [item] :continue-only? true})))]
    (is (string/includes? queue "reader-queue-preview"))
    (is (string/includes? queue "id=\"queue-cluster-1\""))
    (is (string/includes? queue "class=\"feed-item reader-queue-preview"))
    (is (string/includes? continue "reader-continue-item reader-tool-row"))
    (is (string/includes? continue "1 saved place."))
    (is (string/includes? continue "20% read"))
    (is (string/includes? continue "2 min left · 2 min read"))
    (is (string/includes? continue "resume=checkpoint"))
    (is (string/includes? continue "Place updated"))
    (is (string/includes? continue "Remove from Continue Reading"))
    (is (string/includes? continue "data-action=\"done\""))
    (is (string/includes? continue "Done reading"))
    (is (not (string/includes? continue "Mark unread")))
    (is (not (string/includes? continue "most recently updated first.</p>")))
    (is (not (string/includes? continue "reader-queue-preview")))))

(deftest continue-reading-zero-progress-is-an-honest-start-state
  (let [item {:id 9
              :title "Start here"
              :source-key "feed"
              :ts (time/zoned-date-time)
              :type :item-type/link
              :url "https://example.com/start"
              :tags []
              :checkpoint-progress 0.0
              :checkpoint-updated-ts (time/zoned-date-time)
              :nwords 100}
        rendered (str (h/html (#'uut/render-continue-reading-item
                               {:request-params {}} item)))]
    (is (string/includes? rendered "Ready to begin"))
    (is (not (string/includes? rendered "Start reading")))
    (is (string/includes? rendered "width: 0%"))
    (is (string/includes? rendered "aria-valuenow=\"0\""))
    (is (not (string/includes? rendered "Saved place 0%")))))

(deftest continue-reading-progress-does-not-round-away-real-position
  (is (= 1 (#'uut/continue-progress-percentage 0.001)))
  (is (= 99 (#'uut/continue-progress-percentage 0.999)))
  (is (= 100 (#'uut/continue-progress-percentage 1.0))))

(deftest continue-reading-resume-request-uses-existing-checkpoint-code
  (let [javascript (slurp "resources/status/llar.js")]
    (is (string/includes? javascript
                          "params.get(\"resume\") !== \"checkpoint\""))
    (is (string/includes? javascript
                          "button.trigger(\"click\")"))
    (is (string/includes? javascript
                          "updateContinueReadingAfterState(state)"))))

(deftest reading-queue-filters
  (let [saved {:tags ["saved"] :type :item-type/link}
        in-progress {:tags [] :checkpoint-progress 0.0 :type :item-type/link}
        unread-saved {:tags ["saved" "unread"] :type :item-type/bookmark}
        read-unsaved {:tags [] :type :item-type/bookmark}
        highlighted {:tags ["highlight"] :type :item-type/link}]
    (is (= [true false true true false true]
           (mapv (fn [[filter item]]
                   (#'uut/queue-item-matches-filter? filter item))
                 [[nil saved]
                  [nil highlighted]
                  [:continue-reading in-progress]
                  [:unread unread-saved]
                  [:unread read-unsaved]
                  [:saved unread-saved]])))))

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
        unread-bookmark (assoc saved-short :tags ["saved" "unread"] :type :item-type/bookmark)]
    (is (#'uut/queue-item-matches-filters? :saved :under-5 saved-short))
    (is (not (#'uut/queue-item-matches-filters? :saved :under-5 in-progress-short)))
    (is (not (#'uut/queue-item-matches-filters? :saved :under-5 saved-long)))
    (is (#'uut/queue-item-matches-filters? :unread :under-5 unread-bookmark))))

(deftest reading-queue-time-filters-use-video-duration
  (let [video {:tags ["saved" "has-video"]
               :type :item-type/link
               :entry {:duration 299}
               :nwords 5000}]
    (is (#'uut/queue-item-matches-filters? :saved :under-5 video))
    (is (not (#'uut/queue-item-matches-filters? :saved :5-15 video)))
    (is (not (#'uut/queue-item-matches-filters? :saved :60-plus video)))))

(deftest reading-queue-semantics-have-one-sql-definition
  (let [migration (slurp (io/resource "migrations/20260809000002-bookmark-state.up.sql"))
        fever (slurp (io/resource "sql/fever.sql"))]
    (is (re-find #"CREATE VIEW reading_queue_items" migration))
    (is (re-find #"tag = 'saved'" migration))
    (is (re-find #"reading_progress IS NOT NULL" migration))
    (is (not (re-find #"type = 'bookmark'.*tagi" (last (string/split migration #"CREATE VIEW")))))
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

(deftest search-defaults-to-any-time-without-running-an-empty-query
  (let [calls (atom 0)]
    (with-redefs [uut/frontend-db :db
                  persistency/search (fn [& _] (swap! calls inc) [])]
      (let [rendered (str (h/html (uut/tools-view-handler
                                   {:view :search :request-params {}})))]
        (is (zero? @calls))
        (is (re-find #"<option selected=\"selected\" value=\"\">Any time</option>" rendered))
        (is (re-find #"Search titles, authors, URLs, and stored article text" rendered))
        (is (not (re-find #"reader-search-results" rendered)))
        (is (not (re-find #"reader-search-empty" rendered)))))))

(deftest search-renders-dense-results-and-preserves-filter-links
  (let [seen (atom nil)
        now (time/zoned-date-time)]
    (with-redefs [uut/frontend-db :db
                  persistency/search
                  (fn [_ query options]
                    (reset! seen [query options])
                    [{:id 9 :title "Known article" :key "source-a" :ts now
                      :rank 0.125 :title-rank 0.05
                      :headline "A [[[known]]] matching fragment"}
                     {:id 10 :title "Second article" :key "source-b" :ts now
                      :rank 0.1 :title-rank 0.0
                      :headline "Another [[[known]]] result"}])]
      (let [rendered (str (h/html (uut/tools-view-handler
                                   {:view :search
                                    :request-params {:query " known "
                                                     :syntax "phrase"
                                                     :days-ago "90"}})))]
        (is (= "known" (first @seen)))
        (is (= :phrase (get-in @seen [1 :syntax])))
        (is (= (time/days 90) (get-in @seen [1 :time-ago-period])))
        (is (re-find #"2 shown · best matches first" rendered))
        (is (re-find #"title match" rendered))
        (is (re-find #"<mark>known</mark>" rendered))
        (is (re-find #"with-source-key=source-a" rendered))
        (is (re-find #"days-ago=90" rendered))
        (is (re-find #"syntax=phrase" rendered))))))

(deftest search-passes-and-clears-an-active-source-filter
  (let [seen-options (atom nil)]
    (with-redefs [uut/frontend-db :db
                  persistency/search
                  (fn [_ _ options]
                    (reset! seen-options options)
                    [])]
      (let [rendered (str (h/html (uut/tools-view-handler
                                   {:view :search
                                    :request-params {:query "known"
                                                     :syntax "web"
                                                     :days-ago "7"
                                                     :with-source-key "source-a"}})))]
        (is (= "source-a" (:with-source-key @seen-options)))
        (is (re-find #"name=\"with-source-key\" type=\"hidden\" value=\"source-a\"" rendered))
        (is (re-find #"href=\"/reader/tools/search\?query=known&amp;syntax=web&amp;days-ago=7\"" rendered))
        (is (not (re-find #"with-source-key=source-a" rendered)))))))

(deftest search-distinguishes-empty-results-from-query-errors
  (with-redefs [uut/frontend-db :db
                persistency/search (fn [& _] [])]
    (let [rendered (str (h/html (uut/tools-view-handler
                                 {:view :search
                                  :request-params {:query "nothing"}})))]
      (is (re-find #"No matches for “nothing”" rendered))
      (is (not (re-find #"Search failed" rendered)))))
  (with-redefs [uut/frontend-db :db
                persistency/search (fn [& _] (throw (Exception. "bad tsquery")))]
    (let [rendered (str (h/html (uut/tools-view-handler
                                 {:view :search
                                  :request-params {:query "&amp;"
                                                   :syntax "advanced"}})))]
      (is (re-find #"Search failed" rendered))
      (is (re-find #"bad tsquery" rendered))
      (is (not (re-find #"No matches" rendered))))))
