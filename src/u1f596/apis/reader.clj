(ns u1f596.apis.reader
  (:require
   [u1f596.config :as config]
   [u1f596.fetch :as fetch]
   [u1f596.postproc :as proc]
   [u1f596.update :as update]
   [u1f596.persistency :as persistency]
   [u1f596.store :refer [store-items!]]
   [u1f596.db.core :as db]
   [u1f596.blobstore :as blobstore]
   [u1f596.lab :refer [+current-fetch-preview+ current-clustered-saved-items]]
   [u1f596.appconfig :as appconfig]
   [org.bovinegenius [exploding-fish :as uri]]
   [u1f596.http :refer [try-blobify-url! human-host-identifier]]
   [u1f596.metrics :as metrics]
   [u1f596.converter :as converter]
   [clojure.java.io :as io]
   [taoensso.timbre :as log]
   [java-time :as time]
   [compojure.core :refer [GET POST PUT DELETE context routes]]
   [compojure.route :as route]
   [compojure.coercions :refer [as-int]]
   [clojure.string :as string]
   [ring.util.codec :refer [form-encode form-encode* FormEncodeable]]
   [slingshot.slingshot :refer [try+ throw+]]
   [mount.core :refer [defstate]]
   [hiccup.core :refer [html]]
   [clojure.contrib.humanize :as human]
   [clojure.set :as set]))


;; NEXT
;; set read, saved
;; show only unread
;; show only saved
;; fullscreen item next button
;; add arbitrary tags

;; show last update ts
;; open links in external


;; add link as bookmark


;; State: annotations live in memory

(defn human-datetime-ago [ts]
  (let [raw-duration (time/duration ts (time/zoned-date-time))
        duration (-> raw-duration
                     (.minusNanos (.getNano raw-duration)))
        period (time/period (time/local-date ts) (time/local-date))]
    (if (>= (.toDays duration) 2)
      (subs (string/lower-case (str period)) 1)
      (subs (string/lower-case (str duration)) 2))))

(defn- startup-read-state []
  (let [file (appconfig/annotations-file)]
    (log/info "Reading annotations file" file)
    (try+
     (converter/read-edn-annotations (slurp file))
     (catch java.lang.RuntimeException _
       (log/warn "Failed to read annotations file. Starting with empty annotations")
       {}))))

;; TODO move to database
(defstate annotations
  :start (atom (startup-read-state))
  :stop (spit (appconfig/annotations-file) (converter/print-annotations @annotations)))

(defstate frontend-db
  :start (db/make-postgresql-pooled-datastore
          (appconfig/postgresql-config :frontend)))

(def +max-items+
  "Number of items in item list. All fetched at once."
  23)
(def +word-cloud-sizes+
  "Word cloud sizes. Bootstrap 4 units"
  ["sz-a" "sz-b" "sz-c" "sz-d" "sz-e" "sz-f"])

(def +boring-words-regex+
  "Words to skip from word clouds"
  #"^(\d{1,3}|we|\w['`’()]\w|.*[()\"<>]+.*|are|at|or|be|but|more|said|what|when|who|where|also|their|one|\w{0,3}|as|you|your|mine|if|our|i|will|on|for|they|and|in|to|is|of|was|were|with|a|the|have|it|he|she|https|http|this|that|an|\W{1,2}|der|die|das|dass|uns|den|und)$")

(def +boring-url-path-element-regex+
  "Url parts to remove from word cloud urls"
  #"index\.\w+|filters:focal")

(def +boring-url-regex+
  "Urls to remove from word coulds"
  #"bit\.ly|googleusercontent\.com|cdn\.vox-cdn\.com|(png|jpe?g|gif)(\?.+)?$")

(def +exposed-simple-filter+
  "Predifined items table filters. See db/simple-filter-to-sql"
  {nil ["all" "fas fa-asterisk"]
   :unread ["unread" "far fa-square"]
   :today ["today" "fas fa-calendar-day"]})

(def +reading-estimate-badge+
  {:easy "badge-success"
   :medium "badge-warning"
   :hard "badge-danger"})

(def +favorites+
  [[:all :default]
   [:saved :item-tags]
   [:highlight :item-tags]
   [:blog :source-tag]
   [:hackernews :source-tag]
   [:tech :source-tag]
   [:sci :source-tag]
   [:news :source-tag]
   [:bookmark :type]
   [:recreation :source-tag]])

(declare
 main-list-items
 gallery-list-items
 headlines-list-items)

(def +list-styles+
  {nil {:name "Default"
        :ico "far fa-circle"}
   :preview {:name "Preview"
             :ico "far fa-newspaper"
             :fn #'main-list-items}
   :headlines {:name "Headlines"
               :ico "far fa-list-alt"
               :fn #'headlines-list-items}
   :gallery {:name "Gallery"
             :ico "far fa-images"
             :fn #'gallery-list-items}})

(def +list-style-hints+
  {:storage :headlines
   :shopping :headlines
   :tweet :gallery})

(def +filter-overrides+
  {:saved :total})
;; icons? see https://fontawesome.com/icons

(def +tag-icons-without-buttons+
  "Group list tags that have not icon in +tag-buttons+"
  {:daily "fas fa-coffee"
   :wallpaper "fas fa-tree"
   :has-video "fas fa-film"
   :has-spotify-playlist "fab fa-spotify"
   :has-annotation "fas fa-pen"
   :bookmark "fas fa-bookmark"
   :all "fas fa-asterisk"
   :berlin "fas fa-city"
   :blog "fas fa-blog"
   :comics "far fa-images"
   :news "far fa-newspaper"
   :hackernews "fab fa-y-combinator"
   :storage "fas fa-hdd"
   :tech "fas fa-microchip"
   :youtube-channel "fab fa-youtube"
   :sci "fas fa-flask"
   :gaming "fas fa-gamepad"
   :music "fas fa-music"
   :magazine "fas fa-newspaper"
   :recreation "fas fa-umbrella-beach"
   :highlight "fas fa-sun"})

(def +tags-skip-group-list+
  "Do not display in group list on the left side"
  #{"ibc2018" "test" "unread" "personal" "music-mined"})

(def +tag-buttons+
  "First-class tags show up in the tag bar"
  [{:tag :interesting
    :icon-set "far fa-hand-spock icon-is-set"
    :icon-unset "far fa-hand-spock"}
   {:tag :boring
    :icon-set "far fa-trash-alt icon-is-set"
    :icon-unset "far fa-trash-alt"}
   {:tag :download
    :icon-set "fas fa-download icon-is-set"
    :icon-unset "fas fa-download"}
   {:tag :unread
    :icon-unset "far fa-check-square icon-is-set"
    :icon-set "far fa-square"}
   {:tag :saved
    :icon-set "fas fa-star icon-is-set"
    :icon-unset "far fa-star"}
   {:tag :archive
    :icon-set "fas fa-archive icon-is-set"
    :icon-unset "fas fa-archive"}
   {:tag :in-progress
    :icon-set "fas fa-cog icon-is-set"
    :icon-unset "fas fa-cog"}
   {:tag :book-recommendation
    :icon-set "fas fa-book  icon-is-set"
    :icon-unset "fas fa-book"}
   {:tag :video-recommendation
    :icon-set "fas fa-film  icon-is-set"
    :icon-unset "fas fa-film"}
   {:tag :bug
    :icon-set "fas fa-bug icon-is-set"
    :icon-unset "fas fa-bug"}])

(def +headline-view-tag-buttons+
  "Select from +tag-buttons+"
  #{:interesting :saved :archive :unread})

(defn icon [ico & args]
  [:i (assoc (apply hash-map args) :class ico) "&thinsp;"])

(extend-protocol FormEncodeable
  clojure.lang.Keyword
  (form-encode* [x encoding]
    (form-encode* (name x) encoding)))

(defn reading-time-estimate [item]
  (let [words-per-min 200
        {:keys [nwords readability]} item
        index (or
               (:flesch-index readability)
               51)
        level (cond
                (>= index 70) :easy
                (> 70 index 50) :medium
                (>= 50 index) :hard)
        factor (case level
                 :easy 1
                 :medium 1.5
                 :hard 2)
        estimate (* (/ nwords words-per-min) factor)]
    {:estimate (int (Math/ceil estimate))
     :difficulty level}))

(defn parse-youtube-url
  [url]
  (and (string? url)
       (re-find  #"(youtu\.be\/|youtube\.com\/(watch\?(.*&)?v=|(embed|v)\/))([^\?&\"'>]+)" url)))

(defn make-site-href
  "Make inner site href"
  ([path x]
   (make-site-href path {} x))
  ([path params x]
   (let [params (into {}
                      (remove (fn [[k v]] (or (nil? k) (nil? v)))
                              (merge (select-keys x [:filter :list-style :query :days-ago :with-source-key]) params)))
         query-string (when (some? params) (form-encode params))]
     (if (string/blank? query-string)
       (string/join "/" path)
       (str (string/join "/" path) "?" query-string)))))

(defn html-header [title mode item]
  [:head
   [:meta {:charset "utf-8"}]
   [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no"}]
   [:meta {:name "u1f596-mode" :content mode}]
   [:meta {:name "u1f596-id" :content (:id item)}]
   [:meta {:name "u1f596-title" :content (:title item)}]
   [:title title]
   [:link {:rel "apple-touch-icon" :sizes "180x180" :href "/static/img/apple-touch-icon.png"}]
   [:link {:rel "icon" :type "image/png" :sizes "32x32" :href "/static/img/favicon-32x32.png"}]
   [:link {:rel "icon" :type "image/png" :sizes "16x16" :href "/static/img/favicon-16x16.png"}]
   [:link {:rel "manifest" :href "/static/img/site.webmanifest"}]
   [:link {:rel "stylesheet" :href "/static/bootstrap/bootstrap-4.6.0-dist/css/bootstrap.min.css"}]
   [:link {:rel "stylesheet" :href "/static/ibmplex/Web/css/ibm-plex.min.css"}]
   [:link {:rel "stylesheet" :href "/static/fontawesome/fontawesome-free-5.15.3-web/css/all.min.css"}]
   [:link {:rel "stylesheet" :href "/static/u1f596.css"}]])

(defn html-footer []
  [[:script {:src "/static/jquery/jquery-3.6.0.min.js"}]
   [:script {:src "/static/hammer/hammer.min.js"}]
   [:script {:src "/static/hammer/jquery.hammer.js"}]
   [:script {:src "/static/waypoints/imakewebthings-waypoints-34d9f6d/lib/jquery.waypoints.min.js"}]
   [:script {:src "/static/popper/popper.min.js"}]
   [:script {:src "/static/bootstrap/bootstrap-4.6.0-dist/js/bootstrap.min.js"}]
   [:script {:src "/static/annotator/annotator.min.js"}]
   [:script {:src "/static/u1f596.js"}]])

(defn tag-button [id {:keys [tag is-set? icon-set icon-unset]}]
  [:a {:class (str "btn ajax-toggle " (str "btn-tag-" (name tag)))
       :title (str "Toggle tag " (name tag))
       :data-id id
       :data-icon-set icon-set
       :data-icon-unset icon-unset
       :data-tag (name tag)
       :data-is-set is-set?}
   (if is-set?
     (icon icon-set)
     (icon icon-unset))])

(defn nav-bar
  "Top Navigation Bar: Site Title, Tag Buttons, Branding"
  [x]
  (let [{:keys [group-name group-item source-key mode
                selected-sources items]} x
        {:keys [id tags]} (first items)

        active-group (:group-name x)
        active-key (:group-item x)
        active-source (:source-key x)

        next-item-href (when (> (count items) 1)
                         (let [link-prefix (format "/reader/group/%s/%s/source/%s"
                                                   (name group-name)
                                                   (name group-item)
                                                   (name source-key))]
                           (make-site-href [link-prefix "item/by-id"
                                            (-> items second :id)]
                                           {:mark :read} x)))

        next-item-button (when (> (count items) 1)
                           [:a {:class "btn btn-secondary"
                                :id "btn-next-item"
                                :href next-item-href}
                            (icon "arrow-down")])
        short-title (when (= mode :show-item)
                      (-> items first :title))]

    [:nav {:id "top-nav"
           :class "navbar navbar-dark navbar-expand-md sticky-top bg-dark flex-md-nowrap p-0"}
     [:div {:class "navbar-toggler"}
      [:a {:class "navbar-toggler"
           :data-toggle "collapse"
           :data-target "#navbar"}
       (icon "fas fa-bars")]
      (cond
        (= mode :list-items)
        [:span
         [:a {:class "navbar-toggler"
              :href (make-site-href [(:uri x)] (:range-before x) x)}
          (icon "fas fa-sync")]
         [:a {:class "navbar-toggler"
              :href (make-site-href [(:uri x)] x)}
          (icon "fas fa-step-backward")]
         [:a {:class "navbar-toggler btn-mark-view-read"
               :href "#"}
         (icon "fas fa-glasses")]
         [:a {:class "navbar-toggler"
              :data-toggle "collapse"
              :href "#groupnav"}
          (icon "fas fa-compass")]
         [:a {:class "navbar-toggler"
             :data-toggle "collapse"
             :href "#sourcenav"}
         (icon "fas fa-list")]
         ]
        (= mode :show-item)
        [:span
         [:a {:class "navbar-toggler"
              :href (make-site-href
                     [(format "/reader/group/%s/%s/source"
                              (name active-group) (name active-key))
                      (name active-source)
                      "items"]
                     x)}
          (icon "fas fa-list")]

         (when (> (count items) 1)
           [:a {:class "navbar-toggler"
                :href next-item-href}
            (icon "fas fa-arrow-down")])])

      [:span {:class "navbar-toggler-title"} short-title]]
     [:div {:class "collapse navbar-collapse"
            :id "navbar"}
      [:a {:class "navbar-brand d-none align-middle d-md-block col-md-3 col-lg-2 mr-0"
           :href "#"}
       (icon "far fa-hand-spock")]

      ;; previous:
      ;; "(╯°□°）╯︵ ┻━┻"
      [:ol {:class "col-xs-12 form-control-dark breadcrumb w-100 path"}
       (for [item [group-item]]
         [:li {:class "breadcrumb-item"} (name item)])
       (when-not (= source-key :all)
         [:li {:class "breadcrumb-item"} (:title (first selected-sources))])
       (when (= mode :show-item)
         [:li {:class "breadcrumb-item"} (-> items first :source-key)])
       (when (= mode :show-item)
         [:li {:class "breadcrumb-item"} (-> items first :title)])]

      [:div {:class "navbar-list row justify-content-between col-xs-12 col-md-3 col-lg-2"}
       (cond
         (= mode :list-items)
         [:div {:class "col-xs-8 col-ld-12"}
          [:a {:class "btn btn-secondary"
               :href (make-site-href [(:uri x)] (:range-before x) x)}
           (icon "fas fa-sync")]

          [:a {:class "btn btn-secondary btn-mark-view-read"
               :href "#"}
           (icon "fas fa-glasses")]

          [:a {:class "btn btn-secondary"
               :href (make-site-href [(:uri x)] x)}
           (icon "fas fa-step-backward")]]

         (= mode :show-item)
         [:div {:class "col-xs-8 col-ld-12"}
          (for [btn +tag-buttons+]
            (tag-button id (assoc btn :is-set? (some #(= % (name (:tag btn))) tags))))
          next-item-button])
       ]]]))

(defn group-list
  "Group Item List - Tags, etc."
  [x url-prefix group-items active icons]
  (for [ks group-items
        :when ((some-fn string? keyword?) ks)
        :let [str-ks (cond
                       (keyword? ks)
                       (name ks)
                       (string? ks)
                       ks
                       (coll? ks)
                       (name (first ks)))]]
    [:li {:class "nav-item"}
     [:a {:class (str "nav-link" (when (= str-ks active) " active"))
          :href (make-site-href [url-prefix str-ks "source/all/items"] x)}
      (if-let [ico (get icons (keyword str-ks))]
        [:span (icon ico) "&nbsp;" str-ks]
        str-ks)]]))

(defn group-nav
  "Group Navigation: Add URLs, Filters, Source Tags, Item Tags"
  [x]
  (let [active-group (:group-name x)
        active-key (name (:group-item x))
        icons (merge
               (into {} (for [{:keys [tag icon-set]} +tag-buttons+]
                          [tag (string/replace icon-set #"icon-is-set" "")]))
               +tag-icons-without-buttons+)]

    [:nav {:class (str "collapse col-md-3 col-lg-2 sidebar sidebar-left" " mode-" (name (:mode x)))
           :id "groupnav"}
     [:div {:class "sidebar-sticky" :id "left-nav"}
      [:form {:class "nav flex-column form-inline"
              :id "add-thing"}
       [:div {:class "input-group mb-1"}
        [:input {:type "text"
                 :class "form-control form-control-sm w-80"
                 :id "add-url-1"
                 :placeholder "http://"}]
        [:div {:class "input-group-append"}
         [:button {:class "bookmark-submit form-control-sm btn btn-secondary btn-sm"
                   :type "submit"
                   :data-url-source "#add-url-1"
                   :data-success "#add-url-1-status"
                   :data-type "readability-bookmark"}

          "R"]
         [:button {:class "bookmark-submit form-control-sm btn btn-secondary btn-sm"
                   :type "submit"
                   :data-url-source "#add-url-1"
                   :data-success "#add-url-1-status"
                   :data-type "raw-bookmark"}

          "B"]
         [:button {:class "bookmark-submit form-control-sm btn btn-secondary btn-sm"
                   :type "submit"
                   :data-url-source "#add-url-1"
                   :data-success "#add-url-1-status"
                   :data-type "document"}

          "D"]]]]

      [:ul {:class "nav flex-column"}
       (for [[k [name ico]] +exposed-simple-filter+]
         [:li {:class "nav-item"}
          [:a {:class (str "nav-link" (when (and (not (get +filter-overrides+ (:group-item x)))
                                                 (= (:filter x) k))
                                        " active"))
               :href (make-site-href [(:uri x)] {:filter k} x)}
           (icon ico) "&nbsp;" [:span name]]])]

      [:h6 {:class (str "sidebar-heading d-flex justify-content-between "
                        "align-items-center px-3 mt-4 mb-1 text-muted")}
       [:span "Favorites"]]
      [:ul {:class "nav flex-column"}
       (for [[key group] +favorites+]
         [:li {:class "nav-item"}
          [:a {:class (str "nav-link" (when (and
                                             (= active-group group)
                                             (= (keyword active-key) key)) " active"))
               :href (make-site-href [(str "/reader/group/" (name group) "/" (name key) "/source/all/items")] x)}
           (when-let [ico (get icons key)] [:span (icon ico) "&nbsp;"])

           (name key)]])]

      [:h6 {:class (str "sidebar-heading d-flex justify-content-between "
                        "align-items-center px-3 mt-4 mb-1 text-muted")}
       [:span "Lab"]]
      [:ul {:class "nav flex-column"}
       [:li {:class "nav-item"}
        [:a {:class "nav-link"
             :href (make-site-href ["/reader/lab/saved-overview"] x)}
         (icon "fas fa-glass-whiskey") "&nbsp;" "Saved Overview"]]]
      [:ul {:class "nav flex-column"}
       [:li {:class "nav-item"}
        [:a {:class "nav-link"
             :href (make-site-href ["/reader/lab/dump-data-structure"] x)}
         (icon "fas fa-glass-whiskey") "&nbsp;" "Data Structure"]]]
      [:ul {:class "nav flex-column"}
       [:li {:class "nav-item"}
        [:a {:class "nav-link"
             :href (make-site-href ["/reader/lab/search"] x)}
         (icon "fas fa-glass-whiskey") "&nbsp;" "Search"]]]

      [:h6 {:class (str "sidebar-heading d-flex justify-content-between "
                        "align-items-center px-3 mt-4 mb-1 text-muted")}
       [:span "List Style"]]
      [:ul {:class "nav flex-column" :id "view-style-select"}
       (for [[key {:keys [name ico]}] +list-styles+]
         [:li {:class "nav-item"}
          [:a {:class "nav-link" :href (make-site-href [(:uri x)] {:list-style key} x)}
           (icon ico) "&nbsp;" name]])]

      ;; item tags
      [:h6 {:class "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"}
       [:span "item tags"]]
      [:ul {:class "nav flex-column"}
       (group-list x "/reader/group/item-tags"
                   (->> x :item-tags (map first) (remove +tags-skip-group-list+) sort)
                   (when (= active-group :item-tags) active-key)
                   icons)]

      ;; source tags
      [:h6 {:class "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"}
       [:span "source tags"]]
      [:ul {:class "nav flex-column"}
       (group-list x "/reader/group/source-tag"
                   (->> (:sources x) vals (map :tags) (apply set/union) sort)
                   (when (= active-group :source-tag) active-key)
                   icons)]

      ;; source types
      [:h6 {:class "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"}
       [:span "type"]]
      [:ul {:class "nav flex-column"}
       (group-list x "/reader/group/type"
                   (->> x :sources vals (map :type) (into (sorted-set)))
                   (when (= active-group :type) active-key)
                   icons)]]]))

(defn source-list-item
  "Source Navigation List Item"
  [x prefix source active-key]
  (let [{:keys [key title item-tags]} source
        fltr (or (:filter x) :total)
        nitems (or (get item-tags fltr) 0)
        grey-out? (and (keyword? fltr) (not= fltr :all) (zero? nitems))
        pill [:span {:class "badge badge-pill float-right"}
              (when (pos? nitems) nitems)]]

    [:li {:class (str "nav-item")}
     [:a {:class (str
                  (if grey-out? "nav-link nav-link-secondary" "nav-link")
                  (when (= key active-key) " active "))
          :href (make-site-href [prefix (name (or key :unknown)) "items"] x)}
      (cond
        (= (:type source) :item-type/bookmark)
        (let [bookmark-name (or (:name source) (:title source) (:key source))
              nice-url (second (re-find #"(?:\[Bookmark: ([\w\.]+)\]|(.+))" bookmark-name))]
          [:span pill
           [:span {:class "sidebar-heading-2"}
            (icon "fas fa-bookmark") "&nbsp;" nice-url]])

        :else
        [:span
         pill
         [:span {:class "sidebar-heading-2"} key]
         [:br]
         [:span {:class "font-weight-italic"} (human/truncate title 30 "…")]])]]))

(defn source-nav
  "Source Navigation: List Sources having the selected tag"
  [x]
  (let [active-group (:group-name x)
        active-key (:group-item x)
        active-source (:source-key x)]
    [:nav {:class (str "collapse col-md-3 col-lg-2 sidebar sidebar-right" " mode-" (name (:mode x)))
           :id "sourcenav"}
     [:div {:class "sidebar-sticky" :id "right-nav"}
      [:ul {:class "nav flex-column"}
       (for [src (->> (:active-sources x) (sort-by :key))]
         (source-list-item
          x
          (format "/reader/group/%s/%s/source"
                  (name active-group) (name active-key))
          src
          active-source))]]]))

(defn tags-button-group [item-id tags]
  [:div {:class "btn-group btn-group-sm"}
   [:a {:class "btn"
        :data-toggle "modal"
        :data-target (str "#add-custom-tag-" item-id)}
    "&nbsp;" (icon "fas fa-tag") (string/join ", " tags)]
   [:div {:class "modal" :id (str "add-custom-tag-" item-id) :tabindex "-1" :role "dialog"}
    [:div {:class "modal-dialog modal-dialog-centered"}
     [:div {:class "modal-content"}
      [:div {:class "modal-header"}
       [:h5 "Edit Tags"]
       [:button {:type "button" :class "close"
                 :data-dismiss "modal"}
        [:span "&times;"]]]
      [:div {:class "Modal-body"}
       [:ul
        (for [tag tags]
          [:li
           (tag-button item-id {:tag tag
                                :icon-set "fas fa-check-circle icon-is-set"
                                :icon-unset "far fa-circle"
                                :is-set? true})
           "&nbsp;"
           tag])]
       [:form {:class "add-custom-tag" :data-id item-id}
        [:div {:class "form-group"}
         [:label {:for (str "add-tag-" item-id)} "Add Custom Tag"]
         [:input {:class "form-control" :id (str "add-tag-" item-id)}]]
        [:input {:class "btn btn-primary" :data-modal (str "#add-custom-tag-" item-id) :type "submit"}]]]]]]])

(defn render-special-item-content
  "Renders item content that is somehow unique to a source and benefits from special rendering
  (e.g youtube videos, twitter images)"
  [item options]
  (let [{:keys [id source-key title ts author tags
                nwords names entry url urls top-words]} item
        url-site (some-> url uri/uri uri/host)
        youtube-url (parse-youtube-url url)]
    (html
    (when-let [vid youtube-url]
      (when (some? vid)
        (let [maxres-url (str "https://img.youtube.com/vi/" (last vid) "/maxresdefault.jpg")
              hq-url (str "https://img.youtube.com/vi/" (last vid) "/hqdefault.jpg")
               max-thumb (try-blobify-url! maxres-url)
               thumb (if (= max-thumb maxres-url)
                       (try-blobify-url! hq-url)
                       max-thumb)]
           [:div {:class "embed-responsive embed-responsive-4by3"}
            [:div {:id (str "youtube-container-" (last vid))}
             [:img {:class "lazy-youtube embed-responsive-item"
                    :data-vid (last vid)
                    :data-target (str "youtube-container-" (last vid))
                    :src thumb}]]])))

    (when-let [twit-pic (first (get-in entry [:entities :photos]))]
       [:div {:class "item-preview"} [:img {:src twit-pic}]])

    (when-let [image-url (and (not youtube-url) (or (:thumbnail entry) (:lead-image-url entry)))]
      (when (not (or (string/blank? image-url)
                     (= image-url "self")
                     (= image-url "default")))
         [:div {:class (str "item-preview-small"
                            (when (every? options [:main-list-use-description
                                                   :short-word-cloud])
                              " float-md-left"))}
        [:img {:src image-url}]])))))

(defn get-html-content
  "Show Item Helper: Get best content to display in full"
  [doc sel-descr sel-content-type]
  (if (and (nil? sel-descr) (nil? sel-content-type))
    (let [description (get-in doc [:data :description])
          contents (get-in doc [:data :content])]
      (or (get contents "text/html")
          (get contents "text/plain")
          (get description "text/html")
          (get description "text/plain")
          nil))
    (get-in doc [:data sel-descr sel-content-type])))

(defn main-show-item
  "Show Item View"
  [x]
  (let [item (first (:items x))
        selected-data (:data x)
        selected-content-type (:content-type x)
        reading-estimate (reading-time-estimate item)
        {:keys [id url data ts tags entry nwords]} item
        lang (if (#{"de" "en"} (:language entry))
               (:language entry)
               "en")]
    [:div {:class "item-content" :id "item-content"}
     [:div {:class "d-none"
            :id "item-meta"
            :data-id id}]
     [:div {:class "item-content-nav sticky-top"}
      [:div {:class "btn-toolbar " :role "toolbar"}
       [:div {:class "btn-group btn-group-sm" :role "group"}
        [:a {:class "btn"}
         (icon "far file-word")
         [:span {:class (str "badge " (get +reading-estimate-badge+ (:difficulty reading-estimate)))}
          (:estimate reading-estimate) "m"]
         "&nbsp;-&nbsp;"
         nwords "&nbsp;words"]
        (tags-button-group id tags)]
       (when (some? ts)
         [:div {:class "btn-group btn-group-sm" :role "group"}
          [:a {:class "btn"}
           "&nbsp;&nbsp;"
           (icon "far fa-calendar") (human-datetime-ago ts)]])
       [:div {:class "btn-group btn-group-sm" :role "group"}
        [:a {:target "_blank"
             :href url
             :role "button"
             :class "btn"}
         "&nbsp;" (icon "fas fa-external-link-alt")]
        [:a {:class "btn"
             :href (make-site-href [id "dump"] x)}
         "&nbsp;" (icon "fas fa-code")]
        [:a {:class "btn"
             :href (make-site-href [id "download"] {:data "content"
                                                    :content-type "text/html"} x)}
         "&nbsp;" (icon "fas fa-expand")]]

       [:div {:class "btn-group btn-group-sm " :role "group"}
        [:div {:class "dropdown show "}
         [:a {:class "btn dropdown-toggle btn-sm"
              :href "#"
              :role "button"
              :id "item-data-select"
              :data-toggle "dropdown"}
          "Content Type"]

         [:div {:class "dropdown-menu"}
          (for [[descr contents] data]
            (for [[content-type _] contents]
              [:a {:class "dropdown-item"
                   :href (make-site-href
                          (if (re-find #"text/.+" content-type)
                            [(:uri x)]
                            [(:uri x) "download"])
                          {:data (name descr)
                           :content-type content-type}
                          x)}
               (str (name descr) " - " content-type)]))]]]]]
     [:div {:id "item-content-body-container" :class "container-fluid"}
      [:div {:class "row"}
       [:div {:class "col-11"}
        [:div {:id "item-content-body" :class "item-content-body hyphenate" :lang lang}
         (if-let [html-content (if (and (= (-> x :active-sources first :type) :item-type/document)
                                   (nil? selected-data) (nil? selected-content-type))
                            (get-html-content item :description "text/html")
                            (get-html-content item selected-data selected-content-type))]
           html-content
           (render-special-item-content item #{}))
       [:div {:id "minimap" :class "col-1 sticky-top"}]]]]]]))

(defn list-entry-kv
  "Helper: Key/Value Pair to pretty HTML <li>"
  [k v]
  [:li (icon "far fa-file") [:strong (str k)] "&nbsp;"
   (cond
     (nil? v)
     [:span "nil"]

     (coll? v)
     [:span (format "n=%s [%s]" (count v)
                    (string/join ", " v))]

     (= k "text/html")
     [:span "(" (type v) ")"
      [:code [:pre (org.apache.commons.lang.StringEscapeUtils/escapeHtml v)]]]

     (= k "text/plain")
     [:span "(" (type v) ")"
      [:code [:pre (org.apache.commons.lang.StringEscapeUtils/escapeHtml v)]]]

     :else
     [:span
      (str v) " (" (type v) ")"])])

(defn map-to-tree
  "Convert nested map to semantic ui tree"
  [node]
  (when (map? node)
    (let [nested-pred (fn [[k v]] (when (map? v) [k v]))
          nested (filter nested-pred node)
          rest (remove nested-pred node)]
      [:ul
       (filter some?
               (concat
                (when (coll? nested)
                  (for [[k v] nested]
                    [:li
                     (icon "far fa-folder")
                     [:strong (str k)]
                     (map-to-tree v)]))
                (when (coll? rest)
                  (for [[k v] rest]
                    (list-entry-kv k v)))))])))

(defn dump-item
  "Dump Item Developer Represenation"
  [x]
  (let [item (first (:items x))]
    [:div {:class "item-content"}
     [:div {:class "item-content-nav"}
      [:h3 "Current Item"]
      (map-to-tree item)
      [:h3 "Full Datastructure"]
      (map-to-tree x)]]))

(defn awesome-url-text
  "Helper: Make URL Text from URL, enriched with with Icons, etc."
  [str-url]
  (try+
   (let [url (uri/uri str-url)
         site (some-> url uri/host)
         path (or (some-> url uri/path) "")
         path-seq (-> path (string/split #"/") next vec)
         path-len (count path-seq)
         path-last (or (last path-seq) "")]
     (cond
       (string/includes? site "wikipedia")
       [:span (icon "fab fa-wikipedia-w") "&nbsp;" (string/replace path-last #"_" " ")]

       (and (string/includes? site "github") (= path-len 2))
       [:span (icon "fab fa-github") "&nbsp;" (subs path 1)]

       (and (string/includes? site "twitter"))
       (if (= path-len 1)
         [:span (icon "fab fa-twitter") "&nbsp;" (first path-seq)]
         [:span (icon "fab fa-twitter") "&nbsp;" (first path-seq) "(status)"])

       (string/includes? site "spotify")
       [:span (icon "fab fa-spotify") "&nbsp;" path-last]

       (and (string/includes? site "facebook"))
       (cond
         (= path-len 1)
         [:span (icon "fab fa-facebook-f") "&nbsp;" (first path-seq)]
         (= (first path-seq) "events")
         [:span (icon "fab fa-facebook-f") "&nbsp;" "event"]
         :else
         [:span (icon "fab fa-facebook-f") "&nbsp;" (take-last 2 path-seq)])

       (string/includes? site "youtube")
       [:span (icon "fab fa-youtube") "&nbsp;" path-last]

       (and (string/includes? site "amazon") (neg? path-len))
       (let [dp-entry (.indexOf path-seq "dp")]
         (cond
           (pos? dp-entry)
           [:span (icon "fab fa-amazon") "&nbsp;"
            (string/replace (nth path-seq (dec dp-entry)) #"[_-]" " ")]))

       (> (count str-url) 23)
       (if (re-find +boring-url-path-element-regex+ path-last)
         (str site "⋯")
         (str site "⋯" (human/truncate (last path-seq) 23)))

       :else
       url))
   (catch Object _
     (log/warn (:throwable &throw-context) "Encountered broken url: " str-url)
     str-url)))

(defn word-cloud-fontsize
  "Helper: Get world cloud fontsize as bootstrap class"
  [freq min-freq max-freq]
  (let [max-size (dec (count +word-cloud-sizes+))
        scaled-to-one (if (= max-freq min-freq)
                        1
                        (/ (- freq min-freq) (- max-freq min-freq)))
        size (Math/log (inc (* scaled-to-one 150)))]
    (nth +word-cloud-sizes+
         (-> size int (max 0) (min max-size)))))

(defn short-page-headline
  [x]
  (let [{:keys [mode source-key group-item]} x
        current-item (first (:items x))]
    (cond
      (= mode :dump-item)
      "💩"

      (= mode :show-item)
      (format "🕮 %s [%s]"
              (:title current-item) (name source-key))

      (= mode :list-items)
      (format "▤ %s/%s"
              (name group-item) (name source-key)))))


;; todo - add number of images
;; add number of nouns




(defn main-list-item
  "Main Item List - Word Cloud Style"
  [x link-prefix item]
  (let [{:keys [sources]} x
        {:keys [id source-key title ts author tags
                nwords names entry url urls top-words]} item
        url-site (some-> url uri/uri uri/host)
        source (get sources (keyword source-key))
        boring-filter (fn [word]
                        (not (or
                              (> (count word) 20)
                              (re-find #"^(\W{1,2}|[a-z0-9]\.)" word)
                              (re-find +boring-words-regex+ word))))
        words (take 50 (filter (fn [[word _]] (boring-filter word)) (:words top-words)))
        names (take 50 (filter boring-filter names))
        options (cond-> (set (:options source))
                  (< (+ (count words) (count names) (count urls)) 10)
                  (conj :short-word-cloud))
        min-freq (second (last words))
        max-freq (second (first words))]
    [:div {:id (str "item-" id)
           :class (str "feed-item "
                       (string/join " "
                        (map #(str "option-" (name %)) options)))}
     [:h4 {:class "h4"}
      [:a {:href (make-site-href [link-prefix "item/by-id" id]
                                 {:mark :read} x)}
       (if (string/blank? title)
         "(no title)"
         title)]]

     [:ul {:class "list-inline"}
      [:li {:class "list-inline-item"}
       (icon "far fa-calendar")
       "&nbsp;"
       [:span {:class "timestamp"} (time/format (time/formatter "YYYY-MM-dd 'KW'ww HH:mm") ts)]
       [:span " - "]
       [:span {:class "timestamp"} (human-datetime-ago ts)]]
      (when (>= nwords 0)
        (let [estimate (reading-time-estimate item)
              human-time (:estimate estimate)]
          [:li {:class "list-inline-item"}
           [:a {:class "btn"}
            "&nbsp;" (icon "far fa-file-word") "&nbsp;" human-time "&thinsp;" "min"]]))
      (when (contains? options :mark-read-on-view)
        [:li {:class "list-inline-item"}
         (icon "fas fa-glasses")])

      (when (string? source-key)
        [:li {:class "list-inline-item"}
         "&nbsp;"
         (icon "fas fa-rss") source-key
         (when (= (:type item) :item-type/link)
           [:span "&nbsp;"
            (when-let [comments-url (or (:hn-url entry)
                                        (:comments-url entry))]
              [:a {:href comments-url} "(comments)"])
            " → " (human-host-identifier url)])
         (when (and (string? url) (string? (:url source))
                    (not= (human-host-identifier url)
                          (human-host-identifier (:url source))))
           [:span " → " (human-host-identifier url)])])

      (when-not (string/blank? author)
        [:li {:class "list-inline-item"}
         "&nbsp;"
         (icon "far fa-user") author])]

     (render-special-item-content item options)

     (if (contains? options :main-list-use-description)
       [:p {:class "description"}
        (get-in item [:data :description "text/plain"])]
       [:p {:class "word-cloud"}
      (html
       (for [[word freq] words
             :let [size (word-cloud-fontsize freq min-freq max-freq)]]
         [:span {:class (str "word border text-white " size)} word]))
      (html
       (for [n names]
         [:span {:class "name border"}
          [:a {:href (str "https://en.wikipedia.org/wiki/" n)
               :class "text-white sz-b"} n]]))
      (html
       (for [[text all-text-urls] (->> urls
                                       ;; controversial? remove urls pointing to same site
                                       (remove (fn [str-url]
                                                 (= (some-> str-url uri/uri uri/host)
                                                    url-site)))
                                       (filter #(> (count %) 20))
                                       (take 20)
                                       (map (juxt awesome-url-text identity))
                                       (group-by first)
                                       (doall))
             :let [url (-> all-text-urls
                           first second)]
             :when (not (re-find +boring-url-regex+ url))]
         [:span {:class "url border"}
          [:a {:href url :class "text-white sz-b"}
           text]]))])

     [:div {:class "btn-toolbar justify-content-between" :role "toolbar"}
      [:div {:class "btn-group btn-group-sm mr-2" :role "group"}
       (tags-button-group id tags)
       [:a {:class "btn" :href url}
        "&nbsp;" (icon "fas fa-external-link-alt")]
       [:a {:class "btn"
            :href (make-site-href [link-prefix "item/by-id" id "dump"] x)}
        "&nbsp;" (icon "fas fa-code")]]

      [:div {:class "direct-tag-buttons btn-group btn-group-sm mr-2" :role "group"}
       (for [btn +tag-buttons+]
         (tag-button id (assoc btn :is-set? (some #(= % (name (:tag btn))) tags))))]]]))

(defn headlines-list-items
  "Main Item List - Headlines Style"
  [x]
  (let [{:keys [group-name group-item source-key sources items]} x]
    [:div {:id "headlines" :class "table-responsive"}
     [:table {:class "table table-borderless"}
      [:thead
       [:tr
        [:td (icon "fas fa-rss")]
        [:td "Title"]
        [:td (icon "fas fa-book-reader")]
        [:td (icon "far fa-calendar")]
        [:td "Tools"]]]

      [:tbody
       (for [item items
             :let [link-prefix (format "/reader/group/%s/%s/source/%s"
                                       (name group-name)
                                       (name group-item)
                                       (name source-key))
                   {:keys [id source-key title ts tags nwords url]} item
                   source (get sources (keyword source-key))]]
         [:tr {:data-id id}
          [:td {:class "source"}
           source-key
           (when (= (:type item) :item-type/link)
             [:span "&nbsp;"
              " → " (human-host-identifier url)])
           (when (and (string? url) (string? (:url source))
                      (not= (human-host-identifier url)
                            (human-host-identifier (:url source))))
             [:span " → " (human-host-identifier url)])]

          [:th {:class "title"}
           [:a {:href (make-site-href [link-prefix "item/by-id" id]
                                      {:mark :read} x)}
            (if (string/blank? title)
              "(no title)"
              title)]]

          [:td {:class "nwords"}
           (let [estimate (reading-time-estimate item)
                 human-time (:estimate estimate)]
             human-time)]

          [:td {:class "ts"}
           [:ul {:class "list-inline"}
            [:li {:class "list-inline-item"}
             "&nbsp;"
             [:span {:class "timestamp"} (human-datetime-ago ts)]]]]

          [:td {:class "toolbox"}
           [:div
           (concat
            [[:a {:class "btn" :href url}
              (icon "fas fa-external-link-alt")]]
            (for [btn +tag-buttons+
                  :when (contains? +headline-view-tag-buttons+ (:tag btn))]
              (tag-button id
                          (assoc btn :is-set? (some #(= % (name (:tag btn))) tags)))))]]])]]]))

(defn gallery-list-items
  "Main Item List - Gallery Style"
  [x]
  (let [{:keys [group-name group-item source-key sources items]} x]
    [:div {:class "card-columns" :id "gallery"}
     (for [item items
           :let [link-prefix (format "/reader/group/%s/%s/source/%s"
                                     (name group-name)
                                     (name group-item)
                                     (name source-key))
                 {:keys [id source-key title  ts tags entry url]} item]]
       [:div {:class "card"}
        (let [image (or
                     (first (get-in entry [:entities :photos]))
                     (:lead-image-url entry)
                     (when-let [vid (and (string? url) (re-find #"(youtu\.be\/|youtube\.com\/(watch\?(.*&)?v=|(embed|v)\/))([^\?&\"'>]+)" url))]
                       (let [maxres-url (str "https://img.youtube.com/vi/" (last vid) "/maxresdefault.jpg")
                             hq-url (str "https://img.youtube.com/vi/" (last vid) "/hqdefault.jpg")
                             max-thumb (try-blobify-url! maxres-url)
                             thumb (if (= max-thumb maxres-url)
                                     (try-blobify-url! hq-url)
                                     max-thumb)]
                         thumb)))]
          [:div
           [:a {:type "button"
                :data-target (str "#full-img-" id)
                :data-toggle "modal"}
            [:img {:src image :class "card-img-top"}]]

           [:div {:class "modal " :id (str "full-img-" id) :tabindex "-1" :role "dialog"}
            [:div {:class "modal-dialog modal-dialog-centered modal-lg"}
             [:div {:class "modal-content"}
              [:img {:src image :class "card-img-top"
                     :data-dismiss "modal"}]]]]

           [:div {:class "card-body"}
            [:h5 {:class "card-title"}
             [:a {:href (make-site-href [link-prefix "item/by-id" id]
                                        {:mark :read} x)}
              (if (string/blank? title)
                "(no title)"
                title)]
             [:p {:class "card-text"}]
             [:p {:class= "card-text"} [:small {:class "text-muted"} source-key]]
             [:p {:class= "card-text"} [:small {:class "text-muted"} (human-datetime-ago ts)]]
             [:p {:class "card-text toolbox"}
              (concat
               [[:a {:class "btn" :href url}
                 (icon "fas fa-external-link-alt")]]
               (for [btn +tag-buttons+]
                 (tag-button id
                             (assoc btn :is-set? (some #(= % (name (:tag btn))) tags)))))]]]])])]))

(defn main-list-items
  "Generate Mail Item List"
  [x]
  (let [{:keys [group-name group-item source-key items]} x]
    [:div
     (for [item items
           :let [url (format "/reader/group/%s/%s/source/%s"
                             (name group-name)
                             (name group-item)
                             (name source-key))]]
       (try+
        (main-list-item x url item)
        (catch Object _
          (throw+ {:type ::item-render-error
                   :group-name group-name
                   :group-item group-item
                   :source-key source-key
                   :item item
                   :url url}))))]))

(defn get-list-style [x]
  (let [selected-style (:list-style x)
        hinted-style (get +list-style-hints+ (:group-item x))]
    (cond
      (and (nil? selected-style) (keyword? hinted-style))
      hinted-style

      (keyword? selected-style)
      selected-style

      :else
      :preview)))

(defn list-items [x]
  ((get-in +list-styles+ [(get-list-style x) :fn]) x))

(defn main-view
  "Generate Main Items View, depending on selected style"
  [x]
  [:main {:role "main"
          :class "col-xs-12 col-md-6 col-lg-8"}
   [:div {:class "justify-content-between flex-wrap flex-md-no align-items-center pb-2 mb-3"}
    (case (:mode x)
      :show-item (main-show-item x)
      :dump-item (dump-item x)
      :list-items (list-items x)
      "Unkown mode")]])

(defn get-items-for-current-view
  "Fetch current view items from database"
  [sources params]
  (let [{:keys [range-before mode group-name item-id group-item source-key]} params
        fltr (:filter params)
        common-args {:before (when-not (empty? range-before) range-before)
                     :simple-filter fltr
                     :with-data? false
                     :limit (if (= mode :get-moar-items)
                              1
                              +max-items+)}]

    (cond
      (contains? #{:show-item :download :dump-item} mode)
      (let [current-item (persistency/get-item-by-id frontend-db item-id)
            next-items (get-items-for-current-view
                        sources
                        (-> params
                            (assoc :range-before (select-keys current-item [:ts :id]))
                            (assoc :mode :get-moar-items)))]
        (into [current-item] next-items))

      (and (= group-name :default) (= group-item :all) (= source-key :all))
      (persistency/get-items-recent frontend-db common-args)

      (and (= group-name :default) (= group-item :all) (keyword? source-key))
      (persistency/get-items-recent frontend-db (merge common-args
                                  {:with-preview-data? (contains?
                                                        (get-in sources [source-key :options])
                                                        :main-list-use-description)
                                   :with-source-keys [source-key]}))

      (and (= group-name :item-tags) (keyword? group-item) (= source-key :all))
      (persistency/get-items-recent frontend-db (merge common-args
                                        {:with-tag group-item}))

      (and (= group-name :item-tags) (keyword? group-item) (keyword? source-key))
      (persistency/get-items-recent frontend-db (merge common-args
                                  {:with-preview-data? (contains?
                                                        (get-in sources [source-key :options])
                                                        :main-list-use-description)
                                   :with-source-keys [source-key]
                                   :with-tag group-item}))

      (and (= group-name :source-tag) (keyword? group-item) (= source-key :all))
      (let [selected-sources (->> sources
                             vals
                             (filter #(contains? (:tags %) group-item)))]
        (persistency/get-items-recent frontend-db (merge common-args
                                    {:with-preview-data? (some #(contains? (:options %)
                                                                           :main-list-use-description)
                                                               selected-sources)
                                     :with-source-keys (map :key selected-sources)})))

      (and (= group-name :source-tag) (keyword? group-item) (keyword? source-key))
      (if (->> sources
               vals
               (filter #(and
                         (contains? (:tags %) group-item)
                         (= (:key %) source-key)))
               not-empty)
        (persistency/get-items-recent frontend-db (merge common-args
                                    {:with-preview-data? (contains?
                                                        (get-in sources [source-key :options])
                                                        :main-list-use-description)
                                     :with-source-keys [source-key]}))
        [])

      (and (= group-name :type) (keyword? group-item) (= source-key :all))
      (persistency/get-items-recent frontend-db (merge common-args
                                  {:with-type group-item}))

      (and (= group-name :type) (keyword? group-item) (keyword? source-key))
      (persistency/get-items-recent frontend-db (merge common-args
                                  {:with-source-keys [source-key]
                                   :with-preview-data? (contains?
                                                        (get-in sources [source-key :options])
                                                        :main-list-use-description)
                                   :with-type group-item}))

      :else
      [])))

(defn get-active-group-sources
  "Return active sources, might hit database"
  [sources params]
  (let [{:keys [group-name group-item]} params]
    (cond
      (and (= group-name :default) (= group-item :all))
      (vals sources)

      (= group-name :item-tags)
      (persistency/get-sources-item-tags-counts frontend-db group-item (:filter params) config/*srcs*)

      (= group-name :source-tag)
      (filter #(contains? (:tags %) group-item) (vals sources))

      (= group-name :type)
      (filter #(= (:type %) (keyword "item-type" (name group-item))) (vals sources))

      :else
      [])))

(defn get-selected-sources
  "Return selected sources in current view. (e.g something that should be highlighted"
  [group-sources params]
  (let [{:keys [source-key]} params]
    (if (not= source-key :all)
      (filter #(= (:key %) source-key) group-sources)
      group-sources)))

(defn download-item-content
  "Download Selected Item Content"
  [params]
  (let [sources (metrics/with-prom-exec-time :compile-sources
                  (persistency/get-sources frontend-db config/*srcs*))

        items (metrics/with-prom-exec-time :items-current-view
                (get-items-for-current-view sources params))
        item (first items)

        data (get-in item [:data (:data params) (:content-type params)])

        body (metrics/with-prom-exec-time :render-download
               (cond (string? data) data
                     (instance? (Class/forName "[B") data) (java.io.ByteArrayInputStream. data)
                     :else nil))]

    (if (some? data)
      {:status 200
       :headers {"Content-Type" (:content-type params)}
       :body body}
      {:status 404
       :headers {"Content-Type" "text/plain"}
       :body (format "Item Not Found / Content Not Available\n\nItem ID: %s\nData Type: %s\nContent-Type: %s\nAvailable: \n%s"
                     (:item-id params) (:data params) (:content-type params)
                     (string/join "\n"
                                  (map (fn [[k v]] (str k " - " (keys v))) (:data item))))})))

(defn gather-reader-index-data [index-params]
  (try+
   (let [;; override filter for special groups like saved
         orig-fltr (:filter index-params)

         params (assoc index-params :filter
                       (if-let [override (get +filter-overrides+
                                              (:group-item index-params))]
                         override
                         orig-fltr))

         item-tags (future (metrics/with-prom-exec-time :tag-list
                             (doall (persistency/get-tag-stats frontend-db))))

         sources (metrics/with-prom-exec-time :compile-sources
                   (doall (persistency/get-sources frontend-db config/*srcs*)))
         items (future (metrics/with-prom-exec-time :items-current-view
                         (doall (get-items-for-current-view sources params))))
         ;; right sidebar
         active-sources (metrics/with-prom-exec-time :active-sources
                          (doall
                           (persistency/sources-merge-in-tags-counts
                            frontend-db
                            (get-active-group-sources sources params))))

         selected-sources (get-selected-sources active-sources params)
         params (merge params {:sources sources
                               :active-sources active-sources
                               :selected-sources selected-sources
                               :items @items
                               :item-tags @item-tags
                               :filter orig-fltr
                               :range-recent (-> @items first (select-keys [:ts :id]))
                               :range-before (-> @items last (select-keys [:ts :id]))})]
     params)
   (catch Object _
     (throw+ {:type ::gather-data-error
              :params index-params}))))

(defn reader-index
  "Reader Entrypoint"
  ([]
   (reader-index {}))
  ([index-params]
   (log/debug "[🖖-UI]" index-params)
   (let [params (gather-reader-index-data index-params)]
     (try+
      (let [nav-bar (nav-bar params)
            group-nav (group-nav params)
            main-view (main-view params)
            source-nav (source-nav params)
            title (short-page-headline params)

            html (metrics/with-prom-exec-time
                   :render-html
                   (html
                    [:html {:lang "en"}
                     (html-header title (:mode params) (some-> params :items first))
                     [:body
                      (concat
                       [nav-bar]
                       [[:div {:class "container-fluid"}
                         [:div {:class "row"}
                          group-nav
                          main-view
                          source-nav]]]
                       (html-footer))]]))]
        html)
     (catch Object _
       (throw+ {:type ::render-error
                :params index-params
                :active-sources (map :key (:active-sources params))
                :selected-sources (map :key (:selected-sources params))
                :filter (:filter params)
                :range-before (:range-before params)
                :range-recent (:range-recent params)
                :item-tags (:item-tags params)
                :items {:count (count (:items params))
                        :ids (map :id (:items params))
                        :titles (map :title (:items params))}}))))))

(defmulti lab-view-handler :view)

(defmethod lab-view-handler
  :saved-overview
  [x]
  [:main {:role "main"
          :class "col-xs-12 col-md-6 col-lg-8"}
   [:div {:class "justify-content-between flex-wrap flex-md-no align-items-center pb-2 mb-3"}
    [:h3 (str "Saved, Bookmarked Items (n=" (apply + (map count (vals @current-clustered-saved-items))) ")")]
    [:div {:class "card-columns" :id "saved-items"}
     (for [[group-name items] @current-clustered-saved-items]
       [:div {:class "card"}
        [:div
         [:div {:class "card-body"}
          [:h5 {:class "card-title"} group-name]
          [:p {:class "card-text"}]]
         [:ul {:class "list-group list-group-flush"}
          (for [{:keys [id title]
                 :as item} items
                :let [reading-estimate (reading-time-estimate item)]]
            [:li {:class "list-group-item"}
             [:a {:href (make-site-href ["/reader/group/default/none/source/all/item/by-id" id] x)}
              [:span {:class (str "badge " (get +reading-estimate-badge+ (:difficulty reading-estimate)))}
               (:estimate reading-estimate) "m"]
              "&nbsp;"
              title
              ]])]]])]]])

(defmethod lab-view-handler
  :dump-data-structure
  [x]
  [:main {:role "main"
          :class "col-xs-12 col-md-6 col-lg-8"}
   [:div {:class "justify-content-between flex-wrap flex-md-no align-items-center pb-2 mb-3"}
    [:h3 (str "Data Structure")]
    (map-to-tree x)]])

(defmethod lab-view-handler
  :search
  [x]
  (let [query (get-in x [:request-params :query])
        with-source-key (get-in x [:request-params :with-source-key])
        days-ago (get-in x [:request-params :days-ago])
        results (if (or with-source-key days-ago)
                  (persistency/search frontend-db query {:with-source-key with-source-key
                                          :time-ago-period (when-not (string/blank? days-ago)
                                                             (time/days (some-> days-ago
                                                                                 Integer/parseInt)))})
                  (persistency/search frontend-db query))]
    [:main {:role "main"
            :class "col-xs-12 col-md-6 col-lg-8"}
     [:div {:class "justify-content-between flex-wrap flex-md-no align-items-center pb-2 mb-3"}
      [:h3 "Search"]
      [:div
       [:form {:action "/reader/lab/search" :method "get"}
        [:div {:class "form-group row"}
         [:label {:for "query" :class "col-sm-4 col-form-label"} "postgresql ts_query"]
         [:div {:class "col-sm-8"}
          [:input {:type "text" :class "form-control"
                   :name "query" :id "query" :placeholder "fat & (rat | cat)"
                   :value (or query "")}]]]
        [:div {:class "form-group row"}
         [:label {:for "query" :class "col-sm-4 col-form-label"} ""]
         [:div {:class "col-sm-8"}
          [:p "Quoted (') lexemes. Operators: &, |, !, &lt;-&gt; (followed by), &lt;N&gt; (follwed by with distance)"]]]
        [:div {:class "form-group row"}
         [:label {:for "query" :class "col-sm-4 col-form-label"} "Item fetch in the last:"]
         (for [[name days] [["any" ""]
                            ["7d" "7"]
                            ["14d" "14"]
                            ["90d" "90"]
                            ["180d" "180"]
                            ["1y" "365"]]]
           [:div {:class "form-check form-check-inline"}
            [:input (assoc {:class "form-check-input"
                            :type "radio"
                            :name "days-ago"
                            :id (str "days-ago-" name)
                            :value days}
                           :checked (= days-ago days))]
            [:label {:class "form-check-label" :for (str "days-ago-" name)} name]])]
        [:div {:class "form-group row"}
         [:div {:class "col-sm-10"}
          [:button {:type "submit" :class "btn btn-primary"} "Search"]]]]

       [:h3 "Results"]
       [:p [:td (count results)]]

       [:p {:class "word-cloud"}
        (let [freqs (->> (map :key results)
                         frequencies
                         (sort-by second)
                         reverse)
              min-freq (-> freqs last second)
              max-freq (-> freqs first second)]
          (for [[word freq] freqs
                :let [size (word-cloud-fontsize freq min-freq max-freq)]]
            [:span {:class (str "word border text-white " size)}
             [:a {:href (make-site-href ["/reader/lab/search"]
                                        (merge x {:with-source-key word
                                                  :query query
                                                  :days-ago days-ago}))
                  :class "text-white sz-b"} (str word " (" freq ")")]]))]

       [:table {:class "table table-borderless"}
        [:thead
         [:tr
          [:td "Rank"]
          [:td "Title"]
          [:td "Source"]]]
        [:tbody
         (for [{:keys [title key rank id]} results]

           [:tr [:td (format "%.2f" rank)]

            [:td [:a {:href (make-site-href ["/reader/group/default/none/source/all/item/by-id" id] x)}
                  title]]

            [:td [:a {:href (make-site-href ["/reader/group/default/all/source" key "items"] x)}
                  key]]])]]]]]))

(defn reader-lab-index
  "Reader Entrypoint"
  ([]
   (reader-index {}))
  ([params]
   (log/debug "[🖖-UI]" params)
   (let [;; override filter for special groups like saved
         orig-fltr (:filter params)
         params (assoc params :filter
                       (if-let [override (get +filter-overrides+
                                              (:group-item params))]
                         override
                         orig-fltr))

         item-tags (future (metrics/with-prom-exec-time :tag-list
                             (doall (persistency/get-tag-stats frontend-db))))

         params (merge params {:sources {}
                               :group-group :lab
                               :group-key (:view params)
                               :item-tags @item-tags
                               :mode :lab
                               :filter orig-fltr})]

     (let [nav-bar (nav-bar params)
           view (lab-view-handler params)
           group-nav (group-nav params)
           title (short-page-headline params)

           html (metrics/with-prom-exec-time
                  :render-html
                  (html
                   [:html {:lang "en"}
                    (html-header title (:mode params) (some-> params :items first))
                    [:body
                     (concat
                      [nav-bar]
                      [[:div {:class "container-fluid"}
                        [:div {:class "row"}
                         group-nav
                         view]]]
                      (html-footer))]]))]
       html))))

(defn fetch-preview
  "Preview Mode Entrypoint"
  []
  (html
   [:html {:lang "en"}
    (html-header "preview" "preview" nil)
    [:body
     (concat
      [[:main {:role "main"
               :class "col-xs-12 col-md-6 col-lg-8"}
        [:div {:class "justify-content-between flex-wrap flex-md-no align-items-center pb-2 mb-3"}
         (for [item @+current-fetch-preview+]
           (dump-item {:items [item]}))]]]
      (html-footer))]]))

(defn add-thing
  "Bookmark / Document Add URL Entry Point"
  [feed key]
  (log/debug "[🖖-UI] Add Bookmark: " feed)
  (try+
   (let [state (assoc update/src-state-template :key key)
         items (fetch/fetch feed)
         processed (proc/process feed state items)
         dbks (store-items! processed :overwrite? true)
         item-id (-> dbks first :item :id)
         item (first processed)]
     {:status 200
      :body {:item {:meta (:meta item)
                    :id item-id
                    :title (get-in item [:summary :title])}}})
   (catch Throwable th
     (log/warn th "add-url failed: " feed)
     {:status 500
      :body {:error (str th)}})))

(defn reader-item-modify
  "Item Modification (e.g Set Tag) Entry Point"
  [id action tag]
  (log/debug "[🖖-UI] Item mod:" id action tag)
  (str (case action
         :set (persistency/item-set-tags! frontend-db id [tag])
         :del (persistency/item-remove-tags! frontend-db id [tag]))))

(defn as-keyword
  "Compojure Helper: Parse a string into keyword"
  [s]
  (keyword s))

(defn as-ts
  "Compojure Helper: Parse string into timestamp"
  [s]
  (when-not (nil? s)
    (time/zoned-date-time (time/formatter :iso-date-time) s)))

(defn as-url
  "Compojure Helper: Parse string into URL"
  [s]
  (uri/uri s))

(def app
  "Compojure Routes"
  (routes
   (context
     "/reader"
     [:as req]

     (POST "/item/by-id/:item-id"
       [item-id :<< as-int
        action :<< as-keyword
        tag :<< as-keyword]
       (reader-item-modify item-id action tag))

     (POST "/bookmark/add"
       [url type :<< as-keyword]
       (try
         (case type
           :readability-bookmark (add-thing
                                  (config/make-readability-bookmark-feed url)
                                  :bookmark)
           :raw-bookmark (add-thing
                          (config/make-raw-bookmark-feed url)
                          :bookmark)
           :document (add-thing
                      (config/make-doc-feed url)
                      :document))
         (catch java.net.MalformedURLException ex
           {:status 400
            :body {:error (str "Malformed URL: " url)}})))

     (POST "/document/add"
       [url]
       (add-thing
        (config/make-doc-feed url)
        :document))

     (GET "/lab/:view" [view :<< as-keyword]
       (reader-lab-index {:uri (:uri req)
                          :filter (as-keyword (get-in req [:params :filter]))
                          :request-params (:params req)
                          :group-name :default
                          :group-item :all
                          :source-key :all
                          :view view
                          :list-style (as-keyword (get-in req [:params :list-style]))}))

     (context
       "/group/:group-name/:group-item/source/:source-key"
       [group-name :<< as-keyword
        group-item :<< as-keyword
        source-key :<< as-keyword]

       (GET "/items"
         [id :<< as-int
          ts :<< as-ts]
         (reader-index {:uri (:uri req)
                        :filter (as-keyword (get-in req [:params :filter]))
                        :group-name group-name
                        :group-item group-item
                        :source-key source-key
                        :mode :list-items
                        :list-style (as-keyword (get-in req [:params :list-style]))
                        :range-before {:id id
                                       :ts ts}}))

       (GET "/items" []
         (reader-index {:uri (:uri req)
                        :filter (as-keyword (get-in req [:params :filter]))
                        :group-name group-name
                        :group-item group-item
                        :source-key source-key
                        :list-style (as-keyword (get-in req [:params :list-style]))
                        :mode :list-items}))

       (context
         "/item/by-id/:item-id"
         [item-id :<< as-int]
         (GET "/"
           [data :<< as-keyword
            content-type]
           (when (and (some? item-id) (= (get-in req [:params :mark]) "read"))
             (reader-item-modify item-id :del :unread))
           (reader-index {:uri (:uri req)
                          :filter (as-keyword (get-in req [:params :filter]))
                          :group-name group-name
                          :group-item group-item
                          :source-key source-key
                          :item-id item-id
                          :mode :show-item
                          :content-type content-type
                          :data data}))

         (GET "/" []
           (when (and (some? item-id) (= (get-in req [:params :mark]) "read"))
             (reader-item-modify item-id :del :unread))
           (reader-index {:uri (:uri req)
                          :filter (as-keyword (get-in req [:params :filter]))
                          :group-name group-name
                          :group-item group-item
                          :source-key source-key
                          :item-id item-id
                          :mode :show-item})))

       (context
         "/item/by-id/:item-id/download"
         [item-id :<< as-int]
         (GET "/"
           [data :<< as-keyword
            content-type]
           (download-item-content {:uri (:uri req)
                                   :filter (as-keyword (get-in req [:params :filter]))
                                   :group-name group-name
                                   :group-item group-item
                                   :source-key source-key
                                   :item-id item-id
                                   :mode :download
                                   :data data
                                   :content-type content-type})))

       (context
         "/item/by-id/:item-id/dump"
         [item-id :<< as-int]
         (GET "/" []
           (reader-index {:uri (:uri req)
                          :filter (as-keyword (get-in req [:params :filter]))
                          :group-name group-name
                          :group-item group-item
                          :source-key source-key
                          :item-id item-id
                          :mode :dump-item}))))

     (GET "/" [] (reader-index
                  {:group-name :default
                   :group-item :all
                   :source-key :all
                   :list-style (as-keyword (get-in req [:params :list-style]))
                   :mode :list-items})))

   (GET "/preview" []
     {:status 200
      :body (fetch-preview)})

   (GET "/blob/:h" [h]
     (try+
      (let [blob (metrics/with-prom-exec-time :blobstore-get
                   (blobstore/get-blob h))]
        {:status 200
         :headers {"Content-Type" (:mime-type blob)
                   "Etag" h
                   "Last-Modified" (time/format
                                    (time/formatter "EEE, dd MMM yyyy HH:mm:ss z")
                                    (:created blob))}
         :body (:data blob)})
      (catch Object e
        (log/warn e "get-blob failed: " h)
        {:status 404})))

   (context
     "/ans" [:as req]
     (GET "/search" []
       (let [item-id (as-int (get-in req [:params :item_id]))]
         (if-let [items (get @annotations item-id)]
           {:status 200
            :body {:total (count items)
                   :rows (map-indexed (fn [i item]
                                        (assoc item :id
                                               (str item-id "-" i)))
                                      items)}}

           {:status 200
            :body {:total 0}})))

     (GET "/annotations/:id" [id]
       (let [[item-id anno-entry-i]
             (map as-int (string/split id #"-"))]
         (if-let [entry (get-in @annotations [item-id anno-entry-i])]
           {:status 200
            :body entry}
           {:status 404})))

     (POST "/annotations" []
       (let [body (:json-params req)
             item-id (get body "item_id")
             next (swap! annotations update item-id (fnil conj []) body)
             new-anno-id (dec (count (get next item-id)))
             url (str "/ans/annotations/" item-id "-" new-anno-id)]
         (reader-item-modify item-id :set :has-annotation)
         {:status 303
          :headers {"Location" url}}))

     (DELETE "/annotations/:id" [id]
       (let [[item-id anno-entry-i] (map as-int (string/split id #"-"))
             next (swap! annotations
                         update-in [item-id]
                         #(vec (concat (subvec % 0 anno-entry-i)
                                       (subvec % (inc anno-entry-i)))))]
         {:status 204}))

     (PUT "/annotations/:id" [id]
       (let [[item-id anno-entry-i] (map as-int (string/split id #"-"))
             next (swap! annotations assoc-in [item-id anno-entry-i] (:json-params req))
             url (str "/ans/annotations/" item-id "-" anno-entry-i)]
         {:status 303
          :headers {"Location" url}})))

   (route/resources "/static" {:root "status"})
   (route/not-found "404 Not found")))