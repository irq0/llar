(ns llar.apis.reader
  (:require
   [mount.core :refer [defstate]]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.coercions :refer [as-int]]
   [compojure.core :refer [context DELETE GET POST routes]]
   [compojure.route :as route]
   [hiccup2.core :as h :refer [html]]
   [java-time.api :as time]
   [iapetos.core :as prometheus]
   [org.bovinegenius [exploding-fish :as uri]]
   [ring.util.codec :refer [form-encode form-encode* FormEncodeable]]
   [slingshot.slingshot :refer [throw+ try+]]
   [cheshire.core :as cheshire]
   [llar.apis.blob :as blob-api]
   [llar.appconfig :refer [postgresql-config credentials]]
   [llar.bookmark-capture :as bookmark-capture]
   [llar.config :as config]
   [llar.db.core :as db]
   [llar.events :as events]
   [llar.human :as human]
   [llar.item :as item]
   [llar.item-state :as item-state]
   [llar.lab :refer [+current-fetch-preview+
                     +saved-clusters-not-compiled+
                     current-clustered-saved-items]]
   [llar.metrics :as metrics]
   [llar.persistency :as persistency]
   [llar.rc :as rc]
   [llar.store :as store]
   [llar.update :as update]
   [llar.value-inspector :as value-inspector]
   [llar.vibe :as vibe]
   [llar.db.annotations]
   [llar.db.gems]
   [llar.db.search :as db-search]
   [llar.export.zotero :as zotero]
   [llar.export.url-handler :as url-handler]))

(defn- html5
  "Render a complete HTML5 document with Hiccup 2 escaping semantics.

  hiccup.page/html5 is a Hiccup 1 compatibility renderer.  Mixing it with the
  Hiccup 2 nodes used in this namespace leaves ordinary strings unescaped; an
  item title such as `<template>: ...` can consequently swallow the rest of the
  reader document, including its navigation and scripts."
  [& content]
  (let [document (if (and (= 1 (count content))
                          (vector? (first content))
                          (= :html (ffirst content)))
                   (first content)
                   (into [:html] content))]
    (str (h/html (h/raw "<!DOCTYPE html>\n") document))))

;; NEXT
;; set read, saved
;; show only unread
;; show only saved
;; fullscreen item next button
;; add arbitrary tags

;; show last update ts
;; open links in external

(defstate frontend-db
  :start (db/make-postgresql-pooled-datastore
          (postgresql-config :frontend)))

(def +max-items+
  "Number of items in item list. All fetched at once."
  23)
(def +word-cloud-sizes+
  "Word cloud sizes. Bootstrap 4 units"
  ["sz-a" "sz-b" "sz-c" "sz-d" "sz-e" "sz-f"])

(def +boring-words-regex+
  "Words to skip from word clouds"
  #"^(\d{1,3}|we|\w['`’()]\w|.*[()\"<>]+.*|are|at|or|be|but|more|said|what|when|who|where|also|their|one|\w{0,3}|as|you|your|mine|if|our|i|will|on|for|they|and|in|to|is|of|was|were|with|a|the|have|it|he|she|https|http|this|that|an|\W{1,2}|der|die|das|dass|uns|den|und|href=|xmlns=|information|rel=|sites|i')$")

(def +boring-url-path-element-regex+
  "Url parts to remove from word cloud urls"
  #"index\.\w+|filters:focal")

(def +boring-url-regex+
  "Urls to remove from word coulds"
  #"bit\.ly|googleusercontent\.com|cdn\.vox-cdn\.com|(png|jpe?g|gif)(\?.+)?$")

(def +exposed-simple-filter+
  "Predefined items table filters. See db/simple-filter-to-sql"
  {nil ["all" "fas fa-asterisk"]
   :unread ["unread" "far fa-square"]
   :today ["today" "fas fa-calendar-day"]})

(def +reading-estimate-badge+
  {:easy "text-bg-success"
   :medium "text-bg-warning"
   :hard "text-bg-danger"})

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

(def +sort-orders+
  {:newest {:name "Newest First"
            :ico "fas fa-sort-amount-down"}
   :ranked {:name "Ranked"
            :ico "fas fa-star"}
   :oldest {:name "Oldest First"
            :ico "fas fa-sort-amount-up"}})

(declare get-sort-order)

(def +filter-overrides+
  {:saved :total})
;; icons? see https://fontawesome.com/v5/icons

(def +tag-icon-default+ "fas fa-tag")

(def +tag-icons-without-buttons+
  "Group list tags that have not icon in +tag-buttons+"
  {:daily "fas fa-coffee"
   :feed "fas fa-rss-square"
   :link "fas fa-link"
   :mail "fas fa-envelope"
   :wallpaper "fas fa-tree"
   :has-video "fas fa-film"
   :has-spotify-playlist "fab fa-spotify"
   :bookmark "fas fa-bookmark"
   :all "fas fa-asterisk"
   :berlin "fas fa-city"
   :blog "fas fa-blog"
   :comics "far fa-images"
   :news "far fa-newspaper"
   :hackernews "fab fa-y-combinator"
   :storage "fas fa-hdd"
   :tech "fas fa-microchip"
   :deep-tech "fas fa-microchip"
   :youtube-channel "fab fa-youtube"
   :streaming-channel "fas fa-tv"
   :has-audio "fas fa-headphones"
   :sci "fas fa-flask"
   :gaming "fas fa-gamepad"
   :music "fas fa-music"
   :magazine "fas fa-newspaper"
   :recreation "fas fa-umbrella-beach"
   :highlight "fas fa-sun"
   :has-annotations "fas fa-pen-fancy"})

(def +group-icons+
  {:item-tags "fas fa-tag"
   :source-tag "fas fa-hashtag"
   :type "fas fa-shapes"})

(def +tags-skip-group-list+
  "Do not display in group list on the left side"
  #{"unread" "in-progress"})

(def +state-buttons+
  "First-class item state controls shown in Reader action bars."
  [{:tag :saved
    :state :saved
    :action-set :save
    :action-unset :unsave
    :label-set "Remove from saved"
    :label-unset "Save for later"
    :icon-set "fas fa-star icon-is-set"
    :icon-unset "far fa-star"}
   {:tag :archive
    :state :archived
    :action-set :archive
    :action-unset :unarchive
    :label-set "Remove from archive"
    :label-unset "Archive"
    :icon-set "fas fa-archive icon-is-set"
    :icon-unset "fas fa-archive"}])

(def +tag-buttons+
  "First-class item tags shown in Reader action bars."
  [{:tag :podcast
    :icon-set "fas fa-tv icon-is-set"
    :icon-unset "fas fa-tv"}])

(def +digest-tag-button+
  "Shown only when digest delivery is enabled in the effective runtime config."
  {:tag :digest
   :icon-set "fas fa-book-reader icon-is-set"
   :icon-unset "fas fa-book-reader"})

(defn tag-buttons
  "First-class item-tag buttons, including feature-gated ones. The :digest button is
  only offered when the digest feature is configured."
  []
  (cond-> +tag-buttons+
    (rc/rc [:digest :enabled?]) (conj +digest-tag-button+)))

(defn icon [ico & args]
  [:i (assoc (apply hash-map args) :class ico) "\u2009"])

(defn icon-button
  "Render the common Reader icon-only button markup."
  [attrs icon-class]
  [:a (merge {:class "btn"} attrs)
   "\u00a0"
   (icon icon-class)])

(extend-protocol FormEncodeable
  clojure.lang.Keyword
  (form-encode* [x encoding]
    (form-encode* (name x) encoding)))

(defn parse-youtube-url
  [url]
  (when (string? url)
    (re-find  #"(youtu\.be\/|youtube\.com\/(watch\?(.*&)?v=|(embed|v)\/))([^\?&\"'>]+)" url)))

(defn make-site-href
  "Make inner site href"
  ([path x]
   (make-site-href path {} x))
  ([path params x]
   (let [params (into {}
                      (remove (fn [[k v]] (or (nil? k) (nil? v)))
                              (merge (select-keys x [:filter :list-style :sort-order :query :syntax :days-ago :with-source-key])
                                     params)))
         query-string (when (some? params) (form-encode params))]
     (if (string/blank? query-string)
       (string/join "/" path)
       (str (string/join "/" path) "?" query-string)))))

(defn show-item-href
  "Build the item-detail URL for an item in the current reader context."
  [x item]
  (when-let [id (:id item)]
    (make-site-href ["/reader/group"
                     (name (or (:group-name x) :default))
                     (name (or (:group-item x) :none))
                     "source"
                     (name (or (:source-key x) :all))
                     "item/by-id"
                     id]
                    x)))

(defn html-header [title mode item]
  [:head
   [:meta {:charset "utf-8"}]
   [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no"}]
   [:meta {:name "llar-mode" :content mode}]
   [:meta {:name "llar-id" :content (:id item)}]
   [:meta {:name "llar-title" :content (:title item)}]
   [:title title]
   [:link {:rel "apple-touch-icon" :sizes "180x180" :href "/static/img/apple-touch-icon.png"}]
   [:link {:rel "icon" :type "image/png" :sizes "32x32" :href "/static/img/favicon-32x32.png"}]
   [:link {:rel "icon" :type "image/png" :sizes "16x16" :href "/static/img/favicon-16x16.png"}]
   [:link {:rel "manifest" :href "/static/img/site.webmanifest"}]
   [:link {:rel "stylesheet" :href "/static/bootstrap/css/bootstrap.min.css"}]
   [:link {:rel "stylesheet" :href "/static/ibmplex/Web/css/ibm-plex.min.css"}]
   [:link {:rel "stylesheet" :href "/static/fontawesome/css/all.min.css"}]
   [:link {:rel "stylesheet" :href "/static/llar.css?v=inspector-5"}]])

(defn html-footer []
  [[:script {:src "/static/jquery/jquery.min.js"}]
   [:script {:src "/static/bootstrap/js/bootstrap.bundle.min.js"}]
   [:script {:src "/static/llar-value-inspector.js?v=clojure-3"}]
   [:script {:src "/static/llar.js"}]])

(def ^:private tag-action-labels
  {:podcast "Toggle podcast"
   :digest {:set "Remove from next digest"
            :unset "Include in next digest"}})

(defn state-button
  [id {:keys [state is-set? icon-set icon-unset action-set action-unset
              label-set label-unset]}]
  (let [label (if is-set? label-set label-unset)]
    [:a {:class (str "btn state-toggle btn-state-" (name state))
         :href "#"
         :title label
         :aria-label label
         :data-id id
         :data-state (name state)
         :data-icon-set icon-set
         :data-icon-unset icon-unset
         :data-label-set label-set
         :data-label-unset label-unset
         :data-action-set (name action-set)
         :data-action-unset (name action-unset)
         :data-is-set (str (boolean is-set?))}
     (icon (if is-set? icon-set icon-unset))]))

(defn tag-button [id {:keys [tag is-set? icon-set icon-unset]}]
  (let [configured-label (get tag-action-labels tag)
        label (if (map? configured-label)
                (get configured-label (if is-set? :set :unset))
                (or configured-label (str "Toggle tag " (name tag))))]
    [:a {:class "btn item-tag-toggle"
         :href "#"
         :title label
         :aria-label label
         :data-id id
         :data-icon-set icon-set
         :data-icon-unset icon-unset
         :data-tag (name tag)
         :data-label-set (if (map? configured-label)
                           (:set configured-label)
                           label)
         :data-label-unset (if (map? configured-label)
                             (:unset configured-label)
                             label)
         :data-is-set (str (boolean is-set?))}
     (if is-set?
       (icon icon-set)
       (icon icon-unset))]))

(defn state-buttons [id tags]
  (for [{:keys [tag] :as btn} +state-buttons+]
    (state-button id (assoc btn :is-set? (some #(= % (name tag)) tags)))))

(defn item-tag-buttons [id tags]
  (for [{:keys [tag] :as btn} (tag-buttons)]
    (tag-button id (assoc btn :is-set? (some #(= % (name tag)) tags)))))

(defn done-button [item]
  (let [read? (not (contains? (item-state/tag-set item) :unread))]
    (icon-button {:class "btn state-toggle btn-item-done"
                  :title (if read? "Mark unread" "Done reading")
                  :aria-label (if read? "Mark unread" "Done reading")
                  :data-id (:id item)
                  :data-state "read"
                  :data-action-set "done"
                  :data-action-unset "mark-unread"
                  :data-icon-set "fas fa-check-circle icon-is-set"
                  :data-icon-unset "fas fa-check-circle"
                  :data-label-set "Mark unread"
                  :data-label-unset "Done reading"
                  :data-is-set (str read?)
                  :href "#"}
                 (str "fas fa-check-circle" (when read? " icon-is-set")))))

(defn nav-bar
  "Top Navigation Bar: Site Title, Tag Buttons, Branding"
  [x]
  (let [{:keys [group-name group-item source-key mode
                selected-sources items]} x
        {:keys [id tags] :as current-item} (first items)

        active-group (:group-name x)
        active-key (:group-item x)
        active-source (:source-key x)

        link-prefix (format "/reader/group/%s/%s/source/%s"
                            (name group-name)
                            (name group-item)
                            (name source-key))

        next-item-href (when (> (count items) 1)
                         (make-site-href [link-prefix "item/by-id"
                                          (-> items second :id)]
                                         (cond-> {:mark :read}
                                           (some? (:ranked-pos (first items)))
                                           (assoc :ranked-pos (inc (:ranked-pos (first items)))))
                                         x))

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
           :data-bs-toggle "collapse"
           :data-bs-target "#navbar"}
       (icon "fas fa-bars")]
      (cond
        (= mode :list-items)
        [:span
         [:a {:class "navbar-toggler"
              :href (make-site-href [(:uri x)] x)}
          (icon "fas fa-step-backward")]
         [:a {:class "navbar-toggler"
              :href (if (:page-offset x)
                      (make-site-href [(:uri x)] {:page-offset (:page-offset x)} x)
                      (make-site-href [(:uri x)] (:range-before x) x))}
          (icon "fas fa-forward")]
         [:a {:class "navbar-toggler btn-mark-view-read"
              :href "#"}
          (icon "fas fa-glasses")]
         [:a {:class "navbar-toggler"
              :data-bs-toggle "collapse"
              :href "#groupnav"}
          (icon "fas fa-compass")]
         [:a {:class "navbar-toggler"
              :data-bs-toggle "collapse"
              :href "#sourcenav"}
          (icon "fas fa-list")]]

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
      [:ol {:class "col-xs-12 form-control-dark breadcrumb w-100 path flex-nowrap"
            :style "--bs-breadcrumb-divider-color: #fff;"}
       (when-let [item group-item]
         [:li {:class "breadcrumb-item"}
          (when-let [ico (get +group-icons+ active-group)]
            (icon ico))
          [:a {:href (make-site-href ["/reader/group" (name active-group) (name item) "source/all/items"] x)}
           (name item)]])
       (when-not (= source-key :all)
         [:li {:class "breadcrumb-item"}
          [:a {:href (make-site-href ["/reader/group" (name active-group) (name group-item) "source" (name source-key) "items"] x)}
           (:title (first selected-sources))]])
       (when (= mode :show-item)
         (let [source-key (-> items first :source-key)]
           [:li {:class "breadcrumb-item"}
            [:a {:href (make-site-href ["/reader/group" (name active-group) (name group-item) "source" (name source-key) "items"] x)}
             source-key]]))
       (when (= mode :show-item)
         (let [item (first items)]
           [:li {:class "breadcrumb-item"}
            [:a {:href (show-item-href x item)} (:title item)]]))]

      (when-not (= mode :tools)
        [:div {:class "navbar-list row justify-content-between col-xs-12 col-md-3 col-lg-2"}
         (cond
           (= mode :list-items)
           [:div {:class "col-xs-8 col-ld-12"}
            [:a {:class "btn btn-secondary"
                 :title "Back to first item"
                 :href (make-site-href [(:uri x)] x)}
             (icon "fas fa-fast-backward")]

            [:a {:class "btn btn-secondary"
                 :title "Forward N items"
                 :href (if (:page-offset x)
                         (make-site-href [(:uri x)] {:page-offset (:page-offset x)} x)
                         (make-site-href [(:uri x)] (:range-before x) x))}
             (icon "fas fa-forward")]

            [:a {:class "btn btn-secondary btn-mark-view-read"
                 :title "Remove unread tag from all items in view"
                 :href "#"}
             (icon "fas fa-glasses")]

            [:a {:class "btn btn-secondary btn-update-sources-in-view"
                 :title "Update sources in view"
                 :data-target (make-site-href [link-prefix "update"] x)
                 :data-items (make-site-href [(:uri x)] x)
                 :href "#"}
             (icon "fas fa-download")]]

           (= mode :show-item)
           [:div {:class "col-xs-8 col-ld-12"}
            (state-buttons id tags)
            (item-tag-buttons id tags)
            (done-button current-item)
            next-item-button])])]]))

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
          :title (format "Show %s items" str-ks)
          :href (make-site-href [url-prefix str-ks "source/all/items"] x)}
      (if-let [ico (get icons (keyword str-ks) +tag-icon-default+)]
        [:span (icon ico) "\u00a0" str-ks]
        str-ks)]]))

(defn group-nav
  "Group Navigation: Add URLs, Filters, Source Tags, Item Tags"
  [x]
  (let [active-group (:group-name x)
        active-key (name (:group-item x))
        icons (merge
               (into {} (for [{:keys [tag icon-set]} (concat +state-buttons+ (tag-buttons))]
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
                   :title "Add bookmark with readability engine"
                   :data-bs-title "Add Bookmark with readability engine"
                   :data-url-source "#add-url-1"
                   :data-bs-success "#add-url-1-status"
                   :data-type "readability-bookmark"}

          (icon "fas fa-newspaper")]]]]

      [:ul {:class "nav flex-column"}
       (for [[k [name ico]] +exposed-simple-filter+]
         [:li {:class "nav-item"}
          [:a {:class (str "nav-link" (when (and (not (get +filter-overrides+ (:group-item x)))
                                                 (= (:filter x) k))
                                        " active"))
               :href (make-site-href [(:uri x)] {:filter k} x)}
           (icon ico) "\u00a0" [:span name]]])]

      ;; favorites
      [:h6 {:class (str "sidebar-heading d-flex justify-content-between "
                        "align-items-center px-3 mt-4 mb-1 text-muted")}
       [:span "Favorites"]]
      [:ul {:class "nav flex-column"}
       (for [[key group] (rc/rc [:reader :favorites])
             :when (not= [key group] [:in-progress :item-tags])]
         [:li {:class "nav-item"}
          [:a {:class (str "nav-link" (when (and
                                             (= active-group group)
                                             (= (keyword active-key) key)) " active"))
               :title (format "Show items with %s %s" (name group) (name key))
               :href (make-site-href [(str "/reader/group/" (name group) "/" (name key) "/source/all/items")] x)}
           (when-let [ico (get icons key +tag-icon-default+)] [:span (icon ico) "\u00a0"])

           (name key)]])]

      ;; tools
      [:h6 {:class (str "sidebar-heading d-flex justify-content-between "
                        "align-items-center px-3 mt-4 mb-1 text-muted")}
       [:span "Tools"]]
      [:ul {:class "nav flex-column"}
       [:li {:class "nav-item"}
        [:a {:class (str "nav-link" (when (= (:view x) :saved-overview) " active"))
             :href (make-site-href ["/reader/tools/saved-overview"] x)}
         (icon "fas fa-project-diagram") "\u00a0" "Reading Queue"]]]
      [:ul {:class "nav flex-column"}
       [:li {:class "nav-item"}
        [:a {:class (str "nav-link" (when (= (:view x) :continue-reading) " active"))
             :href (make-site-href ["/reader/tools/continue-reading"] x)}
         (icon "fas fa-map-marker-alt") "\u00a0" "Continue Reading"]]]
      [:ul {:class "nav flex-column"}
       [:li {:class "nav-item"}
        [:a {:class (str "nav-link" (when (= (:view x) :todays-vibe) " active"))
             :href (make-site-href ["/reader/tools/todays-vibe"] x)}
         (icon "fas fa-fire") "\u00a0" "Today’s Vibe"]]]
      [:ul {:class "nav flex-column"}
       [:li {:class "nav-item"}
        [:a {:class (str "nav-link" (when (= (:view x) :gems) " active"))
             :href (make-site-href ["/reader/tools/gems"] x)}
         (icon "fas fa-gem") "\u00a0" "Gems"]]]
      [:ul {:class "nav flex-column"}
       [:li {:class "nav-item"}
        [:a {:class (str "nav-link" (when (= (:view x) :search) " active"))
             :href (make-site-href ["/reader/tools/search"] x)}
         (icon "fas fa-search") "\u00a0" "Search"]]]

      ;; list style
      [:h6 {:class (str "sidebar-heading d-flex justify-content-between "
                        "align-items-center px-3 mt-4 mb-1 text-muted")}
       [:span "List Style"]]
      [:ul {:class "nav flex-column" :id "view-style-select"}
       (for [[key {:keys [name ico]}] +list-styles+]
         [:li {:class "nav-item"}
          [:a {:class "nav-link" :href (make-site-href [(:uri x)] {:list-style key} x)}
           (icon ico) "\u00a0" name]])]

      ;; sort order
      [:h6 {:class (str "sidebar-heading d-flex justify-content-between "
                        "align-items-center px-3 mt-4 mb-1 text-muted")}
       [:span "Sort Order"]]
      (let [active-sort (get-sort-order x)]
        [:ul {:class "nav flex-column" :id "sort-order-select"}
         (for [[key {:keys [name ico]}] +sort-orders+]
           [:li {:class "nav-item"}
            [:a {:class (str "nav-link" (when (= key active-sort) " active"))
                 :href (make-site-href [(:uri x)] {:sort-order key} x)}
             (icon ico) "\u00a0" name]])])

      ;; item tags
      [:h6 {:class "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"}
       [:span (icon (:item-tags +group-icons+)) " Item Tags"]]
      [:ul {:class "nav flex-column"}
       (group-list x "/reader/group/item-tags"
                   (->> x :item-tags (remove +tags-skip-group-list+) sort)
                   (when (= active-group :item-tags) active-key)
                   icons)]

      ;; source tags
      [:h6 {:class "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"}
       [:span (icon (:source-tag +group-icons+)) " Source Tags"]]
      [:ul {:class "nav flex-column"}
       (group-list x "/reader/group/source-tag"
                   (->> (:sources x) vals (map :tags) (apply set/union) sort)
                   (when (= active-group :source-tag) active-key)
                   icons)]

      ;; source types
      [:h6 {:class "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"}
       [:span (icon (:type +group-icons+)) " Type"]]
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
        nitems (or (get item-tags (:group-item x)) (get item-tags fltr) 0)
        grey-out? (and (keyword? fltr) (not= fltr :all) (zero? nitems))
        pill [:span {:class "badge bg-light text-dark float-right"}
              (when (pos? nitems) nitems)]]

    [:li {:class "nav-item"}
     [:a {:class (str
                  (if grey-out? "nav-link nav-link-secondary" "nav-link")
                  (when (= key active-key) " active "))
          :title (format "Filter by source %s" (name (or key :unknown)))
          :href (make-site-href [prefix (name (or key :unknown)) "items"] x)}
      (cond
        (= (:type source) :item-type/bookmark)
        (let [bookmark-name (or (:name source) (:title source) (:key source))
              nice-url (second (re-find #"(?:\[Bookmark: ([\w\.]+)\]|(.+))" bookmark-name))]
          [:span pill
           [:span {:class "sidebar-heading-2"}
            (icon "fas fa-bookmark") "\u00a0" nice-url]])

        :else
        [:span
         pill
         [:span {:class "sidebar-heading-2"} key]
         [:br]
         [:span {:class "font-weight-italic"} (human/truncate-ellipsis title 30)]])]]))

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

(defn tags-button-modal [item-id tags]
  [:div {:class "modal" :id (str "add-custom-tag-" item-id) :tabindex "-1"}
   [:div {:class "modal-dialog"}
    [:div {:class "modal-content"}
     [:div {:class "modal-header"}
      [:h5 "Edit Tags"]
      [:button {:type "button" :class "close"
                :data-bs-dismiss "modal"}
       [:span "×"]]]
     [:div {:class "modal-body"}
      [:ul {:class "list-group list-group-flush item-tag-list"}
       (for [tag tags
             :when (not (contains? item-state/workflow-tags (keyword tag)))]
         [:li {:class "list-group-item item-tag-row"
               :data-id item-id
               :data-tag tag}
          (tag-button item-id {:tag (keyword tag)
                               :icon-set "fas fa-check-circle icon-is-set"
                               :icon-unset "far fa-circle"
                               :is-set? true})
          "\u00a0"
          tag])]
      [:form {:class "add-custom-tag" :data-id item-id}
       [:div {:class "input-group mb-3"}
        [:div {:class "form-floating"}
         [:input {:class "form-control" :id (str "add-tag-" item-id)}]
         [:label {:for (str "add-tag-" item-id)} "Custom Tag"]]
        [:button {:class "btn btn-primary" :data-bs-modal (str "#add-custom-tag-" item-id) :type "submit"} "Add"]]]]]]])

(defn tags-button-group [item-id tags]
  (let [tags (remove #(contains? item-state/workflow-tags (keyword %)) tags)]
    [:div {:class "btn-group btn-group-sm"}
     [:a {:class "btn"
          :data-bs-toggle "modal"
          :data-bs-target (str "#add-custom-tag-" item-id)}
      "\u00a0" (icon "fas fa-tag")
      [:span {:class "item-tags-summary" :data-id item-id}
       (string/join ", " tags)]]]))

(defn video-content? [item]
  (let [{:keys [_entry url]} item
        youtube-url (parse-youtube-url url)]
    (some? youtube-url)))

(defn render-special-item-content
  "Renders item content that is somehow unique to a source and benefits from special rendering
  (e.g youtube videos, twitter images)"
  [item options]
  (let [{:keys [entry url]} item
        youtube-url (parse-youtube-url url)]
    (html
     (when-let [vid youtube-url]
       (when (some? vid)
         (when-let [thumb (:thumbnail entry)]
           [:div {:class "ratio ratio-16x9"}
            [:div {:id (str "youtube-container-" (last vid))
                   :class "youtube-preview-container"}
             [:img {:class "lazy-youtube"
                    :data-vid (last vid)
                    :data-target (str "youtube-container-" (last vid))
                    :alt "Play video on YouTube"
                    :src thumb}]]])))

     (when-let [twit-pic (first (get-in entry [:entities :photos]))]
       [:div {:class "item-preview"} [:img {:src twit-pic}]])

     (when-let [image-url (and (not youtube-url) (or (:thumbnail entry) (:lead-image-url entry)))]
       (when (not (or (string/blank? image-url)
                      (= image-url "self")
                      (= image-url "default")))
         [:div {:class (str "item-preview-small float-end"
                            (when (every? options [:main-list-use-description
                                                   :short-word-cloud])
                              " float-md-end"))
                :style "width: 200px; height: auto;"}
          [:img {:src image-url}]])))))

(defn get-html-content
  "Show Item Helper: Get best content to display in full"
  [doc sel-descr sel-content-type]
  (if (and (nil? sel-descr) (nil? sel-content-type))
    (when-let [{:keys [mime data]} (item/best-content doc)]
      (if (= mime "text/plain")
        [:p {:style "white-space: pre-line"} data]
        (h/raw data)))
    (if (= sel-content-type "text/plain")
      [:p {:style "white-space: pre-line"} (get-in doc [:data sel-descr sel-content-type])]
      (some-> (get-in doc [:data sel-descr sel-content-type]) h/raw))))

(defn related-button [id]
  (icon-button {:title "Find related items"
                :aria-label "Find related items"
                :href (str "/reader/item/by-id/" id "/related")}
               "fas fa-project-diagram"))

(defn external-link-button
  ([url]
   (external-link-button url nil))
  ([url target]
   (icon-button (cond-> {:title "Open item URL"
                         :aria-label "Open item URL"
                         :href url}
                  target (assoc :target target))
                "fas fa-external-link-alt")))

(defn dump-button [href]
  (icon-button {:title "Show internal data representation of this item"
                :aria-label "Show internal data representation of this item"
                :href href}
               "fas fa-code"))

(defn focus-button [href]
  (icon-button {:title "Show item HTML focus mode"
                :aria-label "Show item HTML focus mode"
                :href href}
               "fas fa-expand"))

(defn download-button [href]
  (icon-button {:title "Open raw HTML content"
                :aria-label "Open raw HTML content"
                :href href}
               "fas fa-remove-format"))

(defn annotation-button []
  (icon-button {:id "btn-annotation-mode"
                :title "Annotation Mode (a)"
                :aria-label "Annotation Mode (a)"}
               "fas fa-pen-fancy"))

(defn reading-checkpoint-tools [{:keys [id checkpoint-selector checkpoint-progress]}]
  (let [active? (some? checkpoint-progress)]
    [:div {:class "reading-checkpoint-tools"
           :data-id id}
     (when (and active? checkpoint-selector)
       [:button {:class "reading-checkpoint-control reading-checkpoint-resume btn-resume-checkpoint"
                 :type "button"
                 :title (format "Resume at %.0f%%" (* 100.0 (double checkpoint-progress)))
                 :aria-label "Scroll to the saved reading position"
                 :data-selector (cheshire/generate-string checkpoint-selector)
                 :data-progress checkpoint-progress}
        (icon "fas fa-map-marker-alt")])
     [:button {:class (if active?
                        "reading-checkpoint-control reading-checkpoint-save is-active btn-save-checkpoint"
                        "reading-checkpoint-control reading-checkpoint-save btn-save-checkpoint")
               :type "button"
               :title (if active? "Update saved place" "Save this reading position")
               :aria-label (if active? "Update saved place" "Save this reading position")
               :aria-pressed (str active?)}
      (icon "fas fa-map-pin")]
     (when active?
       [:button {:class "reading-checkpoint-control reading-checkpoint-clear btn-clear-checkpoint"
                 :type "button"
                 :title "Clear saved place"
                 :aria-label "Clear saved place"}
        (icon "fas fa-times")])]))

(defn reading-viewport-overlay [item]
  [:div {:class "reading-viewport-overlay"}
   [:div {:class "reading-step-rail" :aria-hidden "true"}
    [:span {:class "reading-step-indicator reading-step-next"}]
    [:span {:class "reading-step-indicator reading-step-landing"}]]
   [:aside {:class "reading-checkpoint-rail" :aria-label "Saved reading position"}
    (reading-checkpoint-tools item)]])

(defn main-show-item
  "Show Item View"
  [x]
  (let [item (first (:items x))
        selected-data (:data x)
        selected-content-type (:content-type x)
        reading-estimate (item/reading-time-estimate item)
        {:keys [id url data ts tags entry nwords]} item
        lang (if (#{"de" "en"} (:language entry))
               (:language entry)
               "en")]
    [:div {:class "item-content" :id "item-content"}
     (tags-button-modal id tags)
     [:div {:class "d-none"
            :id "item-meta"
            :data-id id}]
     [:div {:class "btn-toolbar d-flex" :role "toolbar"}
      [:div {:class "btn-group btn-group-sm p-2 flex-grow-1" :role "group"}
       [:a {:class "btn" :title "Reading Time Estimate"}
        (icon "far fa-file-word")
        "\u00a0"
        (:estimate reading-estimate) "m"
        "\u00a0\u00a0"
        nwords "\u00a0words"]
       (tags-button-group id tags)
       (done-button item)]
      (when (some? ts)
        [:div {:class "btn-group btn-group-sm  p-2 flex-fill" :role "group"}
         [:a {:class "btn"}
          "\u00a0\u00a0"
          (icon "far fa-calendar") "\u00a0" (human/datetime-ago ts)]])
      [:div {:class "btn-group btn-group-sm  p-2 flex-fill" :role "group"}
       (external-link-button url "_blank")
       (related-button id)
       (dump-button (make-site-href [id "dump"] x))
       (focus-button (make-site-href [id "focus"] {:data "content"
                                                   :content-type "text/html"} x))
       (download-button (make-site-href [id "download"] {:data "content"
                                                         :content-type "text/html"} x))
       (annotation-button)
       (let [has-zotero (some? (credentials :zotero))
             url-handler-cfg (rc/rc [:reader :export :url-handler])
             has-url-handler (some? url-handler-cfg)]
         (when (or has-zotero has-url-handler)
           [:div {:class "dropdown d-inline-block"}
            [:a {:class "btn dropdown-toggle btn-sm"
                 :href "#"
                 :role "button"
                 :id "export-dropdown"
                 :data-bs-toggle "dropdown"
                 :title "Export annotations"}
             (icon "fas fa-file-export")]
            [:div {:class "dropdown-menu"}
             (when has-zotero
               [:a {:class "dropdown-item" :id "btn-export-zotero" :href "#"}
                (icon "fas fa-book") " Zotero"])
             (when has-url-handler
               [:a {:class "dropdown-item" :id "btn-export-url-handler" :href "#"}
                (icon (or (:icon url-handler-cfg) "fas fa-external-link-alt"))
                " " (or (:name url-handler-cfg) "Open in app")])]]))]

      [:div {:class "btn-group btn-group-sm  p-2 flex-fill" :role "group"}
       [:div {:class "dropdown show "}
        [:a {:class "btn dropdown-toggle btn-sm"
             :title "Select Content Type"
             :href "#"
             :role "button"
             :id "item-data-select"
             :data-bs-toggle "dropdown"}
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
              (str (name descr) " - " content-type)]))]]]]
     [:div {:id "annotation-item-notes"
            :class "container-fluid mb-2"
            :style "display:none;"}
      [:div {:class "card"}
       [:div {:class "card-header py-1"}
        [:small (icon "fas fa-sticky-note") " Notes"]]
       [:div {:class "card-body p-2 notes-list"}]]]
     [:div {:id "item-content-body-container" :class "container-fluid"}
      [:div {:class "reading-surface"}
       [:div {:id "item-content-body" :class "item-content-body hyphenate" :lang lang}
        (cond
          ;; if the user selected something, give it to them
          (and (some? selected-data) (some? selected-content-type))
          (get-html-content item selected-data selected-content-type)

          ;; render video content with special first followed by description
          (video-content? item)
          [:div
           (render-special-item-content item #{})
           [:p {:style "white-space: pre-line"} (get-html-content item :description "text/plain")]]

          :else
          (if-let [content (get-html-content item selected-data selected-content-type)]
            content
            (render-special-item-content item #{})))]]]
     [:div {:id "annotation-bottom-bar"
            :class "fixed-bottom bg-light border-top p-2"
            :style "display:none;"}
      [:div {:class "container-fluid"}
       [:div {:id "annotation-highlight-list" :class "mb-1 small" :style "display:none;"}]
       [:div {:class "d-flex align-items-center gap-2"}
        [:div {:id "annotation-selection-actions" :style "display:none;"}
         [:button {:class "btn btn-warning btn-sm" :id "btn-highlight-selection"}
          (icon "fas fa-highlighter") " Highlight"]]
        [:div {:class "flex-grow-1"}
         [:div {:class "input-group input-group-sm"}
          [:textarea {:class "form-control form-control-sm" :id "annotation-note-input"
                      :rows "2" :placeholder "Add a note..."}]
          [:button {:class "btn btn-outline-secondary" :id "btn-add-item-note"}
           (icon "fas fa-sticky-note")]]]]]]]))

(defn dump-item
  "Dump Item Developer Representation"
  [x]
  (let [item (first (:items x))]
    [:div {:class "item-content"}
     [:div {:class "item-content-nav"}
      [:h3 "Clojure value inspector"]
      (value-inspector/value-inspector
       [[:item "Current item" item]
        [:context "Full data structure" x]])]]))

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
       [:span (icon "fab fa-wikipedia-w") "\u00a0" (string/replace path-last #"_" " ")]

       (and (string/includes? site "github") (= path-len 2))
       [:span (icon "fab fa-github") "\u00a0" (subs path 1)]

       (string/includes? site "twitter")
       (if (= path-len 1)
         [:span (icon "fab fa-twitter") "\u00a0" (first path-seq)]
         [:span (icon "fab fa-twitter") "\u00a0" (first path-seq) "(status)"])

       (string/includes? site "spotify")
       [:span (icon "fab fa-spotify") "\u00a0" path-last]

       (string/includes? site "facebook")
       (cond
         (= path-len 1)
         [:span (icon "fab fa-facebook-f") "\u00a0" (first path-seq)]
         (= (first path-seq) "events")
         [:span (icon "fab fa-facebook-f") "\u00a0" "event"]
         :else
         [:span (icon "fab fa-facebook-f") "\u00a0" (take-last 2 path-seq)])

       (string/includes? site "youtube")
       [:span (icon "fab fa-youtube") "\u00a0" path-last]

       (string/includes? site "soundcloud")
       [:span (icon "fab fa-soundcloud") "\u00a0" path-last]

       (string/includes? site "bandcamp")
       [:span (icon "fab fa-bandcamp") "\u00a0" path-last]

       (string/includes? site "media.ccc.de")
       [:span (icon "fas fa-tv") "\u00a0" path-last]

       (and (string/includes? site "amazon") (neg? path-len))
       (let [dp-entry (.indexOf path-seq "dp")]
         (cond
           (pos? dp-entry)
           [:span (icon "fab fa-amazon") "\u00a0"
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
  (let [{:keys [mode source-key group-item view]} x
        current-item (first (:items x))]
    (cond
      (= mode :dump-item)
      "💩"

      (#{:focus-item :show-item} mode)
      (format "🕮 %s [%s]"
              (:title current-item) (name source-key))

      (= mode :list-items)
      (format "▤ %s/%s"
              (name group-item) (name source-key))

      (= mode :tools)
      (case view
        :saved-overview "Reading Queue"
        :continue-reading "Continue Reading"
        :todays-vibe "Today’s Vibe"
        :gems "Gems"
        :search "Search"
        "Reader Tools"))))

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
        words (take 15 (filter (fn [[word _]] (boring-filter word)) (:words top-words)))
        names (take 5 (filter boring-filter names))
        options (cond-> (set (:options source))
                  (< (+ (count words) (count names) (count urls)) 10)
                  (conj :short-word-cloud))
        min-freq (second (last words))
        max-freq (second (first words))]
    [:div {:id (str "item-" id)
           :data-id id
           :data-unread (str (boolean (some #(= % "unread") tags)))
           :class (str "feed-item "
                       (string/join " "
                                    (map #(str "option-" (name %)) options)))}
     [:h4 {:class "h4"}
      [:a {:href (make-site-href [link-prefix "item/by-id" id]
                                 (cond-> {:mark :read}
                                   (:ranked-pos item) (assoc :ranked-pos (:ranked-pos item)))
                                 x)}
       (if (string/blank? title)
         "(no title)"
         title)]]

     [:ul {:class "list-inline"}
      [:li {:class "list-inline-item"}
       (icon "far fa-calendar")
       "\u00a0"
       [:span {:class "timestamp"} (time/format (time/formatter "YYYY-MM-dd 'KW'ww HH:mm") ts)]
       [:span " - "]
       [:span {:class "timestamp"} (human/datetime-ago ts)]]
      (when (>= nwords 0)
        (let [estimate (item/reading-time-estimate item)
              human-time (:estimate estimate)]
          [:li {:class "list-inline-item"}
           [:a {:class "btn"}
            "\u00a0" (icon "far fa-file-word") "\u00a0" human-time "\u2009" "min"]]))
      (when (contains? options :mark-read-on-view)
        [:li {:class "list-inline-item"}
         (icon "fas fa-glasses")])

      (when (string? source-key)
        [:li {:class "list-inline-item"}
         "\u00a0"
         (icon "fas fa-rss") source-key
         (when (= (:type item) :item-type/link)
           [:span "\u00a0"
            (when-let [comments-url (:comments-url entry)]
              [:a {:href comments-url} "(comments)"])
            " → " (human/host-identifier url)])
         (when (and (string? url) (string? (:url source))
                    (not= (human/host-identifier url)
                          (human/host-identifier (:url source))))
           [:span " → " (human/host-identifier url)])])

      (when-not (string/blank? author)
        [:li {:class "list-inline-item"}
         "\u00a0"
         (icon "far fa-user") author])]

     [:div {:class "clearfix"}
      [:p (render-special-item-content item options)]

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
              text]]))])]

     (when-let [highlight (get-in item [:entry :highlight])]
       (html [:p {:class "highlight"}
              (format "Highlighted by %s: %s" (:type highlight)
                      (string/join ", " (:matches highlight)))]))

     [:div {:class "btn-toolbar justify-content-between" :role "toolbar"}
      [:div {:class "btn-group btn-group-sm mr-2" :role "group"}
       (tags-button-group id tags)
       (tags-button-modal id tags)
       (external-link-button url)
       (related-button id)
       (dump-button (make-site-href [link-prefix "item/by-id" id "dump"] x))
       (focus-button (make-site-href [link-prefix "item/by-id" id "focus"] {:data "content"
                                                                            :content-type "text/html"} x))
       (download-button (make-site-href [link-prefix "item/by-id" id "download"] {:data "content"
                                                                                  :content-type "text/html"} x))]

      [:div {:class "item-action-buttons btn-group btn-group-sm mr-2" :role "group"}
       (state-buttons id tags)
       (item-tag-buttons id tags)
       (done-button item)]]]))

(defn headlines-list-items
  "Main Item List - Headlines Style"
  [x]
  (let [{:keys [group-name group-item source-key sources items]} x]
    [:div {:id "headlines" :class "table-responsive"}
     [:table {:class "table table-borderless"}
      [:tbody
       (for [item items
             :let [link-prefix (format "/reader/group/%s/%s/source/%s"
                                       (name group-name)
                                       (name group-item)
                                       (name source-key))
                   {:keys [id source-key title ts tags url]} item
                   source (get sources (keyword source-key))]]
         [:tr {:data-id id}
          [:th {:class "title"}
           [:a {:href (make-site-href [link-prefix "item/by-id" id]
                                      (cond-> {:mark :read}
                                        (:ranked-pos item) (assoc :ranked-pos (:ranked-pos item)))
                                      x)}
            (if (string/blank? title)
              "(no title)"
              title)]
           "\u00a0"
           [:span {:class "source"}
            source-key
            (when (= (:type item) :item-type/link)
              [:span "\u00a0"
               " → " (human/host-identifier url)])
            (when (and (string? url) (string? (:url source))
                       (not= (human/host-identifier url)
                             (human/host-identifier (:url source))))
              [:span " → " (human/host-identifier url)])]]

          [:td {:class "nwords" :title "Reading time estimate in minutes"}
           (let [estimate (item/reading-time-estimate item)
                 human-time (:estimate estimate)]
             human-time)]

          [:td {:class "ts"}
           [:span {:class "timestamp" :title ts} (human/datetime-ago-short ts)]]

          [:td {:class "toolbox"}
           (done-button item)
           [:span {:class "dropstart position-static"}
            [:a {:type "button" :class "btn btn-link" :data-bs-toggle "dropdown"} (icon "fa fa-ellipsis-v fa-lg")]
            [:ul {:class "dropdown-menu position-absolute"}
             [:li
              (external-link-button url)
              (related-button id)
              (focus-button (make-site-href [link-prefix "item/by-id" id "focus"] {:data "content"
                                                                                   :content-type "text/html"} x))]
             [:li
              (state-buttons id tags)
              (item-tag-buttons id tags)]]]]])]]]))

(defn gallery-list-item
  [x link-prefix item]
  (let [{:keys [id source-key title ts tags entry url]} item
        image (or
               (first (get-in entry [:entities :photos]))
               (:thumbnail entry)
               (:lead-image-url entry))]
    [:div {:class "col" :data-id id}
     [:div {:class "card"}
      [:div {:class "card-header"} source-key]
      [:a {:type "button"
           :data-bs-target (str "#full-img-" id)
           :data-bs-toggle "modal"}
       [:img {:src image :class "card-img-top"}]]
      [:div {:class "modal " :id (str "full-img-" id) :tabindex "-1" :role "dialog"}
       [:div {:class "modal-dialog modal-dialog-centered modal-lg"}
        [:div {:class "modal-content"}
         [:img {:src image :class "card-img-top"
                :data-bs-dismiss "modal"}]]]]
      [:div {:class "card-body"}
       [:p {:class "card-title"}
        [:a {:href (make-site-href [link-prefix "item/by-id" id]
                                   (cond-> {:mark :read}
                                     (:ranked-pos item) (assoc :ranked-pos (:ranked-pos item)))
                                   x)}
         (if (string/blank? title)
           "(no title)"
           title)]]
       [:p {:class "card-text"}
        [:small {:class "text-muted"} (human/datetime-ago ts)]]]
      [:div {:class "card-footer toolbox"}
       (external-link-button url)
       (related-button id)
       (state-buttons id tags)
       (item-tag-buttons id tags)
       (done-button item)]]]))

(defn gallery-list-items
  "Main Item List - Gallery Style"
  [x]
  (let [{:keys [group-name group-item source-key items]} x
        link-prefix (format "/reader/group/%s/%s/source/%s"
                            (name group-name)
                            (name group-item)
                            (name source-key))]
    [:div {:class "row row-cols-1 row-cols-md-2 g-4" :id "gallery"}
     (for [item items]
       (gallery-list-item x link-prefix item))]))

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
        hinted-style (rc/rc [:reader :default-list-view (:group-item x)])]
    (cond
      (and (nil? selected-style) (keyword? hinted-style))
      hinted-style

      (keyword? selected-style)
      selected-style

      :else
      :preview)))

(def +valid-sort-orders+ #{:newest :ranked :oldest})

(defn get-sort-order [x]
  (let [selected (:sort-order x)
        group-item (:group-item x)
        defaults (config/get-sort-order-defaults)]
    (cond
      (+valid-sort-orders+ selected) selected
      (+valid-sort-orders+ (get defaults group-item)) (get defaults group-item)
      :else :newest)))

(defn list-items [x]
  ((get-in +list-styles+ [(get-list-style x) :fn]) x))

(defn main-view
  "Generate Main Items View, depending on selected style"
  [x]
  (let [{:keys [mode]} x]
    [:main {:role "main"
            :class (if (= mode :focus-item) "" "col-xs-12 col-md-6 col-lg-8")}
     [:div {:class "justify-content-between flex-wrap flex-md-no align-items-center pb-2 mb-3"}
      (case (:mode x)
        :show-item (main-show-item x)
        :dump-item (dump-item x)
        :focus-item (main-show-item x)
        :list-items (list-items x)
        "Unknown mode")]]))

(defn- build-items-query-args
  "Build common query args from params and effective sort order."
  [params effective-sort]
  (let [{:keys [range-before mode]} params
        ranking-config (rc/rc [:reader :ranking])
        common-args {:sort-order effective-sort
                     :highlight-boost (get ranking-config :highlight-boost-hours 48.0)
                     :rarity-cap (get ranking-config :rarity-boost-cap-hours 168.0)
                     :simple-filter (:filter params)
                     :with-data? false
                     :limit (if (= mode :get-moar-items)
                              1
                              +max-items+)}]
    (if (= effective-sort :ranked)
      (assoc common-args :offset (or (:page-offset params) 0))
      (assoc common-args :before (when-not (empty? range-before) range-before)))))

(defn- get-items-for-current-view*
  "Fetch current view items from database for the given sort order."
  [sources params effective-sort]
  (let [{:keys [mode group-name item-id group-item source-key]} params
        common-args (build-items-query-args params effective-sort)]

    (cond
      (contains? #{:show-item :download :dump-item :focus-item} mode)
      (let [current-item (persistency/get-item-by-id frontend-db item-id)
            ranked-pos (:ranked-pos params)
            next-items (if (and (= effective-sort :ranked) (some? ranked-pos))
                         (persistency/get-items-recent frontend-db
                                                       (merge common-args {:offset (inc ranked-pos) :limit 1}))
                         (get-items-for-current-view*
                          sources
                          (-> params
                              (assoc :range-before (select-keys current-item [:ts :id]))
                              (assoc :mode :get-moar-items))
                          effective-sort))]
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
                                                          :with-source-ids (map :id selected-sources)})))

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

(defn get-items-for-current-view
  "Fetch current view items. Falls back to :newest sort if ranked query fails
  (e.g. source_stats materialized view doesn't exist yet)."
  [sources params]
  (let [effective-sort (get-sort-order params)]
    (if (= effective-sort :ranked)
      (try
        (get-items-for-current-view* sources params :ranked)
        (catch Exception e
          (log/warn e "Ranked query failed, falling back to :newest sort")
          (get-items-for-current-view* sources params :newest)))
      (get-items-for-current-view* sources params effective-sort))))

(defn get-active-group-sources
  "Return active sources, might hit database"
  [sources params]
  (let [{:keys [group-name group-item]} params]
    (cond
      (and (= group-name :default) (= group-item :all))
      (vals sources)

      (= group-name :item-tags)
      (persistency/get-sources-item-tags-counts frontend-db group-item (:filter params) (config/get-sources))

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
  (let [sources (prometheus/with-duration (metrics/prom-registry :llar-ui/compile-sources)
                  (persistency/get-sources frontend-db (config/get-sources)))

        items (prometheus/with-duration (metrics/prom-registry :llar-ui/items-current-view)
                (get-items-for-current-view sources params))
        item (first items)

        data (get-in item [:data (:data params) (:content-type params)])

        body (prometheus/with-duration (metrics/prom-registry :llar-ui/render-download)
               (cond (string? data) data
                     (instance? (Class/forName "[B") data) (java.io.ByteArrayInputStream. data)
                     :else nil))]

    (if (some? data)
      {:status 200
       :headers {"Content-Type" (str (:content-type params) "; charset=utf-8")}
       :body body}
      {:status 404
       :headers {"Content-Type" "text/plain; charset=utf-8"}
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

         item-tags (future (prometheus/with-duration (metrics/prom-registry :llar-ui/tag-list)
                             (doall (persistency/get-tags frontend-db))))

         sources (prometheus/with-duration (metrics/prom-registry :llar-ui/compile-sources)
                   (doall (persistency/get-sources frontend-db (config/get-sources))))
         items (future (prometheus/with-duration (metrics/prom-registry :llar-ui/items-current-view)
                         (doall (get-items-for-current-view sources params))))
         ;; right sidebar
         active-sources (prometheus/with-duration (metrics/prom-registry :llar-ui/active-sources)
                          (doall
                           (persistency/sources-merge-in-tags-counts
                            frontend-db
                            (get-active-group-sources sources params))))

         selected-sources (get-selected-sources active-sources params)
         effective-sort (get-sort-order params)
         base-offset (or (:page-offset params) 0)
         fetched @items
         annotated-items (if (= effective-sort :ranked)
                           (map-indexed (fn [idx item] (assoc item :ranked-pos (+ base-offset idx))) fetched)
                           fetched)
         params (merge params {:sources sources
                               :active-sources active-sources
                               :selected-sources selected-sources
                               :items annotated-items
                               :item-tags @item-tags
                               :filter orig-fltr
                               :range-recent (-> fetched first (select-keys [:ts :id]))
                               :range-before (-> fetched last (select-keys [:ts :id]))
                               :page-offset (when (= effective-sort :ranked)
                                              (+ base-offset (count fetched)))})]
     params)
   (catch Object _
     (throw+ {:type ::gather-data-error
              :params index-params}))))

(defn- render-reader-shell [params main-content title]
  (let [focus? (= :focus-item (:mode params))
        nav-bar (when-not focus? (nav-bar params))
        group-nav (when-not focus? (group-nav params))
        source-nav (when-not focus? (source-nav params))
        reading-overlay (when (#{:show-item :focus-item} (:mode params))
                          (reading-viewport-overlay (first (:items params))))]
    (html5
     (html-header title (:mode params) (first (:items params)))
     [:body {:class (str "reader-mode-" (name (:mode params)))}
      (concat
       [nav-bar]
       [reading-overlay]
       [[:div {:class "container-fluid"}
         [:div {:class "row"}
          group-nav
          main-content
          source-nav]]]
       (html-footer))])))

(defn reader-index
  "Reader Entrypoint"
  ([]
   (reader-index {}))
  ([index-params]
   (log/debug "reader: " index-params)
   (let [params (gather-reader-index-data index-params)]
     (try+
      (prometheus/with-duration (metrics/prom-registry :llar-ui/render-html)
        (render-reader-shell params (main-view params) (short-page-headline params)))
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

(def update-futures (atom {}))

(defn update-sources [params]
  (let [sources (prometheus/with-duration (metrics/prom-registry :llar-ui/compile-sources)
                  (doall (persistency/get-sources frontend-db (config/get-sources))))
        active-sources (prometheus/with-duration (metrics/prom-registry :llar-ui/active-sources)
                         (doall
                          (persistency/sources-merge-in-tags-counts
                           frontend-db
                           (get-active-group-sources sources params))))
        active-source-keys (vec (sort (map :key active-sources)))
        fut (future (update/update-some! active-source-keys))]
    (swap! update-futures assoc active-source-keys fut)
    (log/infof "reader updating sources: %s (%s)" active-source-keys fut)
    {:status 200
     :body {:source-keys active-source-keys
            :future (str fut)}}))

(defn update-sources-status [params]
  (let [sources (prometheus/with-duration (metrics/prom-registry :llar-ui/compile-sources)
                  (doall (persistency/get-sources frontend-db (config/get-sources))))
        active-sources (prometheus/with-duration (metrics/prom-registry :llar-ui/active-sources)
                         (doall
                          (persistency/sources-merge-in-tags-counts
                           frontend-db
                           (get-active-group-sources sources params))))
        active-source-keys (vec (sort (map :key active-sources)))
        fut (get @update-futures active-source-keys)]
    (when fut
      {:status 200
       :body {:source-keys active-source-keys
              :future (str fut)
              :done (future-done? fut)
              :result (when (future-done? fut) @fut)}})))

(defmulti tools-view-handler :view)

(defn- item-has-tag? [item tag]
  (contains? (set (:tags item)) (name tag)))

(defn- queue-item-reasons [item]
  (item-state/queue-reasons item))

(defn- queue-item? [item]
  (item-state/queued? item))

(defn- normalize-queue-filter [queue-filter]
  (case queue-filter
    :in-progress :continue-reading
    queue-filter))

(defn- queue-item-matches-filter? [queue-filter item]
  (case (normalize-queue-filter queue-filter)
    :saved (item-has-tag? item :saved)
    :continue-reading (some? (item-state/checkpoint item))
    :unread (item-has-tag? item :unread)
    (queue-item? item)))

(def ^:private +queue-time-filter-keys+
  #{:under-5 :5-15 :15-30 :30-60 :60-plus})

(defn- normalize-queue-time-filter [queue-time-filter]
  (when (+queue-time-filter-keys+ queue-time-filter)
    queue-time-filter))

(defn- queue-item-reading-minutes [item]
  (when (and (integer? (:nwords item))
             (not (neg? (:nwords item))))
    (:estimate (item/reading-time-estimate item))))

(defn- queue-time-filter-for-minutes [minutes]
  (cond
    (nil? minutes) nil
    (< minutes 5) :under-5
    (< minutes 15) :5-15
    (< minutes 30) :15-30
    (< minutes 60) :30-60
    :else :60-plus))

(defn- queue-item-matches-time-filter? [queue-time-filter item]
  (if-let [queue-time-filter (normalize-queue-time-filter queue-time-filter)]
    (= queue-time-filter
       (queue-time-filter-for-minutes (queue-item-reading-minutes item)))
    true))

(defn- queue-item-matches-filters? [queue-filter queue-time-filter item]
  (and (queue-item-matches-filter? queue-filter item)
       (queue-item-matches-time-filter? queue-time-filter item)))

(defn- queue-stats [items]
  (let [items (filter queue-item? items)]
    {:total (count items)
     :saved (count (filter #(item-has-tag? % :saved) items))
     :continue-reading (count (filter #(some? (item-state/checkpoint %)) items))
     :unread (count (filter #(item-has-tag? % :unread) items))}))

(defn- queue-time-stats [items]
  (let [items (filter queue-item? items)
        buckets (frequencies (keep (comp queue-time-filter-for-minutes
                                         queue-item-reading-minutes)
                                   items))]
    {:total (count items)
     :under-5 (get buckets :under-5 0)
     :5-15 (get buckets :5-15 0)
     :15-30 (get buckets :15-30 0)
     :30-60 (get buckets :30-60 0)
     :60-plus (get buckets :60-plus 0)}))

(defn- queue-reason-label [reason]
  (case reason
    :saved ["Saved" "text-bg-warning"]
    :continue-reading ["Continue Reading" "text-bg-info"]
    [(name reason) "text-bg-secondary"]))

(defn- queue-filter-label [queue-filter]
  (case (normalize-queue-filter queue-filter)
    :saved "Saved"
    :continue-reading "Continue Reading"
    :unread "Unread"
    "All"))

(defn- queue-time-filter-label [queue-time-filter]
  (case (normalize-queue-time-filter queue-time-filter)
    :under-5 "Under 5 minutes"
    :5-15 "5-15 minutes"
    :15-30 "15-30 minutes"
    :30-60 "30-60 minutes"
    :60-plus "60+ minutes"
    "All Times"))

(defn- queue-item-href [x item]
  (let [{:keys [id source-key]} item
        source-key (or source-key "all")
        group (cond
                (item-has-tag? item :saved) [:item-tags :saved]
                :else [:default :none])]
    (make-site-href [(format "/reader/group/%s/%s/source/%s/item/by-id"
                             (name (first group))
                             (name (second group))
                             source-key)
                     id]
                    {:mark :read}
                    x)))

(defn- queue-filter-nav [x active-filter stats]
  (let [filters [[nil "All" (:total stats)]
                 [:saved "Saved" (:saved stats)]
                 [:continue-reading "Continue Reading" (:continue-reading stats)]
                 [:unread "Unread" (:unread stats)]]]
    [:div {:class "btn-group btn-group-sm mb-2 me-2" :role "group"}
     (for [[key label n] filters]
       [:a {:class (str "btn btn-outline-secondary"
                        (when (= key active-filter) " active"))
            :href (make-site-href ["/reader/tools/saved-overview"]
                                  {:queue-filter key
                                   :queue-time-filter (normalize-queue-time-filter
                                                       (some-> (get-in x [:request-params :queue-time-filter])
                                                               keyword))}
                                  x)}
        label
        [:span {:class "badge text-bg-light ms-1"} n]])]))

(defn- queue-time-filter-nav [x active-time-filter stats]
  (let [filters [[nil "All Times" (:total stats)]
                 [:under-5 "< 5 min" (:under-5 stats)]
                 [:5-15 "5-15 min" (:5-15 stats)]
                 [:15-30 "15-30 min" (:15-30 stats)]
                 [:30-60 "30-60 min" (:30-60 stats)]
                 [:60-plus "60+ min" (:60-plus stats)]]]
    [:div {:class "btn-group btn-group-sm mb-3" :role "group"}
     (for [[key label n] filters]
       [:a {:class (str "btn btn-outline-secondary"
                        (when (= key active-time-filter) " active"))
            :href (make-site-href ["/reader/tools/saved-overview"]
                                  {:queue-filter (some-> (get-in x [:request-params :queue-filter])
                                                         keyword)
                                   :queue-time-filter key}
                                  x)}
        label
        [:span {:class "badge text-bg-light ms-1"} n]])]))

(defn- render-reading-queue-item
  [x {:keys [id title source-key author ts tags nwords entry url] :as item}]
  [:div {:id (str "item-" id)
         :class "feed-item"
         :data-id id}
   [:h4 {:class "h4"}
    [:a {:href (queue-item-href x item)}
     (if (string/blank? title) "(no title)" title)]]
   [:ul {:class "list-inline"}
    (for [reason (queue-item-reasons item)
          :let [[label badge-class] (queue-reason-label reason)]]
      [:li {:class "list-inline-item"}
       [:span {:class (str "badge " badge-class)} label]])
    (when (item-has-tag? item :unread)
      [:li {:class "list-inline-item"}
       [:span {:class "badge text-bg-light"} "Unread"]])
    (when-let [checkpoint (item-state/checkpoint item)]
      [:li {:class "list-inline-item"}
       [:span {:class "badge text-bg-info"}
        (format "Saved place %.0f%%" (* 100.0 (:progress checkpoint)))]])
    [:li {:class "list-inline-item"}
     (icon "far fa-calendar") "\u00a0"
     [:span {:class "timestamp"}
      (time/format (time/formatter "YYYY-MM-dd 'KW'ww HH:mm") ts)]
     [:span " - "]
     [:span {:class "timestamp"} (human/datetime-ago ts)]]
    (when (>= nwords 0)
      [:li {:class "list-inline-item"}
       [:a {:class "btn"}
        "\u00a0" (icon "far fa-file-word") "\u00a0"
        (:estimate (item/reading-time-estimate item)) "\u2009min"]])
    (when (string? source-key)
      [:li {:class "list-inline-item"}
       "\u00a0" (icon "fas fa-rss") source-key
       (when (= (:type item) :item-type/link)
         [:span "\u00a0"
          (when-let [comments-url (:comments-url entry)]
            [:a {:href comments-url} "(comments)"])
          " → " (human/host-identifier url)])])
    (when-not (string/blank? author)
      [:li {:class "list-inline-item"}
       "\u00a0" (icon "far fa-user") author])]
   [:div {:class "clearfix"}
    (when-let [image-url (or (:thumbnail entry) (:lead-image-url entry))]
      (when-not (or (string/blank? image-url)
                    (= image-url "self")
                    (= image-url "default"))
        [:figure {:class "figure float-start"}
         [:img {:src image-url
                :class "figure-img"
                :style "max-width: 200px; height: auto;"}]]))
    [:div {:class "description"}
     [:p (human/truncate-ellipsis
          (get-in item [:data :description "text/plain"])
          1200)]]]
   [:div {:class "btn-toolbar" :role "toolbar"}
    [:div {:class "btn-group btn-group-sm mr-2" :role "group"}
     (focus-button
      (make-site-href ["/reader/group/default/none/source/all/item/by-id" id "focus"]
                      {:data "content" :content-type "text/html"}
                      x))]
    [:div {:class "item-action-buttons btn-group btn-group-sm mr-2" :role "group"}
     (state-buttons id tags)
     (item-tag-buttons id tags)
     (done-button item)]]])

(defn- queue-pagination [x offset page-size has-more?]
  (when (or (pos? offset) has-more?)
    (let [params {:queue-filter (some-> (get-in x [:request-params :queue-filter]) keyword)
                  :queue-time-filter (some-> (get-in x [:request-params :queue-time-filter]) keyword)}]
      [:nav {:class "d-flex gap-2 my-3" :aria-label "Reading queue pages"}
       (when (pos? offset)
         [:a {:class "btn btn-outline-secondary btn-sm"
              :href (make-site-href ["/reader/tools/saved-overview"]
                                    (assoc params :queue-offset (max 0 (- offset page-size)))
                                    x)}
          "Previous"])
       (when has-more?
         [:a {:class "btn btn-outline-secondary btn-sm"
              :href (make-site-href ["/reader/tools/saved-overview"]
                                    (assoc params :queue-offset (+ offset page-size))
                                    x)}
          "Next"])])))

(defn- render-reading-queue
  [x {:keys [items clusters last-update continue-only? offset page-size has-more?]}]
  (let [items (if clusters (vec (mapcat second clusters)) items)
        queue-filter (if continue-only?
                       :continue-reading
                       (normalize-queue-filter
                        (some-> (get-in x [:request-params :queue-filter]) keyword)))
        queue-time-filter (normalize-queue-time-filter
                           (some-> (get-in x [:request-params :queue-time-filter]) keyword))
        stats (queue-stats items)
        time-stats (queue-time-stats (filter #(queue-item-matches-filter? queue-filter %)
                                             items))
        filtered-items (filter #(queue-item-matches-filters?
                                 queue-filter queue-time-filter %)
                               items)]
    [:div
     [:h2 (if continue-only? "Continue Reading" "Reading Queue")]
     [:p {:class "text-secondary"}
      (if continue-only?
        (format "%s active item%s, most recently updated first."
                (:total stats) (if (= 1 (:total stats)) "" "s"))
        (format "Showing %s queue item%s in %s cluster%s. Last clustered: %s."
                (:total stats)
                (if (= 1 (:total stats)) "" "s")
                (count clusters)
                (if (= 1 (count clusters)) "" "s")
                (if last-update
                  (time/format (time/formatter "YYYY-MM-dd HH:mm") last-update)
                  "not yet")))]
     (when-not continue-only?
       [:p {:class "text-secondary"}
        (format "Saved: %s · Continue reading: %s · Unread: %s"
                (:saved stats)
                (:continue-reading stats)
                (:unread stats))])
     (when-not continue-only? (queue-filter-nav x queue-filter stats))
     (when-not continue-only? (queue-time-filter-nav x queue-time-filter time-stats))
     (when (zero? (:total stats))
       [:p {:class "text-secondary"}
        (if continue-only?
          "Nothing is in progress. Pin your place in an item to put it here."
          "No saved or partially read items in the queue.")])
     (when (and (pos? (:total stats)) (empty? filtered-items))
       [:p {:class "text-secondary"}
        (format "No %s items for %s on this page."
                (string/lower-case (queue-filter-label queue-filter))
                (string/lower-case (queue-time-filter-label queue-time-filter)))])
     (if continue-only?
       (for [item filtered-items]
         (render-reading-queue-item x item))
       (for [[{:keys [words]} cluster-items] clusters
             :let [cluster-items (filter #(queue-item-matches-filters?
                                           queue-filter queue-time-filter %)
                                         cluster-items)]
             :when (seq cluster-items)]
         [:section
          [:h2
           [:nav {:class "fst-italic" :style "--bs-breadcrumb-divider: '·'"}
            [:ol {:class "breadcrumb"}
             (for [word words]
               [:li {:class "breadcrumb-item"} word])]]]
          (for [item cluster-items]
            (render-reading-queue-item x item))]))
     (when-not continue-only?
       (queue-pagination x offset page-size has-more?))]))

(defn- queue-row->item [{:keys [description-text] :as row}]
  (cond-> (dissoc row :description-text)
    description-text (assoc-in [:data :description "text/plain"] description-text)))

(def ^:private +unclustered-queue+
  {:id :unclustered :words ["Unclustered"]})

(defn- live-reading-queue-state [cluster-state rows]
  (let [{:keys [clusters last-update]}
        (if (= +saved-clusters-not-compiled+ cluster-state)
          {:clusters {} :last-update nil}
          cluster-state)
        cluster-by-id (into {}
                            (for [[cluster items] clusters
                                  item items]
                              [(:id item) cluster]))
        items (mapv queue-row->item rows)]
    {:clusters (group-by #(get cluster-by-id (:id %) +unclustered-queue+) items)
     :last-update last-update}))

(defmethod tools-view-handler
  :saved-overview
  [x]
  (let [page-size 100
        offset (max 0 (or (some-> (get-in x [:request-params :queue-offset]) parse-long) 0))
        rows (persistency/get-reading-queue-items
              frontend-db {:limit (inc page-size) :offset offset})
        cluster-state (live-reading-queue-state @current-clustered-saved-items
                                                (take page-size rows))]
    (render-reading-queue
     x (assoc cluster-state
              :offset offset
              :page-size page-size
              :has-more? (> (count rows) page-size)))))

(defmethod tools-view-handler
  :continue-reading
  [x]
  (let [items (mapv queue-row->item
                    (persistency/get-reading-progress-items frontend-db {:limit 100}))]
    (render-reading-queue x {:items items :continue-only? true})))

(def ^:private +gem-sort-orders+ #{"relevance" "newest" "oldest"})

(declare render-search-headline)

(defn- gem-param [value]
  (when (string? value)
    (let [value (string/trim value)]
      (when-not (string/blank? value) value))))

(defn- normalize-gem-params [request-params]
  (let [query (some-> (gem-param (:query request-params))
                      (subs 0 (min 500 (count (gem-param (:query request-params))))))
        tag (gem-param (:tag request-params))
        source (gem-param (:source request-params))
        requested-sort (gem-param (:sort request-params))
        sort (if (+gem-sort-orders+ requested-sort)
               requested-sort
               (if query "relevance" "newest"))]
    {:query query
     :tag tag
     :source source
     :sort sort
     :offset (max 0 (or (some-> (:offset request-params) str parse-long) 0))
     :batch (max 0 (or (some-> (:batch request-params) str parse-long) 0))
     :browse? (= "true" (str (:browse request-params)))
     :related-to (let [id (some-> (:related-to request-params) str parse-long)]
                   (when (and id (pos? id)) id))}))

(defn- gems-href
  ([] "/reader/tools/gems")
  ([params]
   (let [params (cond-> (dissoc params :browse?)
                  (:browse? params) (assoc :browse "true"))
         params (into {}
                      (remove (fn [[_ value]]
                                (or (nil? value) (false? value) (= "" value))))
                      params)
         params (cond-> params
                  (zero? (long (or (:offset params) 0))) (dissoc :offset)
                  (zero? (long (or (:batch params) 0))) (dissoc :batch))
         query-string (form-encode params)]
     (if (string/blank? query-string)
       (gems-href)
       (str (gems-href) "?" query-string)))))

(defn- gem-topic-tags [item]
  (remove #{"archive" "saved" "unread" "in-progress"} (:tags item)))

(defn- gem-open-href [{:keys [id offer-id]}]
  (str "/reader/group/item-tags/archive/source/all/item/by-id/" id
       (when offer-id (str "?offer=" offer-id))))

(defn- offer-gems [items kind metadata]
  (try
    (let [offered (persistency/record-results-offered!
                   frontend-db
                   (mapv #(assoc % :reasons [(keyword kind)]) items)
                   (events/context :related :related-generated
                     (merge {:feature "gems" :kind kind} metadata)))]
      (mapv #(assoc %1 :offer-id (:id %2)) items offered))
    (catch Throwable throwable
      (log/warn throwable "Could not record Gems offers" {:kind kind})
      items)))

(defn- render-gem-meta [item]
  (let [minutes (when (and (integer? (:nwords item)) (not (neg? (:nwords item))))
                  (:estimate (item/reading-time-estimate item)))]
    [:div {:class "d-flex flex-wrap gap-2 small text-secondary"}
     [:span (icon "fas fa-rss") " " (:source-key item)]
     [:span {:class "timestamp" :title (:ts item)}
      (human/datetime-ago-short (:ts item))]
     (when minutes [:span (icon "far fa-clock") " " minutes " min"])
     [:span (icon "fas fa-shapes") " " (name (:type item))]]))

(defn- gem-tag-links [params item]
  (for [tag (take 3 (gem-topic-tags item))]
    [:a {:class "badge text-bg-light text-decoration-none me-1"
         :href (gems-href (assoc params :tag tag :browse "true"
                                 :offset nil :related-to nil))}
     tag]))

(defn- gem-archive-button [item]
  (for [{:keys [tag] :as button} +state-buttons+
        :when (= :archive tag)]
    (state-button (:id item) (assoc button :is-set? true))))

(defn- render-gem-card [params item]
  (let [image-url (or (get-in item [:entry :thumbnail])
                      (get-in item [:entry :lead-image-url]))
        description (get-in item [:data :description "text/plain"])]
    [:article {:id (str "item-" (:id item))
               :class (str "card gem-card mb-3"
                           (when (:offer-id item) " result-offer"))
               :data-offer-id (:offer-id item)}
     [:div {:class "card-body"}
      [:div {:class "d-flex gap-3"}
       [:div {:class "flex-grow-1 min-width-0"}
        [:h3 {:class "h5 card-title mb-2"}
         [:a {:class "link-dark" :href (gem-open-href item)}
          (if (string/blank? (:title item)) "(no title)" (:title item))]]
        (render-gem-meta item)
        [:div {:class "mt-2"} (gem-tag-links params item)]
        (when-not (string/blank? description)
          [:p {:class "card-text mt-2 mb-2"}
           (human/truncate-ellipsis description 420)])]
       (when-not (string/blank? image-url)
         [:img {:class "gem-card-image rounded"
                :src image-url
                :alt ""}])]
      [:div {:class "d-flex align-items-center gap-2 mt-2"}
       [:a {:class "btn btn-sm btn-primary" :href (gem-open-href item)} "Open"]
       [:a {:class "btn btn-sm btn-outline-secondary"
            :href (gems-href {:related-to (:id item)})}
        "Related gems"]
       [:div {:class "btn-group btn-group-sm ms-auto"}
        (gem-archive-button item)]
       [:span {:class "small text-secondary"}
        (if-let [last-resurfaced (:last-resurfaced item)]
          (str "Last resurfaced " (human/datetime-ago-short last-resurfaced))
          "Not shown in Gems yet")]]]]))

(defn- render-gem-result [params item]
  [:article {:id (str "item-" (:id item))
             :class (str "gem-result py-3 border-bottom"
                         (when (:offer-id item) " result-offer"))
             :data-offer-id (:offer-id item)}
   [:div {:class "d-flex justify-content-between gap-2"}
    [:h3 {:class "h5 mb-1"}
     [:a {:class "link-dark" :href (gem-open-href item)} (:title item)]]
    [:div {:class "btn-group btn-group-sm"} (gem-archive-button item)]]
   (render-gem-meta item)
   [:div {:class "mt-1"} (gem-tag-links params item)]
   (when-not (string/blank? (:headline item))
     [:div {:class "small text-secondary search-headline mt-2"}
      (render-search-headline (:headline item))])
   [:div {:class "mt-2"}
    [:a {:class "small" :href (gems-href {:related-to (:id item)})}
     "Related gems"]]])

(defn- gem-facet-link [params key {:keys [value count]}]
  (let [label (if (= value "__untagged__") "Untagged" value)]
    [:a {:class "list-group-item list-group-item-action d-flex justify-content-between"
         :href (gems-href (assoc params key value :browse "true"
                                 :offset nil :related-to nil))}
     [:span label]
     [:span {:class "badge text-bg-light"} count]]))

(defn- render-gem-facet [params title key rows]
  (let [visible (take 12 rows)
        remaining (drop 12 rows)]
    [:section {:class "col-lg-6 mb-3"}
     [:h3 {:class "h5"} title]
     [:div {:class "list-group list-group-flush"}
      (for [row visible] (gem-facet-link params key row))]
     (when (seq remaining)
       [:details {:class "mt-2"}
        [:summary {:class "small text-secondary"} "Show all"]
        [:div {:class "list-group list-group-flush mt-2"}
         (for [row remaining] (gem-facet-link params key row))]])]))

(defn- render-gem-search [params]
  [:form {:action (gems-href) :method "get" :class "mb-4"}
   (when (:tag params) [:input {:type "hidden" :name "tag" :value (:tag params)}])
   (when (:source params) [:input {:type "hidden" :name "source" :value (:source params)}])
   [:div {:class "input-group input-group-lg"}
    [:input {:class "form-control" :type "search" :name "query"
             :value (or (:query params) "")
             :placeholder "Search titles, authors, URLs, and article text"
             :aria-label "Search Gems"}]
    [:button {:class "btn btn-primary" :type "submit"}
     (icon "fas fa-search") " Search"]]
   [:div {:class "form-text"}
    "Use quotes for a phrase, OR for alternatives, and -word to exclude."]])

(defn- gem-active-filter [params key label]
  (when-let [value (get params key)]
    [:span {:class "badge rounded-pill text-bg-secondary me-2"}
     label ": " (if (= value "__untagged__") "Untagged" value) " "
     [:a {:class "text-white" :aria-label (str "Clear " label)
          :href (gems-href (assoc params key nil :offset nil))} "×"]]))

(defn- render-gem-pagination [params total]
  (let [offset (:offset params)
        page-size 50]
    (when (or (pos? offset) (> total (+ offset page-size)))
      [:nav {:class "d-flex gap-2 mt-3" :aria-label "Gems result pages"}
       (when (pos? offset)
         [:a {:class "btn btn-outline-secondary btn-sm"
              :href (gems-href (assoc params :offset (max 0 (- offset page-size))))}
          "Previous"])
       (when (> total (+ offset page-size))
         [:a {:class "btn btn-outline-secondary btn-sm"
              :href (gems-href (assoc params :offset (+ offset page-size)))}
          "Next"])])))

(defn- gem-search-results [params]
  (if (:query params)
    (let [rows (persistency/search frontend-db (:query params)
                                   {:syntax :web
                                    :with-source-key (:source params)
                                    :with-tag (when-not (= "__untagged__" (:tag params))
                                                (:tag params))
                                    :untagged? (= "__untagged__" (:tag params))
                                    :archived-only? true
                                    :sort (:sort params)
                                    :limit 50
                                    :offset (:offset params)})]
      {:total (or (:total-count (first rows)) 0)
       :items (mapv #(-> %
                         (assoc :source-key (:key %))
                         (dissoc :total-count))
                    rows)})
    (persistency/get-gem-items frontend-db params)))

(defn- render-gem-results [params {:keys [total items]} heading]
  [:section
   [:div {:class "d-flex flex-wrap justify-content-between align-items-center gap-2 mb-2"}
    [:div
     [:h2 {:class "h4 mb-1"} heading]
     [:div {:class "text-secondary"} total " gem" (when (not= total 1) "s")]]
    [:a {:class "btn btn-sm btn-outline-secondary" :href (gems-href)} "Back to Gems"]]
   [:div {:class "mb-2"}
    (gem-active-filter params :tag "Topic")
    (gem-active-filter params :source "Source")]
   (when (:query params)
     [:div {:class "btn-group btn-group-sm mb-2"}
      (for [[sort label] [["relevance" "Relevance"] ["newest" "Newest"] ["oldest" "Oldest"]]]
        [:a {:class (str "btn btn-outline-secondary" (when (= sort (:sort params)) " active"))
             :href (gems-href (assoc params :sort sort :offset nil))}
         label])])
   (if (seq items)
     [:div (for [result items] (render-gem-result params result))]
     [:div {:class "alert alert-light border"} "No gems match this search."])
   (render-gem-pagination params total)])

(defn- render-gems [x]
  (let [params (normalize-gem-params (:request-params x))
        facets (persistency/get-gem-facets frontend-db {})
        result-mode? (or (:query params) (:tag params) (:source params)
                         (:browse? params) (:related-to params))]
    [:div {:class "gems-view px-2"}
     [:div {:class "d-flex justify-content-between align-items-start gap-3 mb-2"}
      [:div
       [:h2 {:class "mb-1"} (icon "fas fa-gem") " Gems"]
       [:p {:class "text-secondary"}
        "Find something you kept, browse the collection, or rediscover a forgotten gem."]]
      [:div {:class "text-end text-secondary small"}
       [:strong (:total facets)] " gems"
       [:br] (:topic-count facets) " topics · " (:source-count facets) " sources"]]
     (render-gem-search params)
     (cond
       (:related-to params)
       (if-let [{:keys [item results]} (persistency/get-related-gems frontend-db
                                                                     (:related-to params))]
         (let [results (offer-gems results "related"
                                   {:seed-item-id (:related-to params)})]
           (render-gem-results params {:total (count results) :items results}
                               (str "Related to “" (:title item) "”")))
         [:div {:class "alert alert-warning"} "That archived gem was not found."])

       result-mode?
       (try
         (render-gem-results params (gem-search-results params)
                             (if (:query params) "Search results" "Browse Gems"))
         (catch Exception exception
           (log/warn exception "Gems search failed" (select-keys params [:query :tag :source]))
           [:div {:class "alert alert-warning"}
            "Search failed. Check the query and try again."]))

       :else
       (let [zone (time/zone-id "UTC")
             day (time/local-date zone)
             total (:total facets)
             batch-count (max 1 (long (Math/ceil (/ (double total) 5.0))))
             batch (mod (:batch params) batch-count)
             items (persistency/get-gem-rediscovery-candidates
                    frontend-db {:day-cutoff (.atStartOfDay day zone)
                                 :day-key (str day)
                                 :candidate-limit 5
                                 :candidate-offset (* batch 5)
                                 :limit 5})
             items (offer-gems items "rediscover" {:day (str day) :batch batch})]
         [:div
          [:section {:class "mb-4"}
           [:div {:class "d-flex justify-content-between align-items-center mb-2"}
            [:div
             [:h3 {:class "h4 mb-0"} "Rediscover"]
             [:div {:class "small text-secondary"} "A stable daily selection, biased toward forgotten items."]]
            (when (> batch-count 1)
              [:a {:class "btn btn-sm btn-outline-secondary"
                   :href (gems-href {:batch (mod (inc batch) batch-count)})}
               "Another set"])]
           (if (seq items)
             (for [gem items] (render-gem-card params gem))
             [:div {:class "alert alert-light border"}
              "Archive an item to make it a Gem."])]
          (when (pos? total)
            [:section
             [:div {:class "d-flex justify-content-between align-items-center"}
              [:h3 {:class "h4"} "Browse"]
              [:a {:href (gems-href {:browse "true"})} "Browse all"]]
             [:div {:class "row"}
              (render-gem-facet params "Topics" :tag (:tags facets))
              (render-gem-facet params "Sources" :source (:sources facets))]])]))]))

(defmethod tools-view-handler
  :gems
  [x]
  (render-gems x))

(defn- render-search-headline [headline]
  (letfn [(render-parts [s]
            (if-let [start (string/index-of s "[[[")]
              (let [before (subs s 0 start)
                    after-start (+ start 3)
                    stop (string/index-of s "]]]" after-start)]
                (if stop
                  (concat
                   (when-not (string/blank? before) [before])
                   [[:mark (subs s after-start stop)]]
                   (render-parts (subs s (+ stop 3))))
                  [s]))
              (when-not (string/blank? s) [s])))]
    (into [:span] (render-parts headline))))

(defn reader-related [item-id]
  (if-let [{:keys [item results query]} (db-search/related-items frontend-db item-id)]
    (let [offered-results (mapv #(assoc % :reasons [:lexical-match]) results)
          offers (persistency/record-results-offered!
                  frontend-db offered-results
                  (events/context :related :related-generated
                    {:seed-item-id item-id
                     :query query
                     :generator "postgres-full-text"
                     :feature-version 1}))
          results (mapv #(assoc %1 :offer-id (:id %2)) results offers)
          params (gather-reader-index-data
                  {:uri (str "/reader/item/by-id/" item-id "/related")
                   :group-name :default
                   :group-item :none
                   :source-key :all
                   :item-id item-id
                   :mode :show-item})
          view [:main {:role "main" :class "col-xs-12 col-md-6 col-lg-8"}
                [:div {:class "justify-content-between flex-wrap flex-md-no align-items-center pb-2 mb-3"}
                 [:h2 "Related to “" (:title item) "”"]
                 [:p {:class "text-secondary"}
                  "Ranked by lexical overlap. Titles count most; names and nouns count next; "
                  "verbs, body text, author, and URLs count less. Relative scores compare with "
                  "the strongest result in this list."]
                 (if (seq results)
                   [:div {:class "list-group list-group-flush"}
                    (for [{:keys [title key rank title-rank relative-score matched-terms
                                  id ts headline offer-id]} results]
                      [:div {:class "list-group-item px-0 py-3"}
                       [:div {:class "d-flex w-100 justify-content-between"}
                        [:h5 {:class "mb-1"}
                         [:a {:class "link-dark result-offer"
                              :data-offer-id offer-id
                              :href (make-site-href
                                     ["/reader/group/default/none/source/all/item/by-id" id]
                                     {:mark :read :offer offer-id}
                                     params)}
                          title]]
                        [:small {:class "timestamp" :title ts} (human/datetime-ago-short ts)]]
                       [:div {:class "d-flex flex-wrap gap-1 align-items-center small mb-1"}
                        [:span {:class "badge bg-primary"
                                :title "Lexical score relative to the strongest result shown"}
                         (format "%.0f%% of top" (* 100.0 (double (or relative-score 0.0))))]
                        [:span {:class "badge bg-light text-dark"
                                :title "PostgreSQL normalized full-text rank"}
                         (format "search %.3f" (double (or rank 0.0)))]
                        (when (pos? (double (or title-rank 0.0)))
                          [:span {:class "badge bg-info text-dark"
                                  :title "Matching words occur in the title, the highest-weight field"}
                           (format "title %.3f" (double title-rank))])
                        [:span {:class "text-secondary ms-1"} key]]
                       (when (seq matched-terms)
                         [:div {:class "small mb-1"}
                          [:span {:class "text-secondary"} "Matched: "]
                          (for [term matched-terms]
                            [:span {:class "badge bg-secondary me-1"} term])])
                       (when-not (string/blank? headline)
                         [:div {:class "small mt-1"} (render-search-headline headline)])])]
                   [:p {:class "text-secondary"}
                    "No sufficiently strong lexical matches yet."])]]]
      (render-reader-shell params view (str "Related — " (:title item))))
    {:status 404 :body "Item not found"}))

(defn reader-record-impressions [offer-ids]
  (let [ids (->> (string/split (or offer-ids "") #",")
                 (keep parse-long)
                 (filter pos?)
                 vec)]
    {:status 204
     :body (do (persistency/record-impressions! frontend-db ids) "")}))

(defn- ordered-cluster-items [{:keys [representative-id items]}]
  (sort-by #(if (= representative-id (:id %)) 0 1) items))

(defn- vibe-item-link [x item offer-id]
  (make-site-href ["/reader/group/default/none/source/all/item/by-id" (:id item)]
                  {:mark :read :offer offer-id}
                  x))

(defn- render-vibe-cluster [x snapshot cluster offers]
  (let [ordered (ordered-cluster-items cluster)
        representative (first ordered)
        offer-by-item (into {} (map (juxt :item-id :id) offers))]
    [:article {:class "card mb-3"
               :data-vibe-cluster-id (:id cluster)
               :data-vibe-source-count (:source-count cluster)
               :data-vibe-match-score (:match-score cluster)
               :data-vibe-terms (string/join "," (:terms cluster))}
     [:div {:class "card-body"}
      [:div {:class "d-flex justify-content-between"}
       [:h4 {:class "card-title"}
        [:a {:class "link-dark result-offer"
             :data-offer-id (offer-by-item (:id representative))
             :href (vibe-item-link x representative (offer-by-item (:id representative)))}
         (:title representative)]
        [:small {:class "text-secondary ms-2"}
         (:source-key representative)]]
       [:span {:class "timestamp text-secondary" :title (:latest-ts cluster)}
        (human/datetime-ago-short (:latest-ts cluster))]]
      [:p {:class "text-secondary"}
       (:source-count cluster) " sources · " (:article-count cluster) " articles · "
       (:unseen-count cluster) " unseen"
       (when-let [match-score (:match-score cluster)]
         (format " · %.0f%% lexical match" (* 100 match-score)))]
      [:p (for [term (:terms cluster)]
            [:span {:class "badge bg-light text-dark me-1"} term])]
      [:p (for [source (distinct (map :source-key (:items cluster)))]
            [:span {:class "badge bg-secondary me-1"} source])]
      [:form {:method "post" :action "/reader/tools/todays-vibe/seen" :class "d-inline"}
       [:input {:type "hidden" :name "run-id" :value (:run-id snapshot)}]
       [:input {:type "hidden" :name "cluster-id" :value (:id cluster)}]
       [:button {:class "btn btn-sm btn-outline-secondary" :type "submit"}
        "Mark story seen"]]
      (when (> (count ordered) 1)
        [:details {:class "mt-3"}
         [:summary "Show all reports"]
         [:div {:class "list-group list-group-flush mt-2"}
          (for [item (rest ordered)
                :let [offer-id (offer-by-item (:id item))]]
            [:a {:class "list-group-item list-group-item-action result-offer"
                 :data-offer-id offer-id
                 :href (vibe-item-link x item offer-id)}
             [:span (:title item)]
             [:small {:class "text-secondary ms-2"} (:source-key item)]])]])]]))

(defn- render-todays-vibe [x include-seen?]
  (let [snapshot @vibe/current-vibe]
    (if (= vibe/not-compiled snapshot)
      [:div
       [:h2 "Today’s Vibe"]
       [:p {:class "text-secondary"}
        "The first clustering run has not completed yet."]]
      (let [clusters (if include-seen?
                       (:clusters snapshot)
                       (filterv #(pos? (:unseen-count %)) (:clusters snapshot)))
            {:keys [multi-source other shown-count total-count]}
            (vibe/select-clusters clusters)
            clusters (into multi-source other)
            offered-items (mapv #(assoc % :reasons [:story-cluster])
                                (mapcat ordered-cluster-items clusters))
            offers (persistency/record-results-offered!
                    frontend-db offered-items
                    (events/context :today-vibe :vibe-generated
                      {:run-id (:run-id snapshot)
                       :generator "weka-cobweb"
                       :feature-version (or (:feature-version snapshot) 1)}))
            offers-by-cluster (loop [remaining offers
                                     clusters clusters
                                     result []]
                                (if-let [cluster (first clusters)]
                                  (let [n (count (:items cluster))]
                                    (recur (drop n remaining) (rest clusters)
                                           (conj result [cluster (take n remaining)])))
                                  result))
            offer-map (into {} (map (fn [[cluster offers]] [(:id cluster) offers])
                                    offers-by-cluster))]
        [:div
         [:h2 "Today’s Vibe"]
         [:p {:class "text-secondary"} "Generated "
          [:span {:class "timestamp" :title (:generated-at snapshot)}
           (human/datetime-ago-short (:generated-at snapshot))]
          " · "
          [:a {:href (make-site-href ["/reader/tools/todays-vibe"]
                                     {:include-seen (when-not include-seen? true)}
                                     x)}
           (if include-seen? "Hide fully seen" "Include fully seen")]]
         [:p {:class "text-secondary small"}
          "Showing " shown-count " of " total-count
          " quality-ranked clusters (maximum "
          (:max-clusters (merge {:max-clusters 12} (rc/rc [:reader :vibe]))) ")."]
         [:h2 "Reported across sources"]
         (if (seq multi-source)
           (for [cluster multi-source]
             (render-vibe-cluster x snapshot cluster (offer-map (:id cluster))))
           [:p {:class "text-secondary"} "No cross-source stories in this window."])
         [:h2 {:class "mt-4"} "Other recent stories"]
         (for [cluster other]
           (render-vibe-cluster x snapshot cluster (offer-map (:id cluster))))]))))

(defmethod tools-view-handler
  :todays-vibe
  [x]
  (render-todays-vibe x (= "true" (get-in x [:request-params :include-seen]))))

(defn reader-todays-vibe [include-seen?]
  {:status 303
   :headers {"Location" (str "/reader/tools/todays-vibe"
                             (when include-seen? "?include-seen=true"))}
   :body ""})

(defn reader-mark-story-seen [run-id cluster-id]
  (if (vibe/apply-to-current-cluster!
       run-id cluster-id
       (fn [cluster]
         (persistency/transition-items-state!
          frontend-db (mapv :id (:items cluster)) :seen)))
    {:status 303 :headers {"Location" "/reader/tools/todays-vibe"} :body ""}
    {:status 409 :body "This Vibe snapshot is stale; reload and try again."}))

(defn- render-opened-reader-item [params context offer-id]
  (let [body (reader-index params)
        item-id (:item-id params)]
    (persistency/record-item-opened!
     frontend-db item-id context offer-id)
    body))

(defmethod tools-view-handler
  :search
  [x]
  (let [query (get-in x [:request-params :query])
        with-source-key (get-in x [:request-params :with-source-key])
        days-ago (get-in x [:request-params :days-ago])
        syntax (db-search/normalize-search-syntax (get-in x [:request-params :syntax]))
        search-result (try
                        {:results (persistency/search frontend-db query {:syntax syntax
                                                                         :with-source-key (when-not (string/blank? with-source-key)
                                                                                            with-source-key)
                                                                         :time-ago-period (when-not (string/blank? days-ago)
                                                                                            (time/days (some-> days-ago
                                                                                                               Integer/parseInt)))})}
                        (catch Exception e
                          {:results []
                           :error (ex-message e)}))
        results (:results search-result)
        error (:error search-result)]

    [:div {:class "px-3"}
     [:h3 "Search"]
     [:form {:action "/reader/tools/search" :method "get"}
      [:div {:class "row mb-3"}
       [:label {:for "query" :class "col-sm-4 col-form-label"}
        "Query"]
       [:div {:class "col-sm-8"}
        [:input {:type "text" :class "form-control"
                 :name "query" :id "query" :placeholder "fat rats, \"fat rat\", rats OR cats, -crab"
                 :value (or query "")}]]]
      [:div {:class "row mb-3"}
       [:label {:for "syntax" :class "col-sm-4 col-form-label"} "Mode"]
       [:div {:class "col-sm-8"}
        [:select {:class "form-select" :name "syntax" :id "syntax"}
         (for [[value label] [[:web "Web"]
                              [:plain "All Words"]
                              [:phrase "Phrase"]
                              [:advanced "PostgreSQL tsquery"]]]
           [:option (cond-> {:value (name value)}
                      (= value syntax) (assoc :selected "selected"))
            label])]]]
      [:fieldset {:class "row mb-3"}
       [:legend {:class "col-sm-4 col-form-label"} "Fetched in the last"]
       [:div {:class "col-sm-8"}
        (for [[name days] [["any" ""]
                           ["7d" "7"]
                           ["14d" "14"]
                           ["90d" "90"]
                           ["180d" "180"]
                           ["1y" "365"]]]
          [:div {:class "form-check"}
           [:input (assoc {:class "form-check-input"
                           :type "radio"
                           :name "days-ago"
                           :id (str "days-ago-" name)
                           :value days}
                          :checked (= days-ago days))]
           [:label {:class "form-check-label" :for (str "days-ago-" name)} name]])]]
      [:div {:class "row mb-3"}
       [:label {:class "col-sm-4 col-form-label"} "Actions"]
       [:button {:type "submit" :class "btn btn-primary col-sm-2"} "Search"]]]

     (when error
       [:div {:class "alert alert-warning" :role "alert"}
        (format "Search failed: %s" error)])

     [:h3 "Results"]
     [:p "Found: " [:td (count results)]]

     [:p {:class "word-cloud"}
      (let [freqs (->> (map :key results)
                       frequencies
                       (sort-by second)
                       reverse)
            min-freq (-> freqs last second)
            max-freq (-> freqs first second)]
        (for [[word freq] freqs
              :let [size (word-cloud-fontsize freq min-freq max-freq)]]
          [:span {:class (str "border source-key " size)}
           [:a {:href (make-site-href ["/reader/tools/search"]
                                      (merge x {:with-source-key word
                                                :query query
                                                :syntax syntax
                                                :days-ago days-ago}))
                :class (str "text-black " size)} (str word " (" freq ")")]]))]
     (when-not (string/blank? with-source-key)
       [:p [:a {:href (make-site-href ["/reader/tools/search"]
                                      (merge x {:with-source-key nil
                                                :query query
                                                :syntax syntax
                                                :days-ago days-ago}))}
            "All sources"]])

     [:table {:class "table table-borderless"}
      [:thead
       [:tr
        [:td "Title"]
        [:td "Source"]
        [:td "Fetched"]]]
      [:tbody
       (for [{:keys [title key rank id ts headline]} results]
         [:tr
          [:td
           [:a {:class "link-dark link-offset-1"
                :href (make-site-href
                       ["/reader/group/default/none/source/all/item/by-id" id]
                       {:mark :read}
                       x)}
            title]
           (when-not (string/blank? headline)
             [:div {:class "text-secondary small search-headline"}
              (render-search-headline headline)])
           [:div {:class "text-secondary small"} (format "Rank %.2f" rank)]]
          [:td [:a {:class "link-dark link-offset-1" :href (make-site-href ["/reader/group/default/all/source" key "items"] x)}
                key]]
          [:td [:span {:class "timestamp" :title ts} (human/datetime-ago-short ts)]]])]]]))

(defn reader-tools-index
  "Reader Entrypoint"
  ([]
   (reader-index {}))
  ([params]
   (log/debug "reader: " params)
   (let [;; override filter for special groups like saved
         orig-fltr (:filter params)
         params (assoc params :filter
                       (if-let [override (get +filter-overrides+
                                              (:group-item params))]
                         override
                         orig-fltr))

         item-tags (future (prometheus/with-duration (metrics/prom-registry :llar-ui/tag-list)
                             (doall (persistency/get-tags frontend-db))))

         sources (prometheus/with-duration (metrics/prom-registry :llar-ui/compile-sources)
                   (doall (persistency/get-sources frontend-db (config/get-sources))))

         params (merge params {:sources sources
                               :group-group :tools
                               :group-key (:view params)
                               :item-tags @item-tags
                               :mode :tools
                               :filter orig-fltr})
         nav-bar (nav-bar params)
         view (tools-view-handler params)
         group-nav (group-nav params)
         title (short-page-headline params)

         html (prometheus/with-duration (metrics/prom-registry :llar-ui/render-html)
                (html5
                 (html-header title (:mode params) (some-> params :items first))
                 [:body
                  (concat
                   [nav-bar]
                   [[:div {:class "container-fluid"}
                     [:div {:class "row"}
                      group-nav
                      [:main {:role "main"
                              :class "col-xs-12 col-md-8 col-lg-9"}
                       [:div {:class "justify-content-between flex-wrap flex-md-no align-items-center pb-2 mb-3"}
                        view]]]]]
                   (html-footer))]))]
     html)))

(defn fetch-preview
  "Preview Mode Entrypoint"
  []
  (html5
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

(defn enqueue-bookmark [url]
  (try
    (let [capture (bookmark-capture/enqueue! store/backend-db url nil "reader")
          outcome (bookmark-capture/enqueue-outcome capture)]
      {:status (case outcome :queued 201 :needs-attention 409 200)
       :body {:capture-id (:id capture)
              :item-id (:item-id capture)
              :state (:status capture)
              :result outcome
              :message (get bookmark-capture/outcome-messages outcome)}})
    (catch clojure.lang.ExceptionInfo exception
      (if (#{:llar.bookmark-capture/invalid-url
             :llar.bookmark-capture/invalid-title}
           (:type (ex-data exception)))
        {:status 400 :body {:error (ex-message exception)}}
        (do
          (log/error exception "reader bookmark enqueue failed")
          {:status 503 :body {:error "Llar could not durably queue this capture"}})))
    (catch Throwable throwable
      (log/error throwable "reader bookmark enqueue failed")
      {:status 503 :body {:error "Llar could not durably queue this capture"}})))

(defn- valid-checkpoint-selector? [selector]
  (and (map? selector)
       (let [position (:position selector)
             quote (:quote selector)]
         (and (map? position)
              (= "TextPositionSelector" (:type position))
              (nat-int? (:start position))
              (nat-int? (:end position))
              (<= (:start position) (:end position))
              (map? quote)
              (= "TextQuoteSelector" (:type quote))
              (string? (:exact quote))
              (not (string/blank? (:exact quote)))
              (= (- (:end position) (:start position))
                 (count (:exact quote)))
              (<= (count (:exact quote)) 256)
              (<= (count (or (:prefix quote) "")) 64)
              (<= (count (or (:suffix quote) "")) 64)))))

(defn reader-item-state
  ([id action]
   (reader-item-state id action nil nil nil))
  ([id action tag selector-str progress-str]
   (try
     (let [command (case action
                     :save-checkpoint
                     (let [selector (cheshire/parse-string selector-str true)
                           progress (parse-double progress-str)]
                       (when (and (valid-checkpoint-selector? selector)
                                  (number? progress)
                                  (<= 0.0 progress 1.0))
                         {:action action :selector selector :progress progress}))

                     (:add-tag :remove-tag)
                     (when (keyword? tag) {:action action :tag tag})

                     (when (contains? item-state/actions action) action))]
       (if-not command
         {:status 400 :body {:error "Invalid item state action"}}
         (if-let [row (persistency/transition-item-state! frontend-db id command)]
           {:status 200 :body (item-state/canonical row)}
           {:status 404 :body {:error "Item not found"}})))
     (catch Exception _
       {:status 400 :body {:error "Invalid item state action"}}))))

(defn reader-get-annotations [item-id]
  {:status 200
   :body {:annotations (persistency/get-annotations frontend-db item-id)}})

(defn reader-create-annotation [item-id selector-str body]
  (try
    (let [selector (when selector-str (cheshire/parse-string selector-str true))]
      {:status 200
       :body {:annotation (persistency/create-annotation! frontend-db item-id selector body)}})
    (catch Exception e
      (log/warn e "create-annotation failed" item-id)
      {:status 400
       :body {:error (str "Failed to create annotation: " (ex-message e))}})))

(defn reader-delete-annotation [annotation-id]
  (if-let [result (persistency/delete-annotation! frontend-db annotation-id)]
    {:status 200
     :body result}
    {:status 404
     :body {:error "Annotation not found"}}))

(defn reader-export-zotero [item-id]
  (try+
   (let [item (persistency/get-item-by-id frontend-db item-id)
         annotations (persistency/get-annotations frontend-db item-id)
         result (zotero/export-item! item annotations)]
     {:status 200
      :body {:success true :zotero-key (:parent-key result)}})
   (catch [:type :llar.export.zotero/zotero-auth-error] _
     {:status 401
      :body {:error "Zotero authentication failed"}})
   (catch [:type :llar.export.zotero/credentials-missing] _
     {:status 500
      :body {:error "Zotero credentials not configured"}})
   (catch Object e
     (log/warn e "Zotero export failed for item" item-id)
     {:status 500
      :body {:error (str "Export failed: " (ex-message e))}})))

(defn reader-export-url-handler [item-id]
  (try
    (let [item (persistency/get-item-by-id frontend-db item-id)
          annotations (persistency/get-annotations frontend-db item-id)
          url (url-handler/build-export-url item annotations)]
      {:status 200
       :body {:url url}})
    (catch Exception e
      (log/warn e "URL handler export failed for item" item-id)
      {:status 500
       :body {:error (str "Export failed: " (ex-message e))}})))

(defn as-keyword
  "Compojure Helper: Parse a string into keyword"
  [s]
  (keyword s))

(defn as-ts
  "Compojure Helper: Parse string into timestamp"
  [s]
  (when-not (nil? s)
    (time/zoned-date-time (time/formatter :iso-date-time) s)))

(def app
  "Compojure Routes"
  (routes
   (context
     "/reader"
     [:as req]

     (POST "/item/by-id/:item-id/state"
       [item-id :<< as-int]
       (let [{:keys [action tag selector progress]} (:params req)]
         (reader-item-state item-id
                            (as-keyword action)
                            (as-keyword tag)
                            selector
                            progress)))

     (POST "/events/impression" [offer-ids]
       (reader-record-impressions offer-ids))

     (GET "/todays-vibe" []
       (reader-todays-vibe (= "true" (get-in req [:params :include-seen]))))
     (POST "/todays-vibe/seen" [run-id cluster-id :<< as-int]
       (reader-mark-story-seen run-id cluster-id))
     (POST "/tools/todays-vibe/seen" [run-id cluster-id :<< as-int]
       (reader-mark-story-seen run-id cluster-id))

     (context "/annotation" []
       (GET "/:item-id" [item-id :<< as-int]
         (reader-get-annotations item-id))
       (POST "/:item-id" [item-id :<< as-int selector body]
         (reader-create-annotation item-id selector body))
       (DELETE "/:id" [id :<< as-int]
         (reader-delete-annotation id)))

     (context "/export" []
       (POST "/:item-id/zotero" [item-id :<< as-int]
         (reader-export-zotero item-id))
       (GET "/:item-id/url-handler" [item-id :<< as-int]
         (reader-export-url-handler item-id)))

     (GET "/item/by-id/:item-id/related" [item-id :<< as-int]
       (reader-related item-id))

     (POST "/bookmark/add"
       [url type :<< as-keyword]
       (if (= type :readability-bookmark)
         (enqueue-bookmark url)
         {:status 400 :body {:error "Unsupported bookmark type"}}))

     (GET "/tools/:view" [view :<< as-keyword]
       (reader-tools-index {:uri (:uri req)
                            :filter (as-keyword (get-in req [:params :filter]))
                            :syntax (as-keyword (get-in req [:params :syntax]))
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

       (POST "/update" []
         (update-sources {:uri (:uri req)
                          :group-name group-name
                          :group-item group-item
                          :source-key source-key}))

       (GET "/update" []
         (update-sources-status {:uri (:uri req)
                                 :group-name group-name
                                 :group-item group-item
                                 :source-key source-key}))

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
                        :sort-order (as-keyword (get-in req [:params :sort-order]))
                        :page-offset (some-> (get-in req [:params :page-offset]) parse-long (max 0))
                        :range-before {:id id
                                       :ts ts}}))

       (GET "/items" []
         (reader-index {:uri (:uri req)
                        :filter (as-keyword (get-in req [:params :filter]))
                        :group-name group-name
                        :group-item group-item
                        :source-key source-key
                        :list-style (as-keyword (get-in req [:params :list-style]))
                        :sort-order (as-keyword (get-in req [:params :sort-order]))
                        :page-offset (some-> (get-in req [:params :page-offset]) parse-long (max 0))
                        :mode :list-items}))

       (context
         "/item/by-id/:item-id"
         [item-id :<< as-int]
         (GET "/"
           [data :<< as-keyword
            content-type]
           (let [auto-read? (and (some? item-id) (= (get-in req [:params :mark]) "read"))
                 offer (some-> (get-in req [:params :offer]) parse-long)
                 event-context (events/context :item-detail
                                 (if auto-read? :open-and-mark-read
                                     :item-rendered)
                                 {:auto-read auto-read?
                                  :group (name group-name)
                                  :source (name source-key)})]
             (when auto-read?
               (reader-item-state item-id :seen))
             (render-opened-reader-item {:uri (:uri req)
                                         :filter (as-keyword (get-in req [:params :filter]))
                                         :group-name group-name
                                         :group-item group-item
                                         :source-key source-key
                                         :item-id item-id
                                         :mode :show-item
                                         :list-style (as-keyword (get-in req [:params :list-style]))
                                         :sort-order (as-keyword (get-in req [:params :sort-order]))
                                         :page-offset (some-> (get-in req [:params :page-offset]) parse-long (max 0))
                                         :ranked-pos (some-> (get-in req [:params :ranked-pos]) parse-long (max 0))
                                         :content-type content-type
                                         :data data}
                                        event-context offer)))

         (GET "/" []
           (let [auto-read? (and (some? item-id) (= (get-in req [:params :mark]) "read"))
                 offer (some-> (get-in req [:params :offer]) parse-long)
                 event-context (events/context :item-detail
                                 (if auto-read? :open-and-mark-read
                                     :item-rendered)
                                 {:auto-read auto-read?
                                  :group (name group-name)
                                  :source (name source-key)})]
             (when auto-read?
               (reader-item-state item-id :seen))
             (render-opened-reader-item {:uri (:uri req)
                                         :filter (as-keyword (get-in req [:params :filter]))
                                         :group-name group-name
                                         :group-item group-item
                                         :source-key source-key
                                         :item-id item-id
                                         :mode :show-item
                                         :list-style (as-keyword (get-in req [:params :list-style]))
                                         :sort-order (as-keyword (get-in req [:params :sort-order]))
                                         :page-offset (some-> (get-in req [:params :page-offset]) parse-long (max 0))
                                         :ranked-pos (some-> (get-in req [:params :ranked-pos]) parse-long (max 0))}
                                        event-context offer)))

         (GET "/download"
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
                                   :content-type content-type}))
         (GET "/focus"
           [data :<< as-keyword
            content-type]
           (reader-index {:uri (:uri req)
                          :filter (as-keyword (get-in req [:params :filter]))
                          :group-name group-name
                          :group-item group-item
                          :source-key source-key
                          :item-id item-id
                          :mode :focus-item
                          :sort-order (as-keyword (get-in req [:params :sort-order]))
                          :page-offset (some-> (get-in req [:params :page-offset]) parse-long (max 0))
                          :ranked-pos (some-> (get-in req [:params :ranked-pos]) parse-long (max 0))
                          :data data
                          :content-type content-type}))
         (GET "/dump" []
           (reader-index {:uri (:uri req)
                          :filter (as-keyword (get-in req [:params :filter]))
                          :group-name group-name
                          :group-item group-item
                          :source-key source-key
                          :item-id item-id
                          :mode :dump-item
                          :sort-order (as-keyword (get-in req [:params :sort-order]))
                          :page-offset (some-> (get-in req [:params :page-offset]) parse-long (max 0))
                          :ranked-pos (some-> (get-in req [:params :ranked-pos]) parse-long (max 0))}))))

     (GET "/" [] (reader-index
                  {:group-name :default
                   :group-item :all
                   :source-key :all
                   :list-style (as-keyword (get-in req [:params :list-style]))
                   :sort-order (as-keyword (get-in req [:params :sort-order]))
                   :page-offset (some-> (get-in req [:params :page-offset]) parse-long (max 0))
                   :mode :list-items})))

   (GET "/preview" []
     {:status 200
      :body (fetch-preview)})

   (GET "/blob/:h" [h]
     (blob-api/response h))

   (route/resources "/static" {:root "status"})
   (route/not-found "404 Not found")))
