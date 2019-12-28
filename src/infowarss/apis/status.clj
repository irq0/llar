(ns infowarss.apis.status
  (:require
   [infowarss.src :as src]
   [infowarss.core :as core]
   [infowarss.fetch :as fetch]
   [infowarss.postproc :as proc]
   [infowarss.update :as update]
   [infowarss.db :as db]
   [infowarss.persistency :as persistency]
   [infowarss.sched :refer [get-sched-info]]
   [infowarss.blobstore :as blobstore]
   [infowarss.repl :refer [+current-fetch-preview+]]
   [infowarss.http :refer [try-blobify-url!]]
   [clj-memory-meter.core :as mm]
   [clojure.java.io :as io]
   [taoensso.timbre :as log]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [clj-time.format :as tf]
   [compojure.core :refer :all]
   [compojure.route :as route]
   [compojure.coercions :refer [as-int]]
   [clojure.string :as string]
   [ring.util.codec :refer [form-encode form-encode* FormEncodeable url-encode]]
   [slingshot.slingshot :refer [throw+ try+]]
   [schema.core :as s]
   [mount.core :refer [defstate]]
   [hiccup.core :refer [html]]
   [hiccup.util :refer [escape-html]]
   [clojure.contrib.humanize :as human]
   [clojure.set :as set]
   [clojure.pprint :as pprint]
   [mount.core :as mount]))

(defmacro with-log-exec-time [& body]
  `(let [start# (java.lang.System/nanoTime)
         result# (do ~@body)
         fin# (java.lang.System/nanoTime)
         elasped# (- fin# start#)
         elasped-sec# (/ elasped# 1000000)]
     (log/debugf "%s: %.2fms" (quote ~@body) (float elasped-sec#))
     result#))

(def +max-items+ 23)
(def +word-cloud-sizes+ ["sz-a" "sz-b" "sz-c" "sz-d" "sz-e" "sz-f"])
(def +boring-words-regex+
  #"^(\d{1,3}|we|\w['`’]\w|are|at|or|be|but|more|said|what|when|who|where|also|their|one|\w{0,3}|as|you|your|mine|if|our|i|will|on|for|they|and|in|to|is|of|was|were|with|a|the|have|it|he|she|https|http|this|that|an|\W{1,2}|der|die|das|dass|uns|den|und)$")

(def +boring-url-path-element-regex+
  #"index\.\w+|filters:focal")

(def +boring-url-regex+
  #"(googleusercontent\.com/[\w-]{20,})|cdn\.vox-cdn\.com|(png|jpe?g|gif)(\?.+)?$")

(def +exposed-simple-filter+
  {nil ["all" "fas fa-asterisk"]
   :unread ["unread" "far fa-square"]
   :new ["new" "fas fa-spinner"]
   :today ["today" "fas fa-calendar-day"]})

(def +tag-buttons+
  [{:tag :interesting
    :icon-set "far fa-hand-spock icon-is-set"
    :icon-unset "far fa-hand-spock"}
   {:tag :boring
    :icon-set "far fa-trash-alt icon-is-set"
    :icon-unset "far fa-trash-alt"}
   {:tag :unread
    :icon-unset "far fa-check-square icon-is-set"
    :icon-set "far fa-square"}
   {:tag :saved
    :icon-set "fas fa-star  icon-is-set"
    :icon-unset "far fa-star"}
   {:tag :archive
    :icon-set "fas fa-archive icon-is-set"
    :icon-unset "fas fa-archive"}
   {:tag :book-recommendation
    :icon-set "fas fa-book  icon-is-set"
    :icon-unset "fas fa-book"}
   {:tag :video-recommendation
    :icon-set "fas fa-film  icon-is-set"
    :icon-unset "fas fa-film"}
   {:tag :bug
    :icon-set "fas fa-bug icon-is-set"
    :icon-unset "fas fa-bug"}])

(defn icon [ico & args]
  [:i (assoc (apply hash-map args) :class ico) "&thinsp;"] )

(extend-protocol FormEncodeable
  clojure.lang.Keyword
  (form-encode* [x encoding]
    (form-encode* (name x) encoding)))

(defn make-site-href
  "Make inner site href"
  ([path x]
   (make-site-href path {} x))
  ([path params x]
   (let [params (into {}
                  (remove (fn [[k v]] (or (nil? k) (nil? v)))
                    (merge (select-keys x [:filter :list-style]) params)))
         query-string (when (some? params) (form-encode params))]
     (if (string/blank? query-string)
       (string/join "/" path)
       (str (string/join "/" path) "?" query-string)))))

(defn html-header []
  [:head
   [:meta {:charset "utf-8"}]
   [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no"}]
   [:title "Infowarss"]
   [:link {:rel "stylesheet" :href "/static/css/bootstrap.min.css"}]
   [:link {:rel "stylesheet" :href "/static/fonts/fira/fira.css"}]
   [:link {:rel "stylesheet" :href "/static/fonts/charter/webfonts/stylesheet.css"}]
   [:link {:rel "stylesheet" :href "/static/css/fontawesome_all.css"}]
   [:link {:rel "stylesheet" :href "/static/css/my.css"}]])

(defn html-footer []
  [[:script {:src "/static/js/jquery.min.js"}]
   [:script {:src "/static/js/jquery.waypoints.min.js"}]
   [:script {:src "/static/js/popper.min.js"}]
   [:script {:src "/static/js/bootstrap.min.js"}]
   [:script {:src "/static/js/annotator.min.js"}]
   [:script {:src "/static/js/infowarss.js"}]])

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

(defn wrap-body [body]
  (html [:html {:lang "en"}
         (html-header)
         [:body
          (concat
            body
            (html-footer))]]))

(defn- get-state [k]
  (if (instance? clojure.lang.Atom (get @update/state k))
    @(get @update/state k) (get @update/state k)))

(defn source-status []
  (html
    [:h2 "Sources"]
    [:div {:class "table-responsive"}
     [:table {:class "table"}
      [:thead {:class "table-dark"}
       [:tr
        [:th "Key"]
        [:th "Status"]
        [:th "Source"]
        [:th "Sched"]
        [:th "Last Successful Fetch"]
        [:th "Last Attempt"]
        [:th "Last Exception"]]]
      [:tbody
       (for [[k src] core/*srcs*]
         (let [state (get-state k)
               status (:status state)
               sched (:schedule (get-sched-info k))]
           [:tr
            {:class
             (cond (= :perm-fail status) "table-danger"
                   (or (= :temp-fail status) (nil? sched)) "table-warning"
                   :default "")}
            [:td {:class "col-xs-1"} k]
            [:td {:class "col-xs-1"} (:status state)]
            [:td {:class "col-xs-3" :style "overflow: hidden; text-overflow: ellipsis; max-width: 30em;"}
             (str (:src src))]
            [:td {:class "col-xs-1"} (or sched [:emph "No sched"])]
            [:td {:class "col-xs-1"} (:last-successful-fetch-ts state)]
            [:td {:class "col-xs-1"} (:last-attempt-ts state)]
            [:td {:class "col-xs-4" :style "overflow: hidden; text-overflow: ellipsis; max-width: 30em;"}
             [:pre [:code
              (with-out-str (pprint/pprint (get-in state [:last-exception :message])))]]]]))]]]))

(defn list-to-table [header data]
  [:table {:class "table"}
   [:thead {:class "table-dark"}
    [:tr
     (for [h header]
       [:th h])]]
   [:tbody
    (for [row data]
      [:tr
       (for [cell row]
         [:td cell])])]])

(defn memory-stats []
  (html
    [:h4 "Memory Stats"]
    [:div {:class "table-responsive"}
     [:table {:class "table"}
      [:thead {:class "table-dark"}
       [:tr
        [:th "Metric"]
        [:th "Size"]]]
      [:tbody
       [:tr
        [:td "Total"]
        [:td (human/filesize (.totalMemory (Runtime/getRuntime)))]]
       [:tr
        [:td "Free"]
        [:td (human/filesize (.freeMemory (Runtime/getRuntime)))]]
       [:tr
        [:td "Used"]
        [:td (human/filesize
               (- (.totalMemory (Runtime/getRuntime))
                 (.freeMemory (Runtime/getRuntime))))]]
       ]]]))


(defn database-stats []
  (html
   [:h4 "Word Count Groups"]
   [:div {:class "table-responsive"}
    [:table {:class "table"}
     [:thead {:class "table-dark"}
      [:tr
       [:th "Group"]
       [:th "# Documents"]]]
     [:tbody
      (for [[start count] (db/get-word-count-groups)]
        (let [txt (cond
                    (nil? start)
                    "Unknown Length"
                    (zero? start)
                    "Long Read"
                    :default
                    (str "< " start " Words"))]
          [:tr [:th txt] [:td count]]))]]]
   [:h4 "Tags"]
   (list-to-table ["Tag" "# Documents"]  (db/get-tag-stats))
   [:h4 "Types"]
   (list-to-table ["Type" "# Documents"] (db/get-type-stats))))

(defn state-stats []
  (let [states (mount/find-all-states)
        running (mount/running-states)]
  (html
    [:h2 "State"]
    [:div {:class "table-responsive"}
     [:table {:class "table"}
      [:thead {:class "table-dark"}
       [:tr
        [:th "State"]
        [:th "Running?"]
        [:th "Current"]]]
      [:tbody
       (for [state states]
         [:tr
          [:td state]
          [:td (some? (some #{state} running))]
          [:td (mount/current-state state)]])]]])))


(defn status-index []
  (wrap-body
    (html [:h1 "Infowarss Status"]
      [:div {:class "container-fluid"}
       [:div {:class "row"}
        (memory-stats)
        (database-stats)
        (state-stats)
        (source-status)]])))

(defn nav-bar [x]
  (let [{:keys [group-name group-item source-key mode
                selected-sources items range-recent range-before]} x
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
                      ;; (human/truncate
                      ;;   42))]

    [:nav {:class "navbar navbar-dark navbar-expand-md sticky-top bg-dark flex-md-nowrap p-0"}
     [:div {:class "navbar-toggler"}
      [:a {:class "navbar-toggler"
                :data-toggle "collapse"
                :data-target "#navbar"}
       (icon "fas fa-cog")]
      (cond
        (= mode :list-items)
        [:span
         [:a {:class "navbar-toggler"
              :href (make-site-href [(:uri x)] (:range-before x) x)}
          (icon "fas fa-sync")]
         [:a {:class "navbar-toggler"
              :href (make-site-href [(:uri x)] x)}
          (icon "fas fa-step-backward")]]

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
       (icon "far fa-hand-spock")
       ]
      ;; previous:
      ;; "(╯°□°）╯︵ ┻━┻"
      [:ol {:class "col-xs-12 form-control-dark breadcrumb w-100 path"}
       (for [item [group-name group-item source-key]]
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

          [:a {:class "btn btn-secondary"
               :id "btn-mark-view-read"
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
       [:div {:class "col-xs-4 text-right d-block d-sm-none"}
        [:a {:class "btn btn-secondary"
             :data-toggle "collapse"
             :href "#groupnav"
             :role "button"}
         (icon "fas fa-ellipsis-v")]
        [:a {:class "btn btn-secondary"
             :data-toggle "collapse"
             :href "#sourcenav"
             :role "button"}
         (icon "fas fa-list")]]]]
     ]))


(defn group-list [x url-prefix group-items active icons]
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
      (if-let [ico (get icons str-ks)]
        [:span (icon ico) str-ks]
        str-ks)]]))

(defn group-nav [x]
  (let [active-group (:group-name x)
        active-key (name (:group-item x))]

    ;; simple filter toggle
    [:nav {:class "collapse col-md-3 col-lg-2 bg-light sidebar sidebar-left"
           :id "groupnav"}
     [:div {:class "sidebar-sticky" :id "left-nav"}

      [:form {:class "nav flex-column form-inline"}
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
                   :data-type "readability-bookmark"
                   }
          "R"]
         [:button {:class "bookmark-submit form-control-sm btn btn-secondary btn-sm"
                   :type "submit"
                   :data-url-source "#add-url-1"
                   :data-success "#add-url-1-status"
                   :data-type "raw-bookmark"
                   }
          "B"]
         [:button {:class "bookmark-submit form-control-sm btn btn-secondary btn-sm"
                   :type "submit"
                   :data-url-source "#add-url-1"
                   :data-success "#add-url-1-status"
                   :data-type "document"
                   }
          "D"]
         ]]]

      [:ul {:class "nav flex-column"}
       (for [[k [name ico]] +exposed-simple-filter+]
         [:li {:class "nav-item"}
          [:a {:class (str "nav-link" (when (= (:filter x) k) " active"))
               :href (make-site-href [(:uri x)] {:filter k} x)}
           (icon ico) "&nbsp;" [:span name]]])]

      [:h6 {:class "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"}
       [:span "List Style"]]
      [:ul {:class "nav flex-column" :id "view-style-select"}
       [:li {:class "nav-item"}
        [:a {:class "nav-link" :href (make-site-href [(:uri x)] {:list-style nil} x)}
         (icon "far fa-newspaper") "&nbsp;" "Default"]]
       [:li {:class "nav-item"}
        [:a {:class "nav-link" :href (make-site-href [(:uri x)] {:list-style :headlines} x)}
        (icon "far fa-list-alt") "&nbsp;" "Headlines"]]
       [:li {:class "nav-item"}
        [:a {:class "nav-link" :href (make-site-href [(:uri x)] {:list-style :gallery} x)}
        (icon "far fa-images") "&nbsp;" "Gallery"]]
       ]

      [:br]
      [:ul {:class "nav flex-column"}
       [:li {:class "nav-item"}
        [:a {:class (str "nav-link" (when
                                        (and (= active-group :default)
                                          (= active-key :all)) " active"))
             :href (make-site-href ["/reader/group/default/all/source/all/items"] x)}
         "any"]]]

      ;; source types
      [:h6 {:class "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"}
       [:span "type"]]
      [:ul {:class "nav flex-column"}
       (group-list x "/reader/group/type"
         (->> x :sources vals (map :type) (into (sorted-set)))
         (when (= active-group :type) active-key)
         nil)]

      ;; item tags
      [:h6 {:class "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"}
       [:span "item tags"]]
      [:ul {:class "nav flex-column"}
       (group-list x "/reader/group/item-tags"
         (->> x :item-tags (map first) sort)
         (when (= active-group :item-tags) active-key)
         (into {} (for [{:keys [tag icon-set]} +tag-buttons+]
                    [(name tag) (string/replace icon-set #"icon-is-set" "")])))]

      ;; source tags
      [:h6 {:class "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"}
       [:span "source tags"]]
      [:ul {:class "nav flex-column"}
       (group-list x "/reader/group/source-tag"
         (->> (:sources x) vals (map :tags) (apply set/union) sort)
         (when (= active-group :source-tag) active-key)
         nil)]

      ]]))

(defn source-list-item [x prefix source active-key]
  (let [{:keys [key id title item-tags]} source
        fltr (:filter x)
        nitems (or (get item-tags fltr) 0)
        show-num? (and (keyword? fltr) (not= fltr :all) (pos? nitems))
        grey-out? (and (keyword? fltr) (not= fltr :all) (zero? nitems))
        {:keys [all] :as item-tags} item-tags]

    [:li {:class (str "nav-item")}
     [:a {:class (str
                  (if grey-out? "nav-link nav-link-secondary" "nav-link" )
                  (when (= key active-key) " active "))
          :href (make-site-href [prefix (name (or key :unknown)) "items"] x)}
      [:span {:class "sidebar-heading-2"} key]

      [:span {:class "badge badge-pill badge-secondary float-right"}
       (when (pos? nitems) nitems)]

      [:br]
      [:span {:class "font-weight-italic"} title]]]))

(defn source-nav [x]
  (let [active-group (:group-name x)
        active-key (:group-item x)
        active-source (:source-key x)]
    [:nav {:class "collapse col-md-3 col-lg-2 bg-light sidebar sidebar-right"
           :id "sourcenav"}
     [:div {:class "sidebar-sticky"}
      [:ul {:class "nav flex-column"}
       (for [src (:active-sources x)]
         (source-list-item
           x
           (format "/reader/group/%s/%s/source"
             (name active-group) (name active-key))
           src
           active-source))]]]))



(defn get-html-content [doc sel-descr sel-content-type]
  (if (and (nil? sel-descr) (nil? sel-content-type))
    (let [description (get-in doc [:data :description])
          contents (get-in doc [:data :content])]
      (or (get contents "text/html")
        (get contents "text/plain")
        (get description "text/html")
        (get description "text/plain")
        ""))
    (get-in doc [:data sel-descr sel-content-type])))

(defn main-show-item [x]
  (let [item (first (:items x))
        selected-data (:data x)
        selected-content-type (:content-type x)
        {:keys [id url title data ts author tags read saved entry
                nwords names verbs urls top-words]} item
        lang (if (#{"de" "en"} (:language entry))
               (:language entry)
               "en")]
    [:div {:class "item-content"}
     [:div {:class "d-none"
            :id "item-meta"
            :data-id id}]
     [:div {:class "item-content-nav"}
      [:div {:class "btn-toolbar " :role "toolbar"}
       [:div {:class "btn-group btn-group-sm" :role "group"}
        [:a {:class "btn"}
         "&nbsp;&nbsp;" (icon "far file-word") nwords]
        [:a {:class "btn"}
         "&nbsp;&nbsp;" (icon "fas fa-tag") (string/join ", " tags)]]
       (when (some? ts)
       [:div {:class "btn-group btn-group-sm" :role "group"}
        [:a {:class "btn"}
         "&nbsp;&nbsp;"
         (icon "far fa-calendar") (human/datetime ts)]])
       [:div {:class "btn-group btn-group-sm" :role "group"}
        [:a {:target "_blank"
             :href url
             :role "button"
             :class "btn"}
         (icon "fas fa-external-link-alt")]
        [:a {:class "btn"
             :href (make-site-href [id "dump"] x)}
         "&nbsp;" (icon "fas fa-code")]]


       ;; [:div {:class "btn-group btn-group-sm mr-2" :role "group"}
       ;; (for [btn +tag-buttons+]
       ;;   (tag-button id (assoc btn :is-set? (some #(= % (name (:tag btn))) tags))))]

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
              (str (name descr) " - " content-type)]))]]]
       ]]
     [:div {:class "item-content-body hyphenate" :lang lang}
      (if (and (= (-> x :active-sources first :type) :item-type/document)
            (nil? selected-data) (nil? selected-content-type))
        (get-html-content item :description "text/html")
        (get-html-content item selected-data selected-content-type))]
     ]))

(defn list-entry-kv
  "List entry key / value pair"
  [k v]
  [:li (icon "far fa-file") [:strong (str k)] "&nbsp;"
   (cond
     (coll? v)
     [:span (format "n=%s [%s]" (count v)
              (string/join ", " v))]

     (= k "text/html")
     [:code [:pre  (org.apache.commons.lang.StringEscapeUtils/escapeHtml v)]]

     (= k "text/plain")
     [:code [:pre (org.apache.commons.lang.StringEscapeUtils/escapeHtml v)]]

     :default
     [:span
      (str v) " (" (type v) ")"])])

(defn map-to-tree
  "Convert nested map to semantic ui tree"
  [node]
  (if (map? node)
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

(defn dump-item [x]
  (let [item (first (:items x))]
    [:div {:class "item-content"}
     [:div {:class "item-content-nav"}
      (map-to-tree item)]]))


(defn awesome-url-text [str-url]
  (try+
   (let [url (io/as-url str-url)
         proto (some-> url .getProtocol)
         site (some-> url .getHost)
         path (or (some-> url .getPath) "")
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

       (and (string/includes? site "facebook"))
       (cond
         (= path-len 1)
         [:span (icon "fab fa-facebook-f") "&nbsp;" (first path-seq)]
         (= (first path-seq) "events")
         [:span (icon "fab fa-facebook-f") "&nbsp;" "event"]
         :default
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
         (str site "⋯" (last path-seq)))

       :default
       url))
   (catch Object _
     (log/warn (:throwable &throw-context) "Encountered broken url: " str-url)
     str-url)))


(defn word-cloud-fontsize [freq min-freq max-freq]
  (let [max-size (dec (count +word-cloud-sizes+))
        scaled-to-one (if (= max-freq min-freq)
                        1
                        (/ (- freq min-freq) (- max-freq min-freq)))
        scaled scaled-to-one
        size (Math/log (inc (* scaled-to-one 150)))]
    (nth +word-cloud-sizes+
         (-> size int (max 0) (min max-size)))))

(defn human-host-identifier [str-url]
  (let [url (io/as-url str-url)
        host (.getHost url)]
    (try+
      (let [domain (com.google.common.net.InternetDomainName/from host)
            site (.topPrivateDomain host)]
        (.name site))
      (catch Object _
        (str host)))))

;; (defn tag-item-widget [item]
;;   [:div {:class "dropdown-menu"}
;;    [:form {:class "px-4 py-3"}
;;     [:div {:class "form-group"}
;;      [:label {:for "tag-dropdown-"
;;               [:select
;; <div class="dropdown-menu">
;;   <form class="px-4 py-3">
;;     <div class="form-group">
;;       <label for="exampleDropdownFormEmail1">Email address</label>
;;       <input type="email" class="form-control" id="exampleDropdownFormEmail1" placeholder="email@example.com">
;;     </div>
;;     <div class="form-group">
;;       <label for="exampleDropdownFormPassword1">Password</label>
;;       <input type="password" class="form-control" id="exampleDropdownFormPassword1" placeholder="Password">
;;     </div>
;;     <div class="form-group">
;;       <div class="form-check">
;;         <input type="checkbox" class="form-check-input" id="dropdownCheck">
;;         <label class="form-check-label" for="dropdownCheck">
;;           Remember me
;;         </label>
;;       </div>
;;     </div>
;;     <button type="submit" class="btn btn-primary">Sign in</button>
;;   </form>
;;   <div class="dropdown-divider"></div>
;;   <a class="dropdown-item" href="#">New around here? Sign up</a>
;;   <a class="dropdown-item" href="#">Forgot password?</a>
;; </div>

(defn main-list-item [x link-prefix item]
  (let [{:keys [sources]} x
        {:keys [id source-key title data ts author tags read saved
                nwords names entry verbs url urls top-words]} item
        source (get sources (keyword source-key))
        options (:options source)
        boring-filter (fn [word]
                        (not (or
                              (> (count word) 20)
                              (re-find #"^(\W{1,2}|[a-z0-9]\.)" word)
                              (re-find +boring-words-regex+ word))))
        words (take 50 (filter (fn [[word _]] (boring-filter word)) (:words top-words)))
        names (take 50 (filter boring-filter names))
        min-freq (second (last words))
        max-freq (second (first words))]
    [:div {:id (str "item-" id)
           :class (str "feed-item "
                       (string/join
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
       [:span {:class "timestamp"} ts]
       [:span " - "]
       [:span {:class "timestamp"} (human/datetime ts)]]
      (when (>= nwords 0)
        [:li {:class "list-inline-item"}
         [:a {:class "btn"}
          "&nbsp;" (icon "far fa-file-word") nwords]])
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
         (when (and (string? url) (string? (:url source)) (not= (human-host-identifier url))
                    (human-host-identifier (:url source)))
           [:span " → " (human-host-identifier url)])


         ])
      (when-not (string/blank? author)
        [:li {:class "list-inline-item"}
         "&nbsp;"
         (icon "far fa-user") author])]

     (when-let [vid (and (string? url) (re-find #"(youtu\.be\/|youtube\.com\/(watch\?(.*&)?v=|(embed|v)\/))([^\?&\"'>]+)" url))]
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
                    :src thumb}]]
            ])))

     (when-let [twit-pic (first (get-in entry [:entities :photos]))]
       [:div {:class "item-preview"} [:img {:src twit-pic}]])

     (when-let [image-url (:lead-image-url entry)]
       [:div {:class "item-preview-small"} [:img {:src image-url}]])

     (when (contains? options :main-list-use-description)
       [:p {:class "description"}
        (get-in item [:data :description "text/plain"])])

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
           text]]))
      ]

     [:div {:class "btn-toolbar justify-content-between" :role "toolbar"}
      [:div {:class "btn-group btn-group-sm mr-2" :role "group"}


       [:a {:class "btn" :data-toggle "modal" :data-target (str "#add-custom-tag-" id)}
        "&nbsp;" (icon "fas fa-tag") (string/join ", " tags)]

       [:div {:class "modal" :id (str "add-custom-tag-" id) :tabindex "-1" :role "dialog"}
        [:div {:class "modal-dialog modal-dialog-centered"}
         [:div {:class "modal-content"}
          [:div {:class "modal-header"}
           [:h5 "Edit Tags"]
           [:button {:type "button" :class "close"
                     :data-dismiss "modal"}
            [:span "&times;"]]
           ]

          [:div {:class "Modal-body"}
           [:ul
            (for [tag tags]
              [:li
               (tag-button id {:tag tag
                               :icon-set "fas fa-check-circle icon-is-set"
                               :icon-unset "far fa-circle"
                               :is-set? true})
               "&nbsp;"
               tag])]

           [:form {:class "add-custom-tag" :data-id id}
            [:div {:class "form-group"}
             [:label {:for (str "add-tag-" id)} "Add Custom Tag"]
             [:input {:class "form-control" :id (str "add-tag-" id)}]]
            [:input {:class "btn btn-primary" :data-modal (str "#add-custom-tag-" id) :type "submit"}]]]]]]

       [:a {:class "btn" :href url}
        "&nbsp;" (icon "fas fa-external-link-alt")]
       [:a {:class "btn"
            :href (make-site-href [link-prefix "item/by-id" id "dump"] x)}
        "&nbsp;" (icon "fas fa-code")]]

      [:div {:class "direct-tag-buttons btn-group btn-group-sm mr-2" :role "group"}
       (for [btn +tag-buttons+]
         (tag-button id (assoc btn :is-set? (some #(= % (name (:tag btn))) tags))))
       ]]
     ]))


(defn main-list-items [x]
  (let [{:keys [group-name group-item source-key selected-sources items]} x]
    [:div
     (for [item items
           :let [url (format "/reader/group/%s/%s/source/%s"
                             (name group-name)
                             (name group-item)
                             (name source-key))]]
       (main-list-item x url item))]))

(defn headlines-list-items [x]
  (let [{:keys [group-name group-item source-key selected-sources sources items]} x]
    [:div {:id "headlines" :class "table-responsive"}
     [:table {:class "table table-borderless"}
      [:thead
       [:tr
        [:td (icon "fas fa-rss")]
        [:td "Title"]
        [:td (icon "far fa-file-word")]
        [:td (icon "far fa-calendar")]
        [:td "Tools"]]]

      [:tbody
       (for [item items
             :let [link-prefix (format "/reader/group/%s/%s/source/%s"
                                       (name group-name)
                                       (name group-item)
                                       (name source-key))
                   {:keys [id source-key title data ts author tags read saved
                           nwords names entry verbs url urls top-words]} item
                   source (get sources (keyword source-key))
                   options (:options source)]]
         [:tr {:data-id id}
          [:td {:class "source"}
           source-key
           (when (= (:type item) :item-type/link)
             [:span "&nbsp;"
              " → " (human-host-identifier url)])
           (when (and (string? url) (string? (:url source)) (not= (human-host-identifier url))
                      (human-host-identifier (:url source)))
             [:span " → " (human-host-identifier url)])]

          [:th {:class "title"}
           [:a {:href (make-site-href [link-prefix "item/by-id" id]
                                      {:mark :read} x)}
            (if (string/blank? title)
              "(no title)"
              title)]]

          [:td {:class "nwords"} nwords]

          [:td {:class "ts"}
           [:ul {:class "list-inline"}
            [:li {:class "list-inline-item"}
             "&nbsp;"
             [:span {:class "timestamp"} (human/datetime ts)]]]]

          [:td {:class "toolbox"}
           (concat
            [[:a {:class "btn" :href url}
              (icon "fas fa-external-link-alt")]]
            (for [btn +tag-buttons+]
              (tag-button id
                          (assoc :is-set? (some #(= % (name (:tag btn))) tags)))))]])]]]))

(defn gallery-list-items [x]
  (let [{:keys [group-name group-item source-key selected-sources sources items]} x]
    [:div {:class "card-columns" :id "gallery"}
     (for [item items
           :let [link-prefix (format "/reader/group/%s/%s/source/%s"
                                     (name group-name)
                                     (name group-item)
                                     (name source-key))
                 {:keys [id source-key title data ts author tags read saved
                         nwords names entry verbs url urls top-words]} item
                 source (get sources (keyword source-key))
                 options (:options source)]]
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
             [:p {:class="card-text"} [:small {:class "text-muted"} source-key]]
             [:p {:class="card-text"} [:small {:class "text-muted"} (human/datetime ts)]]
             [:p {:class "card-text toolbox"}
              (concat
               [[:a {:class "btn" :href url}
                 (icon "fas fa-external-link-alt")]]
               (for [btn +tag-buttons+]
                 (tag-button id
                             (assoc btn :is-set? (some #(= % (name (:tag btn))) tags)))))]]]])])]))



(defn main-view [x]
  [:main {:role "main"
          :class "col-xs-12 col-md-6 col-lg-8"}
   [:div {:class "justify-content-between flex-wrap flex-md-no align-items-center pb-2 mb-3"}
    (case (:mode x)
      :show-item (main-show-item x)
      :dump-item (dump-item x)
      :list-items (case (:list-style x)
                    :headlines (headlines-list-items x)
                    :gallery (gallery-list-items x)
                    (main-list-items x))
      "Unkown mode")]])

;; NEXT
;; set read, saved
;; show only unread
;; show only saved
;; fullscreen item next button
;; add arbitrary tags

;; show last update ts
;; open links in external


;; add link as bookmark

(defn get-items-for-current-view [sources params]
  (let [{:keys [range-before mode group-name item-id group-item source-key]} params
        fltr (:filter params)
        common-args {:before (when-not (empty? range-before) range-before)
                     :simple-filter fltr
                     :with-data? false
                     :with-preview-data? true
                     :limit +max-items+}]

    (cond
      (contains? #{:show-item :download :dump-item} mode)
      (let [current-item (first (db/get-items-by-id [item-id]))
            next-items (get-items-for-current-view
                         sources
                         (-> params
                           (assoc :range-before (select-keys current-item [:ts :id]))
                           (assoc :mode :get-moar-items)))]
        (into [current-item] next-items))

      (and (= group-name :default) (= group-item :all) (= source-key :all))
      (db/get-items-recent common-args)

      (and (= group-name :default) (= group-item :all) (keyword? source-key))
      (db/get-items-recent (merge common-args
                             {:with-source-keys [source-key]}))

      (and (= group-name :item-tags) (keyword? group-item) (= source-key :all))
      (db/get-items-recent (merge common-args
                             {:with-tag group-item}))

      (and (= group-name :item-tags) (keyword? group-item) (keyword? source-key))
      (db/get-items-recent (merge common-args
                             {:with-source-keys [source-key]
                              :with-tag group-item}))

      (and (= group-name :source-tag) (keyword? group-item) (= source-key :all))
      (db/get-items-recent (merge common-args
                             {:with-source-keys (->> sources
                                                  vals
                                                  (filter #(contains? (:tags %) group-item))
                                                  (map :key))}))

      (and (= group-name :source-tag) (keyword? group-item) (keyword? source-key))
      (if (->> sources
            vals
            (filter #(and
                      (contains? (:tags %) group-item)
                      (= (:key %) source-key)))
            not-empty)
        (db/get-items-recent (merge common-args
                               {:with-source-keys [source-key]}))
        [])

      (and (= group-name :type) (keyword? group-item) (= source-key :all))
      (db/get-items-recent (merge common-args
                             {:with-type group-item}))

      (and (= group-name :type) (keyword? group-item) (keyword? source-key))
      (db/get-items-recent (merge common-args
                             {:with-source-keys [source-key]
                              :with-type group-item}))

      :default
      [])))


(defn get-active-group-sources [sources params]
  (let [{:keys [mode group-name item-id group-item source-key]} params]
    (cond
      (and (= group-name :default) (= group-item :all))
      (vals sources)

      (= group-name :item-tags)
      (filter #(contains? (:item-tags %) group-item) (vals sources))

      (= group-name :source-tag)
      (filter #(contains? (:tags %) group-item) (vals sources))

      (= group-name :type)
      (filter #(= (:type %) (keyword "item-type" (name group-item))) (vals sources))

      :default
      [])))

(defn get-selected-sources [group-sources params]
  (let [{:keys [source-key]} params]
    (if (not= source-key :all)
      (filter #(= (:key %) source-key) group-sources)
      group-sources)))

(defn download-item-content [params]
  (let [sources (with-log-exec-time
                  (-> (db/get-sources)
                    (db/sources-merge-in-config)
                    (db/sources-merge-in-item-tags-with-count)
                    (update/sources-merge-in-state)
                    (doall)))
        items (with-log-exec-time (doall (get-items-for-current-view sources params)))
        item (first items)

        data (get-in item [:data (:data params) (:content-type params)])

        body (cond (string? data) data
                   (instance? (Class/forName "[B") data) (java.io.ByteArrayInputStream. data)
                   :default nil)]

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

(defn reader-index
  ([]
   (reader-index {}))
  ([params]
   (log/debug "[INFOWARSS-UI]" params)
   (let [{:keys [mode group-name group-item source-key]} params
         item-tags (future (db/get-tag-stats))

         sources (with-log-exec-time
                   (-> (db/get-sources)
                     (db/sources-merge-in-config)
                     (db/sources-merge-in-item-tags-with-count)
                     (update/sources-merge-in-state)))
         ;; right sidebar
         active-sources (with-log-exec-time (get-active-group-sources sources params))

         ;; main view
         selected-sources (with-log-exec-time (get-selected-sources active-sources params))
         items (with-log-exec-time (doall (get-items-for-current-view sources params)))

         params (merge params {:sources sources
                               :active-sources active-sources
                               :selected-sources selected-sources
                               :item-tags @item-tags
                               :items items
                               :range-recent (-> items first (select-keys [:ts :id]))
                               :range-before (-> items last (select-keys [:ts :id]))
                               })]

     (let [nav-bar (nav-bar params)
           group-nav (html (group-nav params))
           main-view (html (main-view params))
           source-nav (html (source-nav params))

           html (with-log-exec-time
                  (wrap-body
                    (html
                      nav-bar
                      [:div {:class "container-fluid"}
                       [:div {:class "row "}
                        group-nav main-view source-nav]])))]
       html))))


(defn fetch-preview []
  (wrap-body
    (html
      [:body
       [:main {:role "main"
               :class "col-xs-12 col-md-6 col-lg-8"}
        [:div {:class "justify-content-between flex-wrap flex-md-no align-items-center pb-2 mb-3"}
         (for [item @+current-fetch-preview+]
           (dump-item {:items [item]}))]]])))

(defn reader-item-modify [id action tag]
  (log/debug "[INFOWARSS-UI] Item mod:" id action tag)
  (str (case action
         :set (db/item-set-tags id tag)
         :del (db/item-remove-tags id tag))))

(defn as-keyword
  "Parse a string into keyword"
  [s]
  (keyword s))

(defn as-ts
  [s]
  (tc/from-string s))

(defn as-url
  [s]
  (io/as-url s))

(defn add-thing [feed key]
  (log/debug "[INFOWARSS-UI] Add Bookmark: " feed)
  (try+
    (let [state (assoc update/src-state-template :key key)
          items (fetch/fetch feed)
          processed (proc/process feed state items)
          dbks (persistency/store-items! processed :overwrite? true)]
      (str (first dbks)))
    (catch Object e
      (log/warn e "add-url failed. Probably broken url: " feed)
      "fail")))


(defn- startup-read-state []
  (let [res (io/resource "annotations.edn")
        backup (io/file (str "/tmp/infowarss_annotations.edn." (tc/to-string (time/now))))]
    (log/info "Reading state file. Backup in " backup)
    (io/copy (io/file (.getFile res)) backup)
    (try+
      (infowarss.converter/read-edn-string (slurp res))
      (catch java.lang.RuntimeException _
        (log/warn "Failed to read state file. Starting with clean state")
        {}))))


(defstate annotations
  :start (atom (startup-read-state))
  :stop (spit (io/resource "annotations.edn") (prn-str @annotations)))

(def app
  (routes
   (context "/reader" [:as req]

            (POST "/item/by-id/:item-id"
                  [item-id :<< as-int
                   action :<< as-keyword
                   tag :<< as-keyword]
                  (reader-item-modify item-id action tag))

            (POST "/bookmark/add"
                  [url type :<< as-keyword]
                  (case type
                    :readability-bookmark (add-thing
                                           (core/make-readability-bookmark-feed url)
                                           :bookmark)
                    :raw-bookmark (add-thing
                                   (core/make-raw-bookmark-feed url)
                                   :bookmark)
                    :document (add-thing
                               (core/make-doc-feed url)
                               :document)))


            (POST "/document/add"
                  [url]
                  (add-thing
                   (core/make-doc-feed url)
                   :document))

            (context "/group/:group-name/:group-item/source/:source-key"
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


                     (context "/item/by-id/:item-id"
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
                                                  :mode :show-item
                                                  })))

                     (context "/item/by-id/:item-id/download"
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

                     (context "/item/by-id/:item-id/dump"
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
   (GET "/" [] (status-index))

   (GET "/preview" []
        {:status 200
         :body (fetch-preview)})

   (GET "/blob/:h" [h]
        (try+
         (let [blob (with-log-exec-time (blobstore/get-blob h))]
           {:status 200
            :headers {"Content-Type" (:mime-type blob)
                      "Etag" h
                      "Last-Modified" (tf/unparse
                                       (tf/formatter "EEE, dd MMM yyyy HH:mm:ss z")
                                       (:created blob))}
            :body (:data blob)})
         (catch Object e
           (log/warn e "get-blob failed: " h)
           {:status 404})))

   (context "/ans" [:as req]
            (GET "/search" []
                 (let [item-id (as-int (get-in req [:params :item_id]))]
                   (if-let [items (get @annotations item-id)]
                     {:status 200
                      :body {:total (count items)
                             :rows (map-indexed (fn [i item]
                                                  (assoc item :id
                                                         (str item-id "-" i )))
                                                items
                                                )}}
                     {:status 200
                      :body {:total 0}})))

            (GET "/annotations/:id" [id]
                 (let [[item-id anno-entry-i]
                       (map as-int (string/split id #"-"))]
                   (log/info id item-id anno-entry-i)
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
