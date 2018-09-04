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

(def +word-cloud-sizes+ ["sz-a" "sz-b" "sz-c" "sz-d" "sz-e" "sz-f"])
(def +boring-words-regex+
  #"^(\d{1,3}|we|\w['`’]\w|are|at|or|be|but|more|said|what|when|who|where|also|their|one|\w{0,3}|as|you|your|mine|if|our|i|will|on|for|they|and|in|to|is|of|was|were|with|a|the|have|it|he|she|https|http|this|that|an|\W{1,2}|der|die|das|dass|uns|den|und)$")

(def +boring-url-path-element-regex+
  #"index\.\w+|filters:focal")

(def +boring-url-regex+
  #"(googleusercontent\.com/[\w-]{20,})|cdn\.vox-cdn\.com|(png|jpe?g|gif)(\?.+)?$")

(def +exposed-simple-filter+
  {nil ["all" "target"]
   :unread ["unread" "square"]
   :new ["new" "loader"]
   :today ["today" "clock"]})

(def +tag-buttons+
  [{:tag :interesting
    :icon-set "command"
    :icon-unset "command"
    :icon-set-fill "orange"}
   {:tag :boring
    :icon-set "trash-2"
    :icon-unset "trash-2"
    :icon-set-fill "orange"}
   {:tag :unread
    :icon-set "square"
    :icon-unset "check-square"}
   {:tag :saved
    :icon-set "star"
    :icon-set-fill "orange"
    :icon-unset "star"}
   {:tag :archive
    :icon-set "folder"
    :icon-set-fill "orange"
    :icon-unset "folder"}
   {:tag :book-recommendation
    :icon-set "book"
    :icon-set-stroke "orange"
    :icon-unset "book"}
   {:tag :bug
    :icon-set "alert-octagon"
    :icon-unset "alert-octagon"
    :icon-set-fill "orange"}])

(defn icon [ico & args]
  [:i (assoc (apply hash-map args) :data-feather ico) ico])

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
                    (merge (select-keys x [:filter]) params)))
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
   [:link {:rel "stylesheet" :href "/static/fonts/bitter.css"}]
   [:link {:rel "stylesheet" :href "/static/fonts/fira.css"}]
   [:script {:src "/static/js/feather.min.js"}]
   [:link {:rel "stylesheet" :href "/static/css/my.css"}]])

(defn html-footer []
  [[:script {:src "/static/js/jquery.min.js"}]
   [:script {:src "/static/js/popper.min.js"}]
   [:script {:src "/static/js/bootstrap.min.js"}]
   [:script {:src "/static/js/hyphenator_loader.js"}]
   [:script
    (string/join " "
      ["Hyphenator_Loader.init("
       "{"
       "  \"en\": \"automatically\","
       "  \"de\": \"Silbentrennungsalgorithmus\""
       "},"
       "\"/static/js/hyphenator.js\""
       ");"])]

   [:script
    (string/join " "
      ["$(\".ajax-toggle\").click(function () {"
       "  var action = \"set\";"
       "  if ($(this).data(\"is-set\")) {"
       "    action = \"del\";"
       "  }"
       "  var x = $(this);"
       "$.post(\"/reader/item/by-id/\" + x.data(\"id\"),"
       "   {\"action\": action, \"tag\": x.data(\"tag\")},"
       "   function(data) {"
       "      if (x.data(\"is-set\")) {"
       "         x.data(\"is-set\", false);"
       "         x.html(feather.icons[x.data(\"icon-unset\")]"
       "             .toSvg({'fill': x.data('icon-unset-fill'),"
       "                     'stroke': x.data('icon-unset-stroke')}));"
       "      } else {"
       "         x.data(\"is-set\", true);"
       "         x.html(feather.icons[x.data(\"icon-set\")]"
       "             .toSvg({'fill': x.data('icon-set-fill'),"
       "                    'stroke': x.data('icon-set-stroke')}));"
       "      }"
       "   });"
       "});"])]
;;      [:script
    ;; (string/join " "
    ;;   ["$(function () { $('[data-toggle=\"popover\"]').popover({html: true})})"
    ;;    ;; "$(function () {"
    ;;    ;; "   $('[data-toggle=\"popover\"]').popover({ "
    ;;    ;; "        html : true,"
    ;;    ;; "        content: function(this) {"
    ;;    ;; "             if (this.data(\"from-doc\")) {"
    ;;    ;; "                  return $(this.data(\"from-doc\")).html();"
    ;;    ;; "             } else {"
    ;;    ;; "                  return this.data(\"content\");"
    ;;    ;; "             }"
    ;;    ;; "        }"
    ;;    ;; "   });"
    ;;    ;; "});"
    ;;    ])]
   [:script
    (string/join " "
      ["$(\".bookmark-submit\").click(function () {"
       "  var x = $(this);"
       "  console.log($(x.data(\"url-source\")));"
       "  x.removeClass(\"btn-warning\");"
       "  x.removeClass(\"btn-secondary\");"
       "  x.addClass(\"btn-info\");"
       "  $.post({url: \"/reader/bookmark/add\","
       "         data: {\"url\": $(x.data(\"url-source\")).val(), "
       "                \"type\": x.data(\"type\")},"
       "         success: function(data) {"
       "             x.removeClass(\"btn-warning\");"
       "             x.removeClass(\"btn-info\");"
       "             $(x.data(\"url-source\")).val(\"\");"
       "             x.addClass(\"btn-secondary\");"
       ;; "             window.location.href = \"/reader/group/source-tag/bookmark/source/all/item/by-id/\" + data ;"
       "             return false;"
       "         }}).fail(function(data) {"
       "             x.addClass(\"btn-warning\");"
       "             x.removeClass(\"btn-secondary\");"
       "             x.removeClass(\"btn-info\");"
       "         });"
       "  return false;"
       "});"
       ])]
   [:script
    (string/join " "
      ["$(\"body\").keypress(function(event) {"
       "  var main_top = $(\"main\").offset().top;"
       "  var main_bottom = window.innerHeight;"
       "  if ($(\".feed-item\").length > 0) {"
       "    content = $(\".feed-item\");"
       "    if (event.which == 32 ) {"
       ;; add viewport info to data attrib
       "      $(\".feed-item\").each(function (index) {"
       "        var this_top = $(this)[0].getBoundingClientRect().top;"
       "        var this_bottom = this_top + $(this).height();"
       "        if (this_top >= main_top && this_bottom < main_bottom) {"
       "          $(this).attr(\"view\", \"full\");"
       "        } else if (this_top < main_top && this_bottom < main_bottom) {"
       "          $(this).attr(\"view\", \"partial-top\");"
       "        } else if (this_top >= main_top && this_bottom >= main_bottom && this_top < main_bottom) {"
       "          $(this).attr(\"view\", \"partial-bottom\");"
       "        } else {"
       "          $(this).attr(\"view\", \"out\");"
       "        }"
       "      });"
       ;; debug code - colorized above decisions
       ;; "      $(\".feed-item\").each(function (index) {"
       ;; "         $(this).css(\"background-color\", \"white\");"
       ;; "         if ($(this).attr(\"view\") == \"out\")  {"
       ;; "           $(this).css(\"background-color\", \"red\");"
       ;; "         }"
       ;; "         if ($(this).attr(\"view\") == \"partial-top\")  {"
       ;; "           $(this).css(\"background-color\", \"blue\");"
       ;; "         }"
       ;; "         if ($(this).attr(\"view\") == \"partial-bottom\")  {"
       ;; "           $(this).css(\"background-color\", \"yellow\");"
       ;; "         }"
       ;; "         if ($(this).attr(\"view\") == \"full\")  {"
       ;; "           $(this).css(\"background-color\", \"green\");"
       ;; "         }"
       ;; "      });"
       "      var scroll_to = $(\".feed-item\").last();"
       "      var candidate = $(\".feed-item[view=\\\"out\\\"]\");"
       "      if (candidate.length > 0) {"
       "         scroll_to = candidate.first();"
       "      }"
       "      candidate = $(\".feed-item[view=\\\"partial-bottom\\\"]\");"
       "      if (candidate.length > 0) {"
       "         scroll_to = candidate.first();"
       "      }"
       "      console.log(scroll_to);"
       "      event.preventDefault();"
       "      scroll_to.css(\"background-color\", \"#f4f4f4\");"
       "      $(\"body\").animate({scrollTop: scroll_to.offset().top - main_top - 5});"
       "    }"
       "  } else if ($(\".item-content-body\").length > 0) {"
       "    content = $(\".item-content-body\");"
       "    if (event.which == 32 && content.css(\"columns\") != \"auto auto\") {"
       "      event.preventDefault();"
       "      content.animate({scrollLeft: content.scrollLeft() + content.width()})"
       "    } else {"
       "      event.preventDefault();"
       "      var items = $(\".item-content-body\").find(\"h1, h2, h3, h4, h5, h6, h7, p, section, pre, ul\");"
       "      items.each(function (index) {"
       "        var this_top = $(this)[0].getBoundingClientRect().top;"
       "        var this_bottom = this_top + $(this).height();"
       "        if (this_top >= main_top && this_bottom < main_bottom) {"
       "          $(this).attr(\"view\", \"full\");"
       "        } else if (this_top < main_top && this_bottom < main_bottom) {"
       "          $(this).attr(\"view\", \"partial-top\");"
       "        } else if (this_top >= main_top && this_bottom >= main_bottom && this_top < main_bottom) {"
       "          $(this).attr(\"view\", \"partial-bottom\");"
       "        } else {"
       "          $(this).attr(\"view\", \"out\");"
       "        }"
       "      });"
       ;; "      items.each(function (index) {"
       ;; "         $(this).css(\"background-color\", \"white\");"
       ;; "         if ($(this).attr(\"view\") == \"out\")  {"
       ;; "           $(this).css(\"background-color\", \"red\");"
       ;; "         }"
       ;; "         if ($(this).attr(\"view\") == \"partial-top\")  {"
       ;; "           $(this).css(\"background-color\", \"blue\");"
       ;; "         }"
       ;; "         if ($(this).attr(\"view\") == \"partial-bottom\")  {"
       ;; "           $(this).css(\"background-color\", \"yellow\");"
       ;; "         }"
       ;; "         if ($(this).attr(\"view\") == \"full\")  {"
       ;; "           $(this).css(\"background-color\", \"green\");"
       ;; "         }"
       ;; "      });"
       "      var scroll_to = items.last();"
       "      var candidate = items.filter(\"[view=\\\"out\\\"]\");"
       "      console.log(candidate);"
       "      if (candidate.length > 0) {"
       "         scroll_to = candidate.first();"
       "      }"
       "      candidate = items.filter(\"[view=\\\"partial-bottom\\\"]\");"
       "      console.log(candidate);"
       "      if (candidate.length > 0) {"
       "         scroll_to = candidate.first();"
       "      }"
       "      event.preventDefault();"
       "      scroll_to.css(\"background-color\", \"#f4f4f4\");"
       "      $(\"body\").animate({scrollTop: scroll_to.offset().top - main_top - 5});"
       "    }"
       "  }"
       "});"

       ])]
   [:script "feather.replace()"]])

(defn tag-button [id {:keys [tag is-set?
                             icon-set icon-set-fill icon-set-stroke
                             icon-unset icon-unset-fill icon-unset-stroke]
                      :or {
                           icon-set-fill "none"
                           icon-set-stroke "black"
                           icon-unset-fill "none"
                           icon-unset-stroke "black"}}]
  [:a {:class "btn ajax-toggle"
       :title (str "Toggle tag " (name tag))
       :data-id id
       :data-icon-set icon-set
       :data-icon-set-fill icon-set-fill
       :data-icon-set-stroke icon-set-stroke
       :data-icon-unset icon-unset
       :data-icon-unset-fill icon-unset-fill
       :data-icon-unset-stroke icon-unset-stroke
       :data-tag (name tag)
       :data-is-set is-set?}
   (if is-set?
     (icon icon-set :fill icon-set-fill)
     (icon icon-unset :fill icon-unset-fill))])

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
                     (= start 0)

                     "Long Read"
                     (nil? start)
                     "Unknown Length"
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
        (database-stats)
        (state-stats)
        (source-status)]])))

(defn nav-bar [x]
  (let [{:keys [group-name group-item source-key mode
                selected-sources items range-recent range-before]} x
        {:keys [id tags]} (first items)]
    [:nav {:class "navbar navbar-dark sticky-top bg-dark flex-md-nowrap p-0"}
     [:a {:class "navbar-brand d-none align-middle d-md-block col-md-3 col-lg-2 mr-0"
          :href "#"}
      "(╯°□°）╯︵ ┻━┻"]

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
          (icon "refresh-ccw")]
         [:a {:class "btn btn-secondary"
              :href (make-site-href [(:uri x)] x)}
          (icon "skip-back")]]

       (= mode :show-item)
       [:div {:class "col-xs-8 col-ld-12"}
         (for [btn +tag-buttons+]
           (tag-button id (assoc btn :is-set? (some #(= % (name (:tag btn))) tags))))

        (when (> (count items) 1)
          (let [link-prefix (format "/reader/group/%s/%s/source/%s"
                           (name group-name)
                           (name group-item)
                           (name source-key))]
           [:a {:class "btn btn-secondary"
                :href (make-site-href [link-prefix "item/by-id" (-> items second :id)] {:mark :read} x)}
            (icon "arrow-down")]))])
      [:div {:class "col-xs-4 text-right d-block d-sm-none"}
      [:a {:class "btn btn-secondary"
           :data-toggle "collapse"
           :href "#groupnav"
           :role "button"}
       (icon "sidebar")]
      [:a {:class "btn btn-secondary"
           :data-toggle "collapse"
           :href "#sourcenav"
           :role "button"}
       (icon "list")]]]
     ]))


(defn group-list [x url-prefix group-items active]
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
      str-ks]]))

(defn group-nav [x]
  (let [active-group (:group-name x)
        active-key (name (:group-item x))]

    ;; simple filter toggle
    [:nav {:class "collapse col-md-3 col-lg-2 bg-light sidebar sidebar-left"
           :id "groupnav"}
     [:div {:class "sidebar-sticky"}

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
                   :data-type "bookmark"
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
           (icon ico) [:span name]]])]

      [:br]
      [:ul {:class "nav flex-column"}
       [:li {:class "nav-item"}
        [:a {:class (str "nav-link" (when
                                        (and (= active-group :default)
                                          (= active-key :all)) " active"))
             :href (make-site-href ["/reader/group/default/all/source/all/items"] x)}
         "any"]]]

      ;; item tags
      [:h6 {:class "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"}
       [:span "item tags"]]
      [:ul {:class "nav flex-column"}
       (group-list x "/reader/group/item-tags"
         (map first (:item-tags x))
         (when (= active-group :item-tags) active-key))]

      ;; source tags
      [:h6 {:class "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"}
       [:span "source tags"]]
      [:ul {:class "nav flex-column"}
       (group-list x "/reader/group/source-tag"
         (->> (:sources x) vals (map :tags) (apply set/union))
         (when (= active-group :source-tag) active-key))]

      [:h6 {:class "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"}
       [:span "type"]]
      [:ul {:class "nav flex-column"}
       (group-list x "/reader/group/type"
         (into #{} (->> (:sources x) vals (map :type)))
;;         (->> (:sources x) vals (group-by :type))
         (when (= active-group :type) active-key))]]]))

(defn source-list-item [x prefix source active-key]
  (let [{:keys [key id title item-tags]} source
        fltr (:filter x)
        nitems (or (get item-tags fltr) 0)
        show-num? (and (keyword? fltr) (not= fltr :all) (> nitems 0))
        grey-out? (and (keyword? fltr) (not= fltr :all) (= nitems 0))
        {:keys [all] :as item-tags} item-tags]

    [:li {:class (str "nav-item")}
     [:a {:class (str
                   (if grey-out? "nav-link nav-link-secondary" "nav-link" )
                   (when (= key active-key) " active "))
          :href (make-site-href [prefix (name (or key :unknown)) "items"] x)}
      [:span {:class "sidebar-heading-2"} key]

      [:span {:class "badge badge-pill badge-secondary float-right"}
       (when (> nitems 0) nitems)]

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
     [:div {:class "item-content-nav"}
      [:div {:class "btn-toolbar " :role "toolbar"}
       [:div {:class "btn-group btn-group-sm" :role "group"}
        [:a {:class "btn"}
         "&nbsp;&nbsp;" (icon "hash") nwords]
        [:a {:class "btn"}
         "&nbsp;&nbsp;" (icon "tag") (string/join ", " tags)]]
       (when (some? ts)
       [:div {:class "btn-group btn-group-sm" :role "group"}
        [:a {:class "btn"}
         "&nbsp;&nbsp;"
         (icon "calendar") (human/datetime ts)]])
       [:div {:class "btn-group btn-group-sm" :role "group"}
        [:a {:target "_blank"
             :href url
             :role "button"
             :class "btn"}
         (icon "external-link")]
        [:a {:class "btn"
             :href (make-site-href [id "dump"] x)}
         "&nbsp;" (icon "code")]]


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
         "Select Content"]

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
        (get-html-content item selected-data selected-content-type))]]))



(defn list-entry-kv
  "List entry key / value pair"
  [k v]
  [:li (icon "file") "&nbsp;" [:strong (str k)] "&nbsp;"
   (if (coll? v)
     [:span (format "n=%s [%s]" (count v)
              (string/join ", " v))]
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
                (icon "folder")
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
      [:span (icon "book-open") "&nbsp;" (string/replace path-last #"_" " ")]

      (and (string/includes? site "github") (= path-len 2))
      [:span (icon "github") "&nbsp;" (subs path 1)]

      (and (string/includes? site "twitter"))
      (if (= path-len 1)
        [:span (icon "twitter") "&nbsp;" (first path-seq)]
        [:span (icon "twitter") "&nbsp;" (first path-seq) "(status)"])

      (and (string/includes? site "facebook"))
      (cond
        (= path-len 1)
        [:span (icon "facebook") "&nbsp;" (first path-seq)]
        (= (first path-seq) "events")
        [:span (icon "facebook") "&nbsp;" "event"]
        :default
        [:span (icon "facebook") "&nbsp;" (take-last 2 path-seq)])

      (string/includes? site "youtube")
      [:span (icon "film") "&nbsp;" path-last]

      (and (string/includes? site "amazon") (> 0 path-len))
      (let [dp-entry (.indexOf path-seq "dp")]
        (cond
          (> dp-entry 0)
          [:span (icon "book") "&nbsp;"
           (string/replace (nth path-seq (- dp-entry 1)) #"[_-]" " ")]))

      (> (count str-url) 23)
      (if (re-find +boring-url-path-element-regex+ path-last)
        (str site "⋯")
        (str site "⋯" (->> path-seq last)))

      :default
      url))
  (catch Object _
    (log/warn (:throwable &throw-context) "Encountered broken url: " str-url)
    str-url)))


(defn word-cloud-fontsize [freq min-freq max-freq]
  (let [max-size (- (count +word-cloud-sizes+) 1)
        scaled-to-one (if (= max-freq min-freq)
                        1
                        (/ (- freq min-freq) (- max-freq min-freq)))
        scaled scaled-to-one
        size (Math/log (+ (* scaled-to-one 150) 1))]
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

(defn main-list-item [x link-prefix item]
  (let [{:keys [id source-key title data ts author tags read saved
                nwords names entry verbs url urls top-words]} item
        boring-filter (fn [word]
                        (not (or
                               (> (count word) 20)
                               (re-find #"^(\W{1,2}|[a-z0-9]\.)" word)
                               (re-find +boring-words-regex+ word))))
        words (take 50 (filter (fn [[word _]] (boring-filter word)) (:words top-words)))
        names (take 50 (filter boring-filter names))
        min-freq (second (last words))
        max-freq (second (first words))]

  [:div {:class "feed-item"}
   [:h4 {:class "h4"}
    [:a {:href (make-site-href [link-prefix "item/by-id" id]
                 {:mark :read} x)}
     title]]

   [:ul {:class "list-inline"}
    [:li {:class "list-inline-item"}
     (icon "calendar")
     "&nbsp;"
     [:span {:class "timestamp"} ts]
     [:span " - "]
     [:span {:class "timestamp"} (human/datetime ts)]]
    (when (string? source-key)
      [:li {:class "list-inline-item"}
       "&nbsp;"
       (icon "rss") source-key
       (when (= (:type item) :item-type/link)
         [:span " → " (human-host-identifier url)])])
    (when-not (string/blank? author)
      [:li {:class "list-inline-item"}
       "&nbsp;"
       (icon "user") author])]

   (when-let [vid (and (string? url) (re-find #"(youtu\.be\/|youtube\.com\/(watch\?(.*&)?v=|(embed|v)\/))([^\?&\"'>]+)" url))]
     (when (some? vid)
           [:p {:class "embed-responsive embed-responsive-16by9"}
            [:iframe {:class "embed-responsive-item"
                      :src (str "https://www.youtube.com/embed/" (last vid))
                      :allowfullscreen true}]]))

   (when-let [twit-pic (first (get-in entry [:entities :photos]))]
     [:div {:class "item-preview"} [:img {:src twit-pic}]])

   (when-let [image-url (:lead-image-url entry)]
     [:div {:class "item-preview-small"} [:img {:src image-url}]])

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
     [:a {:class "btn"}
      "&nbsp;" (icon "hash") nwords]
     [:a {:class "btn"}
      "&nbsp;" (icon "tag") (string/join ", " tags)]
     [:a {:class "btn" :href url}
      "&nbsp;" (icon "external-link")]
     [:a {:class "btn"
          :href (make-site-href [link-prefix "item/by-id" id "dump"] x)}
      "&nbsp;" (icon "code")]]

    [:div {:class "btn-group btn-group-sm mr-2" :role "group"}
     (for [btn +tag-buttons+]
       (tag-button id (assoc btn :is-set? (some #(= % (name (:tag btn))) tags))))
     ]]
   ]))


(defn main-list-items [x]
  (let [{:keys [group-name group-item source-key selected-sources items]} x]
    [:div
      (for [item items]
        (main-list-item
          x
          (format "/reader/group/%s/%s/source/%s"
            (name group-name)
            (name group-item)
            (name source-key))
          item))]))

(defn main-view [x]
  [:main {:role "main"
          :class "col-xs-12 col-md-6 col-lg-8"}
   [:div {:class "justify-content-between flex-wrap flex-md-no align-items-center pb-2 mb-3"}
    (case (:mode x)
      :show-item (main-show-item x)
      :dump-item (dump-item x)
      :list-items (main-list-items x)
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
                     :limit 10}]

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
   (log/info "[INFOWARSS-UI]" params)
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
  (log/info "[INFOWARSS-UI] Item mod:" id action tag)
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
  (log/info "[INFOWARSS-UI] Add Bookmark: " feed)
  (try+
    (let [state (assoc update/src-state-template :key key)
          items (fetch/fetch feed)
          processed (proc/process feed state items)
          dbks (persistency/store-items! processed)]
      (str (first dbks)))
    (catch Object e
      (log/warn e "add-url failed. Probably broken url: " feed)
      "fail")))


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
          :bookmark (add-thing
                      (core/make-bookmark-feed url)
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
                         :range-before {:id id
                                        :ts ts}}))

        (GET "/items" []
          (reader-index {:uri (:uri req)
                         :filter (as-keyword (get-in req [:params :filter]))
                         :group-name group-name
                         :group-item group-item
                         :source-key source-key
                         :mode :list-items}))


        (context "/item/by-id/:item-id"
          [item-id :<< as-int]
          (GET "/"
            [data :<< as-keyword
             content-type]
            (when (= (get-in req [:params :mark]) "read")
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
            (when (= (get-in req [:params :mark]) "read")
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
                    :mode :list-items})))
    (GET "/" [] (status-index))

    (GET "/preview" []
      {:status 200
       :body (fetch-preview)})

    (GET "/blob/:h" [h]
      (try+
        (let [blob (blobstore/get-blob h)]
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
    (route/resources "/static" {:root "status"})
    (route/not-found "404 Not found")))
