(ns u1f596.apis.status
  (:require
   [u1f596.config :as config]
   [u1f596.update :as update]
   [java-time :as time]
   [u1f596.live :as live]
   [u1f596.persistency :as persistency]
   [u1f596.appconfig :as appconfig]
   [u1f596.apis.reader :refer [frontend-db]]
   [u1f596.metrics :as metrics]
   [u1f596.human :as human]
   [compojure.core :refer [routes GET]]
   [compojure.route :as route]
   [hiccup.core :refer [html]]
   [iapetos.export :as prometheus-export]
   [mount.core :as mount :refer [defstate]]
   [clj-stacktrace.core :as stacktrace]
   [clj-stacktrace.repl :as stacktrace-repl]
   [puget.printer :as puget])
  (:import [org.bovinegenius.exploding_fish Uri]
           [org.apache.commons.text StringEscapeUtils]))

(defn html-header []
  [:head
   [:meta {:charset "utf-8"}]
   [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no"}]
   [:title "u1f596 🔢 🔥 🚧"]
   [:link {:rel "stylesheet" :href "/static/bootstrap/bootstrap-4.6.0-dist/css/bootstrap.min.css"}]
   [:link {:rel "stylesheet" :href "/static/ibmplex/Web/css/ibm-plex.min.css"}]
   [:link {:rel "stylesheet" :href "/static/fontawesome/fontawesome-free-5.15.3-web/css/all.min.css"}]
   [:link {:rel "stylesheet" :href "/static/datatables/jquery.dataTables.min.css"}]
   [:link {:rel "stylesheet" :href "/static/u1f596.css"}]])

(defn html-footer []
  [[:script {:src "/static/jquery/jquery-3.6.0.min.js"}]
   [:script {:src "/static/datatables/jquery.dataTables.min.js"}]
   [:script {:src "/static/bootstrap/bootstrap-4.6.0-dist/js/bootstrap.min.js"}]
   [:script {:src "/static/u1f596-status.js"}]])

(defn wrap-body [body]
  (html [:html {:lang "en"}
         (html-header)
         [:body
          (concat
           body
           (html-footer))]]))

(defn- get-state [k]
  (cond
    (contains? @update/state k)
    (get @update/state k)

    (contains? (:sources live/live) k)
    @(get-in (:sources live/live) [:hn-show :state])

    :else
    nil))

(def +pprint-handlers+
  {java.time.ZonedDateTime
   (puget/tagged-handler
    'inst (fn [x] (-> x time/instant str)))
   clojure.lang.ExceptionInfo
   (puget/tagged-handler
    'clojure.lang.ExceptionInfo.message (partial ex-message))
   Uri
   (puget/tagged-handler
    'uri str)})

(defn pprint-html [x]
  [:pre {:class "clj-pprint"}
   (let [pr-str-orig pr-str
         pr-str  (fn [& xs] (StringEscapeUtils/escapeHtml4 (apply pr-str xs)))]
    (puget/pprint-str
     x
     {:width 60
      :sort-keys true
      :print-color true
      :print-handlers +pprint-handlers+
      :print-fallback (fn [_ value] [:span (StringEscapeUtils/escapeHtml4 (pr-str-orig value))])
      :color-markup :html-inline}))])

(defn source-status []
  (html
   [:h2 "Sources"]
   [:table {:id "sources-datatable"}
    [:thead
     [:tr
      [:th ""]
      [:th "Key"]
      [:th "Status"]
      [:th "Source"]
      [:th "Last Exception"]
      [:th "Last Success / Update"]
      [:th "Last Attempt / Start"]]]

    [:tbody
     (for [[k src] (config/get-sources)]
       (let [state (get-state k)
             status (:status state)]
         [:tr {:class
               (cond (= :perm-fail status) "table-danger"
                     (= :bug status) "table-info"
                     (contains? #{:updating :running} status) "table-success"
                     (or (= :temp-fail status) (and (not (= :running status)))) "table-warning"
                     :else "")}
          [:td {:class "details-control"}]
          [:td {:class "col-xs-1"} k]
          [:td {:class "col-xs-1"} (str status (when (= :temp-fail status) (str " (" (:retry-count state) ")")))]
          [:td {:class "col-xs-3" :style "overflow: hidden; text-overflow: ellipsis; max-width: 30em;"}
           (str (:src src))]
          [:td {:class "col-xs-4" :style "overflow: hidden; text-overflow: ellipsis; max-width: 30em;"}
           (get-in state [:last-exception :object :type])]
          [:td {:class "col-xs-1"}  (some-> (or (:last-successful-fetch-ts state)
                                                (:last-update-ts state))
                                            human/datetime-ago)]
          [:td {:class "col-xs-1"}  (some-> (or (:last-attempt-ts state)
                                                (:start-ts state))
                                            human/datetime-ago)]]))]]))

(defn source-details [k]
  (let [state (get-state (keyword k))
        status (:status state)]
    (html
     [:div
      [:h5 "State Structure"]
      (pprint-html state)]
     (when-let [th (some-> (get-in state [:last-exception :throwable])
                           stacktrace/parse-exception)]
       [:div
        [:h5 "Exception Details"]
        [:pre (get-in state [:last-exception :object :message])]
        [:h6 "Stack Trace"]
        [:ol
         (for [s (:trace-elems th)
               :let [formatted (stacktrace-repl/pst-elem-str false s 70)]]
           [:li [:pre formatted]])]]))))

(defn list-to-table [header data]
  [:table {:class "datatable"}
   [:thead
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
   [:table {:class "datatable"}
    [:thead
     [:tr
      [:th "Metric"]
      [:th "Size"]]]
    [:tbody
     (for [[name value]
           [["Runtime Total" (human/filesize (.totalMemory (Runtime/getRuntime)))]
            ["Runtime Free" (human/filesize (.freeMemory (Runtime/getRuntime)))]
            ["Runtime Used" (human/filesize
                             (- (.totalMemory (Runtime/getRuntime))
                                (.freeMemory (Runtime/getRuntime))))]]]
       [:tr
        [:td name]
        [:td value]])]]))

(defn database-stats []
  (html
   [:h4 "Word Count Groups"]
   [:table {:class "datatable"}
    [:thead
     [:tr
      [:th "Group"]
      [:th "# Documents"]]]
    [:tbody
     (for [[start count] (persistency/get-word-count-groups frontend-db)]
       (let [txt (cond
                   (nil? start)
                   "Unknown Length"
                   (zero? start)
                   "Long Read"
                   :else
                   (str "< " start " Words"))]
         [:tr [:th txt] [:td count]]))]]
   [:h4 "Tags"]
   (list-to-table ["Tag" "# Documents"]  (persistency/get-tag-stats frontend-db))
   [:h4 "Types"]
   (list-to-table ["Type" "# Documents"] (persistency/get-type-stats frontend-db))))

(defn state-stats []
  (let [states (mount/find-all-states)
        running (mount/running-states)]
    (html
     [:h2 "State"]
     [:table {:class "table"}
      [:thead
       [:tr
        [:th "State"]
        [:th "Running?"]
        [:th "Current"]]]
      [:tbody
       (for [state states]
         [:tr
          [:td state]
          [:td (some? (some #{state} running))]
          [:td (mount/current-state state)]])]])))

(defn thread-stats []
  (let [stack-traces (sort-by #(-> % key .getState) (Thread/getAllStackTraces))]
    [:h2 "Current Threads"]
    [:table {:id "threads-datatable"}
     [:thead
      [:tr
       [:th ""]
       [:th "Group"]
       [:th "Name"]
       [:th "State"]
       [:th "Top Frame"]]]
     [:tbody
       (for [[th stack] stack-traces]
         [:tr {:data-stacktrace (html [:ol
                                       (for [s (stacktrace/parse-trace-elems stack)
                                             :let [formatted (stacktrace-repl/pst-elem-str false s 70)]]
                                         [:li [:pre formatted]])])
               :class
               (cond
                 false "table-info"
                 :else "")}
          [:td {:class "details-control"}]
          [:td {:class "col-xs-1"} (-> th .getThreadGroup .getName)]
          [:td {:class "col-xs-1"} (.getName th)]
          [:td {:class "col-xs-1"} [:pre (.getState th)]]
          [:td {:class "col-xs-4"} [:pre (first stack)]]])]]))



(defn home-tab [])

(def tabs
  {:home home-tab
   :memory memory-stats
   :database database-stats
   :state state-stats
   :threads thread-stats
   :sources source-status})

(defn status-index []
  (wrap-body
   (html [:h1 "u1f596 🔢 🔥 🚧"]
         [:div {:class "contianer-fluid"}
          [:ul {:class "nav nav-tabs"}
           (for [[k _] tabs
                 :let [tab-name (name k)
                       tab-id (str tab-name "-tab")
                       tab-href (str "#" tab-name)]]

             [:li {:class "nav-item"}
              [:a {:class (str "nav-link" (when (= k :home) " active"))
                   :id tab-id
                   :data-toggle "tab"
                   :role "tab"
                   :href tab-href}
               tab-name]])]
          [:div {:class "tab-content"
                 :id "nav-tab-content"}
           (for [[k func] tabs
                 :let [tab-name (name k)
                       cont-id tab-name]]
             [:div {:class (str "tab-pane" (when (= k :home) " fade show active"))
                    :id cont-id
                    :role "tabpanel"}
              (func)])]])))

(def app
  (routes

   (GET "/" [] (status-index))

   (GET "/source-details/:key" [key] (source-details key))

   (GET "/metrics" []
     {:status 200
      :body (prometheus-export/text-format metrics/prom-registry)})

   (route/resources "/static" {:root "status"})
   (route/not-found "404 Not found")))
