(ns llar.apis.dashboard
  (:require
   [clj-stacktrace.core :as stacktrace]
   [clj-stacktrace.repl :as stacktrace-repl]
   [compojure.core :refer [GET POST routes context]]
   [compojure.route :as route]
   [hiccup2.core :refer [html]]
   [hiccup.page :refer [html5]]
   [iapetos.export :as prometheus-export]
   [java-time.api :as time]
   [mount.core :as mount]
   [puget.printer :as puget]
   [llar.apis.reader :refer [frontend-db map-to-tree] :as reader]
   [llar.appconfig :refer [appconfig-redact-secrets]]
   [llar.config :as config]
   [llar.converter]
   [llar.human :as human]
   [llar.metrics :as metrics]
   [llar.persistency :as persistency]
   [llar.update :as update])
  (:import
   [org.apache.commons.text StringEscapeUtils]
   [org.bovinegenius.exploding_fish Uri]))

(defn html-header []
  [:head
   [:meta {:charset "utf-8"}]
   [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no"}]
   [:title "LiveLongAndRead Status Page 🔢 🔥 🚧"]
   [:link {:rel "apple-touch-icon" :sizes "180x180" :href "/static/img/apple-touch-icon.png"}]
   [:link {:rel "icon" :type "image/png" :sizes "32x32" :href "/static/img/favicon-32x32.png"}]
   [:link {:rel "icon" :type "image/png" :sizes "16x16" :href "/static/img/favicon-16x16.png"}]
   [:link {:rel "icon" :type "image/x-icon" :href "/static/img/favicon.ico"}]
   [:link {:rel "manifest" :href "/static/img/site.webmanifest"}]
   [:link {:rel "stylesheet" :href "/static/bootstrap/css/bootstrap.min.css"}]
   [:link {:rel "stylesheet" :href "/static/ibmplex/Web/css/ibm-plex.min.css"}]
   [:link {:rel "stylesheet" :href "/static/fontawesome/css/all.min.css"}]
   [:link {:rel "stylesheet" :href "/static/datatables/dataTables.bootstrap5.min.css"}]
   [:link {:rel "stylesheet" :href "/static/datatables/buttons.bootstrap5.min.css"}]
   [:link {:rel "stylesheet" :href "/static/llar.css"}]])

(defn html-footer []
  [[:script {:src "/static/jquery/jquery.min.js"}]
   [:script {:src "/static/bootstrap/js/bootstrap.bundle.min.js"}]
   [:script {:src "/static/datatables/dataTables.min.js"}]
   [:script {:src "/static/datatables/dataTables.bootstrap5.min.js"}]
   [:script {:src "/static/datatables/dataTables.buttons.min.js"}]
   [:script {:src "/static/datatables/buttons.bootstrap5.min.js"}]
   [:script {:src "/static/llar-status.js"}]])

(defn wrap-body [body]
  (html5
   (html-header)
   [:body
    (concat
     body
     (html-footer))]))

(defn- get-state [k]
  (cond
    (contains? @update/state k)
    (get @update/state k)

    :else
    nil))

(def +pprint-handlers+
  {java.time.ZonedDateTime
   (puget/tagged-handler
    'inst (fn [x] (-> x time/instant str)))

   llar.converter.ExceptionContext
   (puget/tagged-handler
    'llar-exception-context
    (fn [x] (-> x
                (select-keys [:message :data :cause])
                (assoc :note "redacted - see below"))))

   Uri
   (puget/tagged-handler
    'uri str)})

(def +light-mode-color-scheme+
  "Light mode friendly color scheme for puget pretty printing"
  {:delimiter [:bold :black]
   :tag [:bold :blue]
   :nil [:bold :black]
   :boolean [:bold :blue]
   :number [:bold :blue]
   :string [:bold :green]
   :character [:bold :green]
   :keyword [:bold :magenta]
   :symbol [:bold :black]
   :function-symbol [:bold :blue]
   :class-delimiter [:bold :black]
   :class-name [:bold :blue]})

(defn pprint-html [x]
  [:pre {:class "clj-pprint"}
   (let [pr-str-orig pr-str
         #_:clj-kondo/ignore
         pr-str  (fn [& xs] (StringEscapeUtils/escapeHtml4 (apply pr-str xs)))]
     (puget/pprint-str
      x
      {:width 60
       :seq-limit 5
       :sort-keys true
       :print-color true
       :color-scheme +light-mode-color-scheme+
       :print-handlers +pprint-handlers+
       :print-fallback (fn [_ value] [:span (StringEscapeUtils/escapeHtml4 (pr-str-orig value))])
       :color-markup :html-inline}))])

(defn source-tab []
  [:div
   [:table {:id "sources-datatable" :class "table"}
    [:thead
     [:tr
      [:th "Source Key"]
      [:th "Status"]
      [:th "Source"]
      [:th "Last Success / Update"]
      [:th "Last Attempt / Start"]
      [:th "Actions"]]]]])

(defn source-details [src-k]
  (let [k (keyword src-k)
        source (config/get-source k)
        state (get-state k)]
    (str (html
          [:div
           [:h5 "Source Configuration"]
           (pprint-html source)
           [:h5 "State Structure"]
           (pprint-html state)]
          (when-let [th (some-> (get-in state [:last-exception :throwable])
                                stacktrace/parse-exception)]
            [:div
             [:h5 "Exception Details"]
             [:ul
              [:li "Exception Class: " [:pre (class (get-in state [:last-exception :throwable]))]]
              [:li "Message: " [:pre (get-in state [:last-exception :message])]]
              [:li "Cause: " [:pre (get-in state [:last-exception :cause])]]
              [:li "Data: " (pprint-html (get-in state [:last-exception :data]))]]
             [:h6 "Stack Trace"]
             [:ol
              (for [s (:trace-elems th)
                    :let [formatted (stacktrace-repl/pst-elem-str false s 70)]]
                [:li [:pre formatted]])]])))))

(defn list-to-table [header data]
  [:table {:class "datatable table"}
   [:thead
    [:tr
     (for [h header]
       [:th h])]]
   [:tbody
    (for [row data]
      [:tr
       (for [cell row]
         [:td cell])])]])

(defn memory-tab []
  (html
   [:table {:class "datatable table"}
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

(defn database-tab []
  (html
   [:div
    [:div
     [:h5 "Word Count Groups"]
     [:table {:class "datatable table"}
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
           [:tr [:th txt] [:td count]]))]]]
    [:div {:class "pt-4"} [:h5 "Tags"]
     [:div (list-to-table ["Tag" "# Documents"]  (persistency/get-tag-stats frontend-db))]]
    [:div {:class "pt-4"} [:h5 "Types"]
     [:div (list-to-table ["Type" "# Documents"] (persistency/get-type-stats frontend-db))]]]))

(defn state-tab []
  (let [states (mount/find-all-states)
        running (mount/running-states)]
    (html
     [:table {:class "table"}
      [:thead
       [:tr
        [:th "State"]
        [:th "Running?"]
        [:th "Type"]
        [:th "Value"]]]
      [:tbody
       (for [state states
             :let [val (mount/current-state state)
                   pretty (if (= "#'llar.update/state" state) "..omitted.." (pprint-html val))]]
         [:tr
          [:td state]
          [:td (some? (some #{state} running))]
          [:td (type val)]
          [:td pretty]])]])))

(defn thread-tab []
  (let [stack-traces (sort-by #(-> % key .getState) (Thread/getAllStackTraces))]
    [:table {:id "threads-datatable" :class "table"}
     [:thead
      [:tr
       [:th ""]
       [:th "Group"]
       [:th "Name"]
       [:th "State"]
       [:th "Top Frame"]]]
     [:tbody
      (->>
       (for [[th stack] stack-traces]
         [(-> th .getThreadGroup .getName)
          (.getName th)
          (.getState th)
          (first stack)
          stack])
       (sort-by second)
       (map (fn [[group name state top-of-stack stack]]
              [:tr {:data-stacktrace (html [:ol
                                            (for [s (stacktrace/parse-trace-elems stack)
                                                  :let [formatted (stacktrace-repl/pst-elem-str false s 70)]]
                                              [:li [:pre formatted]])])
                    :class
                    (cond
                      false "table-info"
                      :else "")}
               [:td {:class "details-control"}]
               [:td {:class "col-xs-1"} group]
               [:th {:class "col-xs-1"} name]
               [:td {:class "col-xs-1"} [:pre state]]
               [:td {:class "col-xs-4"} [:span {:title top-of-stack} (human/truncate-ellipsis (str top-of-stack) 20)]]])))]]))

(defn metrics-tab []
  [:div
   [:a {:href "/metrics"} "Prometheus Metrics"]])

(defn schedule-tab []
  (let [states (mount/find-all-states)]
    [:div
     [:table {:class "table"}
      [:thead
       [:tr
        [:th "Name"]
        [:th "State "]
        [:th "Type"]
        [:th "Canned"]
        [:th "Code"]]]
      [:tbody
       (for [state-name states
             :let [state (mount/current-state state-name)
                   {:keys [sched-name sched-type chime-times pred]} (meta state)]
             :when (re-find #"chime\.core" (str state))]

         [:tr
          [:td sched-name]
          [:td state-name]
          [:td sched-type]
          [:td chime-times]
          [:td (pprint-html pred)]])]]]))

(defn config-tab []
  [:div [:h5 "appconfig"]
   (map-to-tree (appconfig-redact-secrets))])

(def tabs
  {:sources #'source-tab
   :memory #'memory-tab
   :database #'database-tab
   :schedules #'schedule-tab
   :state #'state-tab
   :metrics #'metrics-tab
   :threads #'thread-tab
   :config #'config-tab})

(defn status-index []
  (wrap-body
   (html
    [:div {:class "container-fluid mt-3"}
     [:h1 "LLAR Live Long and Read 🖖 Dashboard"]
     [:ul {:class "nav nav-underline"}
      (for [[k _] tabs
            :let [tab-name (name k)
                  tab-id (str tab-name "-tab")
                  tab-href (str "#" tab-name)]]
        [:li {:class "nav-item"}
         [:a {:class (str "nav-link" (when (= k :sources) " active"))
              :id tab-id
              :data-bs-toggle "tab"
              :role "tab"
              :href tab-href}
          tab-name]])]
     [:div {:class "tab-content"
            :id "nav-tab-content"}
      (for [[k func] tabs
            :let [tab-name (name k)
                  cont-id tab-name]]
        [:div {:class (str "tab-pane pt-2  " (when (= k :sources) " fade show active"))
               :id cont-id
               :role "tabpanel"}
         (func)])]])))

(def update-futures (atom {}))

(defn update-source [str-k overwrite]
  {:pre [(string? str-k)
         (boolean? overwrite)]}
  (let [k (keyword str-k)
        src (get (config/get-sources) k)
        existing-fut (get @update-futures k)]
    (cond
      (nil? src)
      {:status 404
       :body {:source-key k
              :status :not-found}}

      (and existing-fut (not (future-done? existing-fut)))
      {:status 200
       :body {:source-key k
              :future (str existing-fut)
              :status :already-updating}}

      :else
      (let [fut (future (update/update! k :force true :overwrite? overwrite))]
        (swap! update-futures assoc (keyword k) fut)
        {:status 200
         :body {:source-key k
                :status :updating
                :future (str fut)}}))))

(defn- source-status-row [k src state]
  (let [status (:status state)]
    [k ; key
     (str (if status (name status) "?") (when (= :temp-fail status) (str " (" (:retry-count state) ")"))) ; status for humans
     (str (:src src)) ; source name
     (some-> (or (:last-successful-fetch-ts state) ; last success or update
                 (:last-update-ts state))
             human/datetime-ago)
     (some-> (or (:last-attempt-ts state) ; last attempt or start
                 (:start-ts state))
             human/datetime-ago)]))

(defn source-status [str-k]
  (let [k (keyword str-k)
        fut (get @update-futures k)
        src (get (config/get-sources) k)
        state (get-state k)]
    (if state
      {:status 200
       :body {:source-key k
              :row (source-status-row k src state)
              :update-status {:future (str fut)
                              :done (future-done? fut)
                              :result (when (future-done? fut) @fut)}}}
      {:status 404
       :body {:source-key k
              :error :not-found}})))

(defn all-sources-status []
  {:status 200
   :body {:data
          (for [[k src] (config/get-sources)]
            (let [state (get-state k)]
              (source-status-row k src state)))}})

(def app
  (routes

   (GET "/" [] (status-index))

   (context "/api" []
     (POST "/update/:source-key" req
       (let [{:keys [source-key overwrite]} (:params req)]
         (update-source source-key (boolean ((fnil parse-boolean "") overwrite)))))

     (GET "/source/:source-key" [source-key]
       (source-status source-key))

     (GET "/sources" []
       (all-sources-status)))

   (GET "/source-details/:key" [key] (source-details key))

   (GET "/metrics" []
     {:status 200
      :body (prometheus-export/text-format metrics/prom-registry)})

   (route/resources "/static" {:root "status"})
   (route/not-found "404 Not found")))
