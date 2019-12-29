(ns infowarss.apis.status
  (:require
   [infowarss.core :as core]
   [infowarss.update :as update]
   [infowarss.db :as db]
   [infowarss.sched :refer [get-sched-info]]
   [infowarss.metrics :as metrics]
   [clj-memory-meter.core :as mm]
   [clojure.java.io :as io]
   [taoensso.timbre :as log]
   [compojure.core :refer :all]
   [compojure.route :as route]
   [clojure.string :as string]
   [slingshot.slingshot :refer [throw+ try+]]
   [hiccup.core :refer [html]]
   [hiccup.util :refer [escape-html]]
   [clojure.contrib.humanize :as human]
   [clojure.pprint :as pprint]
   [iapetos.export :as prometheus-export]
   [mount.core :as mount]))


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

(def app
  (routes

   (GET "/" [] (status-index))

   (GET "/metrics" []
        {:status 200
         :body (prometheus-export/text-format metrics/prom-registry)})

   (route/resources "/static" {:root "status"})
   (route/not-found "404 Not found")))
