(ns u1f596.apis.dataworkbench
  (:require
   [u1f596.src :as src]
   [u1f596.fetch :as fetch]
   [u1f596.postproc :as proc]
   [u1f596.update :as update]
   [u1f596.persistency :as persistency]
   [u1f596.blobstore :as blobstore]
   [u1f596.lab :as lab]
   [clj-memory-meter.core :as mm]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
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
   [cheshire.core :refer :all]
   [mount.core :as mount]))

(defn html-header []
  [:head
   [:meta {:charset "utf-8"}]
   [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no"}]
   [:title "🖖 Data Workbench"]
   [:link {:rel "stylesheet" :href "/static/css/bootstrap.min.css"}]
   [:link {:rel "stylesheet" :href "/static/fonts/fira/fira.css"}]
   [:link {:rel "stylesheet" :href "/static/fonts/charter/webfonts/stylesheet.css"}]
   [:script {:src "/static/js/feather.min.js"}]
   [:link {:rel "stylesheet" :href "/static/css/leaflet.css"}]
   [:link {:rel "stylesheet" :href "/static/css/nerd-fonts-generated.css"}]
   [:script {:src "/static/js/leaflet.js"}]
   [:link {:rel "stylesheet" :href "/static/css/my.css"}]])

(defn html-footer []
  [[:script {:src "/static/js/jquery.min.js"}]
   [:script {:src "/static/js/jquery.waypoints.min.js"}]
   [:script {:src "/static/js/jquery.dataTables.min.js"}]
   [:link {:rel "stylesheet" :href "/static/css/jquery.dataTables.min.css"}]
   [:script {:src "/static/js/popper.min.js"}]
   [:script {:src "/static/js/bootstrap.min.js"}]
   [:script
    (str
     "$(document).ready(function() {"
     "  $('#map-points').DataTable({"
     "  select: {style: 'multi'},"
     "  paging:   false,"
     "  }); "
     "});"
     "$('#map-points tbody').on( 'click', 'tr', function () {"
     "  $(this).toggleClass('selected');"
     "  var selected = $(this).hasClass('selected');"
     "  var lat = $(this).data('lat');"
     "  var lon = $(this).data('lon');"
     "  var mark = marker[[lat, lon]];"
     "  if (selected) {"
     "    mark.setOpacity(1.0);"
     "  } else {"
     "    mark.setOpacity(0.5);"
     "  }"
     "} );")]])

(defn usc-icon [categories]
  (let [names (map :name categories)
        main (first names)]
    "nf-fa-star"))

(defn usc-to-map-pos [usc-venue]
  (merge (select-keys usc-venue [:address :name :district])
         {:icon (usc-icon (:categories usc-venue))
          :categories (map :name (:categories usc-venue))
          :pos (vals (:location usc-venue))}))

(def datasets
  {:usc-l (fn []
            (let [venues (-> (lab/get-usc-venues)
                             :data :venues)
                  l-above (->> venues
                               (filter #(= 3 (apply min (:planTypeIds %)))))]
              (map usc-to-map-pos l-above)))
   :test (fn []
           [{:pos [52.5053712 13.3756906]
             :title "Holmes Place Potsdamer Platz"
             :icon "nf-fae-mountains"}])})

(defn wrap-body [body]
  (html [:html {:lang "en"}
         (html-header)
         [:body
          (concat
           body
           (html-footer))]]))

(defn leaflet-map []
  [:div {:id "leaflet-map"}])

(defn map-data [id])

(defn data-table [data]
  [:table {:id "map-points"}
   [:thead
    [:tr
     [:th "Icon"]
     [:th "Name"]
     [:th "Address"]
     [:th "District"]
     [:th "Categories"]
     [:th "Pos"]]]
   [:tbody
    (for [row data]
      [:tr {:data-pos (->> row :pos (string/join ","))
            :data-lat (-> row :pos first)
            :data-lon (-> row :pos second)}
       [:td [:span {:class (str "nf " (:icon row))}]]
       (for [k [:name :address :district :categories :pos]]
         [:td (get row k)])])]])

(defn populate-leaflet [data]
  [:script
   "function toggle_table(e) {"
   "   var strpos = e.latlng.lat + ',' + e.latlng.lng;"
   "   var row = $('#map-points tr[data-pos=\"'+strpos+'\"]');"
   "   row.toggleClass('selected');"
   "  var selected = row.hasClass('selected');"
   "  var mark = marker[[e.latlng.lat, e.latlng.lng]];"
   "  if (selected) {"
   "    mark.setOpacity(1.0);"
   "  } else {"
   "    mark.setOpacity(0.5);"
   "  }"
   "}\n"
   "marker = [];"
   (string/join " "
                (for [{:keys [pos name icon]} data]
                  (str
                   "marker[[" (string/join "," pos) "]] =  L.marker([" (string/join ", " pos) "], {"
                   "  icon: L.divIcon({className: 'leaflet-icon nf " icon "'}),"
                   "  opacity: 0.2,"
                   "  title: '" (string/replace name #"['\"\n\t\r]" "") "',"
                   "}).on('click', toggle_table).addTo(mymap);")))
   ""])

(defn create-leaflet []
  [:script
   (string/join " "
                ["var mymap = L.map('leaflet-map').setView([
52.531677, 13.381777], 13);"
                 "L.tileLayer('https://api.tiles.mapbox.com/v4/{id}/{z}/{x}/{y}.png?access_token={accessToken}', {"
                 "attribution: 'Map data &copy; <a href=\"https://www.openstreetmap.org/\">OpenStreetMap</a> contributors, <a href=\"https://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA</a>, Imagery © <a href=\"https://www.mapbox.com/\">Mapbox</a>',"
                 "maxZoom: 18,"
                 "id: 'mapbox.streets',"
                 "accessToken: 'pk.eyJ1IjoiaXJxMCIsImEiOiJjajJ0ZHQwbTAwMDFvMzJsdmZ1eGJxbG4yIn0.KGBGSpn9T5SfUH9vi6Zwxg'"
                 "}).addTo(mymap);"])])

(defn map-index []
  (wrap-body
   (html
    [:div {:class "container-fluid"}
     [:div {:class "row "}]
     [:div
      [:i {:class "nerd-font nerd-font-fa-heart"}]
      [:i {:class "nf nf-custom-folder"}]]
     (leaflet-map)
     (create-leaflet)
     (let [data ((:usc-l datasets))]
       [:div
        (populate-leaflet data)
        (data-table data)])])))

(defn as-keyword
  "Parse a string into keyword"
  [s]
  (keyword s))

(def app
  (routes
   (context "/map" [:as req]
     (GET "/" [] (map-index))
     (GET "/data" []
       (keys datasets))
     (GET "/data/:id" [id :<< as-keyword]
       (map-data id)))

   (route/resources "/static" {:root "status"})
   (route/not-found "404 Not found")))
