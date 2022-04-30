(ns u1f596.fetch.bookmark
  (:require
   [u1f596.fetch :refer [make-item-hash]]
   [u1f596.http :as http]
   [u1f596.src :as src]
   [u1f596.postproc :as proc]
   [slingshot.slingshot :refer [try+]]
   [schema.core :as s]
   [taoensso.timbre :as log]
   [clj-http.client :as http-client]
   [hickory.core :as hick]
   [java-time :as time]
   [hiccup.core :refer [html]]
   [hickory.render :as hick-r]
   [org.bovinegenius [exploding-fish :as uri]]))



;;; Bookmarks

(defn bookmark-html [i]
  (html
   [:h1 (get-in i [:summary :title])]
   [:div {:class "summary"}
    [:ul
     [:li [:span {:class "key"} "URL: "]
      [:a {:href (get-in i [:entry :url])} (get-in i [:entry :url])]]
     [:li [:span {:class "key"} "Added: "] (time/format (time/zoned-date-time))]
     [:li [:span {:class "key"} "Published: "] (time/format (get-in i [:summary :ts]))]
     [:li [:span {:class "key"} "Next Page URL: "]
      [:a {:href (get-in i [:entry :next-page-url])} (get-in i [:entry :next-pageurl])]]]]
   [:div {:class "description"}
    [:h2 "Summary"]
    [:p (get-in i [:entry :descriptions "text/plain"])]]
   [:div {:class "nlp"}
    [:h2 "Names / Places"]
    [:p (map (fn [name] [:span [:a {:href (str "https://www.startpage.com/do/search?query=" name)} (str " " name " ")] "&nbsp;"]) (get-in i [:entry :nlp :names]))]]
   [:h1 "Content"]))



(defn make-readability-bookmark-feed [url]
  (let [src (src/mercury url)]
    {:src src
     :tags #{:bookmark}
     :proc (proc/make
            :post [(fn [item]
                     (let [summary (bookmark-html item)
                           html (get-in item [:entry :contents "text/html"])
                           url (some-> item :entry :url uri/uri)
                           site (http/human-host-identifier url)]
                       (-> item
                           (assoc-in [:entry :contents "text/html"]
                                     (str summary "\n\n\n" html))
                           (assoc-in [:meta :source-key]
                                     (if (some? site)
                                       (keyword (str "bookmark-" (str site)))
                                       :bookmark))
                           (assoc :hash (make-item-hash url))
                           (assoc-in [:meta :source-name]
                                     (if (some? site)
                                       (format "[Bookmark: %s]" (str site))
                                       "[Bookmark]")))))])}))

(defn make-raw-bookmark-feed [url]
  (let [src (src/website url)]
    {:src src
     :tags #{:bookmark}
     :proc (proc/make
            :post [(fn [item]
                     (let [summary (bookmark-html item)
                           html (get-in item [:entry :contents "text/html"])
                           url (some-> item :entry :url uri/uri)
                           site (http/human-host-identifier url)]
                       (-> item
                           (assoc-in [:entry :contents "text/html"]
                                     (str summary "\n\n\n" html))
                           (assoc-in [:meta :source-key]
                                     (if (some? site)
                                       (keyword (str "bookmark-" (str site)))
                                       :bookmark))
                           (assoc :hash (make-item-hash url))
                           (assoc-in [:meta :source-name]
                                     (if (some? site)
                                       (format "[Bookmark: %s]" (str site))
                                       "[Bookmark]")))))])}))
