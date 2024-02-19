(ns llar.fetch.bookmark
  (:require
   [hiccup.core :refer [html]]
   [java-time.api :as time]
   [org.bovinegenius [exploding-fish :as uri]]
   [llar.fetch :refer [make-item-hash]]
   [llar.item]
   [llar.human :as human]
   [llar.postproc :as proc]
   [llar.src :as src]))

;;; Bookmarks

(defn bookmark-html [i]
  (html
   [:h1 (get-in i [:summary :title])]
   [:div {:class "summary"}
    [:p [:span {:class "key"} "URL: "]
     [:a {:href (get-in i [:entry :url])} (get-in i [:entry :url])]]
    [:p [:span {:class "key"} "Added: "] (time/format (time/zoned-date-time))]
    [:p [:span {:class "key"} "Published: "] (time/format (get-in i [:summary :ts]))]]
   [:div {:class "description"}
    [:h2 "Summary"]
    [:p (get-in i [:entry :descriptions "text/plain"])]]
   [:h1 "Content"]))

(defn make-readability-bookmark-feed [url]
  (let [src (src/mercury url)]
    {:src src
     :tags #{:bookmark}
     :proc (proc/new
            {:post [(fn [item]
                      (let [summary (bookmark-html item)
                            html (get-in item [:entry :contents "text/html"])
                            url (some-> item :entry :url uri/uri)
                            site (human/host-identifier url)]
                        (-> item
                            (assoc-in [:entry :contents "text/html"]
                                      (str summary "\n\n\n" html))
                            (assoc-in [:meta :source-key]
                                      (if (some? site)
                                        (keyword (str "bookmark-" (str site)))
                                        :bookmark))
                            (assoc :hash (make-item-hash (str url)))
                            (assoc-in [:meta :source-name]
                                      (if (some? site)
                                        (format "[Bookmark: %s]" (str site))
                                        "[Bookmark]")))))]})}))

(defn make-raw-bookmark-feed [url]
  (let [src (src/website url)]
    {:src src
     :tags #{:bookmark}
     :proc (proc/new
            {:post [(fn [item]
                      (let [summary (bookmark-html item)
                            html (get-in item [:entry :contents "text/html"])
                            url (some-> item :entry :url uri/uri)
                            site (human/host-identifier url)]
                        (-> item
                            (assoc-in [:entry :contents "text/html"]
                                      (str summary "\n\n\n" html))
                            (assoc-in [:meta :source-key]
                                      (if (some? site)
                                        (keyword (str "bookmark-" (str site)))
                                        :bookmark))
                            (assoc :hash (make-item-hash (str url)))
                            (assoc-in [:meta :source-name]
                                      (if (some? site)
                                        (format "[Bookmark: %s]" (str site))
                                        "[Bookmark]")))))]})}))
