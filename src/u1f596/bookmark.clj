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
