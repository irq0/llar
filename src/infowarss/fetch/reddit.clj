(ns infowarss.fetch.reddit
  (:require
   [infowarss.fetch :as fetch]
   [infowarss.schema :as schema]
   [infowarss.persistency :as persistency]
   [infowarss.postproc :as postproc]
   [infowarss.analysis :as analysis]
   [infowarss.http :as infowarss-http]
   [digest]
   [hiccup.core :refer [html]]
   [clj-http.client :as http]
   [java-time :as time]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [org.bovinegenius [exploding-fish :as uri]]
   [schema.core :as s]))

(s/defrecord RedditItem
             [meta :- schema/Metadata
              summary :- schema/Summary
              hash :- schema/Hash
              entry :- schema/RedditEntry]
  Object
  (toString [item] (fetch/item-to-string item)))

(extend-protocol postproc/ItemProcessor
  RedditItem
  (post-process-item [item src state]
    (let [nlp (analysis/analyze-entry (:entry item))]
      (update item :entry merge (:entry item) nlp)))
  (filter-item [item src state] false))

(extend-protocol persistency/CouchItem
  RedditItem
  (to-couch [item]
    (-> item
        (assoc :type :link)
        (dissoc :raw)
        (dissoc :body)
        (assoc-in [:meta :source :args] nil))))

(defn reddit-get [url]
  (try+
   (let [resp (http/get url {:accept :json
                             :as :json
                             :headers {:user-agent "java:infowarss:23: (by /u/irq0x00)"}})]
     (:body resp))
   (catch (contains? #{500 501 502 503 504} (get % :status))
          {:keys [headers body status]}
     (log/errorf "Server Error (%s): %s %s" status headers body)
     (throw+ {:type ::server-error-retry-later}))
   (catch (contains? #{400 401 402 403 404 405 406 410} (get % :status))
          {:keys [headers body status]}
     (log/errorf "Client error probably due to broken request (%s): %s %s"
                 status headers body)
     (throw+ {:type ::request-error}))
   (catch Object _
     (log/error "Unexpected error: " (:throwable &throw-context))
     (throw+ {:type ::unexpected-error}))))

(defn reddit-ts-to-zoned-date-time [t]
  (when (number? t)
    (time/zoned-date-time (time/instant (* 1000 (long t))) (time/zone-id "UTC"))))    

(defn reddit-html-summary [c]
  (html
   [:h1 (:title c)]
   [:div {:class "summary"}
    [:ul
     (when (some? (:subreddit_name_prefixed c))
       [:li {:class "item-key-subreddit"}
             [:span {:class "key"} "Subreddit: "] (:subreddit_name_prefixed c)])
     [:li {:class "item-key-score"} [:span {:class "key"} "Score: "] (:score c)]
     [:li {:class "item-key-timestamp"}
           [:span {:class "key"} "Time: "] (reddit-ts-to-zoned-date-time (:created_utc c))]
     [:li {:class "item-key-url"} [:a {:href (:url c)} "URL"]]
     [:li {:class "item-key-comments-url"}
      [:a {:href (str "https://www.reddit.com" (:permalink c))} "Comments"]]]]
   [:p {:style "white-space: pre-line"} (:selftext c)]))

(defn make-reddit-entry [c]
  {:url (infowarss-http/absolutify-url (:url c) "https://www.reddit.com")
   :comments-url (uri/uri (str "https://www.reddit.com" (:permalink c)))
   :thumbnail (:thumbnail c)
   :pub-ts (reddit-ts-to-zoned-date-time (:created_utc c))
   :title (:title c)
   :authors [(:author c)]
   :id (:id c)
   :score (:score c)
   :contents {"text/plain" (:title c)
              "text/html" (reddit-html-summary c)}
   :descriptions {"text/plain" ""}})

(extend-protocol fetch/FetchSource
  infowarss.src.Reddit
  (fetch-source [src]
    (let [reddit (reddit-get (format "https://www.reddit.com/r/%s/%s/.json?limit=100"
                                     (:subreddit src) (:feed src)))]
      (for [child (get-in reddit [:data :children])
            :let [item (:data child)]]
        (->RedditItem
         (fetch/make-meta src)
         {:ts (reddit-ts-to-zoned-date-time (:created_utc item)) :title (:title item)}
;          (fetch/make-item-hash (:title item) (:selftext item))
         (fetch/make-item-hash (:id item))
         (make-reddit-entry item))))))
