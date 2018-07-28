(ns infowarss.fetch.reddit
  (:require
   [infowarss.converter :as conv]
   [infowarss.src :as src]
   [infowarss.fetch :as fetch]
   [infowarss.schema :as schema]
   [infowarss.persistency :as persistency]
   [infowarss.postproc :as postproc]
   [infowarss.analysis :as analysis]
   [digest]
   [hiccup.core :refer [html]]
   [clj-http.client :as http]
   [hickory.core :as hick]
   [clj-time.coerce :as tc]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojure.java.io :as io]
   [schema.core :as s]
   ))

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
      (-> item
        (update :entry merge (:entry item) nlp))))

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
    (catch (not= 200 (get % :status))
        {:keys [headers body status]}
      (log/errorf "Client error probably due to broken request (%s): %s %s"
        status headers body)
      (throw+ {:type ::request-error}))
    (catch Object _
      (log/error "Unexpected error: " (:throwable &throw-context))
      (throw+ {:type ::unexpected-error}))))

(defn reddit-ts-to-joda [t]
  (when (number? t)
    (tc/from-long (* 1000 (long t)))))


(defn reddit-html-summary [c]
  (html
    [:h1 (:title c)]
    [:div {:class "summary"}
     [:ul
      (when (some? (:subreddit_name_prefixed c))
        [:li [:spam {:class "key"} "Subreddit: "] (:subreddit_name_prefixed c)])
      [:li [:span {:class "key"} "Score: "] (:score c)]
      [:li [:span {:class "key"} "Time: "] (reddit-ts-to-joda (:created_utc c))]
    [:div {:class "links"}
     [:ul
      [:li [:span {:class "key"} "URL: "]
       [:a {:href (:url c)} (:url c)]]
      [:li [:span {:class "key"} "Comments: "]
       [:a {:href (str "https://www.reddit.com" (:permalink c))}
        (str "https://www.reddit.com" (:permalink c))]]]]]]))


(defn make-reddit-entry [c]
  {:url (io/as-url (:url c))
   :comments-url (io/as-url (:permalink (str "https://www.reddit.com"
                                          (:permalink c))))
   :thumbnail (:thumbnail c)
   :pub-ts (reddit-ts-to-joda (:created_utc c))
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
    (let [reddit (reddit-get (format "https://www.reddit.com/r/%s/%s/.json?limit=42"
                               (:subreddit src) (:feed src)))]
      (for [child (get-in reddit [:data :children])
            :let [item (:data child)]]
        (->RedditItem
          (fetch/make-meta src)
          {:ts (reddit-ts-to-joda (:created_utc item)) :title (:title item)}
;          (fetch/make-item-hash (:title item) (:selftext item))
          (fetch/make-item-hash (:id item))
          (make-reddit-entry item))))))
