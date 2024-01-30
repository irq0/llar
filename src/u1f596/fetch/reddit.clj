(ns u1f596.fetch.reddit
  (:require
   [u1f596.fetch :as fetch]
   [u1f596.specs]
   [u1f596.persistency :as persistency]
   [u1f596.postproc :as postproc]
   [u1f596.analysis :as analysis]
   [u1f596.item]
   [u1f596.http :as u1f596-http]
   [clojure.spec.alpha :as s]
   [digest]
   [hiccup.core :refer [html]]
   [clj-http.client :as http]
   [java-time.api :as time]
   [clojure.tools.logging :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [org.bovinegenius [exploding-fish :as uri]]))

(defrecord RedditItem
           [meta
            summary
            hash
            entry]
  Object
  (toString [item] (fetch/item-to-string item)))

(defn make-reddit-item [meta summary hash entry]
  {:pre [(s/valid? :irq0/item-metadata meta)
         (s/valid? :irq0/item-summary summary)
         (s/valid? :irq0/item-hash hash)]}
  (->RedditItem meta summary hash entry))

(extend-protocol postproc/ItemProcessor
  RedditItem
  (post-process-item [item _src _state]
    (let [nlp (analysis/analyze-entry (:entry item))]
      (update item :entry merge (:entry item) nlp)))
  (filter-item [_ _ _] false))

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
                             :headers {:user-agent "java:u1f596:23: (by /u/irq0x00)"}})]
     (:body resp))
   (catch (fn [resp] (#{500 501 502 503 504} (:status resp)))
          {:keys [headers body status]}
     (log/errorf "Server Error (%s): %s %s" status headers body)
     (throw+ {:type  :u1f596.http/server-error-retry-later}))
   (catch (fn [resp] (#{400 401 402 403 404 405 406 410} (:status resp)))
          {:keys [headers body status]}
     (log/errorf "Client error probably due to broken request (%s): %s %s"
                 status headers body)
     (throw+ {:type :u1f596.http/request-error}))
   (catch java.net.UnknownHostException ex
     (log/error ex "Host resolution error" url)
     (throw+ {:type :u1f596.http/server-error-retry-later
              :url url}))
   (catch Object _
     (log/error "Unexpected error: " (:throwable &throw-context))
     (throw+ {:type :u1f596.http/unexpected-error}))))

(defn reddit-get-scores [src]
  (let [reddit (reddit-get (format "https://www.reddit.com/r/%s/%s/.json?limit=100&t=%s"
                                   (:subreddit src) (:listing src) (:timeframe src)))]
    (->> (get-in reddit [:data :children])
         (map #(get-in % [:data :score])))))

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
  {:url (u1f596-http/absolutify-url (:url c) "https://www.reddit.com")
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

(s/def :irq0-src-reddit/title string?)
(s/def :irq0-src-reddit/created_utc number?)
(s/def :irq0-src-reddit/url :irq0/url-str)
(s/def :irq0-src-reddit/permalink string?)
(s/def :irq0-src-reddit/thumbnail string?)
(s/def :irq0-src-reddit/author string?)
(s/def :irq0-src-reddit/id string?)
(s/def :irq0-src-reddit/score number?)
(s/def :irq0-src-reddit/selftext string?)
(s/def :irq0-src-reddit/item (s/keys :req-un [:irq0-src-reddit/title
                                              :irq0-src-reddit/created_utc
                                              :irq0-src-reddit/url
                                              :irq0-src-reddit/permalink
                                              :irq0-src-reddit/thumbnail
                                              :irq0-src-reddit/author
                                              :irq0-src-reddit/id
                                              :irq0-src-reddit/score
                                              :irq0-src-reddit/selftext]))

(extend-protocol fetch/FetchSource
  u1f596.src.Reddit
  (fetch-source [src]
    (let [reddit (reddit-get (format "https://www.reddit.com/r/%s/%s/.json?limit=100&t=%s"
                                     (:subreddit src) (:listing src) (:timeframe src)))]
      (for [child (get-in reddit [:data :children])
            :let [item (:data child)
                  c (s/conform :irq0-src-reddit/item item)]]
        (if (s/invalid? c)
          (log/warn "Invalid reddit item: " c (s/explain-str :irq0-src-reddit/item item))
          (make-reddit-item
           (fetch/make-meta src)
           {:ts (reddit-ts-to-zoned-date-time (:created_utc item)) :title (:title item)}
           (fetch/make-item-hash (str (:id item)))
           (make-reddit-entry item)))))))
