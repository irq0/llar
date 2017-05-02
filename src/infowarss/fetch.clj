(ns infowarss.fetch
  (:require
   [infowarss.converter :as conv]
   [infowarss.src :as src]
   [clj-rome.reader :as rome]
   [infowarss.schema :as schema]
   [digest]
   [clj-http.client :as http]
   [hickory.core :as hick]
   [hickory.select :as hick-s]
   [hickory.render :as hick-r]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [clj-time.format :as tf]
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojure.tools.nrepl.server :as nrepl]
   [clojure.java.io :as io]
   [twitter.api.restful :as twitter]
   [schema.core :as s]
   )
  (:import
   [java.util.Base64.Encoder]
   [org.joda.time.DateTime]))


;;;; Fetcher - Fetch a Source (infowars.src) to get *Items
;;;; Items have types depending on their sources (e.g Http -> HttpItem)

;;; Item data structures

(s/defrecord HttpItem
    [meta :- schema/Metadata
     summary :- schema/Summary
     hash :- schema/Hash
     raw :- schema/HttpResponse
     hickory :- s/Any])

(s/defrecord FeedItem
    [meta :- schema/Metadata
     summary :- schema/Summary
     hash :- schema/Hash
     entry :- schema/FeedEntry
     raw :- s/Any
     feed :- schema/Feed])

(s/defrecord TweetItem
    [meta :- schema/Metadata
     summary :- schema/Summary
     hash :- schema/Hash
     raw :- schema/Tweet
     entry :- schema/TweetEntry])

;;; Constructors

(s/defn make-item-hash :- schema/Hash
  "Make hash to use in *Item"
  [& args]
  (str "SHA-256:" (-> args string/join digest/sha-256)))

(s/defn make-meta :- schema/Metadata
  "Make meta entry from source and optional initial tags"
  [src :- s/Any]
  {:source src
   :source-name (str src)
   :app "infowarss"
   :ns (str *ns*)
   :fetch-ts (time/now)
   :tags #{}
   :version 0})

;;; Content extraction helper functions

(defn- parse-http-ts [ts]
  (when-not (nil? ts)
    (tf/parse (tf/formatter "EEE, dd MMM yyyy HH:mm:ss z") ts)))

(defn- parse-twitter-ts [ts]
  (when-not (nil? ts)
    (tf/parse (tf/formatter "EEE MMM dd HH:mm:ss Z yyyy") ts)))

(defn- tweet-title [s]
  (let [new (subs s 0 (min (count s) 50))]
    (if (< (count new) (count s))
      (str new "…")
      new)))

(defn- tweet-type [tweet]
  (cond
    (contains? tweet :retweeted_status) :retweet
    (not (nil? (get tweet :in_reply_to_user_id))) :reply
    :else :tweet))

(def tweet-symbols
  {:hashtags "#"
   :user_mentions "@"
   :urls "URL"})

(defn htmlize-entity
  "Return html representation of entity"
  [category data]

  (condp = category
    :hashtags
    (format "<a class=\"hashtag-entity\" href=\"https://twitter.com/hashtag/%s\">%s%s</a>"
      data (get tweet-symbols category) data)
    :user_mentions
    (format "<a class=\"mention-entity\" href=\"https://twitter.com/%s\">%s%s</a>"
      data (get tweet-symbols category) data)
    :media
    (format "<img class=\"media-entity\" src=\"%s\">" data)
    :urls
    (format "<a class=\"url-entity\" href=\"%s\">%s</a>"
      data (get tweet-symbols category))
    nil
    ""))

(defn- tweet-text-changes
  "Generate sorted list of changes to apply to tweet text"
  [tweet]
  (sort
    (apply concat
      (for [[category key] [[:hashtags :text]
                            [:user_mentions :screen_name]
                            [:media :media_url]
                            [:urls :expanded_url]]]
        (for [item (get-in tweet [:entities category])]
          (let [{:keys [indices]} item]
            [[(first indices) (second indices)]
             [category (get item key)]]))))))

(defn htmlize-tweet-text
  "Get html representation of tweet text"
  [tweet]

  ;;Java strings are UTF16 and substring/subs indexes by char not code
  ;;point. The tweet text usually contains many UTF16 chars that are
  ;;not in the basic plane. We therefore first convert the string to a
  ;;vec of code points which can be indexed by "perceived" character.
  ;;We use a mutable StringBuffer to reassemble since it supports
  ;;adding code points.

  (let [text (get tweet :text)
        changes (tweet-text-changes tweet)
        sb (StringBuilder.)
        text-code-points (->> text
                           .codePoints
                           .toArray
                           vec)
        text-len (count text-code-points)
        ]
    (doseq [ [[[start end] [cat data]]
              [[next-start next-end] [next-cat next-data]]]
            (partition 2 1 (concat
                             [[[0 0] [nil nil]]]
                             changes
                             [[[text-len text-len] [nil nil]]]))]
      (let [subs (subvec text-code-points end next-start)]
                  (.append sb (htmlize-entity cat data))
                  (doseq [s subs]
                    (.appendCodePoint sb s))
                  ))
      (str sb)))

(defn tweet-to-entry [tweet]
  (let [user (get-in tweet [:user :screen_name])
        id (get tweet :id)
        text (get tweet :text)
        entities (get tweet :entities)]

    {:url (io/as-url (format "https://twitter.com/%s/status/%s"
                       user id))
     :pub-ts (parse-twitter-ts (get tweet :created_at))
     :score {:favs (get tweet :favorite_count)
             :retweets (get tweet :retweet_count)}
     :language (keyword (get tweet :lang))
     :id id
     :type (tweet-type tweet)
     :entities {:hashtags (some->> (get entities :hashtags)
                            (map :text))
                :mentions (some->> (get entities :user_mentions)
                            (map :screen_name))
                :photos (some->> (get entities :media)
                          (map :media_url))}
     :authors [(get-in tweet [:user :screen_name])]
     :contents {"text/plain" text
                "text/html" (htmlize-tweet-text tweet)}}))

(defn- extract-http-title
  [parsed-html]
  (some-> (hick-s/select (hick-s/child
                      (hick-s/tag :title))
       parsed-html)
    first
    :content
    first
    string/trim))

(defn- extract-http-timestamp
  [resp]
  (let [{:keys [headers]} resp
        parser (partial
                 tf/parse (tf/formatter "EEE, dd MMM yyyy HH:mm:ss z"))]
    (try+
      (or (parser (get headers "Last-Modified"))
        (parser (get headers "Date")))
      (catch Object _
        (time/now)))))

(defn- extract-feed-authors
  "Extract feed author from rome feed item"
  [authors]
  (for [{:keys [name email]} authors]
    (str
      (when-not (nil? name)
        name)
      (when-not (nil? email)
        (str " <" email ">")))))


(defn- extract-feed-description
  "Extract feed description from rome reed item"
  [description]
  (if (= (:type description) "text/html")
    {"text/html" (:value description)
     "text/plain" (conv/html2text (:value description))}
    {"text/plain" (:value description)}))

(def rome-content-type-to-mime {"html" "text/html"
                                "text" "text/plain"})

(defn- extract-feed-content
  "Extract feed content from rome feed item"
  [contents]
  (let [by-type (into {}
                  (for [{:keys [type value]} contents]
                    [(get rome-content-type-to-mime
                       type "application/octet-stream") value]))]
    ;; Convert non plain text content types
    (condp #(contains? %2 %1) by-type
      "text/html" (assoc by-type "text/plain" (conv/html2text (get by-type "text/html")))
      (assoc by-type "text/plain" (first (vals by-type))))))

(defn- extract-feed-timestamp
  "Extract feed entry timestamp"
  [e http]
  (or (some-> e :published-date tc/from-date)
    (some-> e :updated-date tc/from-date)
    (get-in http [:meta :fetch-ts])))

(defn- maybe-extract-url
  [s]
  (try+
    (io/as-url s)
    (catch java.net.MalformedURLException _
      nil)))

;;; Fetcher

(defn fetch-http-generic
  "Generic HTTP fetcher"
  [src]
  (try+
    (let [url (-> src :url str)
          response (http/get url)
          parsed-html (-> response :body hick/parse hick/as-hickory)]
      (log/debugf "Fetched HTTP: %s -> %s bytes body" url (count (get response :body)))
      (map->HttpItem
        {:meta (make-meta src)
         :raw response
         :hash (make-item-hash (:body response))
         :hickory parsed-html
         :summary {:ts (extract-http-timestamp response)
                   :title (extract-http-title parsed-html)}}))

    (catch (contains? #{400 401 402 403 404 405 406 410} (get % :status))
        {:keys [headers body status]}
      (log/errorf "Client error probably due to broken request (%s): %s %s"
        status headers body)
      (throw+ (assoc &throw-context :type ::request-error)))

    (catch (contains? #{500 501 502 503 504} (get % :status))
        {:keys [headers body status] :as orig}
      (log/errorf "Server Error (%s): %s %s" status headers body)
      (throw+ (assoc &throw-context :type ::server-error-retry-later)))

    (catch [:status 408]
        {:keys [headers body status]}
      (log/errorf "Client Error (%s): %s %s" status headers body)
      (throw+ (assoc &throw-context :type ::client-error-retry-later)))

    (catch Object _
      (log/error "Unexpected error: " (:throwable &throw-context))
      (throw+ (assoc &throw-context :type ::unexpected-error)))))

(defn- http-get-feed-content [src]
  (log/debug "Fetching feed item content of " (str src))
  (let [http-item (fetch-http-generic src)
        body (->> http-item
               :hickory
               (hick-s/select
                 (hick-s/child
                   (hick-s/tag :body)))
               first
               hick-r/hickory-to-html)]
    {"text/html" body
     "text/plain" (conv/html2text body)}))

;;; Fetch source protocol

(defprotocol FetchSource
  "Protocol to work with data sources"
  (fetch-source [src]))


(extend-protocol FetchSource
  infowarss.src.Http
  (fetch-source [src]
    [(fetch-http-generic src)])
  infowarss.src.Feed
  (fetch-source [src]
    (let [http-item (fetch-http-generic src)
          res (-> http-item :raw :body rome/build-feed)
          feed {:title (-> res :title)
                :language (-> res :language)
                :url (-> res :link  maybe-extract-url)
                :description {"text/plain" (-> res :description)}
                :encoding (-> res :encoding)
                :pub-ts (some-> res :published-date tc/from-date)
                :feed-type (-> res :feed-type)}]

      (for [re (:entries res)]
        (let [timestamp (extract-feed-timestamp re http-item)
              authors (extract-feed-authors (:authors re))
              in-feed-contents (extract-feed-content (:contents re))
              contents-url (-> re :link maybe-extract-url)
              contents (if (and (nil? (get in-feed-contents "text/plain"))
                             (get-in src [:args :deep?])
                             (not (nil? contents-url)))
                         (http-get-feed-content
                           (src/http (str contents-url)))
                         in-feed-contents)
              descriptions (extract-feed-description (:description re))
              base-entry {:updated-ts (some-> re :updated-date tc/from-date)
                               :pub-ts (some-> re :published-date tc/from-date)
                               :url contents-url
                               :title (-> re :title)}]
          (map->FeedItem
            (-> http-item
              (dissoc :hash :hickory :summary)
              (merge {:raw re
                      :feed feed
                      :entry (merge base-entry
                               {:authors authors
                                :contents contents
                                :description descriptions})
                      :hash (make-item-hash
                              (:title re) (:link re))
                      :summary {:ts timestamp
                                :title (:title re)}})))))))

  infowarss.src.TwitterSearch
  (fetch-source [src]
    (let [{:keys [query oauth-creds]} src
          resp (twitter/search-tweets
                 :oauth-creds oauth-creds
                 :params {:q query})
          tweets (get-in resp [:body :statuses])]
      (for [tweet tweets
            :let [entry (tweet-to-entry tweet)
                  content (get-in entry [:contents "text/plain"])]]
        (map->TweetItem
          {:raw tweet
           :meta (make-meta src)
           :summary {:ts (get entry :pub-ts )
                     :title (tweet-title content)}
           :hash (make-item-hash
                   (get entry :id)
                   content)
           :entry entry})))))


(defn fetch
  "Fetch feed. Return seq of new items"
  [feed]
  (let [{:keys [src]} feed]
    (log/info "Fetching: " (str src))
    (fetch-source src)))
