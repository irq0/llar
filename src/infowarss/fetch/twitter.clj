(ns infowarss.fetch.twitter
  (:require [infowarss.fetch :refer [FetchSource item-to-string make-meta make-item-hash]]
            [infowarss.postproc :refer [ItemProcessor]]
            [infowarss.persistency :refer [CouchItem]]
            [infowarss.schema :as schema]
            [infowarss.analysis :as analysis]
            [twitter.api.restful :as twitter]
            [schema.core :as s]
            [clojure.java.io :as io]
            [clj-time.format :as tf]))

(s/defrecord TweetItem
    [meta :- schema/Metadata
     summary :- schema/Summary
     hash :- schema/Hash
     raw :- schema/Tweet
     entry :- schema/TweetEntry]
  Object
  (toString [item] (item-to-string item)))

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


(extend-protocol FetchSource
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
                   (first (get entry :authors))
                   content)
           :entry entry})))))


(extend-protocol FetchSource
  infowarss.src.TwitterApi
  (fetch-source [src]
    (let [{:keys [api-fn params oauth-creds]} src
          resp (api-fn
                 :oauth-creds oauth-creds
                 :params params)
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
                   (first (get entry :authors))
                   content)
           :entry entry})))))


(extend-protocol ItemProcessor
  TweetItem
  (post-process-item [item src state]
    (-> item
      (update :entry merge (:entry item) (analysis/analyze-entry (:entry item)))
    ))
  (filter-item [item src state] false))


(extend-protocol CouchItem
  TweetItem
  (to-couch [item]
    (-> item
      (assoc :type :tweet)
      (dissoc :raw)
      (dissoc :body)
      (assoc-in [:meta :source :oauth-creds] nil))))
