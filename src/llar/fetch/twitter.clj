(ns llar.fetch.twitter
  (:require [llar.fetch :refer [FetchSource item-to-string make-meta make-item-hash]]
            [llar.postproc :refer [ItemProcessor]]
            [llar.persistency :refer [CouchItem]]
            [llar.analysis :as analysis]
            [llar.item]
            [clojure.tools.logging :as log]
            [llar.http :refer [try-blobify-url!]]
            [twitter.api.restful :as twitter]
            [clojure.spec.alpha :as s]
            [org.bovinegenius [exploding-fish :as uri]]
            [java-time.api :as time]))

(defrecord TweetItem
           [meta
            summary
            hash
            raw
            entry]
  Object
  (toString [item] (item-to-string item)))

(defn make-tweet-item [meta summary hash entry raw]
  {:pre [(s/valid? :irq0/item-metadata meta)
         (s/valid? :irq0/item-summary summary)
         (s/valid? :irq0/item-hash hash)]}
  (->TweetItem meta summary hash entry raw))

(defn- parse-twitter-ts [ts]
  (when-not (nil? ts)
    (time/zoned-date-time (time/formatter "EEE MMM dd HH:mm:ss Z yyyy") ts)))

(defn- tweet-title [s]
  (let [s (or s "")
        new (subs s 0 (min (count s) 50))]
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
    (format "<img class=\"media-entity\" src=\"%s\">" (try-blobify-url! data))
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

  (let [text (or (get tweet :full_text) (get tweet :text))
        changes (tweet-text-changes tweet)
        sb (StringBuilder.)
        text-code-points (->> text
                              .codePoints
                              .toArray
                              vec)
        text-len (count text-code-points)]
    (doseq [[[[_ end] [cat data]]
             [[next-start _] [_ _]]]
            (partition 2 1 (concat
                            [[[0 0] [nil nil]]]
                            changes
                            [[[text-len text-len] [nil nil]]]))]
      (let [subs (subvec text-code-points end next-start)]
        (.append sb (htmlize-entity cat data))
        (doseq [s subs]
          (.appendCodePoint sb s))))
    (str sb)))

(defn tweet-to-entry [tweet]
  (let [user (get-in tweet [:user :screen_name])
        id (get tweet :id)
        text (or (get tweet :full_text) (get tweet :text))
        entities (get tweet :entities)]

    {:url (uri/uri (format "https://twitter.com/%s/status/%s"
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
                                 (map :media_url)
                                 (map try-blobify-url!))}
     :authors [(get-in tweet [:user :screen_name])]
     :contents {"text/plain" text
                "text/html" (if (string? text)
                              (htmlize-tweet-text tweet)
                              "")}}))

(extend-protocol FetchSource
  llar.src.TwitterSearch
  (fetch-source [src _conditional-tokens]
    (let [{:keys [query oauth-creds]} src
          resp (twitter/search-tweets
                :oauth-creds oauth-creds
                :params {:q query
                         :tweet_mode "extended"})
          tweets (get-in resp [:body :statuses])]
      (for [tweet tweets
            :let [entry (tweet-to-entry tweet)
                  content (get-in entry [:contents "text/plain"])]]
        (make-tweet-item
         (make-meta src)
         {:ts (get entry :pub-ts)
          :title (tweet-title content)}
         (make-item-hash
          (first (get entry :authors))
          content)
         entry
         tweet)))))

(extend-protocol FetchSource
  llar.src.TwitterApi
  (fetch-source [src _conditional-tokens]
    (let [{:keys [api-fn params oauth-creds]} src
          resp (api-fn
                :oauth-creds oauth-creds
                :params (assoc params :tweet_mode "extended"))
          tweets (:body resp)]
      (log/info (:full_text (first tweets)))
      (for [tweet tweets
            :let [entry (tweet-to-entry tweet)
                  content (get-in entry [:contents "text/plain"])]]
        (make-tweet-item
         (make-meta src)
         {:ts (get entry :pub-ts)
          :title (tweet-title content)}
         (make-item-hash
          (first (get entry :authors))
          content)
         entry
         tweet)))))

(extend-protocol ItemProcessor
  TweetItem
  (post-process-item [item _src _state]
    (update item :entry merge (:entry item) (analysis/analyze-entry (:entry item))))
  (filter-item [_ _ _] false))

(extend-protocol CouchItem
  TweetItem
  (to-couch [item]
    (-> item
        (assoc :type :tweet)
        (dissoc :raw)
        (dissoc :body)
        (assoc-in [:meta :source :oauth-creds] nil))))
