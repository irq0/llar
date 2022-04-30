(ns u1f596.src
  (:require [schema.core :as s]
            [u1f596.schema :as schema]
            [slingshot.slingshot :refer [throw+]]
            [org.bovinegenius [exploding-fish :as uri]]
            [twitter.oauth]
            [twitter.oauth :refer [make-oauth-creds]]
            [twitter.api.restful :as twitter-rest])
  (:import [twitter.oauth OauthCredentials]))

;;;; Sources - Containers for information needed by the Fetcher / Live
;;;; to retrieve Items

;;; Http

(def +http-default-args+
  {:user-agent :default})

(def +feed-default-args+
  {:user-agent :default
   :deep? false
   :force-update? false})

(s/defrecord GenericWebsite
             [url :- schema/URL
              args :- {:user-agent (s/cond-pre s/Str s/Keyword)}]

  Object
  (toString [src] (str "[GenericWebsite: " (:url src) "]")))

(defn website
  [url
   & {:keys [user-agent] :as args}]
  (GenericWebsite. (uri/uri url) (merge +http-default-args+ args)))

(s/defrecord Custom
    [id :- s/Keyword
     fn :- schema/Func]

  Object
  (toString [src] (str "[Custom: " (name (:id src)) "]")))

(defn custom
  [id fn]
  (Custom. id fn))


(s/defrecord PaywalledWebsite
             [url :- schema/URL
              cookie-getter :- s/Any
              args :- {:user-agent (s/cond-pre s/Str s/Keyword)}]
  Object
  (toString [src] (str "[PaywalledWebsite: " (:url src) "]")))

(defn website+paywall
  [url
   cookie-getter
   & {:keys [user-agent] :as args}]
  (PaywalledWebsite. (uri/uri url) cookie-getter (merge +http-default-args+ args)))

;;; Feed

(s/defrecord Feed
             [url :- schema/URL
              args :- {:deep? s/Bool
                       :force-update? s/Bool
                       :user-agent (s/cond-pre s/Str s/Keyword)}]
  Object
  (toString [src] (str "[Feed: " (:url src) "]")))

(defn feed
  [url
   & {:keys [deep? force-update? user-agent]
      :as args}]
  (->Feed (uri/uri url) (merge +feed-default-args+ args)))

(s/defrecord SelectorFeed
             [url :- schema/URL
              selectors :- {:urls s/Any
                            :ts s/Any
                            :author s/Any
                            :content s/Any
                            :description s/Any}
              extractors :- {:urls s/Any
                             :ts s/Any
                             :author s/Any
                             :content s/Any
                             :description s/Any}
              args :- {:user-agent (s/cond-pre s/Str s/Keyword)}]
  Object
  (toString [src] (str "[SelectorFeed: " (:url src) "]")))

(defn selector-feed
  [url selectors extractors & {:keys [user-agent]
                               :as args}]
  (->SelectorFeed (uri/uri url) selectors extractors (merge +http-default-args+ args)))

(s/defrecord WordpressJsonFeed
             [url :- schema/URL
              args :- {:user-agent (s/cond-pre s/Str s/Keyword)}]
  Object
  (toString [src] (str "[WpJsonFeed: " (:url src) "]")))

(defn wp-json
  [url
   & {:keys [user-agent]
      :as args}]
  (->WordpressJsonFeed (uri/uri url) (merge +http-default-args+ args)))

(def TwitterCreds
  {:app-key schema/NotEmptyStr
   :app-secret schema/NotEmptyStr
   :user-token schema/NotEmptyStr
   :user-token-secret schema/NotEmptyStr})

(s/defrecord TwitterSearch
             [query :- schema/NotEmptyStr
              url :- schema/URL
              oauth-creds :- OauthCredentials]
  Object
  (toString [src] (str "[TwitterSearch: " (:query src) "]")))

(s/defn twitter-search :- TwitterSearch
  "Search twitter https://dev.twitter.com/rest/public/search"
  [query :- schema/NotEmptyStr
   oauth-creds :- TwitterCreds]
  (->TwitterSearch
   query
   (uri/uri (str "https://twitter.com/search?q=" query))
   (make-oauth-creds
    (:app-key oauth-creds)
    (:app-secret oauth-creds)
    (:user-token oauth-creds)
    (:user-token-secret oauth-creds))))

(s/defrecord TwitterApi
             [api-fn :- schema/Func
              params :- s/Any
              oauth-creds :- OauthCredentials]
  Object
  (toString [src] (format "[TwitterApi/%s: %s]" api-fn params)))

(s/defn twitter-timeline :- TwitterApi
  [oauth-creds]
  (->TwitterApi
   twitter-rest/statuses-home-timeline
   {:count 200}
   (make-oauth-creds
    (:app-key oauth-creds)
    (:app-secret oauth-creds)
    (:user-token oauth-creds)
    (:user-token-secret oauth-creds))))

(s/defrecord MercuryWebParser
             [url :- schema/URL]
  Object
  (toString [src] (str "[MercuryWebParser: " (:url src) "]")))

(s/defn mercury :- MercuryWebParser
  "Fetch URL using Mercury Web Parser API"
  [url :- schema/NotEmptyStr]
  (->MercuryWebParser
   (uri/uri url)))

(s/defrecord Document
             [url :- schema/URL]
  Object
  (toString [src] (str "[Document: " (:url src) "]")))

(s/defn doc :- Document
  "Fetch URL and process as document"
  [url :- schema/NotEmptyStr]
  (->Document (uri/uri url)))

(def reddit-supported-listings #{:controversial :best :hot :new :random :rising :top})
(def reddit-supported-timeframes #{:hour :day :week :month :year :all})

(s/defrecord Reddit
             [subreddit :- schema/NotEmptyStr
              listing :- schema/NotEmptyStr
              timeframe :- schema/NotEmptyStr]
  Object
  (toString [src] (format "[Reddit: r/%s/%s]" (:subreddit src) (:listing src) (:timeframe src))))

(s/defn reddit :- Reddit
  "Fetch URL using Mercury Web Parser API"
  ([subreddit :- schema/NotEmptyStr
    listing :- s/Keyword]
   (reddit subreddit listing :week))
  ([subreddit :- schema/NotEmptyStr
   listing :- s/Keyword
   timeframe :- s/Keyword]
  (when-not (contains? reddit-supported-listings listing)
    (throw+ {:type ::invalid-reddit-listing :supported reddit-supported-listings}))
  (when-not (contains? reddit-supported-timeframes timeframe)
    (throw+ {:type ::invalid-reddit-timeframe :supported reddit-supported-timeframes}))
  (->Reddit
   subreddit
   (name listing)
   (name timeframe))))

(s/defrecord ImapMailbox
             [uri :- schema/URL
              creds :- {:username schema/NotEmptyStr
                        :password schema/NotEmptyStr}]
  Object
  (toString [src] (format "[IMAP: %s]" (:uri src))))

(s/defn imap :- ImapMailbox
  "Fetch from IMAP Mailbox"
  [uri-str :- schema/NotEmptyStr
   creds :- {:username schema/NotEmptyStr
             :password schema/NotEmptyStr}]

  (let [uri (uri/uri uri-str)]
    (when-not (#{"imap" "imaps"} (uri/scheme uri))
      (throw+ {:type ::invalid-protocol :uri uri}))
    (when (some? (uri/query uri))
      (throw+ {:type ::query-string-unsupported :uri uri}))
    (->ImapMailbox
     uri
     creds)))

;;; Live

(s/defrecord HackerNews
             [story-feed :- schema/NotEmptyStr
              args :- {:throttle-secs schema/PosInt}]
  Object
  (toString [src] (str "[HackerNews: " (:story-feed src) "(" (:state src) ")]")))

(defn hn
  [story-feed
   & {:keys [throttle-secs]}]
  (->HackerNews story-feed {:throttle-secs (or throttle-secs 120)}))


(defn feed? [src]
  (some #(instance? % src) [Feed SelectorFeed WordpressJsonFeed Reddit]))

(defn twitter? [src]
  (some #(instance? % src) [TwitterSearch TwitterApi]))

(defn reddit? [src]
  (some #(instance? % src) [Reddit]))

(defn mailbox? [src]
  (some #(instance? % src) [ImapMailbox]))
