(ns llar.src
  (:require [clojure.spec.alpha :as spec]
            [llar.specs]
            [slingshot.slingshot :refer [throw+]]
            [org.bovinegenius [exploding-fish :as uri]]
            [twitter.oauth :refer [make-oauth-creds]]
            [twitter.api.restful :as twitter-rest]))

;;;; Sources - Containers for information needed by the Fetcher / Live
;;;; to retrieve Items

(defprotocol Source
  (source-type [this]))

(defn source? [src]
  (satisfies? Source src))

;;; Http

(def +http-default-args+
  {:user-agent :default})

(def +feed-default-args+
  {:user-agent :default
   :deep? false
   :force-update? false})

(spec/def :irq0-src-args/user-agent (spec/or :default keyword?
                                             :custom string?))
(spec/def :irq0-src-args/deep? boolean?)
(spec/def :irq0-src-args/force-update? boolean?)

(defrecord GenericWebsite [url args]
  Source
  (source-type [_] ::fetch)

  Object
  (toString [src] (str "[GenericWebsite: " (:url src) "]")))

(defn website
  [url & {:as args}]
  {:pre [(spec/valid? :irq0/url-str url)
         (spec/valid? (spec/or :none nil?
                               :args (spec/keys :opt-un [:irq0-src-args/user-agent])) args)]
   :post [(spec/valid? source? %)]}
  (GenericWebsite. (uri/uri url) (merge +http-default-args+ args)))

(defrecord Custom [id fn]
  Source
  (source-type [_] ::fetch)

  Object
  (toString [src] (str "[Custom: " (name (:id src)) "]")))

(defn custom
  [id fn]
  {:pre [(spec/valid? keyword? id)
         (spec/valid? fn? fn)]
   :post [(spec/valid? source? %)]}
  (Custom. id fn))

(defrecord PaywalledWebsite
           [url cookie-getter args]
  Source
  (source-type [_] ::fetch)

  Object
  (toString [src] (str "[PaywalledWebsite: " (:url src) "]")))

(defn website+paywall
  [url
   cookie-getter
   & {:as args}]
  {:pre [(spec/valid? :irq0/url-str url)
         (spec/valid? fn? cookie-getter)
         (spec/valid? (spec/or :none nil?
                               :args (spec/keys :opt-un [:irq0-src-args/user-agent])) args)]
   :post [(spec/valid? source? %)]}
  (PaywalledWebsite. (uri/uri url) cookie-getter (merge +http-default-args+ args)))

;;; Feed

(defrecord Feed
           [url args]
  Source
  (source-type [_] ::fetch)

  Object
  (toString [src] (str "[Feed: " (:url src) "]")))

(defn feed
  [url & {:as args}]
  {:pre [(spec/valid? :irq0/url-str url)
         (spec/valid? (spec/or
                       :none nil?
                       :args (spec/keys :opt-un [:irq0-src-args/user-agent :irq0-src-args/deep? :irq0-src-args/force-update?])) args)]
   :post [(spec/valid? source? %)]}
  (->Feed (uri/uri url) (merge +feed-default-args+ args)))

(defrecord SelectorFeed
           [url selectors extractors args]
  Source
  (source-type [_] ::fetch)

  Object
  (toString [src] (str "[SelectorFeed: " (:url src) "]")))

(spec/def :irq0-src-selectors/urls fn?)
(spec/def :irq0-src-selectors/ts fn?)
(spec/def :irq0-src-selectors/title fn?)
(spec/def :irq0-src-selectors/author fn?)
(spec/def :irq0-src-selectors/content fn?)
(spec/def :irq0-src-selectors/description fn?)
(spec/def :irq0-src-selectors/selectors
  (spec/keys
   :req-un [:irq0-src-selectors/urls]
   :opt-un [:irq0-src-selectors/ts
            :irq0-src-selectors/title
            :irq0-src-selectors/author
            :irq0-src-selectors/content
            :irq0-src-selectors/description]))
(spec/def :irq0-src-selectors/extractors
  (spec/keys
   :opt-un [:irq0-src-selectors/urls
            :irq0-src-selectors/ts
            :irq0-src-selectors/title
            :irq0-src-selectors/author
            :irq0-src-selectors/content
            :irq0-src-selectors/description]))

(defn selector-feed
  [url selectors extractors args]
  {:pre [(spec/valid? :irq0/url-str url)
         (spec/valid? :irq0-src-selectors/selectors selectors)
         (spec/valid? :irq0-src-selectors/extractors extractors)
         (spec/valid? (spec/keys :opt-un [:irq0-src-args/user-agent :irq0-src-args/deep? :irq0-src-args/force-update?]) args)]
   :post [(spec/valid? source? %)]}
  (->SelectorFeed (uri/uri url) selectors extractors (merge +http-default-args+ args)))

(defrecord WordpressJsonFeed [url args]
  Source
  (source-type [_] ::fetch)

  Object
  (toString [src] (str "[WpJsonFeed: " (:url src) "]")))

(defn wp-json
  [url & {:as args}]
  {:pre [(spec/valid? :irq0/url-str url)
         (spec/valid? (spec/or :none nil?
                               :args (spec/keys :opt-un [:irq0-src-args/user-agent :irq0-src-args/deep? :irq0-src-args/force-update?])) args)]
   :post [(spec/valid? source? %)]}
  (->WordpressJsonFeed (uri/uri url) (merge +http-default-args+ args)))

(spec/def :irq0-src-twitter/credentials
  (spec/keys
   :req-un [:irq0-src-twitter/app-key
            :irq0-src-twitter/app-secret
            :irq0-src-twitter/user-token
            :irq0-src-twitter/user-token-secret]))

(defrecord TwitterSearch
           [query
            url
            oauth-creds]
  Source
  (source-type [_] ::fetch)
  Object
  (toString [src] (str "[TwitterSearch: " (:query src) "]")))

(defn twitter-search
  "Search twitter https://dev.twitter.com/rest/public/search"
  [query
   oauth-creds]
  {:pre [(spec/valid? string? query)
         (spec/valid? :irq0-src-twitter/credentials oauth-creds)]
   :post [(spec/valid? source? %)]}
  (->TwitterSearch
   query
   (uri/uri (str "https://twitter.com/search?q=" query))
   (make-oauth-creds
    (:app-key oauth-creds)
    (:app-secret oauth-creds)
    (:user-token oauth-creds)
    (:user-token-secret oauth-creds))))

(defrecord TwitterApi
           [api-fn
            params
            oauth-creds]
  Source
  (source-type [_] ::fetch)
  Object
  (toString [src] (format "[TwitterApi/%s: %s]" (:api-fn src) (:params src))))

(defn twitter-timeline
  [oauth-creds]
  {:pre [(spec/valid? :irq0-src-twitter/credentials oauth-creds)]
   :post [(spec/valid? source? %)]}
  (->TwitterApi
   #_:clj-kondo/ignore
   twitter-rest/statuses-home-timeline
   {:count 200}
   (make-oauth-creds
    (:app-key oauth-creds)
    (:app-secret oauth-creds)
    (:user-token oauth-creds)
    (:user-token-secret oauth-creds))))

(defrecord MercuryWebParser
           [url]
  Source
  (source-type [_] ::fetch)
  Object
  (toString [src] (str "[MercuryWebParser: " (:url src) "]")))

(defn mercury
  "Fetch URL using Mercury Web Parser API"
  [url]
  {:pre [(spec/valid? :irq0/url-str url)]
   :post [(spec/valid? source? %)]}
  (->MercuryWebParser
   (uri/uri url)))

(def reddit-supported-listings #{:controversial :best :hot :new :random :rising :top})
(def reddit-supported-timeframes #{:hour :day :week :month :year :all})

(defrecord Reddit
           [subreddit
            listing
            timeframe]
  Source
  (source-type [_] ::fetch)
  Object
  (toString [src] (format "[Reddit: r/%s/%s/%s]" (:subreddit src) (:listing src) (:timeframe src))))

(defn reddit
  "Fetch URL using Mercury Web Parser API"
  ([subreddit
    listing]
   (reddit subreddit listing :week))
  ([subreddit
    listing
    timeframe]
   {:pre [(spec/valid? string? subreddit)
          (spec/valid? reddit-supported-listings listing)
          (spec/valid? reddit-supported-timeframes timeframe)]
    :post [(spec/valid? source? %)]}
   (when-not (contains? reddit-supported-listings listing)
     (throw+ {:type ::invalid-reddit-listing :supported reddit-supported-listings}))
   (when-not (contains? reddit-supported-timeframes timeframe)
     (throw+ {:type ::invalid-reddit-timeframe :supported reddit-supported-timeframes}))
   (->Reddit
    subreddit
    (name listing)
    (name timeframe))))

(spec/def :irq0-src-mailbox/credentials
  (spec/keys
   :req-un [:irq0-src-mailbox/username
            :irq0-src-mailbox/password]))

(defrecord ImapMailbox
           [url
            creds]
  Source
  (source-type [_] ::fetch)

  Object
  (toString [src] (format "[IMAP: %s]" (:uri src))))

(defn imap
  "Fetch from IMAP Mailbox"
  [url-str
   creds]
  {:pre [(spec/valid? string? url-str)
         (spec/valid? :irq0-src-mailbox/credentials creds)]
   :post [(spec/valid? source? %)]}
  (let [uri (uri/uri url-str)]
    (when-not (#{"imap" "imaps"} (uri/scheme uri))
      (throw+ {:type ::invalid-protocol :uri uri}))
    (when (some? (uri/query uri))
      (throw+ {:type ::query-string-unsupported :uri uri}))
    (->ImapMailbox
     uri
     creds)))

(defrecord HackerNews
           [story-feed
            args]
  Source
  (source-type [_] ::live)
  Object
  (toString [src] (str "[HackerNews: " (:story-feed src) "(" (:state src) ")]")))

(defn hn
  [story-feed
   & {:keys [throttle-secs]}]
  {:pre [(spec/valid? string? story-feed)
         (spec/valid? (spec/or :default nil?
                               :value pos-int?) throttle-secs)]
   :post [(spec/valid? source? %)]}
  (->HackerNews story-feed {:throttle-secs (or throttle-secs 120)}))

(defn feed? [src]
  (some #(instance? % src) [Feed SelectorFeed WordpressJsonFeed Reddit]))

(defn twitter? [src]
  (some #(instance? % src) [TwitterSearch TwitterApi]))

(defn reddit? [src]
  (some #(instance? % src) [Reddit]))

(defn mailbox? [src]
  (some #(instance? % src) [ImapMailbox]))
