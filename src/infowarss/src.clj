(ns infowarss.src
  (:require [schema.core :as s]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [infowarss.schema :as schema]
            [slingshot.slingshot :refer [throw+ try+]]
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
    [url :- java.net.URL
     args :- {:user-agent (s/cond-pre s/Str s/Keyword)}]

  Object
  (toString [src] (str "[GenericWebsite: " (:url src) "]")))

(defn website
  [url
   & {:keys [user-agent] :as args}]
  (GenericWebsite. (io/as-url url) (merge +http-default-args+ args)))

(s/defrecord PaywalledWebsite
    [url :- java.net.URL
     cookie-getter :- s/Any
     args :- {:user-agent (s/cond-pre s/Str s/Keyword)}]
  Object
  (toString [src] (str "[PaywalledWebsite: " (:url src) "]")))

(defn website+paywall
  [url
   cookie-getter
   & {:keys [user-agent] :as args}]
  (PaywalledWebsite. (io/as-url url) cookie-getter (merge +http-default-args+ args)))

;;; Feed

(s/defrecord Feed
    [url :- java.net.URL
     args :- {:deep? s/Bool
              :force-update? s/Bool
              :user-agent (s/cond-pre s/Str s/Keyword)}]
  Object
  (toString [src] (str "[Feed: " (:url src) "]")))

(defn feed
  [url
   & {:keys [deep? force-update? user-agent]
      :as args}]
  (->Feed (io/as-url url) (merge +feed-default-args+ args)))

(s/defrecord SelectorFeed
    [url :- java.net.URL
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
  (->SelectorFeed (io/as-url url) selectors extractors (merge +http-default-args+ args)))

(s/defrecord WordpressJsonFeed
    [url :- java.net.URL
     args :- {:user-agent (s/cond-pre s/Str s/Keyword)}]
  Object
  (toString [src] (str "[WpJsonFeed: " (:url src) "]")))

(defn wp-json
  [url
   & {:keys [user-agent]
      :as args}]
  (->WordpressJsonFeed (io/as-url url) (merge +http-default-args+ args)))

(def TwitterCreds
  {:app-key schema/NotEmptyStr
   :app-secret schema/NotEmptyStr
   :user-token schema/NotEmptyStr
   :user-token-secret schema/NotEmptyStr})

(s/defrecord TwitterSearch
    [query :- schema/NotEmptyStr
     url :- java.net.URL
     oauth-creds :- OauthCredentials]
  Object
  (toString [src] (str "[TwitterSearch: " (:query src) "]")))

(s/defn twitter-search :- TwitterSearch
  "Search twitter https://dev.twitter.com/rest/public/search"
  [query :- schema/NotEmptyStr
   oauth-creds :- TwitterCreds]
  (->TwitterSearch
    query
    (io/as-url (str "https://twitter.com/search?q=" query))
    (make-oauth-creds
      (:app-key oauth-creds)
      (:app-secret oauth-creds)
      (:user-token oauth-creds)
      (:user-token-secret oauth-creds))))

(s/defrecord TwitterApi
    [api-fn :- schema/Func
     params :- s/Any
     url :- java.net.URL
     oauth-creds :- OauthCredentials]
  Object
  (toString [src] (format "[TwitterApi/%s: %s]" api-fn params)))

(s/defn twitter-timeline :- TwitterApi
  [oauth-creds]
  (->TwitterApi
    twitter-rest/statuses-home-timeline
    {:count 200}
    (io/as-url (str "https://twitter.com/irq0"))
    (make-oauth-creds
      (:app-key oauth-creds)
      (:app-secret oauth-creds)
      (:user-token oauth-creds)
      (:user-token-secret oauth-creds))))


(s/defrecord MercuryWebParser
    [url :- java.net.URL]
  Object
  (toString [src] (str "[MercuryWebParser: " (:url src) "]")))

(s/defn mercury :- MercuryWebParser
  "Fetch URL using Mercury Web Parser API"
  [url :- schema/NotEmptyStr]
  (->MercuryWebParser
    (io/as-url url)))

(s/defrecord Document
    [url :- java.net.URL]
  Object
  (toString [src] (str "[Document: " (:url src) "]")))

(s/defn doc :- Document
  "Fetch URL and process as document"
  [url :- schema/NotEmptyStr]
  (->Document (io/as-url url)))

(s/defrecord Reddit
    [subreddit :- schema/NotEmptyStr
     feed :- schema/NotEmptyStr]
  Object
  (toString [src] (format "[Reddit: r/%s/%s]" (:subreddit src) (:feed src))))

(s/defn reddit :- Reddit
  "Fetch URL using Mercury Web Parser API"
  [subreddit :- schema/NotEmptyStr
   feed :- s/Keyword]
  (let [supported #{:hot :top :rising :new}]
    (when-not (contains? supported feed)
      (throw+ {:type ::invalid-feed :supported supported}))
    (->Reddit
      subreddit
      (name feed))))

(s/defrecord ImapMailbox
    [uri :- java.net.URI
     creds :- {:username schema/NotEmptyStr
               :password schema/NotEmptyStr}]
  Object
  (toString [src] (format "[IMAP: %s]" (:uri src))))

(s/defn imap :- ImapMailbox
  "Fetch from IMAP Mailbox"
  [uri-str :- schema/NotEmptyStr
   creds :- {:username schema/NotEmptyStr
             :password schema/NotEmptyStr}]

  (let [uri (java.net.URI/create uri-str)]
    (when-not (#{"imap" "imaps"} (.getScheme uri))
      (throw+ {:type ::invalid-protocol :uri uri}))
    (when (some? (.getQuery uri))
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
