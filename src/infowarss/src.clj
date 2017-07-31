(ns infowarss.src
  (:require [schema.core :as s]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [infowarss.schema :as schema]
            [slingshot.slingshot :refer [throw+ try+]]
            [twitter.oauth :refer [make-oauth-creds]])
  (:import [twitter.oauth OauthCredentials]))

;;;; Sources - Containers for information needed by the Fetcher / Live
;;;; to retrieve Items

;;; Http

(s/defrecord Http
    [url :- java.net.URL]
  Object
  (toString [src] (str "[Http: " (:url src) "]")))

(s/defn http :- Http
  [url :- s/Str]
  (Http. (io/as-url url)))

;;; Feed

(s/defrecord Feed
    [url :- java.net.URL
     args :- {:deep s/Bool}]
  Object
  (toString [src] (str "[Feed: " (:url src) "]")))

(defn feed
  [url
   & {:keys [deep?]
      :or [deep? false]
      :as args}]
  (->Feed (io/as-url url) args))

;;; Twitter Search

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

(s/defrecord MercuryWebParser
    [url :- java.net.URL
     api-key :- schema/NotEmptyStr]
  Object
  (toString [src] (str "[MercuryWebParser: " (:url src) "]")))

(s/defn mercury :- MercuryWebParser
  "Fetch URL using Mercury Web Parser API"
  [url :- schema/NotEmptyStr
   api-key :- schema/NotEmptyStr]
  (->MercuryWebParser
    (io/as-url url)
    api-key))

(s/defrecord GooseArticleExtractor
    [url :- java.net.URL]
  Object
  (toString [src] (str "[GooseArticleExtractor: " (:url src) "]")))

(s/defn goose :- GooseArticleExtractor
  "Fetch URL using goose article extractor"
  [url :- schema/NotEmptyStr]
  (->GooseArticleExtractor
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
