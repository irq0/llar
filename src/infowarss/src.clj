(ns infowarss.src
  (:require [schema.core :as s]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [infowarss.schema :as schema]
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
