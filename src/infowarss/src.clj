(ns infowarss.src
  (:require [schema.core :as s]
            [clojure.test :refer [function?]]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [twitter.oauth :refer [make-oauth-creds]])
  (:import [twitter.oauth OauthCredentials]))

;; Http source

(def NotEmptyStr
  (s/constrained s/Str (complement string/blank?)))

(s/defrecord Http
    [url :- java.net.URL]
  Object
  (toString [src] (str "[Http: " (:url src) "]")))

(s/defn http :- Http
  [url :- s/Str]
  (Http. (io/as-url url)))

;; Feed source

(s/defrecord Feed
    [url :- java.net.URL]
  Object
  (toString [src] (str "[Feed: " (:url src) "]")))

(s/defn feed :- Feed
  [url :- s/Str]
  (->Feed (io/as-url url)))

;; Twitter search


(def TwitterCreds
  {:app-key NotEmptyStr
   :app-secret NotEmptyStr
   :user-token NotEmptyStr
   :user-token-secret NotEmptyStr})

(s/defrecord TwitterSearch
    [query :- NotEmptyStr
     url :- java.net.URL
     oauth-creds :- OauthCredentials]
  Object
  (toString [src] (str "[TwitterSearch: " (:query src) "]")))

(s/defn twitter-search :- TwitterSearch
  "Search twitter https://dev.twitter.com/rest/public/search"
  [query :- NotEmptyStr
   oauth-creds :- TwitterCreds]
  (->TwitterSearch
    query
    (io/as-url (str "https://twitter.com/search?q=" query))
    (make-oauth-creds
      (:app-key oauth-creds)
      (:app-secret oauth-creds)
      (:user-token oauth-creds)
      (:user-token-secret oauth-creds))))

;; Live sources

(s/defrecord HackerNews
    [story-feed :- NotEmptyStr
     state :- s/Any]
  Object
  (toString [src] (str "[HackerNews: " (:story-feed src) "(" (:state src) ")]")))


(s/defn hn :- HackerNews
  [story-feed]
  (->HackerNews story-feed (atom {})))
