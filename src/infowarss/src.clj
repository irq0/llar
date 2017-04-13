(ns infowarss.src
  (:require [schema.core :as s]
            [clojure.test :refer [function?]]
            [clojure.java.io :as io]
            ))

;; Http source

(s/defrecord Http
    [url :- java.net.URL
     title :- s/Str])

(s/defn http :- Http
  [url title]
  (Http. (io/as-url url) title))

;; Feed source

(s/defrecord Feed
    [url :- java.net.URL
     title :- s/Str])

(s/defn feed :- Feed
  [url title]
  (Feed. (io/as-url url) title))
