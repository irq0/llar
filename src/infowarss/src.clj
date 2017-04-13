(ns infowarss.src
  (:require [schema.core :as s]
            [clojure.test :refer [function?]]
            [clojure.java.io :as io]
            ))

(def Func
  (s/pred function?))

(def FuncList
  [Func])

(s/defrecord Http
    [url :- java.net.URL
     title :- s/Str
     postproc :- FuncList])

(s/defn http :- Http
  ([url title]
   (Http. (io/as-url url) title []))
  ([url title postproc]
   (Http. (io/as-url url) title postproc)))


(s/defrecord Feed
    [url :- java.net.URL
     title :- s/Str
     postproc :- FuncList])

(s/defn feed :- Feed
  ([url title]
   (Feed. (io/as-url url) title []))
  ([url title postproc]
   (Feed. (io/as-url url) title postproc)))
