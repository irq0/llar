(ns u1f596.fetch.custom
  (:require
   [u1f596.http :as http]
   [u1f596.fetch :refer [FetchSource item-to-string make-meta make-item-hash tag-items]]
   [u1f596.postproc :refer [ItemProcessor]]
   [u1f596.schema :as schema]
   [u1f596.converter :as conv]
   [u1f596.analysis :as analysis]
   [u1f596.persistency :refer [CouchItem]]
   [slingshot.slingshot :refer [try+]]
   [schema.core :as s]
   [clojure.tools.logging :as log]
   [clj-http.client :as http-client]
   [hickory.core :as hick]
   [hickory.render :as hick-r]
   [org.bovinegenius [exploding-fish :as uri]]))

(s/defrecord CustomItem
             [meta :- schema/Metadata
              summary :- schema/Summary
              entry :- s/Any
              hash :- schema/Hash]
  Object
  (toString [item] (item-to-string item)))

(extend-protocol FetchSource
  u1f596.src.Custom
  (fetch-source [src]
    (let [{:keys [fn]} src]
      (doall
       (for [{:keys [summary entry hash]} (fn)]
         (map->CustomItem
          {:meta (make-meta src)
           :summary summary
           :hash (make-item-hash (:title summary))
           :entry entry}))))))

(extend-protocol ItemProcessor
  CustomItem
  (post-process-item [item src state] (assoc item :foo src))
  (filter-item [item src state] false))

(extend-protocol CouchItem
  CustomItem
  (to-couch [item]
    (assoc item :type :custom)))
