(ns u1f596.fetch.custom
  (:require
   [schema.core :as s]
   [u1f596.fetch :refer [FetchSource item-to-string make-item-hash make-meta]]
   [u1f596.persistency :refer [CouchItem]]
   [u1f596.postproc :refer [ItemProcessor]]
   [u1f596.schema :as schema]))

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
       (for [{:keys [summary entry _]} (fn)]
         (map->CustomItem
          {:meta (make-meta src)
           :summary summary
           :hash (make-item-hash (:title summary))
           :entry entry}))))))

(extend-protocol ItemProcessor
  CustomItem
  (post-process-item [item src _] (assoc item :foo src))
  (filter-item [_ _ _] false))

(extend-protocol CouchItem
  CustomItem
  (to-couch [item]
    (assoc item :type :custom)))
