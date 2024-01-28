(ns u1f596.fetch.custom
  (:require
   [clojure.spec.alpha :as s]
   [u1f596.item]
   [u1f596.fetch :refer [FetchSource item-to-string make-item-hash make-meta]]
   [u1f596.persistency :refer [CouchItem]]
   [u1f596.postproc :refer [ItemProcessor]]))

(defrecord CustomItem
           [meta
            summary
            entry
            hash]
  Object
  (toString [item] (item-to-string item)))

(defn make-custom-item [meta summary hash entry]
  {:pre [(s/valid? :irq0/item-metadata meta)
         (s/valid? :irq0/item-summary summary)
         (s/valid? :irq0/item-hash hash)]}
  (->CustomItem meta summary entry hash))

(extend-protocol FetchSource
  u1f596.src.Custom
  (fetch-source [src]
    (let [{:keys [fn]} src]
      (doall
       (for [{:keys [summary entry _]} (fn)]
         (make-custom-item
          (make-meta src)
          summary
          (make-item-hash (:title summary))
          entry))))))

(extend-protocol ItemProcessor
  CustomItem
  (post-process-item [item src _] (assoc item :foo src))
  (filter-item [_ _ _] false))

(extend-protocol CouchItem
  CustomItem
  (to-couch [item]
    (assoc item :type :custom)))
