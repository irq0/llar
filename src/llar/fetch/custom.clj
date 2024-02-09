(ns llar.fetch.custom
  (:require
   [clojure.spec.alpha :as s]
   [llar.item]
   [llar.fetch :refer [FetchSource item-to-string make-item-hash make-meta]]
   [llar.persistency :refer [CouchItem]]
   [llar.postproc :refer [ItemProcessor]]))

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
  llar.src.Custom
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
