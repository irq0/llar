(ns infowarss.persistency
  (:require
   [infowarss.db.modify :as db-mod]
   [infowarss.converter :as conv]
   [digest]
   [java-time :as time]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [cheshire.generate :refer [add-encoder]]
   [pantomime.mime :as pm]
   [clojure.string :as string]))

(add-encoder java.time.ZonedDateTime
             (fn [dt jg]
               (.writeString jg (time/format :iso-zoned-date-time dt))))

(add-encoder org.bovinegenius.exploding_fish.UniformResourceIdentifier
             (fn [dt jg]
               (.writeString jg (str dt))))

(defprotocol CouchItem
  (to-couch [item] "Convert item to database form"))

(defn store-items!
  "Store items (may be a mixture of different types). Pass overwrite? true to
  overwrite existing items"
  [mixed-items & {:keys [overwrite?]
                  :or {overwrite? false}
                  :as args}]
  ;; Each vector may contain multiple item types.
  ;; -> Group them by type and call the store method
  (let [by-type (group-by type mixed-items)]
    (when (>= (count mixed-items) 1)
      (log/debugf "Persisting %d items with types: %s"
                  (count mixed-items) (keys by-type))
      (doall
       (apply concat
              (for [[type items] by-type]
                (do (log/debugf "Persisting %s items" type)
                    (remove nil? (map #(db-mod/store-item-and-data!
                                        (to-couch %) :overwrite? overwrite?) items)))))))))
