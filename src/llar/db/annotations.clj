(ns llar.db.annotations
  (:require
   [llar.db.core]
   [llar.db.sql :as sql]
   [llar.persistency :refer [AnnotationPersistency]]
   [next.jdbc :as jdbc])
  (:import
   (llar.db.core PostgresqlDataStore)))

(extend-protocol
 AnnotationPersistency
  PostgresqlDataStore

  (get-annotations [this item-id]
    (sql/get-annotations-for-item this {:item-id item-id}))

  (create-annotation! [this item-id selector body]
    (jdbc/with-transaction [tx (:datasource this)]
      (let [ann (sql/create-annotation tx {:item-id item-id
                                           :selector selector
                                           :body body})
            cnt (:count (sql/count-annotations-for-item tx {:item-id item-id}))]
        (when (= cnt 1)
          (sql/ensure-tags tx {:tags [["has-annotations"]]})
          (sql/set-tags tx {:tags ["has-annotations"]
                            :where [(sql/tag-cond-by-id {:id item-id})]}))
        ann)))

  (delete-annotation! [this annotation-id]
    (jdbc/with-transaction [tx (:datasource this)]
      (if-let [{:keys [item_id]} (sql/get-annotation-by-id tx {:id annotation-id})]
        (do
          (sql/delete-annotation tx {:id annotation-id})
          (let [cnt (:count (sql/count-annotations-for-item tx {:item-id item_id}))]
            (when (zero? cnt)
              (sql/remove-tags tx {:tags ["has-annotations"]
                                   :where [(sql/tag-cond-by-id {:id item_id})]})))
          {:deleted annotation-id :item-id item_id})
        nil))))
