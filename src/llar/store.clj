(ns llar.store
  (:require
   [clojure.tools.logging :as log]
   [digest]
   [mount.core :refer [defstate]]
   [llar.appconfig :as appconfig]
   [llar.db.core :as db]
   [llar.persistency :as persistency]))

(defstate backend-db
  :start (db/make-postgresql-pooled-datastore
          (appconfig/postgresql-config :backend)))

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
      (log/debugf "persisting %d items with types (overwrite:%s): %s"
                  (count mixed-items) overwrite? (keys by-type))
      (doall
       (apply concat
              (for [[type items] by-type]
                (do (log/debugf "persisting %s items (overwrite:%s)" overwrite? type)
                    (remove nil? (map #(persistency/store-item! backend-db
                                                                (persistency/to-couch %) args) items)))))))))
