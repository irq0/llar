(ns u1f596.store
  (:require
   [u1f596.converter :as conv]
   [u1f596.db.core :as db]
   [u1f596.persistency :as persistency]
   [u1f596.appconfig :as appconfig]
   [digest]
   [java-time :as time]
   [mount.core :refer [defstate]]
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.java.io :as io]
   [cheshire.generate :refer [add-encoder]]
   [clojure.string :as string]))

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
      (log/debugf "Persisting %d items with types: %s"
                  (count mixed-items) (keys by-type))
      (doall
       (apply concat
              (for [[type items] by-type]
                (do (log/debugf "Persisting %s items" type)
                    (remove nil? (map #(persistency/store-item! backend-db
                                                                (persistency/to-couch %) args) items)))))))))
