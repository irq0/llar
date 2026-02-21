(ns llar.db.search
  (:require
   [digest]
   [java-time.api :as time]
   [llar.db.core]
   [llar.db.sql :as sql]
   [llar.persistency :refer [DataStoreSearch]]
   [next.jdbc :as jdbc])
  (:import
   (llar.db.core PostgresqlDataStore)))

(defn- refresh-search-index [db]
  (jdbc/execute! db ["refresh materialized view search_index"]))

(defn- refresh-idf [db]
  (jdbc/execute! db ["refresh materialized view idf_top_words"]))

(extend-protocol DataStoreSearch
  PostgresqlDataStore

  (search
    ([this query {:keys [with-source-key time-ago-period]}]
     (sql/search-item
      this
      {:query query
       :source-key with-source-key
       :time-ago (when-not (nil? time-ago-period)
                   (time/minus (time/zoned-date-time) time-ago-period))}))
    ([this query]
     (sql/search-item
      this
      {:query query})))

  (update-index! [this]
    (refresh-search-index this)
    (refresh-idf this)))

(defn saved-items-tf-idf [db]
  (rest (map (fn [{:keys [id json_agg]}]
               (assoc (into {} json_agg) "item_id" (double id)))
             (sql/saved-items-tf-idf db))))

(defn saved-items-tf-idf-terms [db min-tf-idf]
  (->>
   (sql/saved-items-tf-idf-terms
    db
    {:min-tf-idf min-tf-idf})
   first
   :array_agg))
