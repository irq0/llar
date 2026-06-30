(ns llar.db.search
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
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

(defn- refresh-source-stats [db]
  (jdbc/execute! db ["REFRESH MATERIALIZED VIEW CONCURRENTLY source_stats"]))

(def ^:private valid-search-syntaxes
  #{:web :plain :phrase :advanced})

(defn normalize-search-syntax [syntax]
  (cond
    (valid-search-syntaxes syntax) syntax
    (string? syntax) (normalize-search-syntax (keyword syntax))
    :else :web))

(extend-protocol DataStoreSearch
  PostgresqlDataStore

  (search
    ([this query {:keys [syntax with-source-key time-ago-period]}]
     (if (string/blank? query)
       []
       (sql/search-item
        this
        {:query query
         :syntax (name (normalize-search-syntax syntax))
         :source-key with-source-key
         :time-ago (when-not (nil? time-ago-period)
                     (time/minus (time/zoned-date-time) time-ago-period))})))
    ([this query]
     (if (string/blank? query)
       []
       (sql/search-item
        this
        {:query query
         :syntax (name (normalize-search-syntax nil))}))))

  (update-index! [this]
    (refresh-search-index this)
    (refresh-idf this)
    (try (refresh-source-stats this)
         (catch Exception e
           (log/warn e "Failed to refresh source_stats materialized view")))))

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
