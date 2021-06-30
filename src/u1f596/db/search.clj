(ns u1f596.db.search
  (:require
   [u1f596.db.sql :as sql]
   [u1f596.persistency :refer [DataStoreSearch]]
   [u1f596.db.core]
   [digest]
   [java-time :as time]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojure.java.jdbc :as j]
   [mpg.core :as mpg]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [byte-streams :refer [to-byte-buffer]]
   [cheshire.core :refer :all]
   [mount.core :refer [defstate]]
   [org.bovinegenius [exploding-fish :as uri]]
   [hikari-cp.core :as hikari]
   [hugsql.core :as hugsql]
   [cheshire.generate :as json :refer [encode-str]])
  (:import (u1f596.db.core PostgresqlDataStore)))

(defn- refresh-search-index [db]
  (j/execute! db ["refresh materialized view search_index"]))

(defn- refresh-idf [db]
  (j/execute! db ["refresh materialized view idf_top_words"]))

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
  (rest (sql/saved-items-tf-idf
   db
   nil
   {}
   {:as-arrays? true
    :row-fn (fn [[id term-tf-idf]]
              (assoc (into {} term-tf-idf) "item_id" (double id)))})))


(defn saved-items-tf-idf-terms [db]
  (first (second (sql/saved-items-tf-idf-terms
                  db
                  nil
                  {}
                  {:as-arrays? true}))))
