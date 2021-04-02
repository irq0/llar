(ns infowarss.db.search
  (:require
   [infowarss.db.core :as core :refer [db]]
   [infowarss.db.sql :as sql]
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
   [cheshire.generate :as json :refer [encode-str]]))

(defn search
  ([query {:keys [with-source-key time-ago-period]}]
   (sql/search-item
    db
    {:query query
     :source-key with-source-key
     :time-ago (when-not (nil? time-ago-period)
                 (time/minus (time/zoned-date-time) time-ago-period))})
   )
  ([query]
   (search query {})))

(defn refresh-search-index []
  (j/execute! db ["refresh materialized view search_index"]))

(defn refresh-idf []
  (j/execute! db ["refresh materialized view idf_top_words"]))

(defn saved-items-tf-idf []
  (rest (sql/saved-items-tf-idf
   db
   nil
   {}
   {:as-arrays? true
    :row-fn (fn [[id term-tf-idf]]
              (assoc (into {} term-tf-idf) "item_id" (double id)))})))


(defn saved-items-tf-idf-terms []
  (first (second (sql/saved-items-tf-idf-terms
                  db
                  nil
                  {}
                  {:as-arrays? true}))))

