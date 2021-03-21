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

(defn item-tf-idf [id]
  (j/query db [(str
                "select term_tf->>0 as term, (term_tf->>1)::float as tf, idf_top_words.ln as idf, (term_tf->>1)::float * idf_top_words.ln as tf_idf "
                " from "
                "(select id, jsonb_array_elements(nlp_top->'words') as term_tf from items where id = ?) as i "
                "inner join "
                "idf_top_words on (term_tf->0 = idf_top_words.term) "
                "order by tf_idf asc") id]))

;; TF = term frequence
;; IDF inverse document frequency

(defn top-idf [max]
  (j/query db ["select term, ln from idf_top_words limit ?" max]))

(defn recommendations
  "Find similar items for id"
  [_])
  ;; compute cosine distance to all docs in db

(defn saved-items-tf-idf []
  (rest (j/query db [(str "select "
                          "id, "
                          "json_agg(json_build_array(term_tf->>0 , "
                          " (term_tf->>1)::float * idf_top_words.ln )) "
                          " from "
                          "(select id, jsonb_array_elements(nlp_top->'words') as term_tf "
                          "from items "
                          "where exist_inline(tags, 'saved') or (items.type = 'bookmark' and exist_inline(tags, 'unread')) ) as i "
                          "inner join "
                          "idf_top_words on (term_tf->0 = idf_top_words.term) "
                          "group by id ")]
                 {:as-arrays? true
                  :row-fn (fn [[id term-tf-idf]]
                            (assoc (into {} term-tf-idf) "item_id" (double id)))})))

;; make adjustmens here to limit the number of words to use in k means


(defn saved-items-tf-idf-terms []
  (first (second (j/query db [(str "select array_agg(foo.term) from (select distinct "
                                   "term_tf->>0 as term, "
                                   "(term_tf->>1)::float * idf_top_words.ln as tf_idf "
                                   " from "
                                   "(select id, jsonb_array_elements(nlp_top->'words') as term_tf "
                                   "from items "
                                   "where exist_inline(tags, 'saved') or (items.type = 'bookmark') and exist_inline(tags, 'unread')) as i "
                                   "inner join "
                                   "idf_top_words on (term_tf->0 = idf_top_words.term) "
                                   "where (term_tf->>1)::float > 1 and length(term_tf->>0) > 4 and not (term_tf->>0) like '%/%'"
                                   ") as foo")]
                          {:as-arrays? true}))))

