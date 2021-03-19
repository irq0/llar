(ns infowarss.db.modify
  (:require
   [digest]
   [infowarss.db.sql :as sql]
   [infowarss.db.core :as core :refer [db]]
   [infowarss.db.query :as query]
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

(defn- create-source-from-doc [doc]
  (let [meta (get doc :meta)
        feed (get doc :feed)]
    (sql/create-source db {:name (-> meta :source-name)
                       :key (-> meta :source-key name)
                       :created_ts (time/zoned-date-time)
                       :type (keyword "item_type" (name (:type doc)))
                       :data (select-keys
                              feed [:feed-type :language :title :url])})))

(defn get-or-create-source-for-doc [doc]
  (let [src (query/get-source-for-doc doc)]
    (if (nil? src)
      (create-source-from-doc doc)
      src)))


(defn make-item-data-row [item-id att-type [mime-type data]]
  (try+
   (when (some? data)
     (if (re-find #"^text/.+" mime-type)
       (when (and (string? data) (not (string/blank? data)))
         [item-id mime-type data nil att-type])
       [item-id mime-type nil (to-byte-buffer data) att-type]))
   (catch Object _
     (log/error (:throwable &throw-context) "Unexpected error: "
                item-id att-type mime-type data)
     (throw+ {:type ::data-row-conversion-fail
              :item-id item-id
              :mime-type mime-type
              :att-type att-type
              :data {:type (type data)
                     :data :data}}))))

(defn make-item-data-rows [item-id doc]
  (let [{:keys [contents descriptions thumbs]} (:entry doc)

        cs (map (partial make-item-data-row item-id :item_data_type/content) contents)
        ds (map (partial make-item-data-row item-id :item_data_type/description) descriptions)
        ths (map (partial make-item-data-row item-id :item_data_type/thumbnail) thumbs)]

    (remove nil? (concat cs ds ths))))

(defn doc-to-sql-row [doc source-id]
  (let [entry (:entry doc)]
    {:hash (:hash doc)
     :source_id source-id
     :ts (or (get-in doc [:summary :ts]) (time/zoned-date-time 0))
     :title (get-in doc [:summary :title])
     :author (string/join "," (get-in doc [:entry :authors]))
     :type (keyword "item_type" (name (:type doc)))
     :tags (into {} (map (fn [x] [(name x) nil]) (get-in doc [:meta :tags])))
     :nlp_nwords (or (get-in doc [:entry :nlp :nwords]) -1)
     :nlp_urls (into [] (or (get-in doc [:entry :nlp :urls]) []))
     :nlp_names (into [] (or (get-in doc [:entry :nlp :names]) []))
     :nlp_nouns (into [] (or (get-in doc [:entry :nlp :nouns]) []))
     :nlp_verbs (into [] (or (get-in doc [:entry :nlp :verbs]) []))
     :nlp_top (or (get-in doc [:entry :nlp :top]) {})
     :revision 2
     :entry (-> entry
                (dissoc :nlp :contents :descriptions :thumbs))}))

(defn add-document [doc]
  (try+
   (j/with-db-transaction [t db]
     (binding [core/*db* t]
       (let [db-src (get-or-create-source-for-doc doc)
             item (first
                   (j/insert! t :items
                              (doc-to-sql-row doc (:id db-src))))

             item-data-rows (make-item-data-rows (:id item) doc)]
         (when (seq item-data-rows)
           (j/insert-multi! t :item_data
                            [:item_id :mime_type :text :data :type]
                            item-data-rows))
         item)))
   (catch java.lang.IllegalArgumentException _
     (log/error (:throwable &throw-context) "Add Document Failed - Programmer error :P"
                doc))
   (catch org.postgresql.util.PSQLException _
     (if (= (.getSQLState (:throwable &throw-context)) "23505")
       (throw+ {:type ::duplicate})
       (do
         (log/error (:throwable &throw-context) "Add Document SQL failed")
         (throw+ {:type ::general :doc doc}))))))

(defn inplace-update-document [doc]
  (try+
   (j/with-db-transaction [t db]
     (binding [core/*db* t]
       (let [db-src (get-or-create-source-for-doc doc)
             ret (first
                  (j/update! t :items
                             (doc-to-sql-row doc (:id db-src))
                             ["hash = ?" (:hash doc)]))]
         (when (zero? ret)
           (throw+ {:type ::general :msg "first update failed" :ret ret :doc doc}))
         (let [item-id (:id (first (j/query t ["select items.id from items where hash = ?"
                                               (:hash doc)])))
               item-data-rows (make-item-data-rows item-id doc)]
           (when (seq item-data-rows)
             (j/delete! t :item_data
                        ["item_id = ?" item-id])
             (j/insert-multi! t :item_data
                              [:item_id :mime_type :text :data :type]
                              item-data-rows))
           item-id))))
   (catch java.lang.IllegalArgumentException _
     (log/error (:throwable &throw-context) "Add Document Failed - Programmer error :P"
                doc))
   (catch org.postgresql.util.PSQLException _
     (log/error (:throwable &throw-context) "Add Document SQL failed")
     (throw+ {:type ::general :doc doc}))))



;;; -------------------------------------------

(defn item-set-tags [id & tags]
  (let [pairs (mapv (fn [tag]
                      (format "['%s', NULL]" (name tag)))
                    tags)
        sql-tags (str "ARRAY[" (string/join "," pairs) "]")
        sql (str "update items set tags = tags || hstore(" sql-tags ") where id = ?")]
    (try+
     (first (j/execute! db [sql id]))
     (catch org.postgresql.util.PSQLException _
       (log/error (:throwable &throw-context) "SQL failed")
       (throw+ {:type ::general :sql sql :id id :tags tags})))))

(defn item-set-all-tags-for-source [source-id & tags]
  (let [pairs (mapv (fn [tag]
                      (format "['%s', NULL]" (name tag))) tags)
        sql-tags (str "ARRAY[" (string/join "," pairs) "]")
        sql (str "update items set tags = tags || hstore(" sql-tags ") where source_id = ?")]
    (try+
     (first (j/execute! db [sql source-id]))
     (catch org.postgresql.util.PSQLException _
       (log/error (:throwable &throw-context) "SQL failed")
       (throw+ {:type ::general :sql sql :id source-id :tags tags})))))

(defn item-remove-tags [id & tags]
  (let [sql-tags (str "ARRAY[" (string/join ","
                                            (map (fn [tag] (str "'" (name tag) "'"))
                                                 tags))
                      "]")
        sql (str "update items set tags = delete(tags, " sql-tags ") where id = ?")]
    (try+
     (first (j/execute! db [sql id]))
     (catch org.postgresql.util.PSQLException _
       (log/error (:throwable &throw-context) "SQL failed")
       (throw+ {:type ::general :sql sql :id id :tags tags})))))

(defn item-remove-all-tags-for-source [source-id & tags]
  (let [sql-tags (str "ARRAY[" (string/join ","
                                            (map (fn [tag] (str "'" (name tag) "'"))
                                                 tags))
                      "]")
        sql (str "update items set tags = delete(tags, " sql-tags ") where source_id = ?")]
    (try+
     (j/execute! db [sql source-id])

     (catch org.postgresql.util.PSQLException _
       (log/error (:throwable &throw-context) "SQL failed")
       (throw+ {:type ::general :sql sql :source-id source-id :tags tags})))))

;; ---------------------------------------

(defn remove-unread-for-items-of-source-older-than [source-keys older-than-ts]
  (let [source-ids (j/query db [(format "select id from sources where key in (%s)"
                                        (->> source-keys
                                             (map name)
                                             (map #(str "'" % "'"))
                                             (string/join ", ")))]
                            {:row-fn :id})]
    (j/execute! db [(format "update items set tags = tags - 'unread'::text where source_id in (%s) and exist_inline(tags, 'unread') and ts <= ?"
                                     (->> source-ids (string/join ", ")))
                    older-than-ts])))
