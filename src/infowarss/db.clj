(ns infowarss.db
  (:require
   [infowarss.core :refer [config *srcs*]]
   [infowarss.converter :as converter]
   [infowarss.couchdb :as couch]
   [digest]
   [clj-http.client :as http]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojurewerkz.serialism.core :as s]
   [clojure.java.jdbc :as j]
   [clojure.java.io :as io]
   [mpg.core :as mpg]
   [clj-time.jdbc]
   [byte-streams :refer [to-byte-buffer to-string]]
   [cheshire.core :refer :all])
  (:import [java.sql SQLException]))

(def ^:dynamic *db*
  (merge {:dbtype "postgresql"}
    (:postgresql config)))

(mpg/patch {:default-map :hstore
            :datetime false
            :date false})

(defn kw->pgenum [kw]
  (let [type (some-> (namespace kw)
                 (string/replace "-" "_"))
        value (name kw)]
    (doto (org.postgresql.util.PGobject.)
      (.setType type)
      (.setValue value))))

(extend-type clojure.lang.Keyword
  clojure.java.jdbc/ISQLValue
  (sql-value [kw]
    (kw->pgenum kw)))

(def +schema-enums+
  "A set of all PostgreSQL enums in schema.sql. Used to convert
  enum-values back into Clojure keywords."
  #{"item_type"})

(extend-type String
  clojure.java.jdbc/IResultSetReadColumn
  (result-set-read-column [val rsmeta idx]
    (let [type (.getColumnTypeName rsmeta idx)]
      (if (contains? +schema-enums+ type)
        (keyword (string/replace type "_" "-") val)
        val))))

;;
;; transition code couchdb -> postgresql
;;

;; was part of fever api
(defn fever-item-id
  "Convert infowarss feed item id to fever compatible id"
  [id]
  ;; Our database keys are to long for the fever api
  ;; shorten to positive java long
  ;; couchdb sequential keys:
  ;; "Monotonically increasing ids with random increments. The first 26
  ;; hex characters are random, the last 6 increment in random amounts
  ;; until an overflow occurs. On overflow, the random prefix is
  ;; regenerated and the process starts over."
  (let [parts [(subs id 0 2)  (subs id 24 32)]]
    (Long/parseLong (string/join parts) 16)))

(defn fever-feed-id
  "Calculate pseudo id from database feed entry"
  [db-feed]
  (let [{:keys [source-name source-key]} db-feed
        source-key (if (keyword? source-key) (name source-key) source-key)]
    (when (or (string/blank? source-name)
            (string/blank? source-key))
      (throw+ {:type ::invalid-input :db-feed db-feed}))

    (let [data (string/join source-name source-key)
          hash (digest/sha-256 data)]
      (Long/parseUnsignedLong (subs hash 0 8) 16))))


(defn couch-to-sql [doc]
  (let [id (fever-item-id (:_id doc))]
    ["insert into items (id) values (?)" id]))

(defn legacy-feeds
  []
  (let [dbfs (couch/get-feeds)
        confs *srcs*]
    (into {}
      (for [dbf dbfs]
        [(get dbf :source-key)
         (merge dbf (get confs (get dbf :source-key)))]))))

;; row of legacy feeds
(defn couch-feed-to-sql-source [row]
  (let [key (name (:source-key row))]
    [(fever-feed-id row) (:source-name row) key (:first-fetch-ts row)
     (select-keys row [:title :url]) (keyword "item_type" (name (:type row)))]))

(defn import-couch-feeds []
  (j/with-db-transaction [t *db*]
    (j/insert-multi! t :sources [:id :name :key :created_ts :data :type]
      (->> (legacy-feeds) vals (map couch-feed-to-sql-source)))))


(defn source-id-by-key [key]
  (some-> (j/query *db* ["select id from sources where key=?" key])
    first
    :id))

(defn process-entry [entry]
  (-> entry
    (dissoc :nlp :contents :descriptions :thumbs)))

(defn couch-doc-to-sql-item [doc]
  (let [source-id (source-id-by-key (get-in doc [:meta :source-key]))
        row [(:hash doc)
             source-id
             (or (tc/from-string (get-in doc [:summary :ts])) (time/date-time 0))
             (get-in doc [:summary :title])
             (string/join "," (get-in doc [:entry :authors]))
             (fever-item-id (:_id doc))
             (keyword "item_type" (:type doc))
             (into {} (map (fn [x] [x nil]) (get-in doc [:meta :tags])))
             (process-entry (:entry doc))
             (or (get-in doc [:entry :nlp :nwords]) -1)
             (or (get-in doc [:entry :nlp :urls]) [])
             (or (get-in doc [:entry :nlp :names]) [])
             (or (get-in doc [:entry :nlp :nouns]) [])
             (or (get-in doc [:entry :nlp :verbs]) [])
             (or (get-in doc [:entry :nlp :top]) {})
             0]]
    row))

(defn couch-doc-att-row [item-id type [mime-type data]]
  (try+
    (when (and (some? data) (not (string/blank? data)))
      (if (re-find #"^text/.+" mime-type)
        [item-id mime-type data nil type]
        [item-id mime-type nil (to-byte-buffer data) type]))
    (catch Object _
      (log/error (:throwable &throw-context) "Unexpected error: "
        item-id type mime-type data))))


(defn couch-doc-att-to-sql [doc]
  (let [{:keys [contents descriptions thumbs]} (:entry doc)
        item-id (fever-item-id (:_id doc))

        cs (map (partial couch-doc-att-row item-id :item_data_type/content)
             contents)
        ds (map (partial couch-doc-att-row item-id :item_data_type/description)
             descriptions)
        ths (map (partial couch-doc-att-row item-id :item_data_type/thumbnail)
             thumbs)]

    (->> (concat cs ds ths)
      (remove nil?))))

(defn import-couch-items [ids]
  (j/with-db-transaction [t *db*]
    (log/info "Importing items: " (count ids))
    (doseq [part (partition 1000 ids)]
        (let [docs (pmap couch/get-document-with-attachments part)
              conv-items (pmap couch-doc-to-sql-item docs)
              conv-atts (apply concat (pmap couch-doc-att-to-sql docs))]
          (log/info "Importing: " (count docs) (count conv-atts))
          (j/insert-multi! t :items
            [:hash :source_id :ts :title :author :id :type :tags :entry :nlp_nwords :nlp_urls :nlp_names :nlp_nouns :nlp_verbs :nlp_top :revision]
            conv-items)
          (j/insert-multi! t :item_data
            [:item_id :mime_type :text :data :type]
            conv-atts)))))


;; (defn get-source-id-or-create [src]
;;   (some-> (j/query *db* ["select id from sources where key=?" key])
;;     first
;;     :id))

;; (defn add-document!
;;   "Add new document. Return document ID"
;;   [data]
;;   (let [source-id (source-id-by-key (get-in doc [:meta :source-key]))]

;;   [(:hash doc)
;;              source-id
;;              (or (tc/from-string (get-in doc [:summary :ts])) (time/date-time 0))
;;              (get-in doc [:summary :title])
;;              (fever-item-id (:_id doc))
;;              (keyword "item_type" (:type doc))
;;              (into {} (map (fn [x] [x nil]) (get-in doc [:meta :tags])))
;;              (into {} (filter (fn [[k v]] (or (string? v) (number? v))) (:entry doc)))
;;              (or (get-in doc [:entry :nlp :nwords]) -1)
;;              (or (get-in doc [:entry :nlp :urls]) [])
;;              (or (get-in doc [:entry :nlp :names]) [])
;;              (or (get-in doc [:entry :nlp :nouns]) [])
;;              (or (get-in doc [:entry :nlp :verbs]) [])
;;              (or (get-in doc [:entry :nlp :top]) {})
;;              0]
;;   (j/with-db-transaction [t *db*]

;; (defn change-document!
;;   "Set new version of document"
;;   [id rev params])

;; (defn swap-document!
;;   "Swap document: Get current version, apply function on it and save
;;   the result as the new version"
;;   [id f]
;;   (let [doc (get-document id)]
;;     (let [id (get doc :_id)
;;           rev (get doc :_rev)
;;           new-doc (f doc)]
;;       (change-document! id rev new-doc))))

;; (defn revert-document! [id to-rev]
;;   (let [to-doc (get-document id to-rev)
;;         latest-rev (get-latest-rev id)]
;;     (change-document! id latest-rev to-doc)))


(defn create-source [doc]
  (let [meta (get doc :meta)
        feed (get doc :feed)]
    (->
      (j/insert! *db* :sources
        {:name (-> meta :source-name)
         :key (-> meta :source-key name)
         :created_ts (time/now)
         :type (keyword "item_type" (name (:type doc)))
         :data (select-keys feed [:feed-type :language :title :url])})
      first)))

(defn get-source-for-doc [doc]
  (let [key (-> (get-in doc [:meta :source-key]) name)]
    (first (j/query *db* ["select * from sources where key = ?" key]))))

(defn get-or-create-source-for-doc [doc]
  (let [src (get-source-for-doc doc)]
    (if (nil? src)
      (create-source doc)
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

    (->> (concat cs ds ths)
      (remove nil?))))


(defn doc-to-sql-row [doc source-id]
  (let [entry (:entry doc)]
  {:hash (:hash doc)
   :source_id source-id
   :ts (or (get-in doc [:summary :ts]) (time/date-time 0))
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
    (j/with-db-transaction [t *db*]
      (binding [*db* t]
        (let [db-src (get-or-create-source-for-doc doc)
              entry (:entry doc)
              item (first
                     (j/insert! t :items
                       (doc-to-sql-row doc (:id db-src))))

              item-data-rows (log/spy (make-item-data-rows (:id item) doc))]
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
    (j/with-db-transaction [t *db*]
      (binding [*db* t]
        (let [db-src (get-or-create-source-for-doc doc)
              entry (:entry doc)
              ret (first
                     (j/update! t :items
                       (doc-to-sql-row doc (:id db-src))
                       ["hash = ?" (:hash doc)]))]
          (when (zero? ret)
            (throw+ {:type ::general :msg "first update failed" :ret ret :doc doc}))
          (let [item-id (:id (first (j/query t ["select items.id from items where hash = ?"
                                                (:hash doc)])))
                item-data-rows (make-item-data-rows item-id doc)]
            (log/info item-id)
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


(defn get-word-count-groups []
  (->>
    (j/query *db*
      [(str "select "
         "case "
         "when nlp_nwords > 0   and nlp_nwords <= 200 then 200 "
         "when nlp_nwords > 200 and nlp_nwords <= 400 then 400 "
         "when nlp_nwords > 400 and nlp_nwords <= 800 then 800 "
         "when nlp_nwords > 800 and nlp_nwords <= 1600 then 1600 "
         "when nlp_nwords > 1600 and nlp_nwords <= 3200 then 3200 "
         "when nlp_nwords > 3200 then 0 "
         "end as nword_groups, "
         "count(*) "
         "from items "
         "group by nword_groups "
         "order by nword_groups")]
      {:as-arrays? true})
    (drop 1)))

(defn get-tag-stats []
  (->>
    (j/query *db* (string/join " "
                    ["SELECT tag, SUM(count)"
                     "FROM ("
                     "SELECT COUNT(AKEYS(tags)), UNNEST(AKEYS(tags)) AS tag"
                     "FROM items"
                     "GROUP BY AKEYS(tags)) AS x GROUP BY tag"])
      {:as-arrays? true})
    (drop 1)))

(defn get-type-stats []
  (j/query *db* ["select count(*), type from items group by type"]))



(defn get-sources []
  (into {}
    (for [src (j/query *db* "select * from sources")
          :let [key (keyword (:key src))]]
      [key
       (-> src
         (assoc :key key)
         (merge (into {}
                  (map (fn [[k v]] [(keyword (subs k 1)) v]) (:data src))))
         (dissoc :data))])))

(defn get-sources-item-tags []
  (into {}
    (j/query *db* (string/join " "
                  '(
                    "SELECT key, array_agg(DISTINCT source_tags) AS \"item-tags\""
                    "FROM ( "
                    "SELECT sources.id AS id, key, UNNEST(AKEYS(items.tags)) AS source_tags"
                    "FROM sources INNER JOIN items"
                    "ON sources.id = items.source_id"
                    "GROUP BY sources.id, AKEYS(items.tags)) AS x"
                    "GROUP BY key"))
      {:row-fn (fn [row] [(keyword (:key row))
                          {:item-tags (apply hash-set
                                        (map keyword (:item-tags row)))}])})))


(defn get-sources-item-tags-with-count []
  (into {}
    (j/query *db* "select key, json_object_agg(tag, count) as \"item-tags\" from (select key, count(*), skeys(tags) as tag from sources inner join items on sources.id = items.source_id group by key, skeys(tags) UNION select key, count(*), 'total' as tag from sources inner join items on sources.id = items.source_id group by key UNION  select key, count(*), 'today' as tag from sources inner join items on sources.id = items.source_id where date(ts) = current_date and exist_inline(tags,'unread') group by key UNION select key, count(*), 'new' as tag from sources inner join items on sources.id = items.source_id where akeys(tags) = ARRAY['unread'] group by key) as A group by key"
      {:row-fn (fn [{:keys [key item-tags]}]
                 [(keyword key) {:item-tags item-tags}])})))

(defn sources-merge-in-config [db-sources]
  (merge-with into db-sources *srcs*))

(defn sources-merge-in-item-tags [db-sources]
  (merge-with merge db-sources (get-sources-item-tags)))

(defn sources-merge-in-item-tags-with-count [db-sources]
  (merge-with merge db-sources (get-sources-item-tags-with-count)))

(defn get-items-by-tag [tag]
  (j/query *db* ["select * from items where exist_inline(tags, ?)" (name tag)]))

(defn get-item-ids-by-tag [tag]
  (j/query *db* ["select id from items where exist_inline(tags, ?)" (name tag)]
    {:row-fn :id}))

(defn item-set-tags [id & tags]
  (let [pairs (->> tags
                (mapv (fn [tag]
                        (format "['%s', NULL]" (name tag)))))
        sql-tags (str "ARRAY[" (string/join "," pairs) "]")
        sql (str "update items set tags = tags || hstore(" sql-tags ") where id = ?")]
    (try+
      (first (j/execute! *db* [sql id]))
      (catch org.postgresql.util.PSQLException _
        (log/error (:throwable &throw-context) "SQL failed")
        (throw+ {:type ::general :sql sql :id id :tags tags})))))

(defn item-set-all-tags-for-source [source-id & tags]
  (let [pairs (->> tags
                (mapv (fn [tag]
                        (format "['%s', NULL]" (name tag)))))
        sql-tags (str "ARRAY[" (string/join "," pairs) "]")
        sql (str "update items set tags = tags || hstore(" sql-tags ") where source_id = ?")]
    (try+
      (first (j/execute! *db* [sql source-id]))
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
      (first (j/execute! *db* [sql id]))
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
      (j/execute! *db* [sql source-id])

      (catch org.postgresql.util.PSQLException _
        (log/error (:throwable &throw-context) "SQL failed")
        (throw+ {:type ::general :sql sql :source-id source-id :tags tags})))))


(defn process-items-row [row]
  (if (some? (:data_types row))
    (-> row
      (assoc :data
        (->>
          (map (fn [t m text bin]
                 [t {m (or text (converter/bytea-hex-to-byte-array bin))}])
            (map keyword (:data_types row))
            (:mime_types row)
            (:text row)
            (:bin-data row))
          (group-by first)
          (map (fn [[k v]]
                 [k (apply merge (map second v))]))
          (into {})))
      (dissoc :data_types :bin-data :mime_types :text))
    row))

(defn process-items-row-fever [row]
  (-> row
    (assoc :data
      (->>
        (map (fn [t m d]
               [t {m d}])
          (map keyword (:data_types row))
          (:mime_types row)
          (:text row))
        (group-by first)
        (map (fn [[k v]]
               [k (apply merge (map second v))]))
        (into {})))
    (update :created_on_time converter/to-fever-timestamp)
    (update :url str)
    (dissoc :data_types :mime_types :text)))

(def get-items-by-id-fever-base-sql
  (str
    "SELECT items.id, items.source_id as feed_id, title, "
    "author, "
    "entry->'url' as url, "
    "exist_inline(tags, 'saved')::int as is_saved, "
    "(not exist_inline(tags, 'unread'))::int as is_read, "
    "ts as created_on_time, "
    "json_agg(mime_type) as mime_types, "
    "json_agg(item_data.type) as data_types, "
    "json_agg(item_data.text) as text "
    "FROM items "
    "INNER JOIN sources ON items.source_id = sources.id "
    "LEFT JOIN item_data ON items.id = item_data.item_id "
    "WHERE "
    "(item_data.type = 'content' "
    "  OR item_data.type = 'description') "))

(def +get-items-by-id-sql-select+
  (str
    "SELECT items.source_id as feed_id, title, "
    "author, "
    "entry->'url' as url, "
    "entry, "
    "exist_inline(tags, 'saved') as saved, "
    "(not exist_inline(tags, 'unread')) as read, "
    "akeys(tags) as tags, "
    "ts, "
    "nlp_names as names, "
    "nlp_verbs as verbs, "
    "nlp_top as \"top-words\", "
    "nlp_urls as urls, "
    "items.type, "
    "nlp_nwords as nwords, "
    "max(sources.key) as \"source-key\", "))

(def +get-items-by-id-sql-join-where+
  (str
    "FROM items "
    "INNER JOIN sources ON items.source_id = sources.id "
    "LEFT JOIN item_data ON items.id = item_data.item_id "
    "WHERE "
    "(item_data.type = 'content' "
    "  OR item_data.type = 'description') "))

(def +get-items-by-id-sql-select-data+
  (str
    "json_agg(mime_type) as mime_types, "
    "json_agg(item_data.type) as data_types, "
    "json_agg(item_data.text) as text, "
    "json_agg(item_data.data) as \"bin-data\", "))

(defn get-items-by-id-sql
  ([] (get-items-by-id-sql "" ""))
  ([extra-select extra-where]
   (str
    +get-items-by-id-sql-select+
    extra-select
    "items.id "
    " "
    +get-items-by-id-sql-join-where+
    " "
    extra-where)))


(defn get-items-by-id-range-fever
  [since max & {:keys [limit] :or {limit "ALL"}}]
  (j/query *db*
    (->
     [(str get-items-by-id-fever-base-sql
       "AND "
       (cond
         (and (number? since) (number? max))
         "items.id >= ? and items.id < ?"
         (and (number? since) (nil? max))
         "items.id >= ?"
         (and (nil? since) (number? max))
          "items.id < ?"
         (and (nil? since) (nil? max))
          "items.id >= 0")
       "  "
       "GROUP BY items.id "
       "ORDER BY items.id "
       "LIMIT " limit)]
     (concat
       (cond
         (and (number? since) (number? max))
         [since max]
         (and (number? since) (nil? max))
         [since]
         (and (nil? since) (number? max))
         [max])))
    {:row-fn process-items-row-fever}))


(defn get-items-by-id-fever
  [ids & {:keys [limit] :or {limit "ALL"}}]
  (let [sql-arr (str "ARRAY[" (string/join "," ids) "]")]
    (j/query *db* [(str get-items-by-id-fever-base-sql
                     " AND items.id = ANY(" sql-arr ") "
                     "GROUP BY items.id "
                     "LIMIT " limit)]
      {:row-fn process-items-row-fever})))

(defn get-items-by-id
  [ids & {:keys [limit with-data?] :or {limit "ALL" with-data? true}}]
  (let [sql-arr (str "ARRAY[" (string/join "," ids) "]")]
    (j/query *db* [(get-items-by-id-sql
                   (if with-data?
                     +get-items-by-id-sql-select-data+
                     "")
                     (str
                       " AND items.id = ANY(" sql-arr ") "
                       " GROUP BY items.id "
                       " LIMIT " limit))]
      {:row-fn process-items-row})))

(defn get-items-by-source-key
  [source-key & {:keys [limit with-tag with-type with-data?]
                 :or {limit "ALL"}}]
  (j/query *db* [(get-items-by-id-sql
                   (if with-data?
                     +get-items-by-id-sql-select-data+
                     "")
                   (str
                     " AND sources.key = ? "
                     (when (keyword? with-tag)
                       (str " AND exist_inline(items.tags,'" (name with-tag) "') "))
                     (when (keyword? with-type)
                       (str " AND items.type = '" (name with-type) "'"))
                     " GROUP BY items.id "
                     " LIMIT ?")) (name source-key) limit]
    {:row-fn process-items-row}))

(defn simple-filter-to-sql [kw]
  (case kw
    :unread "exist_inline(tags, 'unread')"
    :new "akeys(tags) = ARRAY['unread']"
    :today "date(ts) = current_date AND exist_inline(tags, 'unread')"
    "1 = 1"))

(defn get-items-recent
  "Get items sorted by timestamp - used by the reader gui"
  [{:keys [limit before with-tag with-type with-source-keys with-data? simple-filter]
    :or {limit "ALL" with-data? false}
    :as args}]
  (let [sql (get-items-by-id-sql
              (if with-data?
                +get-items-by-id-sql-select-data+
                "")
              (str
                (when (map? before)
                  " AND (items.ts, items.id) < (?, ?) ")
                (when (coll? with-source-keys)
                  (str " AND sources.key = ANY(ARRAY["
                    (->> with-source-keys
                      (map #(str "'" (name %) "'"))
                      (string/join ","))
                    "]) "))
                (when (keyword? simple-filter)
                  (str " AND " (simple-filter-to-sql simple-filter) " "))
                (when (keyword? with-tag)
                  (str " AND exist_inline(items.tags,'" (name with-tag) "') "))
                (when (keyword? with-type)
                  (str " AND items.type = '" (name with-type) "'"))
                " AND items.ts < now() "
                " GROUP BY items.id "
                " ORDER BY ts DESC, id DESC "
                " LIMIT ?"))]
    (try+
      (j/query *db*
        (remove nil? [sql
                      (when (:ts before) (:ts before))
                      (when (:id before) (:id before))
                      limit])
        {:row-fn process-items-row})
      (catch org.postgresql.util.PSQLException _
        (log/error (:throwable &throw-context) "SQL failed"
          sql)
        (throw+ {:type ::general
                 :msg (.getMessage (:throwable &throw-context))
                 :sql-state (.getSQLState (:throwable &throw-context))
                 :sql sql :args args})))))
