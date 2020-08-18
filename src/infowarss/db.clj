(ns infowarss.db
  (:require
   [infowarss.core :refer [config *srcs*]]
   [infowarss.converter :as converter]
   [digest]
   [java-time :as time]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojure.java.jdbc :as j]
   [mpg.core :as mpg]
   [byte-streams :refer [to-byte-buffer]]
   [cheshire.core :refer :all]
   [mount.core :refer [defstate]]
   [cheshire.generate :refer [add-encoder encode-str]])

  (:import [com.mchange.v2.c3p0 ComboPooledDataSource]))

(def ^:dynamic *db*
  (merge {:dbtype "postgresql"}
         (:postgresql config)))

(mpg/patch {:default-map :hstore})

(defmacro with-log-exec-time [& body]
  `(let [start# (java.lang.System/nanoTime)
         result# (do ~@body)
         fin# (java.lang.System/nanoTime)
         elasped# (- fin# start#)
         elasped-sec# (/ elasped# 1000000)]
     (log/debugf "%s: %.2fms" (quote ~@body) (float elasped-sec#))
     result#))

(defstate db
  :start {:datasource (hikari/make-datasource config)})

(defn check-connectivity []
  (log/infof "Database ready. %s items, %s sources"
             (:count (first (j/query db ["select count(*) from items"])))
             (:count (first (j/query db ["select count(*) from sources"])))))

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

(extend-protocol j/ISQLValue
  org.bovinegenius.exploding_fish.Uri
  (sql-value [url]
    (str url)))
  
(extend-protocol j/ISQLValue
  org.bovinegenius.exploding_fish.UniformResourceIdentifier
  (sql-value [url]
    (str url)))

;; (extend-protocol j/ISQLValue
;;   java.time.ZonedDateTime
;;   (sql-value [v]
;;     (time/sql-timestamp v)))

;; (extend-protocol j/IResultSetReadColumn
;;   java.sql.Timestamp
;;   (result-set-read-column [col _ _]
;;     (tc/from-sql-time col)))

(json/add-encoder org.bovinegenius.exploding_fish.Uri encode-str)

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

(defn bytea-hex-to-byte-array [bytea]
  (byte-array
   (map (fn [[a b]] (Integer/parseInt (str a b) 16))
        (drop 1 (partition 2 bytea)))))

(defn to-fever-timestamp
  "Convert time object to fever unix timestamp"
  [time]
  (try+
   (-> time
       time/to-millis-from-epoch
       (/ 1000)
       (.longValue)
       (max 0))
   (catch Object _
     0)))



(defmacro defquery [name sql & opts]
  `(def ~name
     (fn []
       (let [start# (java.lang.System/nanoTime)
             result# (do (j/query db ~sql ~@opts))
             fin# (java.lang.System/nanoTime)
             elasped# (- fin# start#)
             elasped-sec# (/ elasped# 1000000)]
         (log/debugf "%s: %.2fms" ~name (float elasped-sec#))
         result#))))

(defn create-source [doc]
  (let [meta (get doc :meta)
        feed (get doc :feed)]
    (first (j/insert! db :sources
                      {:name (-> meta :source-name)
                       :key (-> meta :source-key name)
                       :created_ts (time/zoned-date-time)
                       :type (keyword "item_type" (name (:type doc)))
                       :data (select-keys
                              feed [:feed-type :language :title :url])}))))

(defn simple-filter-to-sql [kw]
  (case kw
    :unread "exist_inline(tags, 'unread')"
    :today "date(ts) = current_date AND exist_inline(tags, 'unread')"
    "1 = 1"))

(defn get-source-for-doc [doc]
  (let [key (name (get-in doc [:meta :source-key]))]
    (first (j/query db ["select * from sources where key = ?" key]))))

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
   (j/with-db-transaction [t *db*]
     (binding [*db* t]
       (let [db-src (get-or-create-source-for-doc doc)
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

(defn get-word-count-groups []
  (drop 1
        (j/query db
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
                 {:as-arrays? true})))

(defn get-tag-stats []
  (drop 1
        (j/query db
                 (string/join
                  " "
                  ["SELECT tag, SUM(count)"
                   "FROM ("
                   "SELECT COUNT(AKEYS(tags)), UNNEST(AKEYS(tags)) AS tag"
                   "FROM items"
                   "GROUP BY AKEYS(tags)) AS x GROUP BY tag"])
                 {:as-arrays? true})))

(defn get-type-stats []
  (j/query db ["select count(*), type from items group by type"]))

(defn get-sources []
  (into {}
        (for [src (j/query db "select * from sources")
              :let [key (keyword (:key src))]]
          [key
           (-> src
               (assoc :key key)
               (merge (into {}
                            (map (fn [[k v]] [(keyword (subs k 1)) v]) (:data src))))
               (dissoc :data))])))

(defn get-sources-item-tags []
  (into {}
        (j/query db (string/join " ")
                 '("SELECT key, array_agg(DISTINCT source_tags) AS \"item-tags\""
                   "FROM ( "
                   "SELECT sources.id AS id, key, UNNEST(AKEYS(items.tags)) AS source_tags"
                   "FROM sources INNER JOIN items"
                   "ON sources.id = items.source_id"
                   "GROUP BY sources.id, AKEYS(items.tags)) AS x"
                   "GROUP BY key")
                 {:row-fn (fn [row] [(keyword (:key row))]
                            {:item-tags (apply hash-set
                                               (map keyword (:item-tags row)))})})))

(defn new-get-sources []
  (into {}
        (j/query db [(str "SELECT name, key, created_ts, id, type, "
                          "  data->':title' as title "
                          "FROM sources")]
                 {:row-fn (fn [row]
                            (let [key (keyword (:key row))]
                              [key
                               (-> row
                                   (assoc :key key)
                                   (merge (get *srcs* key)))]))})))

(defn new-merge-in-tags-counts [sources]
  (pmap (fn [source]
          (let [id (:id source)
                total (:count
                       (first
                        (j/query db ["SELECT COUNT(*) FROM items WHERE source_id = ?" id])))
                per-tag (j/query db
                                 [(str
                                   "SELECT COUNT(*), SKEYS(tags) as tag "
                                   "  FROM items "
                                   "  WHERE source_id = ? "
                                   "  GROUP BY SKEYS(tags) ") id]
                                 {:row-fn (fn [{:keys [tag count]}]
                                            [(keyword tag) count])})
                today (:count
                       (first
                        (j/query db
                                 [(str
                                   "SELECT COUNT(*) FROM items "
                                   "  WHERE source_id = ? "
                                   "  AND date(ts) = current_date "
                                   "AND exist_inline(tags,'unread')")
                                  id])))]
            (merge source
                   {:item-tags (merge (into {} per-tag)
                                      {:today today
                                       :total total})})))
        sources))

(defn new-get-sources-item-tags-counts [item-tag simple-filter]
  (j/query db [(str "SELECT DISTINCT ON (key) key, "
                    "  name as title, key, created_ts, sources.id, sources.type "
                    "FROM sources "
                    "INNER JOIN items "
                    "ON sources.id = items.source_id "
                    "WHERE exist_inline(items.tags, '" (name item-tag) "')"
                    "AND " (simple-filter-to-sql simple-filter))]
           {:row-fn (fn [row]
                      (let [key (keyword (:key row))]
                        (-> row
                            (assoc :key key)
                            (merge (get *srcs* key)))))}))

(defn get-sources-item-tags-with-count []
  (into {}
        (j/query db "select key, json_object_agg(tag, count) as \"item-tags\" from (select key, count(*), skeys(tags) as tag from sources inner join items on sources.id = items.source_id group by key, skeys(tags) UNION select key, count(*), 'total' as tag from sources inner join items on sources.id = items.source_id group by key UNION  select key, count(*), 'today' as tag from sources inner join items on sources.id = items.source_id where date(ts) = current_date and exist_inline(tags,'unread') group by key UNION select key, count(*), 'new' as tag from sources inner join items on sources.id = items.source_id where akeys(tags) = ARRAY['unread'] group by key) as A group by key"
                 {:row-fn (fn [{:keys [key item-tags]}]
                            [(keyword key) {:item-tags item-tags}])})))

(defn sources-merge-in-config [db-sources]
  (merge-with into db-sources *srcs*))

(defn sources-merge-in-item-tags [db-sources]
  (merge-with merge db-sources (get-sources-item-tags)))

(defn sources-merge-in-item-tags-with-count [db-sources]
  (merge-with merge db-sources (get-sources-item-tags-with-count)))

(defn get-items-by-tag [tag]
  (j/query db ["select key, title, author, items.type, tags, items.id, entry from items inner join sources on items.source_id = sources.id where exist_inline(tags, ?)" (name tag)]))

(defn get-item-ids-by-tag [tag]
  (j/query db ["select id from items where exist_inline(tags, ?)" (name tag)]
           {:row-fn :id}))

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
      (update :created_on_time to-fever-timestamp)
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
   "sources.key as \"source-key\", "))

(def +get-items-by-id-sql-select-with-data+
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

(def +get-items-by-id-sql-join-with-data-where+
  (str
   "FROM items "
   "INNER JOIN sources ON items.source_id = sources.id "
   "LEFT JOIN item_data ON items.id = item_data.item_id "
   "WHERE "
   "(item_data.type = 'content' "
   "  OR item_data.type = 'description') "))

(def +get-items-by-id-sql-join-with-preview-data-where+
  (str
   "FROM items "
   "INNER JOIN sources ON items.source_id = sources.id "
   "LEFT JOIN item_data ON items.id = item_data.item_id "
   " AND item_data.type = 'description' "
   " AND mime_type = 'text/plain' "
   "WHERE "
   " 1 = 1 "))

(def +get-items-by-id-sql-join-without-data-where+
  (str
   "FROM items "
   "INNER JOIN sources ON items.source_id = sources.id "
   "WHERE 1=1"))

(def +get-items-by-id-sql-select-data+
  (str
   "json_agg(mime_type) as mime_types, "
   "json_agg(item_data.type) as data_types, "
   "json_agg(item_data.text) as text, "
   "json_agg(item_data.data) as \"bin-data\", "))

(defn get-items-by-id-sql
  [& {:keys [extra-select extra-where with-data? with-preview-data?]
      :or {extra-select "" extra-where "" with-data? false with-preview-data? false}}]
  (str
   (if (or with-data? with-preview-data?)
     +get-items-by-id-sql-select-with-data+
     +get-items-by-id-sql-select+)
   (if (or with-data? with-preview-data?)
     +get-items-by-id-sql-select-data+
     "")
   extra-select

   "items.id "
   " "
   (cond with-data? +get-items-by-id-sql-join-with-data-where+
         with-preview-data? +get-items-by-id-sql-join-with-preview-data-where+
         :else +get-items-by-id-sql-join-without-data-where+)
   " "
   extra-where))

(defn get-items-by-id-range-fever
  [since max & {:keys [limit] :or {limit "ALL"}}]
  (j/query
   db
   (concat
    [(str get-items-by-id-fever-base-sql
          "AND "
          (cond
            (and (number? since) (number? max))
            "items.id >= ? and items.id < ?"
            (and (number? since) (nil? max))
            "items.id >= ?"
            (and (nil? since) (number? max)
                 "items.id < ?")
            (and (nil? since) (nil? max)
                 "items.id >= 0"))
          "  "
          "GROUP BY items.id "
          "ORDER BY items.id "
          "LIMIT " limit)]
    (cond
      (and (number? since) (number? max))
      [since max]
      (and (number? since) (nil? max))
      [since]
      (and (nil? since) (number? max))
      [max]))
   {:row-fn process-items-row-fever}))

(defn get-items-by-id-fever
  [ids & {:keys [limit] :or {limit "ALL"}}]
  (let [sql-arr (str "ARRAY[" (string/join "," ids) "]")]
    (j/query db [(str get-items-by-id-fever-base-sql
                      " AND items.id = ANY(" sql-arr ") "
                      "GROUP BY items.id "
                      "LIMIT " limit)]
             {:row-fn process-items-row-fever})))

(defn get-items-by-id
  [ids & {:keys [limit with-data?] :or {limit "ALL" with-data? true}}]
  (let [sql-arr (str "ARRAY[" (string/join "," ids) "]")
        where-cond (str
                    " AND items.id = ANY(" sql-arr ") "
                    " GROUP BY items.id "
                    " LIMIT " limit)]
    (j/query db [(get-items-by-id-sql
                  :with-data? with-data?
                  :extra-where where-cond)]
             {:row-fn process-items-row})))

(defn get-items-by-source-key
  [source-key & {:keys [limit with-tag with-type with-data?]
                 :or {limit "ALL"}}]
  (let [where-cond (str
                    " AND sources.key = ? "
                    (when (keyword? with-tag)
                      (str " AND exist_inline(items.tags,'" (name with-tag) "') "))
                    (when (keyword? with-type)
                      (str " AND items.type = '" (name with-type) "'"))
                    ;; " GROUP BY items.id "
                    " LIMIT ?")]
    (j/query db [(get-items-by-id-sql
                  :with-data? with-data?
                  :extra-where where-cond) (name source-key) limit])
    {:row-fn process-items-row}))

(defn remove-unread-for-items-of-source-older-than [source-keys older-than-ts]
  (let [source-ids (j/query db [(format "select id from sources where key in (%s)"
                                        (->> source-keys
                                             (map name)
                                             (map #(str "'" % "'"))
                                             (string/join ", ")))]
                            {:row-fn :id})]
    (j/execute! db [(log/spy (format "update items set tags = tags - 'unread'::text where source_id in (%s) and exist_inline(tags, 'unread') and ts <= ?"
                                     (->> source-ids (string/join ", "))))
                    older-than-ts])))

(defn get-items-recent
  "Get items sorted by timestamp - used by the reader gui"
  [{:keys [limit before with-tag with-type with-source-keys with-data? with-preview-data? simple-filter]
    :or {limit "42" with-data? false with-preview-data? false}
    :as args}]
  (let [where-cond (str
                    (when (map? before)
                      " AND (items.ts, items.id) < (?, ?) ")
                    (when (coll? with-source-keys)
                      (str " AND sources.key = ANY(ARRAY["
                           (->> with-source-keys
                                (map #(str "'" ((fnil name :unknown) %) "'"))
                                (string/join ","))
                           "]) "))
                    (when (keyword? simple-filter)
                      (str " AND " (simple-filter-to-sql simple-filter) " "))
                    (when (keyword? with-tag)
                      (str " AND exist_inline(items.tags,'" (name with-tag) "') "))
                    (when (keyword? with-type)
                      (str " AND items.type = '" (name with-type) "'"))
                    ;; " AND items.ts < now() "
                    (when (or with-data? with-preview-data?)
                      " GROUP BY items.id ")
                    " ORDER BY ts DESC, id DESC "
                    " LIMIT " limit)
        sql (get-items-by-id-sql
             :with-data? with-data?
             :with-preview-data? with-preview-data?
             :extra-where where-cond)]
    (try+
     (let [result (j/query db
                           (remove nil? [sql
                                         (when (:ts before) (:ts before))
                                         (when (:id before) (:id before))])
                           {:row-fn process-items-row})]
       (log/debugf "Got %d rows, requested %s" (count result) limit)
       result)

     (catch org.postgresql.util.PSQLException _
       (log/error (:throwable &throw-context) "SQL failed"
                  sql)
       (throw+ {:type ::general
                :msg (.getMessage (:throwable &throw-context))
                :sql-state (.getSQLState (:throwable &throw-context))
                :sql sql :args args})))))


;; create materialized view search_index as SELECT items.id,
;; items.title,
;; items.ts,
;; sources.key,
;;     (((((setweight(to_tsvector(i1.lang::regconfig, items.title), 'A'::"char") || setweight(to_tsvector('simple'::regconfig, unaccent(items.author)), 'D'::"char")) || setweight(array_to_tsvector(items.nlp_nouns), 'B'::"char")) || setweight(array_to_tsvector(items.nlp_names), 'B'::"char")) || setweight(array_to_tsvector(items.nlp_verbs), 'C'::"char")) || setweight(array_to_tsvector(items.nlp_urls), 'D'::"char")) || setweight(array_to_tsvector(akeys(items.tags)), 'D'::"char") AS document
;;    FROM sources
;;      JOIN items ON sources.id = items.source_id,
;;     LATERAL ( SELECT
;;                 CASE items.entry ->> 'language'::text
;;                     WHEN 'en'::text THEN 'english'::text
;;                     WHEN 'de'::text THEN 'german'::text
;;                     ELSE 'english'::text
;;                 END AS lang) i1;


(defn search
  "Search for query. postgres query format"
  ([query {:keys [with-source-key time-ago-period]}]
   (j/query db [(log/spy (str "select id, title, key, ts, ts_rank(document, to_tsquery('english', ?)) as rank"
                              " from search_index"
                              " where document @@ to_tsquery('english', ?)"
                              "  and "
                              (->>
                               ["1=1"
                                (when time-ago-period
                                  (str "ts > " (j/sql-value
                                                (time/minus (time/zoned-date-time) time-ago-period))))
                                (when with-source-key (str "key = '" (name with-source-key) "'"))]
                               (remove nil?)
                               (string/join " and "))
                              " order by rank desc")) query query]))
  ([query]
   (j/query db ["select id, title, key, ts, ts_rank(document, to_tsquery('english', ?)) as rank from search_index where document @@ to_tsquery('english', ?) order by rank desc" query query])))

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

