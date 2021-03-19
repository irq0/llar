(ns infowarss.db.query
  (:require
   [infowarss.db.core :as core :refer [db]]
   [infowarss.db.sql :as sql]
   [infowarss.converter :as conv]
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
   [cheshire.generate :as json :refer [encode-str]]))

(defn get-source-for-doc [doc]
  (let [key (name (get-in doc [:meta :source-key]))]
   (sql/get-source-by-key db {:key key})))

(defn get-word-count-groups [db]
  (drop 1 (sql/get-word-count-groups db nil {} {:as-arrays? true})))

(defn get-tag-stats []
  (drop 1 (sql/get-tag-stats db nil {} {:as-arrays? true})))

(defn get-type-stats []
  (drop 1 (sql/get-type-stats db nil {} {:as-arrays? true})))

(defn get-sources [config-sources]
  (let [row-add-config-src
        (fn [row]
          (let [key (keyword (:key row))]
            [key (-> row
                     (assoc :key key)
                     (merge (get config-sources key)))]))]
    (into {}
          (sql/get-sources db
                        nil
                        {} 
                        {:row-fn row-add-config-src}))))

(defn get-items-by-tag [tag]
  (sql/get-items-by-tag db {:tag (name tag)}))

;; ---------------------

(defn get-item-count-of-source [id]
  (:count (sql/get-item-count-of-source db {:id id})))

(defn get-item-count-by-tag-of-source [id]
  (into {}
  (sql/get-item-count-by-tag-of-source
   db
   {:id id}
   {}
   {:row-fn (fn [{:keys [tag count]}]
              [(keyword tag) count])})))

(defn get-item-count-unread-today [id]
  (:count (sql/get-item-count-unread-today db {:id id})))
  
(defn sources-merge-in-tags-counts [sources]
  (pmap (fn [source]
          (let [id (:id source)
                total (get-item-count-of-source id)
                per-tag (get-item-count-by-tag-of-source id)
                today (get-item-count-unread-today id)]
            (merge source
                   {:item-tags (merge per-tag
                                      {:today today
                                       :total total})})))
        sources))

;; --------------

(defn- simple-filter-to-sql [kw]
  (case kw
    :unread "exist_inline(tags, 'unread')"
    :today "date(ts) = current_date AND exist_inline(tags, 'unread')"
    "1 = 1"))

(defn get-sources-item-tags-counts [item-tag simple-filter config-sources]
  (sql/get-sources-with-item-tags-count
   db
   {:item-tag (name item-tag)
    :simple-filter (simple-filter-to-sql simple-filter)}
   {}
   {:row-fn (fn [row]
              (let [key (keyword (:key row))]
                (-> row
                    (assoc :key key)
                    (merge (get config-sources key)))))}))


;; ---------

(defn process-items-row [row]
  (if (some? (:data_types row))
    (-> row
        (assoc :data
               (->>
                (map (fn [t m text bin]
                       [t {m (or text (conv/bytea-hex-to-byte-array bin))}])
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


(defn get-items-by-id
  [ids & {:keys [limit with-data?] :or {limit "ALL" with-data? true}}]
  (let [sql-arr (str "ARRAY[" (string/join "," ids) "]")
        where-cond (str
                    " AND items.id = ANY(" sql-arr ") "
                    " GROUP BY items.id "
                    " LIMIT " limit)]
    (j/query core/db [(get-items-by-id-sql
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

