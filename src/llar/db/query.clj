(ns llar.db.query
  (:require
   [llar.persistency :refer [StatsQueries SourceQueries ItemQueries]]
   [llar.db.core]
   [llar.db.sql :as sql]
   [llar.converter :as conv]
   [digest])
  (:import (llar.db.core PostgresqlDataStore)))

(defn- process-items-row
  "Generic item row processor. Convert aggregated item data table entries into a
  nice map. "
  [row]
  (if (and (some? (:data_types row)) (some? (first (:data_types row))))
    ;; data tables is aggregated into 4 json columns
    ;; transform them to tree of {:data {:data_type {mime_type data}}}
    (-> row
        (assoc :data
               (->>
                (map (fn [t m text bin]
                       (when-let [data (or text (conv/bytea-hex-to-byte-array bin))]
                         [t {m data}]))
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

(defn- simple-filter-to-sql [kw]
  (case kw
    :unread "tagi @@ '0'"
    :today "date(ts) = current_date AND tagi @@ '0'"
    nil))

;; ----------

(extend-protocol StatsQueries
  PostgresqlDataStore

  (get-table-row-counts [this]
    (sql/get-table-row-counts this nil {}))

  (get-type-stats [this]
    (drop 1 (sql/get-type-stats this nil {} {:as-arrays? true})))

  (get-tag-stats [this]
    (drop 1 (sql/get-tag-stats this nil {} {:as-arrays? true})))

  (get-tags [this]
    (apply concat
           (drop 1 (sql/get-tags this nil {} {:as-arrays? true}))))

  (get-word-count-groups [this]
    (drop 1 (sql/get-word-count-groups this nil {} {:as-arrays? true}))))

;; ----------

(defn- get-item-count-of-source [db id]
  (:count (sql/get-item-count-of-source db {:id id})))

(defn- get-item-count-by-tag-of-source [db id]
  (into {}
        (sql/get-item-count-by-tag-of-source
         db
         {:id id}
         {}
         {:row-fn (fn [{:keys [tag count]}]
                    [(keyword tag) count])})))

(defn- get-item-count-unread-today [db id]
  (:count (sql/get-item-count-unread-today db {:id id})))

(extend-protocol SourceQueries
  PostgresqlDataStore

  (get-sources [this config-sources]
    (let [row-add-config-src
          (fn [row]
            (let [key (keyword (:key row))]
              [key (-> row
                       (assoc :key key)
                       (merge (get config-sources key)))]))]
      (into {}
            (sql/get-sources this
                             nil
                             {}
                             {:row-fn row-add-config-src}))))

  (get-sources-item-tags-counts [this item-tag simple-filter config-sources]
    (sql/get-sources-with-item-tags-count
     this
     {:item-tag (name item-tag)
      :simple-filter (simple-filter-to-sql simple-filter)}
     {}
     {:row-fn (fn [row]
                (let [key (keyword (:key row))]
                  (-> row
                      (assoc :key key)
                      (merge (get config-sources key)))))}))

  (sources-merge-in-tags-counts [this sources]
    (pmap (fn [source]
            (let [id (:id source)
                  total (get-item-count-of-source this id)
                  per-tag (get-item-count-by-tag-of-source this id)
                  today (get-item-count-unread-today this id)]
              (merge source
                     {:item-tags (merge per-tag
                                        {:today today
                                         :total total})})))
          sources)))

;; ----------

(defn- choose-recent-items-select-snip [args]
  (let [{:keys [with-data? with-preview-data?]} args]
    (cond
      with-data? (sql/item-select-with-data-snip)
      with-preview-data? (sql/item-select-with-data-snip)
      :else (sql/item-select-default-snip))))

(defn- choose-recent-items-from-snip [args]
  (let [{:keys [with-data? with-preview-data?]} args]
    (cond
      with-data? (sql/item-from-join-with-data-table-snip)
      with-preview-data? (sql/item-from-join-with-preview-data-snip)
      :else (sql/item-from-join-default-snip))))

(defn- choose-recent-items-group-by-colums [args]
  (let [{:keys [with-data? with-preview-data?]} args]
    (when (or with-data? with-preview-data?)
      ["items.id"])))

(defn- make-recent-items-where-cond-vec
  "Convert get-items-recent filter parameter into a list of sqlvec where clauses"
  [args]
  (let [{:keys [before with-source-keys with-source-ids simple-filter with-tag with-type]} args
        simple-filter (when (keyword? simple-filter) (simple-filter-to-sql simple-filter))]
    (not-empty
     (interpose ["and"]
                (cond-> []
                  (map? before)
                  (conj (sql/cond-before before))

                  (coll? with-source-keys)
                  (conj (sql/cond-with-source-keys {:keys
                                                    (map name with-source-keys)}))

                  (and (coll? with-source-ids) (not-empty with-source-ids))
                  (conj (sql/cond-with-source-ids {:ids with-source-ids}))

                  (some? simple-filter)
                  (conj [simple-filter])

                  (keyword? with-tag)
                  (conj (sql/cond-with-tag {:tag (name with-tag)}))

                  (keyword? with-type)
                  (conj (sql/cond-with-type {:type (keyword "item_type" (name with-type))})))))))

(extend-protocol ItemQueries
  PostgresqlDataStore

  (get-items-recent [this {:keys [limit] :or {limit 42} :as args}]
    (sql/get-items-recent
     this
     {:select (choose-recent-items-select-snip args)
      :from (choose-recent-items-from-snip args)
      :where (make-recent-items-where-cond-vec args)
      :limit limit
      :group-by-columns (choose-recent-items-group-by-colums args)}))

  (get-items-by-tag [this tag]
    (sql/get-items-by-tag this {:tag (name tag)}))

  (get-item-by-id [this id]
    (sql/get-item-by-id
     this
     {:id id
      :select (sql/item-select-with-data-snip)
      :from (sql/item-from-join-with-data-table-snip)
      :group-by-columns ["items.id"]}
     {}
     {:row-fn process-items-row})))
