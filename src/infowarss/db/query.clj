(ns infowarss.db.query
  (:require
   [infowarss.db.core :as core :refer [db]]
   [infowarss.db.sql :as sql]
   [infowarss.converter :as conv]
   [digest]))

(defn process-items-row
  "Generic item row processor. Convert aggregated item data table entries into a
  nice map. "
  [row]
  (if (and (some? (:data_types row)) (some? (first (:data_types row))))
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

(defn- simple-filter-to-sql [kw]
  (case kw
    :unread "exist_inline(tags, 'unread')"
    :today "date(ts) = current_date AND exist_inline(tags, 'unread')"
    nil))

;; ----------

(defn get-source-for-doc [doc]
  (let [key (name (get-in doc [:meta :source-key]))]
    (sql/get-source-by-key db {:key key})))

(defn get-word-count-groups []
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


;; ----------


(defn get-item-with-data-by-id [id]
  (sql/get-item-by-id
   core/db
   {:id id
    :select (sql/item-select-with-data-snip)
    :from (sql/item-from-join-with-data-table-snip)
    :group-by-columns ["items.id"]}
   {}
   {:row-fn process-items-row}))

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
  (let [{:keys [before with-source-keys simple-filter with-tag with-type]} args]
    (not-empty
     (interpose ["and"]
                (cond-> []
                  (map? before)
                  (conj (sql/cond-before before))

                  (coll? with-source-keys)
                  (conj (sql/cond-with-source {:keys
                                               (map name with-source-keys)}))

                  (keyword? simple-filter)
                  (conj [(simple-filter-to-sql simple-filter)])

                  (keyword? with-tag)
                  (conj (sql/cond-with-tag {:tag (name with-tag)}))

                  (keyword? with-type)
                  (conj (sql/cond-with-type {:type with-type})))))))

(defn get-items-recent
  "
  get-items-recent returns :limit recent items, optionally offsetted with :before
  {:ts :id} to implement pagination.

  Supports the following filters:
  :with-tag $keyword  -  only $keyword tagged items
  :with-type :item-type/$type  -  only items with type $type
  :simple-filter see #'simple-filter-to-sql  -  only items matching simple filter
  :with-source-keys [$source-key ..]  -  only items from source in vec

  Return item without data when :with-data? or :with-preview-data? is not specified
  "

  [{:keys [limit] :or {limit 42} :as args}]
  (sql/get-items-recent
   core/db
   {:select (choose-recent-items-select-snip args)
    :from (choose-recent-items-from-snip args)
    :where (make-recent-items-where-cond-vec args)
    :limit limit
    :group-by-columns (choose-recent-items-group-by-colums args)}))
