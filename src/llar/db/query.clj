(ns llar.db.query
  (:require
   [clojure.tools.logging :as log]
   [java-time.api :as time]
   [llar.persistency :refer [StatsQueries SourceQueries ItemQueries RankingQueries]]
   [llar.db.core]
   [llar.db.sql :as sql]
   [llar.converter :as conv]
   [llar.tags :as tags])
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
    :unread "items.tagi @@ '0'"
    :today "items.ts >= current_date AND items.ts < current_date + interval '1 day' AND items.tagi @@ '0'"
    nil))

;; ----------

(extend-protocol StatsQueries
  PostgresqlDataStore

  (get-table-row-counts [this]
    (sql/get-table-row-counts this))

  (get-type-stats [this]
    (sql/get-type-stats this))

  (get-tag-stats [this]
    (sql/get-tag-stats this))

  (get-tags [this]
    (->> (sql/get-tags this) (map :tag) (map keyword) (into #{})))

  (get-word-count-groups [this]
    (->> (sql/get-word-count-groups this)
         (map (fn [x] [(:nword_groups x) (:count x)])))))

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
            (map row-add-config-src
                 (sql/get-sources this nil {})))))

  (get-source-ids-with-tag [this source-ids tag]
    (if (seq source-ids)
      (into #{}
            (map :source_id)
            (sql/get-source-ids-with-tag
             this
             {:source-ids source-ids
              :tag (tags/normalize-tag tag)}))
      #{}))

  (get-source-item-counts [this {:keys [source-ids simple-filter with-tag]}]
    (if (seq source-ids)
      (into {}
            (map (fn [{:keys [source_id item_count]}]
                   [source_id item_count]))
            (sql/get-source-item-counts
             this
             {:source-ids source-ids
              :simple-filter (simple-filter-to-sql simple-filter)
              :with-tag (some-> with-tag tags/normalize-tag)}))
      {})))

;; ----------

(defn- choose-recent-items-select-snip [args]
  (let [{:keys [with-data?]} args]
    (cond
      with-data? (sql/item-select-with-data-snip)
      :else (sql/item-select-default-snip
             {:with-rank-score? (:with-rank-score? args)}))))

(defn- choose-recent-items-from-snip [args]
  (let [{:keys [with-data?]} args
        ranked (= (:sort-order args) :ranked)]
    (cond
      (and ranked with-data?)          (sql/item-from-join-with-data-table-ranked-snip)
      ranked                           (sql/item-from-join-ranked-snip)
      with-data?                       (sql/item-from-join-with-data-table-snip)
      :else                            (sql/item-from-join-default-snip))))

(defn- choose-recent-items-group-by-colums [args]
  (let [{:keys [with-data?]} args]
    (when with-data?
      ["items.id"])))

(def ^:private +preview-description-query-characters+ 512)

(defn- attach-preview-descriptions
  "Attach bounded plain-text descriptions to an already limited item batch."
  [db items]
  (let [item-ids (mapv :id items)]
    (if (seq item-ids)
      (let [descriptions (->> (sql/get-item-preview-descriptions
                               db
                               {:item-ids item-ids
                                :max-characters +preview-description-query-characters+})
                              (keep (fn [{:keys [description] :as row}]
                                      (when-let [item-id (or (:item-id row)
                                                             (:item_id row))]
                                        [item-id description])))
                              (into {}))]
        (mapv (fn [{:keys [id] :as item}]
                (if-let [description (get descriptions id)]
                  (assoc-in item [:data :description "text/plain"] description)
                  item))
              items))
      items)))

(defn- attach-preview-descriptions-best-effort
  "Keep the selected Reader batch usable when optional description hydration fails."
  [db items]
  (try
    (attach-preview-descriptions db items)
    (catch Exception exception
      (log/warn exception
                (format "Reader preview descriptions unavailable for %d selected items; rendering without them"
                        (count items)))
      items)))

(defn- choose-order-by-snip [args]
  (case (:sort-order args)
    :ranked (sql/order-by-ranked-snip {:highlight-boost (or (:highlight-boost args) 48.0)
                                       :rarity-cap (or (:rarity-cap args) 168.0)})
    :oldest (sql/order-by-oldest-snip)
    (sql/order-by-newest-snip)))

(defn- make-recent-items-where-cond-vec
  "Convert get-items-recent filter parameter into a list of sqlvec where clauses"
  [args & {:keys [include-tag?] :or {include-tag? true}}]
  (let [{:keys [before with-source-keys with-source-ids simple-filter with-tag with-type]} args
        simple-filter (when (keyword? simple-filter) (simple-filter-to-sql simple-filter))]
    (not-empty
     (interpose ["and"]
                (cond-> []
                  (map? before)
                  (conj (if (= (:sort-order args) :oldest)
                          (sql/cond-after before)
                          (sql/cond-before before)))

                  (coll? with-source-keys)
                  (conj (sql/cond-with-source-keys {:keys
                                                    (map name with-source-keys)}))

                  (and (coll? with-source-ids) (not-empty with-source-ids))
                  (conj (sql/cond-with-source-ids {:ids with-source-ids}))

                  (some? simple-filter)
                  (conj [simple-filter])

                  (and include-tag? (keyword? with-tag))
                  (conj (sql/cond-with-tag {:tag (tags/normalize-tag with-tag)}))

                  (keyword? with-type)
                  (conj (sql/cond-with-type {:type (keyword "item_type" (name with-type))})))))))

(extend-protocol ItemQueries
  PostgresqlDataStore

  (get-items-recent [this {:keys [limit offset] :or {limit 42} :as args}]
    (let [bounded-rank-query? (and (= :ranked (:sort-order args))
                                   (not (:with-data? args)))
          gin-first-tag-query? (and (keyword? (:with-tag args))
                                    (not (:with-data? args))
                                    (not= :ranked (:sort-order args)))
          query-params {:select (choose-recent-items-select-snip
                                 (cond-> args
                                   bounded-rank-query? (assoc :with-rank-score? true)))
                        :from (choose-recent-items-from-snip args)
                        :where (make-recent-items-where-cond-vec
                                args
                                :include-tag? (not (or gin-first-tag-query?
                                                       bounded-rank-query?)))
                        :order-by (choose-order-by-snip args)
                        :limit limit
                        :offset offset
                        :group-by-columns (choose-recent-items-group-by-colums args)}
          items (cond
                  bounded-rank-query?
                  (let [highlight-boost (double (or (:highlight-boost args) 48.0))
                        rarity-cap (double (or (:rarity-cap args) 168.0))
                        tag (some-> (:with-tag args) tags/normalize-tag)]
                    (sql/get-items-ranked-bounded
                     this
                     (merge query-params
                            {:ranked-at (or (:ranked-at args) (time/zoned-date-time))
                             :rank-cursor (:rank-cursor args)
                             :highlight-boost highlight-boost
                             :rarity-cap rarity-cap
                             :max-boost (+ highlight-boost rarity-cap)
                             :tag tag
                             :rank-source (if tag "tagged_items" "items")
                             :order-by (sql/order-by-selected-rank-snip)})))

                  gin-first-tag-query?
                  (sql/get-items-recent-by-tag
                   this
                   (assoc query-params :tag (tags/normalize-tag (:with-tag args))))

                  :else
                  (sql/get-items-recent this query-params))]
      (if (and (:with-preview-data? args) (not (:with-data? args)))
        (attach-preview-descriptions-best-effort this items)
        items)))

  (get-items-by-tag [this tag]
    (sql/get-items-by-tag this {:tag (tags/normalize-tag tag)}))

  (get-item-by-id [this id]
    (process-items-row
     (sql/get-item-by-id
      this
      {:id id
       :select (sql/item-select-with-data-snip {:with-reading-progress? true})
       :from (sql/item-from-join-with-data-table-snip)
       :group-by-columns ["items.id"]}))))

(extend-protocol RankingQueries
  PostgresqlDataStore

  (get-source-stats [this args]
    (sql/get-source-stats this args))

  (get-ranked-vs-time-preview [this args]
    (sql/get-ranked-vs-time-preview this args)))
