(ns llar.db.search
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [digest]
   [java-time.api :as time]
   [llar.db.core]
   [llar.db.sql :as sql]
   [llar.persistency :refer [DataStoreSearch]]
   [next.jdbc :as jdbc])
  (:import
   (llar.db.core PostgresqlDataStore)))

(defn- refresh-search-index [db]
  (jdbc/execute! db ["refresh materialized view search_index"]))

(defn- refresh-idf [db]
  (jdbc/execute! db ["refresh materialized view idf_top_words"]))

(defn- refresh-source-stats [db]
  (jdbc/execute! db ["REFRESH MATERIALIZED VIEW CONCURRENTLY source_stats"]))

(def ^:private valid-search-syntaxes
  #{:web :plain :phrase :advanced})

(defn normalize-search-syntax [syntax]
  (cond
    (valid-search-syntaxes syntax) syntax
    (string? syntax) (normalize-search-syntax (keyword syntax))
    :else :web))

(extend-protocol DataStoreSearch
  PostgresqlDataStore

  (search
    ([this query {:keys [syntax with-source-key with-tag untagged? time-ago-period
                         archived-only? sort limit offset]
                  :or {limit 100 offset 0}}]
     (if (string/blank? query)
       []
       (sql/search-item
        this
        {:query query
         :syntax (name (normalize-search-syntax syntax))
         :source-key with-source-key
         :with-tag with-tag
         :untagged? untagged?
         :archived-only? archived-only?
         :sort sort
         :limit limit
         :offset offset
         :time-ago (when-not (nil? time-ago-period)
                     (time/minus (time/zoned-date-time) time-ago-period))})))
    ([this query]
     (if (string/blank? query)
       []
       (sql/search-item
        this
        {:query query
         :syntax (name (normalize-search-syntax nil))
         :limit 100
         :offset 0}))))

  (update-index! [this]
    (refresh-search-index this)
    (refresh-idf this)
    (try (refresh-source-stats this)
         (catch Exception e
           (log/warn e "Failed to refresh source_stats materialized view")))))

(defn saved-items-tf-idf [db]
  (rest (map (fn [{:keys [id json_agg]}]
               (assoc (into {} json_agg) "item_id" (double id)))
             (sql/saved-items-tf-idf db))))

(defn saved-items-tf-idf-terms [db min-tf-idf]
  (->>
   (sql/saved-items-tf-idf-terms
    db
    {:min-tf-idf min-tf-idf})
   first
   :array_agg))

(defn- useful-term? [term]
  (and (string? term)
       (<= 3 (count (string/trim term)) 80)
       (re-find #"[\p{L}\p{N}]" term)))

(defn- quote-websearch-term [term]
  (str "\"" (string/replace (string/trim term) "\"" "") "\""))

(defn- related-query [item tf-idf-terms]
  (let [title-terms (some->> (:title item)
                             (re-seq #"[\p{L}\p{N}][\p{L}\p{N}-]{3,}")
                             (take 8))
        top-terms (map :term tf-idf-terms)
        named-terms (concat (take 8 (:names item))
                            (take 8 (:nouns item)))]
    (->> (concat named-terms title-terms top-terms)
         (filter useful-term?)
         distinct
         (take 24)
         (map quote-websearch-term)
         (string/join " OR "))))

(defn- cap-per-source [limit rows]
  (second
   (reduce (fn [[counts result] row]
             (let [source (:key row)
                   seen (get counts source 0)]
               (if (or (>= seen limit) (>= (count result) 20))
                 [counts result]
                 [(assoc counts source (inc seen)) (conj result row)])))
           [{} []]
           rows)))

(defn- highlighted-terms [headline]
  (->> (re-seq #"\[\[\[(.+?)\]\]\]" (or headline ""))
       (map (comp string/trim second))
       (remove string/blank?)
       (reduce (fn [terms term]
                 (if (some #(= (string/lower-case %) (string/lower-case term)) terms)
                   terms
                   (conj terms term)))
               [])))

(defn- add-related-evidence [rows]
  (let [top-rank (double (or (:rank (first rows)) 0.0))]
    (mapv (fn [row]
            (assoc row
                   :relative-score (if (pos? top-rank)
                                     (/ (double (or (:rank row) 0.0)) top-rank)
                                     0.0)
                   :matched-terms (highlighted-terms (:headline row))))
          rows)))

(defn related-items
  "Return a deliberately simple lexical neighborhood from the current search
  index. Index refresh cadence is accepted as part of this feature's contract."
  ([db item-id]
   (related-items db item-id {}))
  ([db item-id {:keys [archived-only?]}]
   (when-let [item (sql/get-item-by-id db {:id item-id
                                           :select (sql/item-select-default-snip)
                                           :from (sql/item-from-join-default-snip)})]
     (let [tf-idf-terms (sql/item-tf-idf-terms db {:item-id item-id})
           query (related-query item tf-idf-terms)
           matches (if (string/blank? query)
                     []
                     (sql/search-item db {:query query
                                          :syntax "web"
                                          :archived-only? archived-only?
                                          :limit 100
                                          :offset 0}))]
       {:item item
        :query query
        :results (->> matches
                      (remove #(= item-id (:id %)))
                      (filter #(>= (double (or (:rank %) 0.0)) 0.01))
                      (cap-per-source 3)
                      add-related-evidence)}))))
