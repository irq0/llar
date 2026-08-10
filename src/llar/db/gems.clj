(ns llar.db.gems
  (:require
   [llar.db.core]
   [llar.db.search :as search]
   [llar.db.sql :as sql]
   [llar.persistency :refer [GemQueries]])
  (:import
   (llar.db.core PostgresqlDataStore)))

(def ^:private workflow-tags
  #{"archive" "saved" "unread" "in-progress"})

(defn- row->gem [{:keys [description-text] :as row}]
  (cond-> (-> row
              (dissoc :description-text :total-count)
              (update :tags #(vec (or % []))))
    description-text (assoc-in [:data :description "text/plain"] description-text)))

(defn- sql-options [{:keys [tag source sort limit offset]
                     :or {limit 50 offset 0}}]
  {:tag (when (and tag (not= tag "__untagged__")) tag)
   :untagged? (= tag "__untagged__")
   :source source
   :sort sort
   :limit limit
   :offset offset})

(defn- facet-map [rows]
  (let [summary (into {} (for [{:keys [kind value count]} rows
                               :when (= "summary" kind)]
                           [(keyword value) count]))
        facets (fn [kind]
                 (->> rows
                      (filter #(= kind (:kind %)))
                      (mapv #(select-keys % [:value :count]))))]
    {:total (get summary :total 0)
     :topic-count (get summary :topics 0)
     :source-count (get summary :sources 0)
     :tags (facets "tag")
     :sources (facets "source")}))

(defn- diverse-items [rows limit]
  (let [[chosen deferred _]
        (reduce (fn [[chosen deferred sources] row]
                  (if (or (>= (count chosen) limit)
                          (contains? sources (:source-key row)))
                    [chosen (conj deferred row) sources]
                    [(conj chosen row) deferred (conj sources (:source-key row))]))
                [[] [] #{}]
                rows)]
    (vec (take limit (concat chosen deferred)))))

(extend-protocol GemQueries
  PostgresqlDataStore

  (get-gem-facets [this options]
    (facet-map (sql/get-gem-facet-rows this (sql-options options))))

  (get-gem-items [this options]
    (let [rows (sql/get-gem-items this (sql-options options))]
      {:total (or (:total-count (first rows)) 0)
       :items (mapv row->gem rows)}))

  (get-gem-rediscovery-candidates
    [this {:keys [day-cutoff day-key candidate-limit candidate-offset limit]
           :or {candidate-limit 10 candidate-offset 0 limit 5}}]
    (-> (sql/get-gem-rediscovery-candidates
         this {:day_cutoff day-cutoff
               :day_key day-key
               :candidate_limit candidate-limit
               :candidate_offset candidate-offset})
        (as-> rows (mapv row->gem rows))
        (diverse-items limit)))

  (get-related-gems [this item-id]
    (when-let [{:keys [item] :as related}
               (search/related-items this item-id {:archived-only? true})]
      (when (some #{"archive"} (:tags item))
        (update related :results
                #(mapv (fn [row]
                         (-> row
                             (assoc :source-key (:key row))
                             (update :tags (fn [tags]
                                             (vec (remove workflow-tags tags))))))
                       %))))))
