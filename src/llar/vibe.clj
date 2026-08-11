(ns llar.vibe
  "Ephemeral story clustering for the Today’s Vibe reader view."
  (:require
   [clj-ml.clusterers :as ml-clusterers]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [java-time.api :as time]
   [llar.config :as config]
   [llar.persistency :as persistency]
   [llar.rc :as rc])
  (:import (java.util ArrayList UUID)
           (weka.core Attribute Instances SparseInstance)))

(def not-compiled ::not-compiled)
(def current-vibe (atom not-compiled))

(def ^:private tuning-defaults
  {:max-feature-frequency-ratio 0.2
   :min-match-score 0.15
   :max-clusters 12
   :max-single-source-clusters 4})

(defn- tuning-settings []
  (merge tuning-defaults (rc/rc [:reader :vibe])))

(defn- recent-candidates [db]
  (let [{:keys [hours limit source-tags]} (rc/rc [:reader :vibe])
        cutoff (time/minus (time/zoned-date-time) (time/hours hours))
        sources (config/get-sources)]
    (->> (persistency/get-items-recent db {:limit limit})
         (filter #(time/after? (:ts %) cutoff))
         (filter (fn [item]
                   (or (empty? source-tags)
                       (seq (set/intersection
                             (set source-tags)
                             (set (get-in sources [(keyword (:source-key item)) :tags] #{})))))))
         (sort-by (juxt :ts :id))
         vec)))

(defn- tokens [text]
  (when (string? text)
    (->> (re-seq #"[\p{L}\p{N}][\p{L}\p{N}-]{3,}" (string/lower-case text))
         distinct
         (take 12))))

(defn- raw-features [item]
  (let [top-words (:top-words item)
        words (or (:words top-words) (get top-words "words"))]
    (merge-with +
                (into {} (map #(vector (str "title:" %) 3.0) (tokens (:title item))))
                (into {} (for [[term tf] (take 25 words)
                               :when (string? term)]
                           [(str "term:" (string/lower-case term)) (double tf)]))
                (into {} (map #(vector (str "name:" (string/lower-case %)) 2.0)
                              (filter string? (take 12 (:names item)))))
                (into {} (map #(vector (str "noun:" (string/lower-case %)) 1.5)
                              (filter string? (take 12 (:nouns item)))))
                (into {} (map #(vector (str "url:" %) 2.0)
                              (filter string? (:urls item)))))))

(defn- weighted-features [items]
  (let [raw (mapv raw-features items)
        n (count raw)
        df (frequencies (mapcat keys raw))
        max-common-count (max 5 (long (Math/ceil
                                       (* n (:max-feature-frequency-ratio
                                             (tuning-settings))))))]
    (mapv (fn [features]
            (into {}
                  (for [[feature value] features
                        :let [freq (get df feature 0)]
                        :when (and (>= freq 2)
                                   (<= freq max-common-count))]
                    [feature (* value (+ 1.0 (Math/log (/ (inc n) (inc freq)))))])))
          raw)))

(defn- make-dataset [feature-maps]
  (let [features (->> feature-maps (mapcat keys) distinct sort vec)
        attributes (conj features "item-id")
        weka-attributes (mapv (fn [name]
                                (doto (Attribute. name)
                                  (.setWeight (if (= name "item-id") 0.0 1.0))))
                              attributes)
        dataset (Instances. "todays-vibe" (ArrayList. weka-attributes)
                            (count feature-maps))]
    (.setClassIndex dataset -1)
    (doseq [[idx features] (map-indexed vector feature-maps)]
      (let [instance (SparseInstance. (count attributes))]
        (.setDataset instance dataset)
        (doseq [[attribute-index attribute] (map-indexed vector attributes)]
          (.setValue instance (int attribute-index)
                     (double (if (= attribute "item-id") idx
                                 (get features attribute 0.0)))))
        (.add dataset instance)))
    {:dataset dataset :features features}))

(defn- display-term [feature]
  (second (string/split feature #":" 2)))

(defn- cosine [left right]
  (let [dot (reduce-kv (fn [total feature value]
                         (+ total (* value (get right feature 0.0))))
                       0.0 left)
        left-norm (Math/sqrt (reduce + (map #(* % %) (vals left))))
        right-norm (Math/sqrt (reduce + (map #(* % %) (vals right))))]
    (if (zero? (* left-norm right-norm))
      0.0
      (/ dot (* left-norm right-norm)))))

(defn- mean-pairwise-cosine [feature-maps]
  (let [pairs (for [left-index (range (count feature-maps))
                    right-index (range (inc left-index) (count feature-maps))]
                (cosine (nth feature-maps left-index)
                        (nth feature-maps right-index)))]
    (when (seq pairs)
      (/ (reduce + pairs) (count pairs)))))

(defn- summarize-cluster [id pairs]
  (let [items (mapv first pairs)
        feature-maps (map second pairs)
        feature-totals (apply merge-with + {} feature-maps)
        centroid (into {} (map (fn [[feature total]]
                                 [feature (/ total (count pairs))])
                               feature-totals))
        representative (->> pairs
                            (sort-by (fn [[item features]]
                                       [(- (cosine features centroid))
                                        (- (.getEpochSecond (.toInstant (:ts item))))
                                        (:id item)]))
                            ffirst)
        sources (set (map :source-key items))
        match-score (mean-pairwise-cosine feature-maps)]
    {:id id
     :items items
     :representative-id (:id representative)
     :source-count (count sources)
     :article-count (count items)
     :unseen-count (count (filter #(some #{"unread"} (:tags %)) items))
     :match-score match-score
     :terms (->> feature-totals
                 (sort-by val >)
                 (map (comp display-term key))
                 distinct
                 (take 6)
                 vec)
     :latest-ts (apply max-key #(.getEpochSecond (.toInstant %)) (map :ts items))}))

(defn cluster-items [items]
  (if (empty? items)
    []
    (let [feature-maps (weighted-features items)
          pairs (mapv vector items feature-maps)
          clusterable (filterv (comp seq second) pairs)
          unsupported (filterv (comp empty? second) pairs)
          {:keys [dataset]} (make-dataset (mapv second clusterable))
          settings (tuning-settings)
          assignments (if (< (count clusterable) 2)
                        [0]
                        (let [clusterer (ml-clusterers/make-clusterer
                                         :cobweb
                                         {:acuity (:acuity settings)
                                          :cutoff (:cutoff settings)
                                          :random-seed (:random-seed settings)})]
                          (ml-clusterers/clusterer-build clusterer dataset)
                          (mapv #(.clusterInstance clusterer %) dataset)))
          supported-clusters (->> (map vector assignments clusterable)
                                  (group-by first)
                                  (sort-by key)
                                  (mapv (fn [[_ assigned]]
                                          (mapv second assigned))))
          all-groups (concat supported-clusters (map vector unsupported))]
      (mapv (fn [idx group] (summarize-cluster idx group))
            (range)
            all-groups))))

(defn- latest-epoch [{:keys [latest-ts]}]
  (if latest-ts
    (.getEpochSecond (.toInstant latest-ts))
    0))

(defn- quality-cluster? [min-match-score {:keys [article-count match-score]}]
  (or (= 1 article-count)
      (and match-score (>= match-score min-match-score))))

(defn- multi-source-rank [cluster]
  [(- (or (:match-score cluster) 0.0))
   (- (:source-count cluster))
   (- (:article-count cluster))
   (- (:unseen-count cluster))
   (- (latest-epoch cluster))])

(defn- single-source-rank [cluster]
  [(- (:unseen-count cluster))
   (- (latest-epoch cluster))
   (- (:article-count cluster))])

(defn select-clusters
  "Quality-rank and budget clusters for the reader without discarding the raw snapshot."
  [clusters]
  (let [{:keys [min-match-score max-clusters max-single-source-clusters]}
        (tuning-settings)
        quality-clusters (filter #(quality-cluster? min-match-score %) clusters)
        multi-source (sort-by multi-source-rank
                              (filter #(>= (:source-count %) 2) quality-clusters))
        selected-multi (take max-clusters multi-source)
        remaining (- max-clusters (count selected-multi))
        single-budget (min remaining max-single-source-clusters)
        other (sort-by single-source-rank
                       (filter #(< (:source-count %) 2) quality-clusters))
        selected-other (take single-budget other)]
    {:multi-source (vec selected-multi)
     :other (vec selected-other)
     :shown-count (+ (count selected-multi) (count selected-other))
     :total-count (count clusters)}))

(defn build! [db]
  (try
    (locking current-vibe
      (let [items (recent-candidates db)
            _ (log/infof "Today’s Vibe clustering %d candidates from sources %s with settings %s"
                         (count items)
                         (frequencies (map :source-key items))
                         (select-keys (tuning-settings)
                                      [:acuity :cutoff :random-seed
                                       :max-feature-frequency-ratio]))
            clusters (cluster-items items)
            snapshot {:run-id (str (UUID/randomUUID))
                      :generated-at (time/zoned-date-time)
                      :window-hours (get (rc/rc [:reader :vibe]) :hours)
                      :algorithm :cobweb
                      :feature-version 3
                      :clusters clusters}]
        (log/infof "Today’s Vibe built from %d candidates: %d clusters, %d cross-source"
                   (count items)
                   (count clusters)
                   (count (filter #(>= (:source-count %) 2) clusters)))
        (reset! current-vibe snapshot))
      @current-vibe)
    (catch Throwable error
      (log/error error "Today’s Vibe clustering failed")
      {:error (ex-message error)})))

(defn current-cluster [run-id cluster-id]
  (let [snapshot @current-vibe]
    (when (and (map? snapshot) (= run-id (:run-id snapshot)))
      (some #(when (= cluster-id (:id %)) %) (:clusters snapshot)))))

(defn apply-to-current-cluster!
  "Run f only while run-id is current, then reflect seen-state in the snapshot.
  The lock makes stale-run rejection reliable across a concurrent rebuild."
  [run-id cluster-id f]
  (locking current-vibe
    (when-let [cluster (current-cluster run-id cluster-id)]
      (f cluster)
      (swap! current-vibe update :clusters
             (fn [clusters]
               (mapv (fn [candidate]
                       (if (= cluster-id (:id candidate))
                         (-> candidate
                             (assoc :unseen-count 0)
                             (update :items
                                     (fn [items]
                                       (mapv #(update % :tags
                                                      (fn [tags]
                                                        (vec (remove #{"unread"} tags))))
                                             items))))
                         candidate))
                     clusters)))
      cluster)))
