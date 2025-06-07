(ns llar.lab
  (:require
   [llar.persistency :as persistency]
   [llar.store :refer [backend-db]]
   [llar.db.search :as db-search]
   [llar.sched :refer [defsched]]
   [llar.fetch :as fetch]
   [llar.postproc :as proc]
   [llar.metrics]
   [java-time :as time]
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [clj-ml.clusterers :as ml-clusterers]
   [clj-ml.data :as ml-data]))

;;;; Namespace to interact with llar from the REPL

(def +current-fetch-preview+ (atom nil))

;;; Preview - Try out filters, processing, fetch

(defn preview
  [src & {:keys [limit post pre filter skip-postproc]
          :or {limit nil
               post []
               pre []
               filter (constantly false)
               skip-postproc false}}]
  (try
    (let [fetched (fetch/fetch-source src {})
          postproc (proc/make
                    :post post
                    :pre pre
                    :filter filter)
          processed (cond->> fetched
                      limit (take limit)
                      (not skip-postproc) (proc/process {:src src
                                                         :proc postproc} {}))]

      (log/infof "preview %s: fetched: %s, limit: %s, after processing: %s"
                 (str src) (count fetched) limit (count processed))
      (reset! +current-fetch-preview+ processed)
      {:info "Open http://localhost:8023/preview"
       :n (count processed)
       :limited-preview (map #(select-keys % [:summary :feed :meta :hash]) processed)})

    (catch Throwable th
      (log/error th "error fetching " (str src)))))

(def current-clustered-saved-items (atom {}))

(defn- remove-useless-features [feats]
  (remove #(or
            (string/starts-with? % "?")
            (string/starts-with? % ";")
            (string/starts-with? % "-")
            (string/starts-with? % ">")
            (string/starts-with? % "http")
            (string/includes? % "href=")
            (string/includes? % "iframe")
            (string/starts-with? % "\""))
          feats))

(defn- get-features [db]
  (->>
   (map #(db-search/saved-items-tf-idf-terms db %) [1 0.5 0.1 0.05 0.01])
   (remove empty?)
   (map remove-useless-features)
   (remove #(< (count %) 42))
   (first)
   (into #{})))

(defn make-saved-dataset [db]
  ;; must ensure that there are no '/' in terms - messes up keyword/name
  (let [top-features (->> (get-features db)
                          (remove #(= % "item_id"))
                          (into []))
        attributes (-> top-features
                       (conj "item_id"))
        term-tf-idf-maps (db-search/saved-items-tf-idf db)
        weka-attributes (map (fn [s]
                               (let [new-attrib (weka.core.Attribute. s)]
                                 (if (= s "item_id")
                                   (.setWeight new-attrib 0.0)
                                   (.setWeight new-attrib 1.0))
                                 new-attrib))
                             attributes)
        ds (weka.core.Instances. "saved-items" (java.util.ArrayList. weka-attributes)
                                 (count term-tf-idf-maps))]
    (.setClassIndex ds -1)
    (doseq [m term-tf-idf-maps]
      (let [inst (weka.core.SparseInstance. (count attributes))]
        (.setDataset inst ds)
        (doall (map-indexed (fn [i attrib]
                              (try
                                (.setValue inst i (get m attrib 0.0))
                                (catch Throwable th
                                  (log/info th i attrib (get m attrib) (type (get m attrib)))
                                  (throw th))))
                            attributes))
        (.setWeight inst 1.0)
        (.add ds inst)))
    (log/debugf "saved dataset: features:%s instances:%s" (count attributes) (count term-tf-idf-maps))
    ds))

(defn improvise-cluster-name [attributes centroid]
  (let [top (->> (map (fn [att val] [(.name att) val]) attributes centroid)
                 (filter (fn [[att val]] (and (> val 0) (not= "item_id" att))))
                 (sort-by second)
                 (reverse)
                 (map first))]
    (->> top
         (take 5)
         (into []))))

(defn- get-number-of-clusters [ds]
  (max 5
       (int (/ (ml-data/dataset-count ds) 5))))

(defn cluster-saved [db]
  (let [ds (make-saved-dataset db)
        clst (ml-clusterers/make-clusterer :k-means {:random-seed (int (* (rand) 10E8))
                                                     :replace-missing-values false
                                                     :number-iterations 10000
                                                     :number-clusters (get-number-of-clusters ds)})]
    (ml-clusterers/clusterer-build clst ds)
    (log/debug "Clusterer: " clst)
    (let [centroids (:centroids (ml-clusterers/clusterer-info clst))
          names (into {}
                      (map (fn [[k cent]]
                             [k
                              (if-let [words (improvise-cluster-name
                                              (ml-data/dataset-attributes ds)
                                              (-> cent
                                                  ml-data/instance-to-vector))]
                                words
                                (keyword (str k)))])
                           centroids))
          item-id-index (.index (.attribute ds "item_id"))]
      (log/debug names item-id-index)
      (log/debug centroids)
      (->> ds
           (map (fn [instance]
                  (let [id (int (.value instance item-id-index))
                        item (persistency/get-item-by-id db id)
                        cluster (.clusterInstance clst instance)]
                    (-> item
                     ;; (select-keys item [:title :source-key :pub-ts :nwords])
                     (assoc :cluster {:id cluster :words (get names cluster "?")})
                     (assoc :id id)))))
           (group-by :cluster)))))

(defn update-saved-clusters! []
  (try
    (reset!
     current-clustered-saved-items
     {:clusters (cluster-saved backend-db)
      :last-update (time/zoned-date-time)})
    (catch weka.core.WekaException ex
      (log/info "saved items clustering failed: " (ex-message ex)))))

(defsched update-db-search-indices
  :now-and-early-morning
  (log/info "updating database search indices")
  (persistency/update-index! backend-db)
  (update-saved-clusters!))
