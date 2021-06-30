(ns u1f596.lab
  (:require
   [u1f596.persistency :as persistency]
   [u1f596.store :refer [backend-db]]
   [u1f596.db.search :as db-search]
   [u1f596.config :as config]
   [u1f596.fetch :as fetch]
   [u1f596.postproc :as proc]
   [u1f596.update :as update]
   [u1f596.blobstore :as blobstore]
   [u1f596.appconfig :as appconfig]
   [clj-http.client :as http]
   [slingshot.slingshot :refer [throw+ try+]]
   [java-time :as time]
   [taoensso.timbre :as log]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [schema.core :as s]
   [cheshire.core :as json]
   [org.bovinegenius [exploding-fish :as uri]]
   [clojure.java.shell :as shell]
   [mount.core :refer [defstate]]
   [clojure.edn :as edn]
   [nrepl.server :refer [start-server stop-server]]
   [pantomime.mime :as pm]
   [clj-ml.clusterers :as ml-clusterers]
   [clj-ml.data :as ml-data]))

;;;; Namespace to interact with u1f596 from the REPL

(s/set-fn-validation! true)

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
    (let [fetched (fetch/fetch-source src)
          postproc (proc/make
                    :post post
                    :pre pre
                    :filter filter)
          processed (cond->> fetched
                      limit (take limit)
                      (not skip-postproc) (proc/process {:src src
                                                         :proc postproc} {}))]

      (log/infof "Preview of %s: fetched: %s, limit: %s, after processing: %s"
                 (str src) (count fetched) limit (count processed))
      (reset! +current-fetch-preview+ processed)
      {:info "Open http://localhost:8023/preview"
       :n (count processed)
       :limited-preview (map #(select-keys % [:summary :feed :meta :hash]) processed)})

    (catch Throwable th
      (log/error th "Error fetching " (str src)))))


(def current-clustered-saved-items (atom {}))

(defn make-saved-dataset []
  ;; must ensure that there are no '/' in terms - messes up keyword/name
  (let [attributes (vec (conj (into #{} (db-search/saved-items-tf-idf-terms backend-db))
                              "item_id"))
        term-tf-idf-maps (db-search/saved-items-tf-idf backend-db)
        weka-attributes (map (fn [s]
                               (let [new-attrib (weka.core.Attribute. s)]
                                 (if (= s :item_id)
                                   (.setWeight new-attrib 0.0)
                                   (.setWeight new-attrib 1.0))
                                 new-attrib))
                             attributes)
        ds (weka.core.Instances. "saved-items" (java.util.ArrayList. weka-attributes)
                                 (count term-tf-idf-maps))]
        ;; ds (ml-data/make-dataset "saved-items" attributes (count term-tf-idf-maps))]
    (doseq [m term-tf-idf-maps]
      (let [inst (weka.core.SparseInstance. (count attributes))]
        (.setDataset inst ds)
        (doall (map-indexed (fn [i attrib]
                              (try
                                (.setValue inst i (or (get m attrib) 0.0))
                                (catch Throwable th
                                  (log/info th i attrib (get m attrib) (type (get m attrib)))
                                  (throw th))))
                            attributes))
        (.setWeight inst 1.0)
        (.add ds inst)))

    ds))

(defn improvise-cluster-name [attributes centroid]
  (let [take-this (nth (sort centroid) (- (count centroid) 5))]
    (string/join "+"
                 (take 5
                       (remove nil?
                               (map (fn [att val]
                                      (let [name (.name att)]
                                        (when (and (>= val take-this)
                                                   (not (re-find #"\W" name))
                                                   (not= "item_id" name))
                                          name)))
                                    attributes
                                    centroid))))))

(defn cluster-saved []
  (let [ds (make-saved-dataset)
        clst (ml-clusterers/make-clusterer :k-means {:number-clusters (int (/ (ml-data/dataset-count ds) 5))})]
    (ml-clusterers/clusterer-build clst ds)
    (let [ds-clst (ml-clusterers/clusterer-cluster clst ds)
          centroids (:centroids (ml-clusterers/clusterer-info clst))
          names (into {}
                      (map (fn [[k cent]]
                             [(keyword (str k))
                              (if-let [human (improvise-cluster-name
                                              (ml-data/dataset-attributes ds)
                                              (-> cent
                                                  ml-data/instance-to-vector))]
                                human
                                (keyword (str k)))])
                           centroids))]
      (->> ds-clst
           ml-data/dataset-as-maps
           (map (fn [{:keys [item_id class]}]
                  (let [id (int item_id)
                        item (persistency/get-item-by-id backend-db id)
                        class-name (get names class)]
                    {:title (:title item)
                     :nwords (:nwords item)
                     :readability (get-in item [:entry :readability])
                     :class class-name
                     :id id})))
           (group-by :class)))))

(defn youtube-dl-music [url]
  (let [{:keys [exit out err]} (shell/sh
                                (appconfig/command :youtube-dl)
                                "--no-progress"
                                "-f" "bestaudio"
                                "--extract-audio"
                                "--ignore-errors"
                                "--audio-format" "mp3"
                                "--audio-quality" "320"
                                url
                                :dir "/tank/media/Music/Miner")
        youtube-err (second (re-find #"ERROR: (.+)" err))]
    (log/trace "Youtube-dl OUT: " out)
    (log/trace "Youtube-dl ERR: " err)
    (cond (zero? exit)
          (let [[_ out-filename] (re-find #"Destination:\s(.+\.mp3)" out)]
            out-filename)
          (some? youtube-err)
          (throw+ {:type :youtube-error :msg youtube-err})
          :else
          (throw+ {:type :error :stderr err :stdout out :exit exit}))))

(defn youtube-dl-video [url dest-dir]
  (let [{:keys [exit out err]} (shell/sh
                                (appconfig/command :youtube-dl)
                                "--format" "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best"
                                "--recode-video" "mp4"
                                "--embed-subs"
                                "--embed-thumbnail"
                                "--no-call-home"
                                "--no-progress"
                                "--no-continue"
                                "--no-part"
                                "--no-playlist"
                                "--dump-json"
                                url
                                :dir dest-dir)]
    (if (zero? exit)
      (let [j (json/parse-string out true)]
        (log/info "Youtube-dl success: " (:_filename j))
        (select-keys j [:_filename :id :uploader :title]))
      (do
        (log/error "Youtube-dl error: "
                   exit
                   out
                   err)
        (throw+ {:type :youtube-error :exit exit})))))

(defn music-links []
  (let [sources (-> (persistency/get-sources backend-db config/*srcs*)
                    vals)
        music-srcs (->> sources
                        (filter #(contains? (:tags %) :music))
                        (map :key))
        items (persistency/get-items-recent backend-db
               {:with-source-keys music-srcs
                :limit 512
                :with-tag :unread})]
    (->> items
         (map (juxt :id :title :url))
         (filter (fn [[_ _ url]]
                   (re-find #"^(https?\:\/\/)?(www\.)?(youtube\.com|youtu\.?be)\/.+$" url))))))

(defn music-miner []
  (doseq [[id title url] (music-links)]
    (try+
     (log/info "Music Mining: " title url)
     (let [filename (youtube-dl-music url)]
       (log/infof "Music Miner downloaded %s/%s to %s"
                  title url filename)
       (persistency/item-set-tags! backend-db id [:music-mined])
       (persistency/item-remove-tags! backend-db id [:unread]))
     (catch [:type :youtube-error] {:keys [msg]}
       (persistency/item-set-tags! backend-db id [:music-mined :music-miner-failed])
       (persistency/item-remove-tags! backend-db id [:unread])
       (log/errorf "Music miner failed for %s/%s: %s" title url msg))
     (catch Object e
       (log/error e "Unexpected error")))))

(defn download-tagged-stuff []
  (let [items (persistency/get-items-by-tag backend-db :download)]
    (doall
     (for [item items]
       (let [url (get-in item [:entry :url])
             dest-dir (str "/home/seri/MEDIA/"
                           (name (:key item)))]
         (when (re-find #"^(https?\:\/\/)?(www\.)?(youtube\.com|youtu\.?be)\/.+$" url)
           (log/info "Downloading " url " to " dest-dir)
           (-> (io/as-file dest-dir) .mkdirs)
           (future
             (try+
              (let [filename (youtube-dl-video url dest-dir)]
                (persistency/item-remove-tags! backend-db (:id item) :download)
                filename)
              (catch [:type :youtube-error] {:keys [msg]}
                (log/errorf "Miner failed for %s: %s" item msg))
              (catch Object e
                (log/error e "Unexpected error"))))))))))

(defn copy-wallpapers-to-home []
  (doseq [[source destination]
          (some->> (persistency/get-items-by-tag backend-db :wallpaper)
                   (map (fn [item]
                          (when-let [blob-url (get-in item [:entry :lead-image-url])]
                            [(string/replace blob-url "/blob/" "")
                             (:key item)])))
                   (map (fn [[blob source-key]]
                          (let [local-fn (blobstore/get-local-filename blob)
                                mime (pantomime.mime/mime-type-of local-fn)
                                extension (pantomime.mime/extension-for-name mime)]
                            [local-fn (str source-key "_" blob extension)]))))]
    (let [dst-file (io/file "/home/seri/Pictures/wallpaper" destination)]
      (when-not (.exists dst-file)
        (io/copy source dst-file)
        dst-file))))

;;; toying around with urban sports club

(defn get-usc-venues []
  (:body (http/get "https://urbansportsclub.com/studios-map?city=1" {:as :json})))

(defn usc-l-only [usc-venues]
  (->> usc-venues :data :venues
       (filter #(= 3 (apply min (:planTypeIds %))))
       (remove #(some (partial = "Massage") (map :name (:categories %))))
       (map #(merge (select-keys % [:district :name :location])
                    {:categories (string/join ", " (map :name (:categories %)))}))))

