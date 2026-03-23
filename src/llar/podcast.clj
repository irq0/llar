(ns llar.podcast
  (:require
   [clj-http.client :as http]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [java-time.api :as time]
   [llar.appconfig :as appconfig]
   [llar.blobstore :as blobstore]
   [llar.commands :as commands]
   [llar.converter :as conv]
   [llar.persistency :as persistency]
   [llar.postproc :as postproc]
   [llar.sched :refer [defsched]]
   [llar.store :as store]
   [org.bovinegenius [exploding-fish :as uri]]
   [nio2.core :as nio2]
   [slingshot.slingshot :refer [try+]])
  (:import
   [java.awt Color RenderingHints]
   [java.awt.image BufferedImage]
   [java.io ByteArrayOutputStream]
   [javax.imageio ImageIO]))

;;;; Media URL detection

(defn downloadable-media-url?
  "Check if a URL points to downloadable media (YouTube, direct audio/video)"
  [url]
  (let [url-str (str url)]
    (or (postproc/video-url? url-str)
        (some? (re-find #"\.(mp3|mp4|m4a|m4v|ogg|opus|webm|wav|aac)(\?|$)" url-str)))))

(defn media-type
  "Classify URL as :video, :audio, or nil"
  [url]
  (let [url-str (str url)]
    (cond
      (postproc/video-url? url-str) :video
      (re-find #"\.(mp3|m4a|ogg|opus|wav|aac)(\?|$)" url-str) :audio
      (re-find #"\.(mp4|m4v|webm)(\?|$)" url-str) :video
      :else nil)))

;;;; Download state tracking

(defonce download-state (atom {}))
;; {item-id {:status :pending/:downloading/:complete/:failed
;;           :blob-hash "sha256..."
;;           :media-url "original url"
;;           :metadata {:duration 123 :title "..." ...}
;;           :mime-type "video/mp4"
;;           :error nil
;;           :last-attempt nil}}

;;;; Retention configuration

(defonce source-retention-overrides (atom {}))
;; {source-key episode-limit, e.g. {:my-channel 50}}

(defn episode-limit-for-source
  "Return the episode limit for a source. Checks per-source overrides, then global config, fallback 25."
  [source-key]
  (let [k (keyword source-key)]
    (or (get @source-retention-overrides k)
        (get-in (appconfig/podcast) [:retention :default-episode-limit])
        25)))

;;;; Podcast index file
;; Maintained at {blob-store-dir}/podcast-index.edn
;; Maps blob-hash -> {:item-id :source-key :item-title :media-url :mime-type :completed-at}

(defn- podcast-index-file []
  (io/as-file (str (appconfig/blob-store-dir) "/podcast-index.edn")))

(defn read-podcast-index
  "Read the podcast index EDN file. Returns empty map if missing or corrupt."
  []
  (let [f (podcast-index-file)]
    (if (.exists f)
      (try+
       (let [content (slurp f)]
         (if (str/blank? content)
           {}
           (conv/read-edn-propsfile content)))
       (catch Object e
         (log/warn "podcast: failed to read podcast index, returning empty:" e)
         {}))
      {})))

(defonce ^:private index-lock (Object.))

(defn- write-podcast-index!
  "Write podcast index atomically (write to temp, move)."
  [index]
  (let [f (podcast-index-file)
        tmp (io/as-file (str f ".tmp"))]
    (spit tmp (conv/print-propsfile index))
    (java.nio.file.Files/move (.toPath tmp) (.toPath f)
                              (into-array java.nio.file.CopyOption
                                          [java.nio.file.StandardCopyOption/REPLACE_EXISTING
                                           java.nio.file.StandardCopyOption/ATOMIC_MOVE]))))

(defn add-to-podcast-index!
  "Add an entry to the podcast index."
  [blob-hash entry]
  (locking index-lock
    (let [index (read-podcast-index)]
      (write-podcast-index! (assoc index blob-hash entry)))))

(defn remove-from-podcast-index!
  "Remove an entry from the podcast index."
  [blob-hash]
  (locking index-lock
    (let [index (read-podcast-index)]
      (write-podcast-index! (dissoc index blob-hash)))))

(defn- subtitle-lang-priority
  "Score subtitle filename by language preference. Lower is better.
   Prefers original/standard subs over auto-generated and translated variants."
  [filename]
  (cond
    (re-find #"\.en-orig\." filename) 0
    (re-find #"\.en\." filename) 1
    (re-find #"\.en-en\." filename) 2
    (re-find #"\.en-" filename) 10
    :else 20))

(defn- subtitle-format-priority
  "Score by subtitle format preference. Lower is better."
  [filename]
  (cond
    (str/ends-with? filename ".srt") 0
    (str/ends-with? filename ".vtt") 1
    (str/ends-with? filename ".ttml") 2
    :else 3))

(defn- find-subtitle-file
  "Find best subtitle sidecar file in dir.
   Prefers original/non-translated subs over auto-generated and translated.
   Among same language preference, prefers srt > vtt > ttml."
  [dir]
  (let [sub-files (->> (file-seq (nio2/file dir))
                       (filter #(.isFile %))
                       (filter #(re-find #"\.(srt|vtt|ttml)$" (.getName %))))]
    (when (seq sub-files)
      (let [best (->> sub-files
                      (sort-by (fn [f]
                                 (let [n (.getName f)]
                                   [(subtitle-lang-priority n)
                                    (subtitle-format-priority n)])))
                      first)]
        (log/debugf "podcast: selected subtitle %s from %d candidates"
                    (.getName best) (count sub-files))
        best))))

(def ^:private +artwork-size+ 1400)

(defn- pad-to-square-png
  "Read image bytes, center on a dark square background of +artwork-size+ px, return PNG bytes."
  [image-bytes]
  (let [src (ImageIO/read (io/input-stream image-bytes))
        _ (when-not src (throw (ex-info "ImageIO could not decode thumbnail image" {})))
        sw (.getWidth src)
        sh (.getHeight src)
        size +artwork-size+
        scale (min (/ (double size) sw) (/ (double size) sh))
        dw (int (* sw scale))
        dh (int (* sh scale))
        dx (int (/ (- size dw) 2))
        dy (int (/ (- size dh) 2))
        img (BufferedImage. size size BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics img)
        baos (ByteArrayOutputStream.)]
    (try
      (.setRenderingHint g RenderingHints/KEY_INTERPOLATION
                         RenderingHints/VALUE_INTERPOLATION_BILINEAR)
      (.setColor g (Color. 20 20 40))
      (.fillRect g 0 0 size size)
      (.drawImage g src dx dy dw dh nil)
      (finally
        (.dispose g)))
    (ImageIO/write img "png" baos)
    (.toByteArray baos)))

(defn- download-thumbnail
  "Download thumbnail, pad to square 1400x1400 PNG, store in blob store. Returns hash or nil."
  [thumbnail-url]
  (when (and thumbnail-url (not (str/blank? thumbnail-url)))
    (try+
     (let [response (http/get thumbnail-url {:as :byte-array
                                             :socket-timeout 10000
                                             :connection-timeout 5000})
           padded (pad-to-square-png (:body response))
           tmp-file (java.io.File/createTempFile "podcast-thumb" ".png")]
       (try
         (io/copy padded tmp-file)
         (blobstore/add-from-local-file! tmp-file (uri/uri thumbnail-url)
                                         {:mime-type "image/png"})
         (finally
           (.delete tmp-file))))
     (catch Object e
       (log/warn "podcast: thumbnail download failed:" thumbnail-url e)
       nil))))

(defn- store-media!
  "Download and store media for an item. Returns updated state entry."
  [media-url {:keys [item-id source-key item-title]}]
  (commands/with-temp-dir dir
    (let [{:keys [file metadata mime-type]} (commands/download-media media-url dir)
          thumbnail-hash (download-thumbnail (:thumbnail metadata))
          transcript (when-let [sub-file (find-subtitle-file dir)]
                       (slurp sub-file :encoding "UTF-8"))
          enriched-metadata (cond-> metadata
                              thumbnail-hash (assoc :thumbnail-hash thumbnail-hash)
                              transcript (assoc :transcript transcript))
          completed-at (time/zoned-date-time)
          content-hash (blobstore/add-from-local-file!
                        file (uri/uri media-url)
                        {:mime-type mime-type
                         :podcast-metadata enriched-metadata
                         :podcast-item-id item-id
                         :podcast-source-key source-key
                         :podcast-item-title item-title
                         :podcast-completed-at completed-at})]
      (add-to-podcast-index! content-hash
                             {:item-id item-id
                              :source-key source-key
                              :item-title item-title
                              :media-url media-url
                              :mime-type mime-type
                              :completed-at completed-at})
      {:status :complete
       :blob-hash content-hash
       :media-url media-url
       :metadata enriched-metadata
       :mime-type mime-type
       :completed-at completed-at})))

(defn- scan-podcast-items!
  "Scan for podcast-tagged items, detect media URLs, update download state"
  []
  (try+
   (let [items (persistency/get-items-recent store/backend-db
                                             {:with-tag :podcast
                                              :limit 200})]
     (doseq [item items
             :let [item-id (:id item)
                   item-title (:title item)
                   source-key (:source-key item)
                   url (or (get-in item [:entry :url])
                           (:url item))
                   url-str (str url)]
             :when (and (some? url)
                        (downloadable-media-url? url-str)
                        (not (contains? @download-state item-id)))]
       (if-let [existing-hash (blobstore/find-in-url-index url-str)]
         (let [blob (blobstore/get-blob existing-hash)]
           (swap! download-state assoc item-id
                  {:status :complete
                   :blob-hash existing-hash
                   :media-url url-str
                   :item-title item-title
                   :source-key source-key
                   :metadata (:podcast-metadata blob)
                   :mime-type (:mime-type blob)}))
         (swap! download-state assoc item-id
                {:status :pending
                 :media-url url-str
                 :item-title item-title
                 :source-key source-key}))))
   (catch Object e
     (log/error e "podcast scanner: failed to scan items"))))

(def ^:private +max-download-attempts+ 3)
(def ^:private +retry-cooldown-minutes+ 30)

(defn- retry-failed-downloads!
  "Move :failed items past their cooldown back to :pending for retry."
  []
  (let [now (time/zoned-date-time)]
    (doseq [[item-id state] @download-state
            :when (= :failed (:status state))
            :let [attempt (or (:attempt state) 1)
                  retry-after (:retry-after state)]
            :when (and (< attempt +max-download-attempts+)
                       retry-after
                       (time/after? now retry-after))]
      (log/infof "podcast: retrying failed download for item %s (attempt %d/%d)"
                 item-id (inc attempt) +max-download-attempts+)
      (swap! download-state assoc item-id
             (-> state
                 (assoc :status :pending)
                 (assoc :attempt (inc attempt)))))))

(defn- process-pending-downloads!
  "Process one pending download at a time"
  []
  (retry-failed-downloads!)
  (when-let [[item-id state] (->> @download-state
                                  (filter (fn [[_ v]] (= :pending (:status v))))
                                  first)]
    (try+
     (log/infof "podcast: downloading media for item %s: %s" item-id (:media-url state))
     (swap! download-state assoc-in [item-id :status] :downloading)
     (swap! download-state assoc-in [item-id :last-attempt] (time/zoned-date-time))
     (let [result (store-media! (:media-url state)
                                {:item-id item-id
                                 :source-key (:source-key state)
                                 :item-title (:item-title state)})]
       (swap! download-state assoc item-id
              (merge (select-keys state [:item-title :source-key]) result))
       (log/infof "podcast: download complete for item %s -> %s" item-id (:blob-hash result)))
     (catch Object e
       (let [attempt (or (:attempt state) 1)
             retryable? (< attempt +max-download-attempts+)
             retry-after (when retryable?
                           (time/plus (time/zoned-date-time)
                                      (time/minutes +retry-cooldown-minutes+)))]
         (log/warnf "podcast: download failed for item %s (attempt %d/%d, %s): %s"
                    item-id attempt +max-download-attempts+
                    (if retryable? (str "retry after " retry-after) "giving up")
                    e)
         (swap! download-state assoc item-id
                {:status (if retryable? :failed :perm-failed)
                 :media-url (:media-url state)
                 :item-title (:item-title state)
                 :source-key (:source-key state)
                 :attempt attempt
                 :retry-after retry-after
                 :error (str e)
                 :last-attempt (time/zoned-date-time)}))))))

;;;; State rebuild from podcast index

(defonce ^:private state-rebuilt? (atom false))

(defn rebuild-state-from-index!
  "Rebuild download-state atom from podcast index file on startup.
   Only adds entries not already present in the atom."
  []
  (let [index (read-podcast-index)
        existing @download-state
        recovered (atom 0)]
    (doseq [[blob-hash entry] index
            :let [item-id (:item-id entry)]
            :when (and item-id (not (contains? existing item-id)))]
      (swap! download-state assoc item-id
             {:status :complete
              :blob-hash blob-hash
              :media-url (:media-url entry)
              :item-title (:item-title entry)
              :source-key (:source-key entry)
              :mime-type (:mime-type entry)
              :completed-at (:completed-at entry)
              :metadata (try+
                         (let [f (blobstore/get-local-filename blob-hash)
                               pf (io/as-file (str f ".props"))]
                           (when (.exists pf)
                             (:podcast-metadata (blobstore/try-read-propsfile-or-recreate
                                                 (str pf) f))))
                         (catch Object _ nil))})
      (swap! recovered inc))
    (when (pos? @recovered)
      (log/infof "podcast: rebuilt %d entries from podcast index" @recovered))))

;;;; Retention

(defn- item-saved?
  "Check if an item is tagged :saved in the database."
  [item-id]
  (try+
   (let [item (persistency/get-item-by-id store/backend-db item-id)]
     (when item
       (some #(= "saved" %) (:tags item))))
   (catch Object _
     false)))

(defn eviction-candidates
  "For a given source, return completed items eligible for eviction, oldest first.
   Excludes :saved items."
  [source-key]
  (let [source-items (->> @download-state
                          (filter (fn [[_ v]] (and (= :complete (:status v))
                                                   (= source-key (:source-key v)))))
                          (sort-by (fn [[_ v]]
                                     (or (:completed-at v) (:last-attempt v)
                                         (time/zoned-date-time 2000 1 1)))
                                   compare))]
    (remove (fn [[item-id _]] (item-saved? item-id)) source-items)))

(defn- enforce-retention-for-source!
  "Enforce count-based retention for a single source."
  [source-key]
  (let [limit (episode-limit-for-source source-key)
        candidates (eviction-candidates source-key)
        excess (max 0 (- (count candidates) limit))
        to-evict (take excess candidates)]
    (doseq [[item-id state-entry] to-evict]
      (let [blob-hash (:blob-hash state-entry)
            referencing-items (->> @download-state
                                   (filter (fn [[_ v]] (= blob-hash (:blob-hash v))))
                                   (map first)
                                   (into #{}))]
        (log/infof "podcast retention: evicting item %s from %s (hash: %s)"
                   item-id source-key (subs blob-hash 0 12))
        (when (= #{item-id} referencing-items)
          (blobstore/delete-blob! blob-hash)
          (remove-from-podcast-index! blob-hash)
          (log/infof "podcast retention: deleted blob %s (no other references)"
                     (subs blob-hash 0 12)))
        (swap! download-state dissoc item-id)))))

(defn- cleanup-perm-failed!
  "Remove permanently failed download entries older than 7 days."
  []
  (let [cutoff (time/minus (time/zoned-date-time) (time/days 7))]
    (doseq [[item-id state] @download-state
            :when (and (= :perm-failed (:status state))
                       (:last-attempt state)
                       (time/before? (:last-attempt state) cutoff))]
      (log/infof "podcast: removing perm-failed entry %s (last attempt: %s)"
                 item-id (:last-attempt state))
      (swap! download-state dissoc item-id))))

(defn enforce-retention!
  "Enforce count-based retention across all sources. Also cleanup stale perm-failed entries."
  []
  (cleanup-perm-failed!)
  (let [sources (->> @download-state
                     (map (comp :source-key val))
                     (filter some?)
                     distinct)]
    (doseq [src sources]
      (try+
       (enforce-retention-for-source! src)
       (catch Object e
         (log/error e "podcast retention: failed for source" src))))))

;;;; Disk stats (for dashboard)

(defn blob-file-size
  "Get blob file size without opening an InputStream."
  [blob-hash]
  (try
    (.length (blobstore/get-local-filename blob-hash))
    (catch Exception _ 0)))

(defn podcast-disk-stats
  "Compute disk usage stats for podcast blobs."
  []
  (let [completed (->> @download-state
                       (filter (fn [[_ v]] (= :complete (:status v)))))]
    {:total-size (->> completed
                      (map (fn [[_ v]] (blob-file-size (:blob-hash v))))
                      (reduce + 0))
     :total-episodes (count completed)
     :by-source (->> completed
                     (group-by (comp :source-key val))
                     (map (fn [[src items]]
                            (let [limit (episode-limit-for-source src)
                                  sizes (map (fn [[_ v]]
                                               (blob-file-size (:blob-hash v)))
                                             items)]
                              [src {:size (reduce + 0 sizes)
                                    :episode-count (count items)
                                    :limit limit}])))
                     (into {}))}))

;;;; Scheduler

(defn- podcast-scanner-tick! []
  (when (compare-and-set! state-rebuilt? false true)
    (rebuild-state-from-index!))
  (scan-podcast-items!)
  (process-pending-downloads!))

(defsched podcast-scanner :now-and-every-5-minutes
  (when (appconfig/podcast)
    (podcast-scanner-tick!)))

(defsched podcast-retention-enforcer :hourly
  (when (appconfig/podcast)
    (enforce-retention!)))
