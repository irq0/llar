(ns llar.podcast
  (:require
   [clojure.tools.logging :as log]
   [java-time.api :as time]
   [org.bovinegenius [exploding-fish :as uri]]
   [slingshot.slingshot :refer [throw+ try+]]
   [llar.appconfig :as appconfig]
   [llar.blobstore :as blobstore]
   [llar.commands :as commands]
   [llar.persistency :as persistency]
   [llar.postproc :as postproc]
   [llar.sched :refer [defsched]]
   [llar.store :as store]))

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

(defn- store-media!
  "Download and store media for an item. Returns updated state entry."
  [media-url]
  (commands/with-temp-dir dir
    (commands/with-retry 2 [:type ::av-download-error]
      (let [{:keys [file metadata mime-type]} (commands/download-media media-url dir)
            content-hash (blobstore/add-from-local-file!
                          file (uri/uri media-url)
                          {:mime-type mime-type
                           :podcast-metadata metadata})]
        {:status :complete
         :blob-hash content-hash
         :media-url media-url
         :metadata metadata
         :mime-type mime-type}))))

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

(defn- process-pending-downloads!
  "Process one pending download at a time"
  []
  (when-let [[item-id state] (->> @download-state
                                  (filter (fn [[_ v]] (= :pending (:status v))))
                                  first)]
    (try+
     (log/infof "podcast: downloading media for item %s: %s" item-id (:media-url state))
     (swap! download-state assoc-in [item-id :status] :downloading)
     (swap! download-state assoc-in [item-id :last-attempt] (time/zoned-date-time))
     (let [result (store-media! (:media-url state))]
       (swap! download-state assoc item-id
              (merge (select-keys state [:item-title :source-key]) result))
       (log/infof "podcast: download complete for item %s -> %s" item-id (:blob-hash result)))
     (catch Object e
       (log/warn "podcast: download failed for item" item-id e)
       (swap! download-state assoc item-id
              {:status :failed
               :media-url (:media-url state)
               :item-title (:item-title state)
               :source-key (:source-key state)
               :error (str e)
               :last-attempt (time/zoned-date-time)})))))

(defn- podcast-scanner-tick! []
  (scan-podcast-items!)
  (process-pending-downloads!))

(defsched podcast-scanner :now-and-every-5-minutes
  (when (appconfig/podcast)
    (podcast-scanner-tick!)))
