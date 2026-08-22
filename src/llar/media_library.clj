(ns llar.media-library
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [digest :as digest]
   [llar.blobstore :as blobstore]
   [llar.contentdetect :as contentdetect]
   [llar.media-artwork :as artwork]
   [llar.podcast :as podcast]
   [ring.util.codec :as codec])
  (:import
   [java.io FileNotFoundException]
   [java.nio.charset StandardCharsets]
   [java.time.format DateTimeFormatter]))

(def base-path "/library/")

(def ^:private year-formatter (DateTimeFormatter/ofPattern "yyyy"))
(def ^:private month-formatter (DateTimeFormatter/ofPattern "MM"))
(def ^:private reserved-source-names #{"favorite-atv.png" "favorite.png"})

(defn safe-name [value fallback]
  (or (some-> (str value)
              (str/replace #"[\"\\/:|<>*?\p{Cntrl}]" "-")
              (str/replace #"\s+" " ")
              str/trim
              (#(subs % 0 (min 140 (count %))))
              (str/replace #"^[. -]+|[. -]+$" "")
              not-empty)
      fallback))

(defn- media-filename [{:keys [item-id item-title mime-type]}]
  (let [stem (safe-name (or item-title "episode") "episode")
        extension (case mime-type
                    "audio/mpeg" "mp3"
                    "audio/mp4" "m4a"
                    "video/webm" "webm"
                    "mp4")]
    (str stem "-" item-id "." extension)))

(defn- entry-date [{:keys [completed-at]}]
  (try
    {:year (.format year-formatter completed-at)
     :month (.format month-formatter completed-at)}
    (catch Exception _
      {:year "Unknown" :month "Unknown"})))

(defn- source-directory-names [index]
  (let [sources (->> index vals (map :source-key) distinct)
        base-name #(safe-name (some-> % name) "unknown-source")
        counts (frequencies (map base-name sources))]
    (into {}
          (map (fn [source-key]
                 (let [base (base-name source-key)]
                   [source-key
                    (if (or (> (get counts base) 1)
                            (contains? reserved-source-names base))
                      (str base "--" (subs (digest/sha-256 (str source-key)) 0 8))
                      base)])))
          sources)))

(defn entries []
  (let [index (podcast/read-podcast-index)
        source-directories (source-directory-names index)]
    (->> index
         (map (fn [[hash entry]]
                (merge entry
                       {:blob-hash hash
                        :filename (media-filename entry)
                        :source-directory (get source-directories
                                               (:source-key entry))}
                       (entry-date entry))))
         (filter :mime-type)
         (sort-by (juxt (comp str :source-key)
                        (comp str :completed-at)
                        (comp str :item-id))))))

(defn collection [path display-name]
  {:path path :display-name display-name :collection? true})

(defn- blob-metadata [hash context]
  (when hash
    (try
      (blobstore/get-blob-metadata hash)
      (catch FileNotFoundException _
        (log/warn "media library: unavailable blob" context hash)
        nil)
      (catch Exception exception
        (log/warn exception "media library: unavailable blob" context hash)
        nil))))

(defn- media-file [path entry]
  (when-let [blob (blob-metadata (:blob-hash entry)
                                 {:kind :media :item-id (:item-id entry)})]
    {:path path
     :display-name (:filename entry)
     :size (:size blob)
     :mime-type (or (:mime-type blob) (:mime-type entry))
     :etag (:blob-hash entry)
     :completed-at (or (:completed-at entry)
                       (:podcast-completed-at blob)
                       (:created blob))
     :metadata (:podcast-metadata blob)
     :resource-kind :blob
     :blob-hash (:blob-hash entry)
     :entry entry}))

(defn- filename-stem [filename]
  (let [dot (.lastIndexOf ^String filename ".")]
    (if (pos? dot) (subs filename 0 dot) filename)))

(defn- image-extension [mime-type]
  (try
    (or (some-> (contentdetect/mime-extension mime-type)
                (str/replace #"^\.jpeg$" ".jpg"))
        ".png")
    (catch Exception _ ".png")))

(defn- artwork-file [parent-path stem suffix blob-hash completed-at]
  (when-let [blob (blob-metadata blob-hash {:kind :artwork})]
    (let [extension (image-extension (:mime-type blob))
          filename (str stem suffix extension)]
      {:path (conj parent-path filename)
       :display-name filename
       :size (:size blob)
       :mime-type (or (:mime-type blob) "image/png")
       :etag blob-hash
       :completed-at completed-at
       :resource-kind :blob
       :blob-hash blob-hash})))

(defn- transcript-file [parent-path media]
  (let [metadata (:metadata media)
        transcript (:transcript metadata)]
    (when (string? transcript)
      (let [format (or (:transcript-format metadata) "vtt")
            raw-language (or (:transcript-language metadata) "en")
            language (if (str/starts-with? raw-language "en") "en" raw-language)
            filename (str (filename-stem (:display-name media))
                          "-" (safe-name language "en")
                          "." (safe-name format "vtt"))
            bytes (.getBytes transcript StandardCharsets/UTF_8)]
        {:path (conj parent-path filename)
         :display-name filename
         :size (alength bytes)
         :mime-type (or (:transcript-mime-type metadata)
                        (case format
                          "srt" "application/x-subrip"
                          "ttml" "application/ttml+xml"
                          "text/vtt"))
         :etag (digest/sha-256 bytes)
         :completed-at (:completed-at media)
         :resource-kind :bytes
         :bytes bytes}))))

(defn- generated-artwork [path filename label width height]
  (let [bytes (artwork/cover label width height)]
    {:path (conj path filename)
     :display-name filename
     :size (alength ^bytes bytes)
     :mime-type "image/png"
     :etag (digest/sha-256 bytes)
     :resource-kind :bytes
     :bytes bytes}))

(defn- item-files [parent-path entry]
  (if-let [media (media-file (conj parent-path (:filename entry)) entry)]
    (let [metadata (:metadata media)
          stem (filename-stem (:display-name media))
          completed-at (:completed-at media)
          poster-hash (or (:poster-hash metadata)
                          (:fanart-hash metadata)
                          (:thumbnail-hash metadata))
          fanart-hash (or (:fanart-hash metadata) poster-hash)]
      (filterv some?
               [media
                (transcript-file parent-path media)
                (artwork-file parent-path stem "" poster-hash completed-at)
                (artwork-file parent-path stem "-fanart" fanart-hash completed-at)]))
    []))

(defn- source-folder-files [segments source-name entries]
  (into [(generated-artwork segments "folder.png" source-name 500 750)]
        (mapcat #(item-files segments %) entries)))

(defn- favorite-files [segments]
  [(generated-artwork segments "favorite-atv.png" "LLAR Media" 614 346)
   (generated-artwork segments "favorite.png" "LLAR Media" 500 750)])

(defn directory [segments entries]
  (case (count segments)
    0 {:self (collection [] "LLAR Media Library")
       :children [(collection ["By-Source"] "By Source")
                  (collection ["By-Date"] "By Date")]}

    1 (case (first segments)
        "By-Source"
        {:self (collection segments "By Source")
         :children (into (favorite-files segments)
                         (->> entries
                              (map :source-directory)
                              distinct
                              sort
                              (map #(collection ["By-Source" %] %))))}

        "By-Date"
        {:self (collection segments "By Date")
         :children (->> entries
                        (map :year)
                        distinct
                        sort
                        reverse
                        (mapv #(collection ["By-Date" %] %)))}
        nil)

    2 (let [[view value] segments]
        (case view
          "By-Source"
          (or (some (fn [node]
                      (when (= value (:display-name node))
                        {:self node :children []}))
                    (favorite-files ["By-Source"]))
              (let [matching (filter #(= value (:source-directory %)) entries)]
                (when (seq matching)
                  {:self (collection segments value)
                   :children (source-folder-files segments value matching)})))

          "By-Date"
          (let [matching (filter #(= value (:year %)) entries)]
            (when (seq matching)
              {:self (collection segments value)
               :children (->> matching
                              (map :month)
                              distinct
                              sort
                              reverse
                              (mapv #(collection (conj segments %) %)))}))
          nil))

    3 (let [[view first-value second-value] segments]
        (case view
          "By-Source"
          (some (fn [node]
                  (when (= second-value (:display-name node))
                    {:self node :children []}))
                (source-folder-files
                 ["By-Source" first-value]
                 first-value
                 (filter #(= first-value (:source-directory %)) entries)))

          "By-Date"
          (let [matching (filter #(and (= first-value (:year %))
                                       (= second-value (:month %)))
                                 entries)]
            (when (seq matching)
              {:self (collection segments second-value)
               :children (vec (mapcat #(item-files segments %) matching))}))
          nil))

    4 (let [[view year month filename] segments]
        (when (= view "By-Date")
          (some (fn [node]
                  (when (= filename (:display-name node))
                    {:self node :children []}))
                (mapcat #(item-files ["By-Date" year month] %)
                        (filter #(and (= year (:year %))
                                      (= month (:month %)))
                                entries)))))
    nil))

(defn segments [path]
  (if (str/blank? path)
    []
    (->> (str/split path #"/")
         (remove str/blank?)
         vec)))

(defn encoded-path [segments]
  (str/join "/" (map codec/url-encode segments)))
