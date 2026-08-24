(ns llar.media-library
  (:require
   [clojure.data.xml :as xml]
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

(defn- original-url [media]
  (or (get-in media [:metadata :original_url])
      (get-in media [:metadata :webpage_url])
      (get-in media [:entry :media-url])))

(defn- description-with-original [description url]
  (let [description (some-> description str str/trim not-empty)
        url (some-> url str str/trim not-empty)]
    (cond
      (and description url (str/includes? description url)) description
      (and description url) (str description "\n\nOriginal: " url)
      url (str "Original: " url)
      :else description)))

(defn- nfo-date [metadata]
  (when-let [date (or (:upload_date metadata) (:release_date metadata))]
    (let [date (str date)]
      (if (re-matches #"\d{8}" date)
        (str (subs date 0 4) "-" (subs date 4 6) "-" (subs date 6 8))
        date))))

(defn- nfo-file [parent-path media]
  (let [metadata (:metadata media)
        title (or (:title metadata)
                  (get-in media [:entry :item-title])
                  (filename-stem (:display-name media)))
        url (original-url media)
        description (description-with-original (:description metadata) url)
        creator (or (:channel metadata) (:uploader metadata))
        video-id (:id metadata)
        children (concat
                  [(xml/element :title {} title)]
                  (when description [(xml/element :plot {} description)])
                  (when-let [date (nfo-date metadata)]
                    [(xml/element :premiered {} date)])
                  (when creator [(xml/element :studio {} creator)])
                  (map #(xml/element :genre {} (str %)) (:categories metadata))
                  (map #(xml/element :tag {} (str %)) (:tags metadata))
                  (when video-id
                    [(xml/element :uniqueid {:type (or (:extractor metadata) "media")
                                             :default "true"}
                                  (str video-id))]))
        bytes (.getBytes
               (xml/emit-str (apply xml/element :movie {} children))
               StandardCharsets/UTF_8)
        filename (str (filename-stem (:display-name media)) ".nfo")]
    {:path (conj parent-path filename)
     :display-name filename
     :size (alength bytes)
     :mime-type "application/xml; charset=utf-8"
     :etag (digest/sha-256 bytes)
     :completed-at (:completed-at media)
     :resource-kind :bytes
     :bytes bytes}))

(defn- generated-artwork [path filename content width height]
  (let [bytes (artwork/cover content width height)]
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
          fanart-hash (or (:fanart-hash metadata)
                          (:poster-hash metadata)
                          (:thumbnail-hash metadata))]
      (filterv some?
               [media
                (nfo-file parent-path media)
                (transcript-file parent-path media)
                (artwork-file parent-path stem "-fanart" fanart-hash completed-at)]))
    []))

(defn- indexed-source-title [entries]
  (some #(some-> (:source-title %) str str/trim not-empty) entries))

(defn- source-display-name [source-name entries]
  (if-let [title (indexed-source-title entries)]
    (if (= title source-name)
      title
      (str title " · " source-name))
    source-name))

(defn- source-folder-files [segments source-name entries]
  (let [files (vec (mapcat #(item-files segments %) entries))
        source-title (or (indexed-source-title entries)
                         (some #(some-> (or (get-in % [:metadata :channel])
                                            (get-in % [:metadata :uploader]))
                                        str
                                        str/trim
                                        not-empty)
                               files)
                         source-name)
        content {:title source-title
                 :subtitle (when (not= source-title source-name) source-name)}]
    (into [(generated-artwork segments "folder.png" content 500 750)]
          files)))

(defn- favorite-files [segments _entries]
  (let [content {:title "LLAR Media"}]
    [(generated-artwork segments "favorite-atv.png" content 614 346)
     (generated-artwork segments "favorite.png" content 500 750)]))

(defn directory [segments entries]
  (case (count segments)
    0 {:self (collection [] "LLAR Media Library")
       :children [(collection ["By-Source"] "By Source")
                  (collection ["By-Date"] "By Date")]}

    1 (case (first segments)
        "By-Source"
        {:self (collection segments "By Source")
         :children (into (favorite-files segments entries)
                         (->> entries
                              (group-by :source-directory)
                              (sort-by key)
                              (map (fn [[source-name source-entries]]
                                     (collection
                                      ["By-Source" source-name]
                                      (source-display-name source-name
                                                           source-entries))))))}

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
                    (favorite-files ["By-Source"] entries))
              (let [matching (filter #(= value (:source-directory %)) entries)]
                (when (seq matching)
                  {:self (collection segments (source-display-name value matching))
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
