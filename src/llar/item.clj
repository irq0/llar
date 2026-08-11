(ns llar.item
  (:require
   [clojure.string :as string]
   [org.bovinegenius [exploding-fish :as uri]]
   [clojure.spec.alpha :as s]
   [llar.src :as src]))

(s/def :irq0-fetch-item/source src/source?)
(s/def :irq0-fetch-item/source-name string?)
(s/def :irq0-fetch-item/source-key keyword?)
(s/def :irq0-fetch-item/fetch-ts #(instance? java.time.ZonedDateTime %))
(s/def :irq0-fetch-item/tags  (s/coll-of keyword? :kind set?))
(s/def :irq0-fetch-item/version int?)
(s/def :irq0-fetch-item/language (s/nilable string?))
(s/def :irq0-fetch-item/ts #(instance? java.time.ZonedDateTime %))
(s/def :irq0-fetch-item/title string?)
(s/def :irq0-fetch-item/feed-type string?)
(s/def :irq0-fetch-item/pub-ts (s/nilable :irq0/ts))
(s/def :irq0-fetch-item/updated-ts :irq0/ts)
(s/def :irq0-fetch-item/descriptions map?)

(s/def :irq0/item-metadata (s/keys :req-un [:irq0-fetch-item/source
                                            :irq0-fetch-item/source-name
                                            :irq0-fetch-item/source-key
                                            :irq0-fetch-item/fetch-ts
                                            :irq0-fetch-item/tags
                                            :irq0-fetch-item/version]))

(s/def :irq0/item-summary (s/keys :req-un [:irq0-fetch-item/ts
                                           :irq0-fetch-item/title]))

(def item-hash-regex #"SHA-256:[0-9a-f]{64}$")
(s/def :irq0/item-hash (s/and string? #(re-matches item-hash-regex %)))

(s/def :irq0/feed (s/keys :req-un [:irq0-fetch-item/title
                                   :irq0-fetch-item/feed-type
                                   :irq0/url]
                          :opt-un [:irq0-fetch-item/language
                                   :irq0-fetch-item/pub-ts
                                   :irq0-fetch-item/updated-ts
                                   :irq0-fetch-item/descriptions]))

(defn best-content
  "Pick the best available content body from a queried item doc (as returned by
  `get-items-recent`/`get-item-by-id` with item data joined). Prefers the full
  content over the (often truncated) description, and HTML over plain text.
  Returns {:mime \"text/html\"|\"text/plain\" :data string} or nil when no body exists."
  [doc]
  (let [contents (get-in doc [:data :content])
        description (get-in doc [:data :description])]
    (some (fn [[mime m]]
            (when-let [data (get m mime)]
              {:mime mime :data data}))
          [["text/html" contents]
           ["text/plain" contents]
           ["text/html" description]
           ["text/plain" description]])))

(defn reading-time-estimate
  "Estimate reading time (minutes) and difficulty for a queried item doc, based
  on its word count and average top-word length."
  [item]
  (let [words-per-min 200
        {:keys [nwords top-words]} item
        top-word-strings (map first (get top-words "words" []))
        avg-word-len (if (seq top-word-strings)
                       (/ (reduce + (map count top-word-strings))
                          (count top-word-strings))
                       5.0)
        level (cond
                (< avg-word-len 4.5) :easy
                (< avg-word-len 6.0) :medium
                :else :hard)
        factor (case level
                 :easy 1
                 :medium 1.5
                 :hard 2)
        estimate (* (/ (or nwords 0) words-per-min) factor)]
    {:estimate (int (Math/ceil estimate))
     :difficulty level}))

(defn- normalized-tags [tags]
  (into #{}
        (keep (fn [tag]
                (cond
                  (keyword? tag) tag
                  (string? tag) (keyword tag))))
        tags))

(defn- primary-media-kind [url]
  (let [host (when url
               (try
                 (some-> url str uri/uri uri/host string/lower-case)
                 (catch Exception _ nil)))]
    (cond
      (and host
           (re-find #"(^|\.)(youtube\.com|youtu\.be|vimeo\.com|media\.ccc\.de)$|peertube"
                    host))
      :video

      (and host (re-find #"(^|\.)(soundcloud\.com|bandcamp\.com)$" host))
      :audio)))

(defn consumption-time
  "Describe the time needed to consume an item using already stored metadata.

  Media duration is exact and expressed in seconds. Prose duration remains an
  estimated reading time. Recognized video/audio without duration deliberately
  returns its kind without minutes, so callers do not mistake the media
  description's word count for the duration of the media itself."
  [{:keys [entry nwords tags] :as item}]
  (let [tags (normalized-tags tags)
        tagged-media-kind (cond
                            (contains? tags :has-video) :video
                            (contains? tags :has-audio) :audio)
        duration (:duration entry)
        duration-seconds (when (and (number? duration) (pos? duration))
                           (long duration))
        media-kind (or (primary-media-kind (or (:url entry) (:url item)))
                       (when duration-seconds tagged-media-kind))]
    (cond
      media-kind
      (cond-> {:kind media-kind :estimated? false}
        duration-seconds
        (assoc :seconds duration-seconds
               :minutes (int (Math/ceil (/ duration-seconds 60.0)))))

      (and (number? nwords) (pos? nwords))
      (let [{:keys [estimate difficulty]} (reading-time-estimate item)]
        {:kind :reading
         :minutes estimate
         :estimated? true
         :difficulty difficulty})

      :else nil)))
