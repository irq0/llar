(ns llar.fetch.readability
  (:require
   [llar.fetch :as fetch :refer [FetchSource]]
   [llar.postproc :refer [ItemProcessor]]
   [llar.persistency :refer [CouchItem]]
   [llar.analysis :as analysis]
   [llar.http :as http]
   [llar.commands :refer [download-subtitles]]
   [llar.converter :as conv]
   [clojure.string :as string]
   [slingshot.slingshot :refer [try+]]
   [org.bovinegenius [exploding-fish :as uri]]
   [hickory.core :as hick]
   [hickory.render :as hick-r]
   [digest]
   [llar.item]
   [java-time.api :as time]
   [clojure.tools.logging :as log]
   [clojure.spec.alpha :as s]))

(defrecord ReadabilityItem
           [meta
            summary
            hash
            entry]
  Object
  (toString [item] (fetch/item-to-string item)))

(def +fetch-subtitles-publisher+ #{"Youtube"})

(defn make-readability-item [meta summary hash entry]
  {:pre [(s/valid? :irq0/item-metadata meta)
         (s/valid? :irq0/item-summary summary)
         (s/valid? :irq0/item-hash hash)]}
  (->ReadabilityItem meta summary hash entry))

(extend-protocol ItemProcessor
  ReadabilityItem
  (post-process-item [item _src _state]
    (let [nlp (analysis/analyze-entry (:entry item))
          tags (set
                (remove nil?
                        [(when (some #(re-find #"^https?://\w+\.(youtube|vimeo|youtu)" %) (:urls nlp))
                           :has-video)
                         (when (and (string? (:url item)) (re-find #"^https?://\w+\.(youtube|vimeo|youtu)" (:url item)))
                           :has-video)]))]

      (-> item
          (update-in [:meta :tags] into tags)
          (update :entry merge (:entry item) nlp))))

  (filter-item [_ _ _] false))

(extend-protocol CouchItem
  ReadabilityItem
  (to-couch [item]
    (-> item
        (dissoc :raw)
        (dissoc :body)
        (assoc-in [:meta :source :args] nil)
        (assoc :type :bookmark))))

;; TODO(irq0) tag with has-video has-playlist

(defn- subtitle-fetch [url]
  (try+
   (let [{:keys [format subtitles]} (download-subtitles url)]
     (cond
       (= format :ttml)
       (conv/ttml2text subtitles)
       :default nil))
   (catch Object e
     (log/errorf e "subtitle fetch %s failed" url)
     nil)))

(defn- readability-fetch
  [url user-agent]
  {:pre [(s/valid? :irq0/url url)]}
  (let [url (uri/uri url)
        response (http/fetch url :user-agent user-agent
                             :sanitize? true
                             :blobify? false
                             :absolutify-urls? false)]
    (when (= :ok (:status response))
      (let [readab (http/raw-readability (:body response) url)
            hick (some-> (:content readab)
                         hick/parse
                         hick/as-hickory)
            processed (-> hick
                          (http/blobify))
            subtitles (when (contains? +fetch-subtitles-publisher+ (get-in readab [:metadata :publisher]))
                        (subtitle-fetch url))
            ]
        (-> response
            (assoc :readability readab)
            (assoc :transscript subtitles)
            (assoc :body (when processed (hick-r/hickory-to-html processed)))
            (assoc :hickory processed))))))

(extend-protocol FetchSource
  llar.src.Readability
  (fetch-source [src _conditional-tokens]
    (let [url (uri/uri (:url src))
          base-url (http/get-base-url-with-path url)
          fetch (readability-fetch url (http/resolve-user-agent (get src :user-agent :default)))
          data (:readability fetch)
          metadata (:metadata data)
          title (or (when-not (string/blank? (:title data)) (:title data))
                    (when-not (string/blank? (:title metadata)) (:title metadata))
                    (get-in fetch [:summary :title])
                    "")
          pub-ts (or (when-not (string/blank? (:publishedTime data)) (time/zoned-date-time (time/formatter :iso-zoned-date-time)
                                                                                           (:publishedTime data)))
                     (when-not (string/blank? (:date metadata)) (time/zoned-date-time (time/formatter :iso-zoned-date-time)
                                                                                      (:date metadata)))
                     (get-in fetch [:summary :ts]))]
      [(make-readability-item
        (fetch/make-meta src)
        {:ts pub-ts :title title}
        (fetch/make-item-hash (or (:content data) (:url metadata)))
        {:url (http/absolutify-url (uri/uri url) base-url)
         :pub-ts pub-ts
         :title title
         :readability-metadata metadata
         :lead-image-url (get-in data [:metadata :image])
         :authors [(or (:byline data) (:author metadata) (:siteName data) (:publisher metadata))]
         :descriptions {"text/plain" (or (:excerpt data) (:description metadata))}
         :contents {"text/html" (:body fetch)
                    "text/plain" (or (:textContent data) (:transscript fetch))}})])))
