(ns u1f596.fetch.mercury
  (:require
   [u1f596.converter :as conv]
   [u1f596.fetch :as fetch :refer [FetchSource]]
   [u1f596.postproc :refer [ItemProcessor]]
   [u1f596.persistency :refer [CouchItem]]
   [u1f596.analysis :as analysis]
   [u1f596.http :refer [absolutify-url absolutify-links-in-hick get-base-url-with-path blobify try-blobify-url! sanitize]]
   [u1f596.appconfig :as appconfig]
   [org.bovinegenius [exploding-fish :as uri]]
   [hickory.core :as hick]
   [hickory.render :as hick-r]
   [digest]
   [u1f596.item]
   [java-time.api :as time]
   [clojure.tools.logging :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.spec.alpha :as s]
   [clojure.java.shell :as shell]
   [cheshire.core :as json]))

(defrecord MercuryItem
           [meta
            summary
            hash
            entry]
  Object
  (toString [item] (fetch/item-to-string item)))

(defn make-mercury-item [meta summary hash entry]
  {:pre [(s/valid? :irq0/item-metadata meta)
         (s/valid? :irq0/item-summary summary)
         (s/valid? :irq0/item-hash hash)]}
  (->MercuryItem meta summary hash entry))

(extend-protocol ItemProcessor
  MercuryItem
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
  MercuryItem
  (to-couch [item]
    (-> item
        (dissoc :raw)
        (dissoc :body)
        (assoc-in [:meta :source :args] nil)
        (assoc :type :bookmark))))

(defn mercury-local
  [url]
  {:pre [(s/valid? :irq0/url url)]}
  (try+
   (let [url (uri/uri url)
         {:keys [exit out err]} (shell/sh (appconfig/command :mercury-parser) (str url))
         base-url (get-base-url-with-path url)
         json (json/parse-string out true)]
     (if (and (zero? exit) (not (:failed json)))
       (assoc json :content
              (try
                (-> json
                    :content
                    hick/parse
                    hick/as-hickory
                    (absolutify-links-in-hick base-url)
                    sanitize
                    blobify
                    hick-r/hickory-to-html)
                (catch Throwable th
                  (log/warn th "Mercury local post processing failed. Using vanilla. Url:" url)
                  (log/debug {:content json
                              :url url
                              :mercury {:out out
                                        :err err
                                        :exit exit}
                              :json json}))))
       (do
         (log/error "Mercury Error: " url err json out)
         (throw+ {:type ::not-parsable
                  :url url
                  :message (:message json)}))))
   (catch Object _
     (log/error (:throwable &throw-context) "Unexpected error. URL: " url)
     (throw+))))

(extend-protocol FetchSource
  u1f596.src.MercuryWebParser
  (fetch-source [src]
    (let [url (uri/uri (:url src))
          base-url (get-base-url-with-path url)
          mercu (mercury-local url)
          pub-ts (or (when (string? (:date_published mercu)) (time/zoned-date-time (time/formatter :iso-zoned-date-time)
                                                                                   (:date_published mercu)))
                     (time/zoned-date-time))
          title (cond
                  (string? (:title mercu)) (:title mercu)
                  (vector? (:title mercu)) (first (:title mercu))
                  :else "")]
      [(make-mercury-item
        (fetch/make-meta src)
        {:ts pub-ts :title title}
        (fetch/make-item-hash (:content mercu))
        {:url (absolutify-url (uri/uri (:url mercu)) base-url)
         :lead-image-url (some-> (:lead_image_url mercu)
                                 uri/uri
                                 (absolutify-url base-url)
                                 try-blobify-url!)
         :pub-ts pub-ts
         :title title
         :authors [(or (:author mercu) (:domain mercu))]
         :descriptions {"text/plain" (:excerpt mercu)}
         :contents {"text/html" (:content mercu)
                    "text/plain" (conv/html2text (:content mercu))}})])))
