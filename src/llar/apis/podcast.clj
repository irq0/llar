(ns llar.apis.podcast
  (:require
   [cheshire.core :as json]
   [clojure.data.xml :as xml]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [compojure.core :refer [GET routes]]
   [compojure.route :as route]
   [clojure.tools.logging :as log]
   [java-time.api :as time]
   [slingshot.slingshot :refer [try+]]
   [llar.appconfig :as appconfig]
   [llar.blobstore :as blobstore]
   [llar.persistency :as persistency]
   [llar.podcast :as podcast]
   [llar.store :as store])
  (:import
   [java.io FileInputStream]
   [java.time ZonedDateTime]
   [java.time.format DateTimeFormatter]
   [java.util Locale]
   [org.apache.commons.io.input BoundedInputStream]))

(defn wrap-token-auth [handler]
  (fn [request]
    (let [expected-token (appconfig/credentials :podcast-token)
          provided-token (get-in request [:params "token"])]
      (if (and (some? expected-token)
               (= expected-token provided-token))
        (handler request)
        {:status 403
         :headers {"Content-Type" "text/plain"}
         :body "Forbidden: invalid or missing token"}))))

(defn podcast-base-url []
  (appconfig/podcast :base-url))

;;;; RSS Feed Generation

(def ^:private itunes-ns "http://www.itunes.com/dtds/podcast-1.0.dtd")
(def ^:private podcast-ns "https://podcastindex.org/namespace/1.0")
(def ^:private content-ns "http://purl.org/rss/1.0/modules/content/")

(defn format-duration
  "Format seconds as HH:MM:SS for itunes:duration"
  [seconds]
  (when seconds
    (let [s (int seconds)
          h (quot s 3600)
          m (quot (mod s 3600) 60)
          ss (mod s 60)]
      (format "%02d:%02d:%02d" h m ss))))

(def ^:private rfc2822-formatter
  (-> (DateTimeFormatter/ofPattern "EEE, dd MMM yyyy HH:mm:ss Z")
      (.withLocale Locale/ENGLISH)))

(defn format-rfc2822
  "Format ZonedDateTime as RFC 2822 for RSS pubDate"
  [zdt]
  (when zdt
    (.format rfc2822-formatter zdt)))

(defn- linkify-urls
  "Convert URLs in plain text to HTML links"
  [text]
  (str/replace text #"https?://[^\s<>\"\)]+"
               (fn [url] (str "<a href=\"" url "\">" url "</a>"))))

(defn- format-description-html
  "Format description text as HTML with provenance header"
  [description source-key original-url]
  (let [source-line (when source-key
                      (str "<p><strong>Source:</strong> " (name source-key)
                           (when original-url
                             (str " | <a href=\"" original-url "\">Original</a>"))
                           "</p><hr/>"))
        body (when (not (str/blank? description))
               (-> description
                   (str/replace #"&" "&amp;")
                   (str/replace #"<" "&lt;")
                   (str/replace #">" "&gt;")
                   (str/replace #"\n" "<br/>\n")
                   linkify-urls))]
    (str source-line body)))

(defn- chapters->json
  "Convert yt-dlp chapters to Podcasting 2.0 chapters JSON"
  [chapters]
  (json/generate-string
   {:version "1.2.0"
    :chapters (mapv (fn [ch]
                      (cond-> {:startTime (:start_time ch)
                               :title (:title ch)}
                        (:end_time ch) (assoc :endTime (:end_time ch))))
                    chapters)}))

(defn- make-item-xml
  "Build a single RSS <item> element for a podcast episode"
  [item download-info base-url token]
  (let [{:keys [blob-hash metadata mime-type]} download-info
        blob (blobstore/get-blob blob-hash)
        enclosure-url (str base-url "/media/" blob-hash "?token=" token)
        title (or (:title item) (get-in item [:entry :title]) "Untitled")
        pub-date (:ts item)
        plain-desc (or (get-in item [:entry :descriptions "text/plain"])
                       (get-in item [:entry :descriptions :text/plain])
                       (:title item)
                       "")
        guid (str "llar-podcast-" (:id item))
        source-key (:source-key item)
        original-url (or (get-in item [:entry :url]) (:url item))
        thumbnail-hash (:thumbnail-hash metadata)
        chapters (:chapters metadata)
        description (:description metadata)
        transcript (:transcript metadata)]
    (xml/element :item {}
                 (filterv some?
                          [(xml/element :title {} title)
                           (xml/element :description {} (str plain-desc))
                           (xml/element :enclosure {:url enclosure-url
                                                    :length (str (:size blob))
                                                    :type (or mime-type (:mime-type blob) "video/mp4")})
                           (xml/element :guid {:isPermaLink "false"} guid)
                           (xml/element :pubDate {} (format-rfc2822
                                                     (if (instance? ZonedDateTime pub-date)
                                                       pub-date
                                                       (time/zoned-date-time))))
                           (xml/element (xml/qname itunes-ns "duration") {}
                                        (or (format-duration (:duration metadata)) "00:00:00"))
                           (xml/element (xml/qname itunes-ns "author") {}
                                        (or (:author item)
                                            (:uploader metadata)
                                            "LLAR Podcast"))
                           (when source-key
                             (xml/element (xml/qname itunes-ns "subtitle") {}
                                          (str "via " (name source-key))))
                           (when thumbnail-hash
                             (xml/element (xml/qname itunes-ns "image")
                                          {:href (str base-url "/artwork/" thumbnail-hash "?token=" token)}))
                           (when (seq chapters)
                             (xml/element (xml/qname podcast-ns "chapters")
                                          {:url (str base-url "/chapters/" blob-hash "?token=" token)
                                           :type "application/json+chapters"}))
                           (when transcript
                             (xml/element (xml/qname podcast-ns "transcript")
                                          {:url (str base-url "/transcript/" blob-hash "?token=" token)
                                           :type "text/plain"
                                           :language "en"}))
                           (xml/element (xml/qname itunes-ns "explicit") {} "false")
                           (xml/element (xml/qname itunes-ns "episodeType") {} "full")
                           (xml/element (xml/qname content-ns "encoded") {}
                                        (xml/cdata (format-description-html
                                                    (or description plain-desc)
                                                    source-key
                                                    (str original-url))))]))))

(defn generate-feed-xml
  "Generate full RSS 2.0 podcast feed XML string"
  [base-url token]
  (let [completed (->> @podcast/download-state
                       (filter (fn [[_ v]] (= :complete (:status v))))
                       (into {}))
        ;; TODO: add TTS support for text articles in the future
        all-items (try+
                   (persistency/get-items-recent store/backend-db
                                                 {:with-tag :podcast
                                                  :limit 200})
                   (catch Object e
                     (log/error e "podcast: failed to query items for feed")
                     []))
        items-by-id (into {} (map (juxt :id identity) all-items))
        item-elements (->> completed
                           (keep (fn [[item-id dl-info]]
                                   (when-let [item (get items-by-id item-id)]
                                     [item dl-info])))
                           (sort-by (fn [[item _]] (:ts item)) #(compare %2 %1))
                           (map (fn [[item dl-info]]
                                  (make-item-xml item dl-info base-url token))))]
    (xml/emit-str
     (xml/element :rss {:version "2.0"
                        (keyword "xmlns" "itunes") itunes-ns
                        (keyword "xmlns" "podcast") podcast-ns
                        (keyword "xmlns" "content") content-ns}
                  (apply xml/element :channel {}
                         (xml/element :title {} "LLAR Podcast")
                         (xml/element :link {} base-url)
                         (xml/element :description {} "Media from LLAR - Live Long and Read")
                         (xml/element :language {} "en")
                         (xml/element (xml/qname itunes-ns "author") {} "LLAR")
                         (xml/element (xml/qname itunes-ns "explicit") {} "false")
                         (xml/element (xml/qname itunes-ns "type") {} "episodic")
                         (xml/element (xml/qname itunes-ns "summary") {} "Media from LLAR - Live Long and Read")
                         (xml/element (xml/qname itunes-ns "category") {:text "Technology"})
                         item-elements)))))

;;;; Byte-range support

(defn- parse-byte-range
  "Parse Range header value. Returns [start end] or nil."
  [range-header size]
  (when range-header
    (when-let [[_ start end] (re-matches #"bytes=(\d+)-(\d*)" range-header)]
      (let [s (Long/parseLong start)
            e (if (str/blank? end) (dec size) (Long/parseLong end))]
        (when (<= 0 s e (dec size))
          [s e])))))

(defn- serve-blob
  "Serve blob with Range request support. Returns Ring response."
  [blob range-header]
  (let [file (:file blob)
        size (:size blob)
        hash (:hash blob)
        common-headers {"Content-Type" (:mime-type blob)
                        "Accept-Ranges" "bytes"
                        "Etag" hash}]
    (if-let [[start end] (parse-byte-range range-header size)]
      (let [length (inc (- end start))
            fis (FileInputStream. (io/as-file file))]
        (.skip fis start)
        {:status 206
         :headers (merge common-headers
                         {"Content-Length" (str length)
                          "Content-Range" (format "bytes %d-%d/%d" start end size)})
         :body (-> (BoundedInputStream/builder)
                   (.setInputStream fis)
                   (.setMaxCount length)
                   (.get))})
      {:status 200
       :headers (merge common-headers
                       {"Content-Length" (str size)})
       :body file})))

;;;; Routes

(def app
  (routes
   (GET "/feed.xml" req
     (let [token (get-in req [:params "token"])]
       {:status 200
        :headers {"Content-Type" "application/rss+xml; charset=utf-8"}
        :body (generate-feed-xml (podcast-base-url) token)}))

   (GET "/media/:hash" req
     (let [hash (get-in req [:params :hash])
           range-header (get-in req [:headers "range"])]
       (try+
        (let [blob (blobstore/get-blob hash)]
          (serve-blob blob range-header))
        (catch Object e
          (log/warn e "podcast: media get-blob failed:" hash)
          {:status 404
           :headers {"Content-Type" "text/plain"}
           :body "Not Found"}))))

   (GET "/artwork/:hash" [hash]
     (try+
      (let [blob (blobstore/get-blob hash)]
        {:status 200
         :headers {"Content-Type" (or (:mime-type blob) "image/jpeg")
                   "Content-Length" (str (:size blob))
                   "Cache-Control" "public, max-age=86400"
                   "Etag" hash}
         :body (:data blob)})
      (catch Object e
        (log/warn e "podcast: artwork get-blob failed:" hash)
        {:status 404
         :headers {"Content-Type" "text/plain"}
         :body "Not Found"})))

   (GET "/chapters/:hash" [hash]
     (try+
      (let [blob (blobstore/get-blob hash)
            chapters (get-in blob [:podcast-metadata :chapters])]
        (if (seq chapters)
          {:status 200
           :headers {"Content-Type" "application/json+chapters"}
           :body (chapters->json chapters)}
          {:status 404
           :headers {"Content-Type" "text/plain"}
           :body "No chapters available"}))
      (catch Object e
        (log/warn e "podcast: chapters failed for:" hash)
        {:status 404
         :headers {"Content-Type" "text/plain"}
         :body "Not Found"})))

   (GET "/transcript/:hash" [hash]
     (try+
      (let [blob (blobstore/get-blob hash)
            transcript (get-in blob [:podcast-metadata :transcript])]
        (if transcript
          {:status 200
           :headers {"Content-Type" "text/plain; charset=utf-8"}
           :body transcript}
          {:status 404
           :headers {"Content-Type" "text/plain"}
           :body "No transcript available"}))
      (catch Object e
        (log/warn e "podcast: transcript failed for:" hash)
        {:status 404
         :headers {"Content-Type" "text/plain"}
         :body "Not Found"})))

   (route/not-found "404")))
