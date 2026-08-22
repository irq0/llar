(ns llar.apis.podcast
  (:require
   [cheshire.core :as json]
   [clojure.data.xml :as xml]
   [clojure.string :as str]
   [compojure.core :refer [GET HEAD routes]]
   [compojure.route :as route]
   [clojure.tools.logging :as log]
   [java-time.api :as time]
   [slingshot.slingshot :refer [try+]]
   [llar.appconfig :as appconfig]
   [llar.auth :as auth]
   [llar.blobstore :as blobstore]
   [llar.http.middleware :as http-middleware]
   [llar.http.response :as http-response]
   [llar.media-artwork :as media-artwork]
   [llar.persistency :as persistency]
   [llar.podcast :as podcast]
   [llar.store :as store])
  (:import
   [java.nio.charset StandardCharsets]
   [java.time ZonedDateTime]
   [java.time.format DateTimeFormatter]
   [java.util Locale]))

(defn wrap-token-auth [handler]
  (fn [request]
    (let [expected-token (appconfig/credentials :podcast-token)
          provided-token (get-in request [:params "token"])]
      (http-middleware/mark-private
       (if (and (some? expected-token)
                (auth/constant-time= expected-token provided-token))
         (handler request)
         {:status 403
          :headers {"Content-Type" "text/plain"
                    "Cache-Control" "no-store"}
          :body "Forbidden: invalid or missing token"})))))

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
        blob (blobstore/get-blob-metadata blob-hash)
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
                                           :type (or (:transcript-mime-type metadata)
                                                     "text/vtt")
                                           :language "en"}))
                           (xml/element (xml/qname itunes-ns "explicit") {} "false")
                           (xml/element (xml/qname itunes-ns "episodeType") {} "full")
                           (xml/element (xml/qname content-ns "encoded") {}
                                        (xml/cdata (format-description-html
                                                    (or description plain-desc)
                                                    source-key
                                                    (str original-url))))]))))

(defn generate-feed-xml
  "Generate full RSS 2.0 podcast feed XML string.
   Optional opts map: {:source-key :some-source} to filter by source."
  [base-url token & [{:keys [source-key]}]]
  (let [completed (cond->> (->> @podcast/download-state
                                (filter (fn [[_ v]] (= :complete (:status v)))))
                    source-key (filter (fn [[_ v]]
                                         (= (name source-key)
                                            (name (:source-key v)))))
                    true (into {}))
        ;; TODO: add TTS support for text articles in the future
        all-items (try+
                   (persistency/get-items-recent store/backend-db
                                                 {:with-tag :podcast
                                                  :limit 200})
                   (catch Object e
                     (log/error e "podcast: failed to query items for feed")
                     []))
        items-by-id (into {} (map (juxt :id identity) all-items))
        sorted-pairs (->> completed
                          (keep (fn [[item-id dl-info]]
                                  (when-let [item (get items-by-id item-id)]
                                    [item dl-info])))
                          (sort-by (fn [[item _]] (:ts item)) #(compare %2 %1)))
        item-elements (map (fn [[item dl-info]]
                             (make-item-xml item dl-info base-url token))
                           sorted-pairs)
        feed-title (if source-key
                     (str "LLAR Podcast - " (name source-key))
                     "LLAR Podcast")
        feed-desc (if source-key
                    (str "Media from LLAR - " (name source-key))
                    "Media from LLAR - Live Long and Read")
        image-url (str base-url "/channel-image.png?token=" token
                       (when source-key (str "&source=" (name source-key))))
        channel-elements [(xml/element :title {} feed-title)
                          (xml/element :link {} base-url)
                          (xml/element :description {} feed-desc)
                          (xml/element :language {} "en")
                          (xml/element (xml/qname itunes-ns "image") {:href image-url})
                          (xml/element :image {}
                                       (xml/element :url {} image-url)
                                       (xml/element :title {} feed-title)
                                       (xml/element :link {} base-url))
                          (xml/element (xml/qname itunes-ns "author") {} "LLAR")
                          (xml/element (xml/qname itunes-ns "explicit") {} "false")
                          (xml/element (xml/qname itunes-ns "block") {} "Yes")
                          (xml/element (xml/qname itunes-ns "type") {} "episodic")
                          (xml/element (xml/qname itunes-ns "summary") {} feed-desc)
                          (xml/element (xml/qname itunes-ns "category") {:text "Technology"})]]
    (xml/emit-str
     (xml/element :rss {:version "2.0"
                        (keyword "xmlns" "itunes") itunes-ns
                        (keyword "xmlns" "podcast") podcast-ns
                        (keyword "xmlns" "content") content-ns}
                  (apply xml/element :channel {}
                         (concat channel-elements item-elements))))))

;;;; Routes

(defn- media-response [request]
  (let [hash (get-in request [:params :hash])]
    (try+
     (let [blob (blobstore/get-blob-metadata hash)]
       (http-response/ranged-file-response
        {:file (:file blob)
         :size (:size blob)
         :mime-type (:mime-type blob)
         :etag hash
         :last-modified (:created blob)}
        request))
     (catch Object exception
       (log/warn exception "podcast: media get-blob failed:" hash)
       {:status 404
        :headers {"Content-Type" "text/plain"}
        :body "Not Found"}))))

(defn- channel-image-response [request]
  (try+
   (let [label (or (get-in request [:params "source"]) "LLAR")
         bytes (media-artwork/cover label 1400 1400)]
     (assoc-in (http-response/byte-array-response
                bytes {:mime-type "image/png"} request)
               [:headers "Cache-Control"] "public, max-age=604800"))
   (catch Object exception
     (log/warn exception "podcast: channel-image generation failed")
     {:status 500
      :headers {"Content-Type" "text/plain"}
      :body "Internal Server Error"})))

(defn- artwork-response [request]
  (let [hash (get-in request [:params :hash])]
    (try+
     (let [blob (blobstore/get-blob-metadata hash)]
       (assoc-in
        (http-response/ranged-file-response
         {:file (:file blob)
          :size (:size blob)
          :mime-type (or (:mime-type blob) "image/jpeg")
          :etag hash
          :last-modified (:created blob)}
         request)
        [:headers "Cache-Control"] "public, max-age=86400"))
     (catch Object exception
       (log/warn exception "podcast: artwork get-blob failed:" hash)
       {:status 404
        :headers {"Content-Type" "text/plain"}
        :body "Not Found"}))))

(defn- chapters-response [request]
  (let [hash (get-in request [:params :hash])]
    (try+
     (let [blob (blobstore/get-blob-metadata hash)
           chapters (get-in blob [:podcast-metadata :chapters])]
       (if (seq chapters)
         (http-response/byte-array-response
          (.getBytes (chapters->json chapters) StandardCharsets/UTF_8)
          {:mime-type "application/json+chapters"
           :etag (str hash "-chapters")
           :last-modified (:created blob)}
          request)
         {:status 404
          :headers {"Content-Type" "text/plain"}
          :body "No chapters available"}))
     (catch Object exception
       (log/warn exception "podcast: chapters failed for:" hash)
       {:status 404
        :headers {"Content-Type" "text/plain"}
        :body "Not Found"}))))

(defn- transcript-response [request]
  (let [hash (get-in request [:params :hash])]
    (try+
     (let [blob (blobstore/get-blob-metadata hash)
           transcript (get-in blob [:podcast-metadata :transcript])
           transcript-mime-type (get-in blob [:podcast-metadata
                                              :transcript-mime-type])]
       (if transcript
         (http-response/byte-array-response
          (.getBytes ^String transcript StandardCharsets/UTF_8)
          {:mime-type (str (or transcript-mime-type "text/vtt")
                           "; charset=utf-8")
           :etag (str hash "-transcript")
           :last-modified (:created blob)}
          request)
         {:status 404
          :headers {"Content-Type" "text/plain"}
          :body "No transcript available"}))
     (catch Object exception
       (log/warn exception "podcast: transcript failed for:" hash)
       {:status 404
        :headers {"Content-Type" "text/plain"}
        :body "Not Found"}))))

(def app
  (routes
   (GET "/feed.xml" req
     (let [token (get-in req [:params "token"])]
       {:status 200
        :headers {"Content-Type" "application/rss+xml; charset=utf-8"}
        :body (generate-feed-xml (podcast-base-url) token)}))

   (GET "/feed/:source-key.xml" req
     (let [token (get-in req [:params "token"])
           source-key (keyword (get-in req [:params :source-key]))]
       {:status 200
        :headers {"Content-Type" "application/rss+xml; charset=utf-8"}
        :body (generate-feed-xml (podcast-base-url) token {:source-key source-key})}))

   (GET "/media/:hash" request (media-response request))
   (HEAD "/media/:hash" request (media-response request))

   (GET "/channel-image.png" request (channel-image-response request))
   (HEAD "/channel-image.png" request (channel-image-response request))

   (GET "/artwork/:hash" request (artwork-response request))
   (HEAD "/artwork/:hash" request (artwork-response request))

   (GET "/chapters/:hash" request (chapters-response request))
   (HEAD "/chapters/:hash" request (chapters-response request))

   (GET "/transcript/:hash" request (transcript-response request))
   (HEAD "/transcript/:hash" request (transcript-response request))

   (route/not-found "404")))
