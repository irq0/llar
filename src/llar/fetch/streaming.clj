(ns llar.fetch.streaming
  (:require
   [llar.fetch :as fetch]
   [llar.specs]
   [llar.persistency :as persistency]
   [llar.postproc :as postproc]
   [llar.analysis :as analysis]
   [llar.item]
   [llar.appconfig :refer [appconfig]]
   [clojure.spec.alpha :as s]
   [hiccup2.core :refer [html]]
   [clj-http.client :as http]
   [java-time.api :as time]
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [slingshot.slingshot :refer [throw+]]
   [org.bovinegenius [exploding-fish :as uri]])
  (:import
   [org.schabi.newpipe.extractor NewPipe]
   [org.schabi.newpipe.extractor.channel.tabs ChannelTabs]
   [org.schabi.newpipe.extractor.stream StreamInfoItem]
   [org.schabi.newpipe.extractor.downloader Downloader Request Response]
   [org.schabi.newpipe.extractor.localization Localization]
   [org.schabi.newpipe.extractor.exceptions ExtractionException ContentNotAvailableException ContentNotSupportedException]
   [java.util.concurrent Semaphore]))

;;; clj-http backed Downloader for NewPipeExtractor

(def +user-agent+ "Mozilla/5.0 (Windows NT 10.0; rv:128.0) Gecko/20100101 Firefox/128.0")

(defn- flatten-headers
  "Convert Map<String, List<String>> to Map<String, String> for clj-http"
  [headers]
  (into {}
        (map (fn [[k vs]]
               [k (if (and (coll? vs) (seq vs))
                    (first vs)
                    (str vs))]))
        headers))

(defn- expand-headers
  "Convert clj-http response headers to Map<String, List<String>> for NewPipe"
  [headers]
  (java.util.HashMap.
   (into {}
         (map (fn [[k v]]
                [(name k) (java.util.ArrayList. ^java.util.Collection (if (coll? v) v [v]))]))
         headers)))

(defn- request-to-clj-http
  "Convert NewPipe Request to clj-http options map"
  [^Request request]
  (let [url (.url request)
        headers (flatten-headers (.headers request))
        data-to-send (.dataToSend request)
        method (string/lower-case (.httpMethod request))]
    {:url url
     :method method
     :headers (assoc headers "User-Agent" +user-agent+)
     :body (when (and data-to-send (pos? (alength data-to-send))) data-to-send)}))

(defn- make-downloader
  "Create a NewPipe Downloader backed by clj-http"
  []
  (proxy [Downloader] []
    (execute [^Request request]
      (let [{:keys [url method headers body]} (request-to-clj-http request)
            request-fn (case method
                         "get" http/get
                         "post" http/post
                         "head" http/head
                         "put" http/put
                         http/get)
            resp (request-fn url
                             (cond-> {:headers headers
                                      :throw-exceptions false
                                      :redirect-strategy :lax
                                      :socket-timeout 30000
                                      :connection-timeout 15000}
                               body (assoc :body body)))]
        (Response. (int (:status resp))
                   ""
                   (expand-headers (:headers resp))
                   (str (:body resp))
                   url)))))

;;; Initialization

(defonce init!
  (delay
    (log/info "Initializing NewPipeExtractor")
    (NewPipe/init (make-downloader) (Localization. "en" "US"))))

;;; Rate limiting

(defonce +semaphore-streaming+
  (delay (Semaphore. (get-in appconfig [:throttle :streaming-max-concurrent] 1))))

(defmacro with-streaming-throttle [& body]
  `(let [sem# @+semaphore-streaming+]
     (.acquire sem#)
     (try
       ~@body
       (finally
         (.release sem#)))))

;;; StreamingItem

(defrecord StreamingItem
           [meta
            summary
            hash
            entry]
  Object
  (toString [item] (fetch/item-to-string item)))

(defn make-streaming-item [meta summary hash entry]
  {:pre [(s/valid? :irq0/item-metadata meta)
         (s/valid? :irq0/item-summary summary)
         (s/valid? :irq0/item-hash hash)]}
  (->StreamingItem meta summary hash entry))

;;; Protocol extensions

(extend-protocol postproc/ItemProcessor
  StreamingItem
  (post-process-item [item _src _state]
    (let [nlp (analysis/analyze-entry (:entry item))
          url (str (get-in item [:entry :url]))
          tags (set
                (remove nil?
                        [(when (re-find #"(?i)(youtube\.com|youtu\.be|vimeo\.com|peertube|media\.ccc\.de)" url)
                           :has-video)
                         (when (re-find #"(?i)(soundcloud\.com|bandcamp\.com)" url)
                           :has-audio)]))]
      (-> item
          (update-in [:meta :tags] into tags)
          (update :entry merge (:entry item) nlp))))
  (filter-item [_ _ _] false))

(extend-protocol persistency/CouchItem
  StreamingItem
  (to-couch [item]
    (-> item
        (assoc :type :link)
        (dissoc :raw)
        (assoc-in [:meta :source :args] nil))))

;;; Helpers

(defn- format-duration [seconds]
  (when (and seconds (pos? seconds))
    (let [h (quot seconds 3600)
          m (quot (mod seconds 3600) 60)
          s (mod seconds 60)]
      (if (pos? h)
        (format "%d:%02d:%02d" h m s)
        (format "%d:%02d" m s)))))

(defn- stream-info-item-to-date [^StreamInfoItem item]
  (when-let [date (.getUploadDate item)]
    (try
      (let [offset-dt (.offsetDateTime date)]
        (time/zoned-date-time offset-dt (time/zone-id "UTC")))
      (catch Exception e
        (log/debugf "Could not parse upload date for %s: %s" (.getUrl item) (.getMessage e))
        nil))))

(defn- best-thumbnail [^StreamInfoItem item]
  (let [thumbs (.getThumbnails item)]
    (when (seq thumbs)
      (.getUrl (last thumbs)))))

(defn- streaming-html-summary
  "Generate HTML summary for a streaming item"
  [title uploader-name duration-secs thumbnail-url url]
  (str
   (html
    [:div {:class "summary"}
     [:h1 title]
     (when thumbnail-url
       [:p [:img {:src thumbnail-url :style "max-width:100%"}]])
     [:ul
      [:li {:class "item-key-uploader"} [:span {:class "key"} "Channel: "] uploader-name]
      (when duration-secs
        [:li {:class "item-key-duration"} [:span {:class "key"} "Duration: "] (format-duration duration-secs)])
      [:li {:class "item-key-url"} [:a {:href (str url)} "Watch"]]]])))

(defn- stream-info-item-to-entry
  "Convert a NewPipe StreamInfoItem to an LLAR entry map"
  [^StreamInfoItem item channel-name]
  (let [url (uri/uri (.getUrl item))
        title (or (.getName item) "Untitled")
        uploader (or (.getUploaderName item) channel-name)
        pub-ts (stream-info-item-to-date item)
        duration (.getDuration item)
        duration-secs (when (pos? duration) duration)
        thumbnail (best-thumbnail item)
        description (let [d (.getShortDescription item)]
                      (when-not (string/blank? d) d))]
    {:url url
     :title title
     :authors [uploader]
     :pub-ts pub-ts
     :thumbnail thumbnail
     :duration duration-secs
     :descriptions (when description {"text/plain" description})
     :contents {"text/html" (streaming-html-summary title uploader duration-secs thumbnail url)
                "text/plain" description}}))

;;; Extraction

(defn- get-channel-videos
  "Extract video items from a streaming channel URL"
  [url max-results]
  @init!
  (with-streaming-throttle
    (try
      (let [service (NewPipe/getServiceByUrl (str url))
            extractor (.getChannelExtractor service (str url))]
        (.fetchPage extractor)
        (let [channel-name (.getName extractor)
              tabs (.getTabs extractor)
              video-tab (first (filter #(= (first (.getContentFilters %)) ChannelTabs/VIDEOS) tabs))]
          (if video-tab
            (let [tab-extractor (.getChannelTabExtractor service video-tab)]
              (.fetchPage tab-extractor)
              (let [page (.getInitialPage tab-extractor)
                    items (.getItems page)]
                {:channel-name channel-name
                 :items (take max-results
                              (filter #(instance? StreamInfoItem %) items))}))
            (do
              (log/warnf "No videos tab found for channel %s" url)
              {:channel-name channel-name
               :items []}))))
      (catch ContentNotAvailableException e
        (log/warnf "Content not available for %s: %s" url (.getMessage e))
        (throw+ {:type :llar.http/request-error :url url :msg (.getMessage e)}))
      (catch ContentNotSupportedException e
        (log/warnf "Content not supported for %s: %s" url (.getMessage e))
        (throw+ {:type :llar.http/request-error :url url :msg (.getMessage e)}))
      (catch org.schabi.newpipe.extractor.exceptions.ReCaptchaException e
        (log/warnf "ReCaptcha triggered for %s: %s" url (.getMessage e))
        (throw+ {:type :llar.http/server-error-retry-later :url url :msg (.getMessage e)}))
      (catch java.io.IOException e
        (log/warnf "IO error fetching %s: %s" url (.getMessage e))
        (throw+ {:type :llar.http/server-error-retry-later :url url :msg (.getMessage e)}))
      (catch ExtractionException e
        (log/warnf "Extraction error for %s: %s" url (.getMessage e))
        (throw+ {:type :llar.http/server-error-retry-later :url url :msg (.getMessage e)})))))

;;; FetchSource protocol

(extend-protocol fetch/FetchSource
  llar.src.StreamingChannel
  (fetch-source [src _conditional-tokens]
    (let [url (str (:url src))
          max-results (get-in src [:args :max-results] 30)
          {:keys [channel-name items]} (get-channel-videos url max-results)]
      (log/infof "Fetched %d items from streaming channel %s (%s)" (count items) channel-name url)
      (for [^StreamInfoItem item items
            :let [entry (stream-info-item-to-entry item channel-name)]]
        (make-streaming-item
         (fetch/make-meta src)
         {:ts (or (:pub-ts entry) (time/zoned-date-time))
          :title (:title entry)}
         (fetch/make-item-hash (.getUrl item))
         entry)))))
