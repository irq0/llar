(ns llar.apis.podcast
  (:require
   [clojure.data.xml :as xml]
   [compojure.core :refer [GET routes]]
   [compojure.route :as route]
   [clojure.tools.logging :as log]
   [java-time.api :as time]
   [slingshot.slingshot :refer [try+]]
   [llar.appconfig :as appconfig :refer [appconfig]]
   [llar.blobstore :as blobstore]
   [llar.persistency :as persistency]
   [llar.podcast :as podcast]
   [llar.store :as store])
  (:import
   [java.time ZonedDateTime]
   [java.time.format DateTimeFormatter]
   [java.util Locale]))

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

(defn- make-item-xml
  "Build a single RSS <item> element for a podcast episode"
  [item download-info base-url token]
  (let [{:keys [blob-hash metadata mime-type]} download-info
        blob (blobstore/get-blob blob-hash)
        enclosure-url (str base-url "/media/" blob-hash "?token=" token)
        title (or (:title item) (get-in item [:entry :title]) "Untitled")
        pub-date (:ts item)
        description (or (get-in item [:entry :descriptions "text/plain"])
                        (get-in item [:entry :descriptions :text/plain])
                        (:title item)
                        "")
        guid (str "llar-podcast-" (:id item))]
    (xml/element :item {}
                 (xml/element :title {} title)
                 (xml/element :description {} (str description))
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
                                  "LLAR Podcast")))))

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
                        (keyword "xmlns" "itunes") itunes-ns}
                  (apply xml/element :channel {}
                         (xml/element :title {} "LLAR Podcast")
                         (xml/element :link {} base-url)
                         (xml/element :description {} "Media from LLAR - Live Long and Read")
                         (xml/element :language {} "en")
                         (xml/element (xml/qname itunes-ns "author") {} "LLAR")
                         (xml/element (xml/qname itunes-ns "category") {:text "Technology"})
                         item-elements)))))

;;;; Routes

(def app
  (routes
   (GET "/feed.xml" req
     (let [token (get-in req [:params "token"])]
       {:status 200
        :headers {"Content-Type" "application/rss+xml; charset=utf-8"}
        :body (generate-feed-xml (podcast-base-url) token)}))

   (GET "/media/:hash" [hash]
     (try+
      (let [blob (blobstore/get-blob hash)]
        {:status 200
         :headers {"Content-Type" (:mime-type blob)
                   "Content-Length" (str (:size blob))
                   "Accept-Ranges" "bytes"
                   "Etag" hash
                   "Last-Modified" (time/format
                                    (time/formatter "EEE, dd MMM yyyy HH:mm:ss z")
                                    (:created blob))}
         :body (:data blob)})
      (catch Object e
        (log/warn e "podcast: media get-blob failed:" hash)
        {:status 404
         :headers {"Content-Type" "text/plain"}
         :body "Not Found"})))

   (route/not-found "404")))
