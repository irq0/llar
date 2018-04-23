(ns infowarss.fetch.youtube
  (:require [infowarss.fetch :refer [FetchSource item-to-string make-meta make-item-hash]]
            [infowarss.postproc :refer [ItemProcessor]]
            [infowarss.persistency :refer [CouchItem]]
            [infowarss.analysis :as analysis]
            [infowarss.schema :as schema]
            [infowarss.converter :as conv]
            [infowarss.src :as src]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+ try+]]
            [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [clj-time.core :as time]
            [clj-time.format :as tf]
            [clj-http.client :as http]
            [clj-time.coerce :as tc])
  (:import [com.google.api.client.auth.oauth2 Credential]
           [com.google.api.client.http.javanet NetHttpTransport]
           [com.google.api.client.json.jackson2 JacksonFactory]
           [com.google.api.services.youtube YouTube YouTube$Builder YouTubeRequestInitializer]
           [com.google.api.services.youtube.model ChannelListResponse Channel]
           [com.google.api.client.googleapis.auth.oauth2 GoogleClientSecrets GoogleAuthorizationCodeFlow]))

(defn connect [api-key]
  (-> (YouTube$Builder. (NetHttpTransport.) (JacksonFactory.) nil)
    (.setApplicationName "infowarss")
    (.setYouTubeRequestInitializer (YouTubeRequestInitializer. api-key))
    (.build)))


(defn get-channel-id [youtube username]
  (let [chan (-> youtube
               (.channels)
               (.list "id")
               (.setForUsername username)
               (.execute))]
    (some-> chan .getItems (.get 0) (.getId))))

(defn youtube-api [youtube]
  (loop [next-page-token nil
         items nil]
    (when (some? next-page-token)
      (.setPageToken youtube next-page-token))
    (let [results (.execute youtube)
          new-items (concat items (.getItems results))]
      (if (some? (.getNextPageToken results))
        (recur (.getNextPageToken results) new-items)
        new-items))))

(defn get-channel-playlists [youtube username]
  (youtube-api (-> youtube
                 (.playlists)
                 (.list "contentDetails")
                 (.setChannelId (get-channel-id youtube username)))))

(defn get-playlist [youtube playlist-id]
  (youtube-api (-> youtube
                 (.playlistItems)
                 (.list "contentDetails,snippet")
                 (.setPlaylistId "PLmKbqjSZR8TZmG4vFnBIIH35EWswFgsG0"))))

(defn playlist-item-to-youtube-entry [i]
  (let [content (bean (.getContentDetails i))
        snippet (bean (.getSnippet i))
        thumb (conv/data-uri
                (:body (http/get(-> snippet
                                  :thumbnails
                                  .getDefault
                                  .getUrl)))
                :mime-type "image/png")]
    (when-not (= (:title snippet) "Private video")
      {:video-id (:videoId content)
       :title (:title snippet)
       :description {"text/plain" (:description snippet)}
       :url (str "https://www.youtube.com/watch?v=" (:videoId content))
       :authors [(:channelTitle snippet)]
       :pub-ts (tc/from-string (.toStringRfc3339 (:videoPublishedAt content)))
       :contents {}
       :thumbs {"image/png" thumb}})))

(defn channel-info-to-youtube-feed [i]
  (let [content (bean (.getContentDetails i))
        snippet (bean (.getSnippet i))]
    {:title (:title snippet)
     :url (str "https://www.youtube.com/user/" (:title snippet))
     :descriptions {"text/plain" (:description snippet)}
     :pub-ts (tc/from-string (.toStringRfc3339 (:publishedAt snippet)))
     :updated-ts (time/now)}))

(defn get-channel-info [youtube username]
  (youtube-api (-> youtube
                 (.channels)
                 (.list "snippet,topicDetails,contentDetails,contentOwnerDetails,statistics")
                 (.setForUsername username))))


(comment
  (let [playlists (get-channel-playlists youtube "LastWeekTonight")
        feed (channel-info-to-youtube-feed (get-channel-info youtube "LastWeekTonight"))]))



;; (s/defrecord YoutubeItem
;;     [meta :- schema/Metadata
;;      summary :- schema/Summary
;;      hash :- schema/Hash
;;      entry :- schema/FeedEntry
;;      raw :- s/Any
;;      feed :- schema/Feed]
;;   Object
;;   (toString [item] (item-to-string item)))


;; (extend-protocol FetchSource
;;   infowarss.src.YoutubeChannel
;;   (fetch-source [src]
;;     (let [http-item (fetch-http-generic src)
;;           res (-> http-item :raw :body rome/build-feed)
;;           feed {:title (-> res :title)
;;                 :url (-> res :link  maybe-extract-url)
;;                 :descriptions {"text/plain" (-> res :description)}
;;                 :pub-ts (some->> res :published-date tc/from-date)
;;                 :feed-type (-> res :feed-type)}]

;;       (for [re (:entries res)]
;;         (let [timestamp (extract-feed-timestamp re http-item)
;;               authors (extract-feed-authors (:authors re))
;;               in-feed-contents (extract-feed-content (:contents re))
;;               contents-url (-> re :link maybe-extract-url)
;;               contents (if (and (nil? (get in-feed-contents "text/plain"))
;;                              (get-in src [:args :deep?])
;;                              (not (nil? contents-url)))
;;                          (http-get-feed-content
;;                            (src/http (str contents-url)))
;;                          in-feed-contents)
;;               descriptions (extract-feed-description (:description re))
;;               base-entry {:updated-ts (some-> re :updated-date tc/from-date)
;;                           :pub-ts (some-> re :published-date tc/from-date)
;;                           :url contents-url
;;                           :categories (some->> re :categories
;;                                         (map :name) (remove nil?))
;;                           :title (-> re :title)}]
;;           (map->FeedItem
;;             (-> http-item
;;               (dissoc :hash :hickory :summary)
;;               (merge {:raw re
;;                       :feed feed
;;                       :entry (merge base-entry
;;                                {:authors authors
;;                                 :contents contents
;;                                 :descriptions descriptions})
;;                       :hash (make-item-hash
;;                               (:title re) (:link re))
;;                       :summary {:ts timestamp
;;                                 :title (:title re)}}))))))))


;; (extend-protocol ItemProcessor
;;   YoutubeItem
;;   (post-process-item [item src state]
;;     (let [nlp (analysis/analyze-entry (:entry item))]
;;       (-> item
;;         (update :entry merge (:entry item) nlp))))
;;   (filter-item [item src state]
;;     (let [last-fetch (get state :last-successful-fetch-ts)
;;           feed-pub (get-in item [:feed :pub-ts])]
;;       (if-not (or (nil? last-fetch) (nil? feed-pub))
;;         (do
;;           (log/tracef "Filtering out item %s: older than last fetch"
;;             (str item))
;;           (time/before? feed-pub last-fetch))
;;         false))))

;; (extend-protocol CouchItem
;;   FeedItem
;;   (to-couch [item]
;;     (-> item
;;       convert-to-attachments
;;       (assoc :type :video)
;;       (dissoc :raw))))
