(ns infowarss.fetch.feed
  (:require [infowarss.fetch :refer [FetchSource item-to-string make-meta make-item-hash]]
            [infowarss.fetch.http :refer [fetch-http-generic]]
            [infowarss.schema :as schema]
            [infowarss.converter :as conv]
            [infowarss.src :as src]
            [twitter.api.restful :as twitter]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+ try+]]
            [taoensso.timbre :as log]
            [hickory.select :as hick-s]
            [clj-rome.reader :as rome]
            [hickory.render :as hick-r]
            [clojure.java.io :as io]
            [clj-time.format :as tf]
            [clj-time.coerce :as tc]))


(s/defrecord FeedItem
    [meta :- schema/Metadata
     summary :- schema/Summary
     hash :- schema/Hash
     entry :- schema/FeedEntry
     raw :- s/Any
     feed :- schema/Feed]
  Object
  (toString [item] (item-to-string item)))


(defn- extract-feed-authors
  "Extract feed author from rome feed item"
  [authors]
  (for [{:keys [name email]} authors]
    (str
      (when-not (nil? name)
        name)
      (when-not (nil? email)
        (str " <" email ">")))))


(defn- extract-feed-description
  "Extract feed description from rome reed item"
  [description]
  (if (= (:type description) "text/html")
    {"text/html" (:value description)
     "text/plain" (conv/html2text (:value description))}
    {"text/plain" (:value description)}))

(def rome-content-type-to-mime {"html" "text/html"
                                "text" "text/plain"})

(defn- extract-feed-content
  "Extract feed content from rome feed item"
  [contents]
  (let [by-type (into {}
                  (for [{:keys [type value]} contents]
                    [(get rome-content-type-to-mime
                       type "application/octet-stream") value]))]
    ;; Convert non plain text content types
    (condp #(contains? %2 %1) by-type
      "text/html" (assoc by-type "text/plain" (conv/html2text (get by-type "text/html")))
      (assoc by-type "text/plain" (first (vals by-type))))))

(defn- extract-feed-timestamp
  "Extract feed entry timestamp"
  [e http]
  (or (some-> e :published-date tc/from-date)
    (some-> e :updated-date tc/from-date)
    (get-in http [:meta :fetch-ts])))

(defn- maybe-extract-url
  [s]
  (try+
    (io/as-url s)
    (catch java.net.MalformedURLException _
      nil)))

(defn- http-get-feed-content [src]
  (log/debug "Fetching feed item content of " (str src))
  (let [http-item (fetch-http-generic src)
        body (->> http-item
               :hickory
               (hick-s/select
                 (hick-s/child
                   (hick-s/tag :body)))
               first
               hick-r/hickory-to-html)]
    {"text/html" body
     "text/plain" (conv/html2text body)}))


(extend-protocol FetchSource
  infowarss.src.Feed
  (fetch-source [src]
    (let [http-item (fetch-http-generic src)
          res (-> http-item :raw :body rome/build-feed)
          feed {:title (-> res :title)
                :language (-> res :language)
                :url (-> res :link  maybe-extract-url)
                :descriptions {"text/plain" (-> res :description)}
                :encoding (-> res :encoding)
                :pub-ts (some->> res :published-date tc/from-date)
                :feed-type (-> res :feed-type)}]

      (for [re (:entries res)]
        (let [timestamp (extract-feed-timestamp re http-item)
              authors (extract-feed-authors (:authors re))
              in-feed-contents (extract-feed-content (:contents re))
              contents-url (-> re :link maybe-extract-url)
              contents (if (and (nil? (get in-feed-contents "text/plain"))
                             (get-in src [:args :deep?])
                             (not (nil? contents-url)))
                         (http-get-feed-content
                           (src/http (str contents-url)))
                         in-feed-contents)
              descriptions (extract-feed-description (:description re))
              base-entry {:updated-ts (some-> re :updated-date tc/from-date)
                          :pub-ts (some-> re :published-date tc/from-date)
                          :url contents-url
                          :title (-> re :title)}]
          (map->FeedItem
            (-> http-item
              (dissoc :hash :hickory :summary)
              (merge {:raw re
                      :feed feed
                      :entry (merge base-entry
                               {:authors authors
                                :contents contents
                                :descriptions descriptions})
                      :hash (make-item-hash
                              (:title re) (:link re))
                      :summary {:ts timestamp
                                :title (:title re)}}))))))))