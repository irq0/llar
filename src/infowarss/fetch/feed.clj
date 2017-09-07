(ns infowarss.fetch.feed
  (:require [infowarss.fetch :refer [FetchSource item-to-string make-meta make-item-hash]]
            [infowarss.postproc :refer [ItemProcessor]]
            [infowarss.persistency :refer [CouchItem convert-to-attachments]]
            [infowarss.fetch.http :refer [fetch-http-generic absolutify-links-in-hick get-base-url]]
            [infowarss.analysis :as analysis]
            [infowarss.schema :as schema]
            [infowarss.converter :as conv]
            [infowarss.src :as src]
            [twitter.api.restful :as twitter]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+ try+]]
            [taoensso.timbre :as log]
            [hickory.core :as hick]
            [hickory.select :as hick-s]
            [hickory.render :as hick-r]
            [clj-rome.reader :as rome]
            [clojure.java.io :as io]
            [clj-time.core :as time]
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


(defn extract-feed-authors
  "Extract feed author from rome feed item"
  [authors]
  (for [{:keys [name email]} authors]
    (str
      (when-not (nil? name)
        name)
      (when-not (nil? email)
        (str " <" email ">")))))


(defn extract-feed-description
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

(defn- process-feed-html-contents [base-url contents]
  (if-let [html (get-in contents ["text/html"])]
    (-> html
      hick/parse hick/as-hickory
      (absolutify-links-in-hick base-url)
      (hick-r/hickory-to-html))))

(extend-protocol FetchSource
  infowarss.src.Feed
  (fetch-source [src]
    (let [http-item (fetch-http-generic src)
          res (-> http-item :raw :body rome/build-feed)
          feed {:title (-> res :title)
                :language (-> res :language)
                :url (-> res :link maybe-extract-url)
                :descriptions {"text/plain" (-> res :description)}
                :encoding (-> res :encoding)
                :pub-ts (some->> res :published-date tc/from-date)
                :feed-type (-> res :feed-type)}
          base-url (get-base-url (:url feed))]


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
                         (process-feed-html-contents base-url in-feed-contents))
              descriptions (extract-feed-description (:description re))
              base-entry {:updated-ts (some-> re :updated-date tc/from-date)
                          :pub-ts (some-> re :published-date tc/from-date)
                          :url contents-url
                          :categories (some->> re :categories
                                        (map :name) (remove nil?))
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


(extend-protocol ItemProcessor
  FeedItem
  (post-process-item [item src state]
    (let [nlp (analysis/analyze-entry (:entry item))]
      (-> item
        (update :entry merge (:entry item) nlp))))
  (filter-item [item src state]
    (let [last-fetch (get state :last-successful-fetch-ts)
          feed-pub (get-in item [:feed :pub-ts])]
      (if-not (or (nil? last-fetch) (nil? feed-pub))
        (do
          (log/tracef "Filtering out item %s: older than last fetch"
            (str item))
          (time/before? feed-pub last-fetch))
        false))))

(extend-protocol CouchItem
  FeedItem
  (to-couch [item]
    (-> item
      convert-to-attachments
      (assoc :type :feed)
      (dissoc :raw))))
