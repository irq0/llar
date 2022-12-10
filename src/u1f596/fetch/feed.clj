(ns u1f596.fetch.feed
  (:require [u1f596.fetch :refer [FetchSource
                                     item-to-string
                                     make-meta
                                     make-item-hash
                                     tag-items]]
            [u1f596.src]
            [u1f596.postproc :refer [ItemProcessor]]
            [u1f596.persistency :refer [CouchItem]]
            [u1f596.http :refer [fetch
                                    absolutify-links-in-hick
                                    absolutify-url
                                    get-base-url
                                    get-base-url-with-path
                                    blobify
                                    sanitize
                                    resolve-user-agent]]
            [u1f596.analysis :as analysis]
            [u1f596.schema :as schema]
            [u1f596.converter :as conv]
            [schema.core :as s]
            [org.bovinegenius [exploding-fish :as uri]]
            [slingshot.slingshot :refer [throw+ try+]]
            [clojure.tools.logging :as log]
            [hickory.core :as hick]
            [hickory.select :as hick-s]
            [hickory.render :as hick-r]
            [clj-rome.reader :as rome]
            [clj-http.client :as http]
            [cheshire.core :as cheshire]
            [java-time :as time])
  (:import (u1f596.src Feed)))

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

(defn- feed-date-to-zoned-date-time [x]
  (time/zoned-date-time x (time/zone-id "UTC")))

(defn- extract-feed-timestamp
  "Extract feed entry timestamp"
  [e http]
  (or (some-> e :published-date)
      (some-> e :updated-date)
      (get-in http [:meta :fetch-ts])))

(defn http-get-feed-content [url]
  (log/debug "Fetching feed item content of " url)
  (let [http-item (fetch url)
        hick (->> http-item
                  :hickory
                  (hick-s/select
                   (hick-s/child
                    (hick-s/tag :body)))
                  first)
        body (hick-r/hickory-to-html hick)]
    {"text/html" body
     "text/plain" (conv/html2text body)}))

(defn- process-feed-html-contents [base-url contents]
  (if-let [html (get-in contents ["text/html"])]
    (assoc contents "text/html"
           (-> html
               hick/parse
               hick/as-hickory
               (absolutify-links-in-hick base-url)
               sanitize
               blobify
               (hick-r/hickory-to-html)))
    contents))

(extend-protocol FetchSource
  Feed
  (fetch-source [src]
    (let [url (uri/uri (:url src))
          base-url (get-base-url url)
          http-item (fetch url :user-agent (get-in src [:args :user-agent]))
          res (try+
               (-> http-item :raw :body rome/build-feed)
               (catch Object _
                 (log/error (:throwable &throw-context) "rome parse failed" (:summary http-item))
                 (throw+ {:type ::rome-failure
                          :http-item (-> http-item
                                         (assoc-in [:raw :body] :removed)
                                         (assoc-in [:hickory] :removed)
                                         (assoc-in [:body] :removed))})))
          raw-feed-url (:link res)
          feed-url (if (nil? raw-feed-url)
                     url
                     (absolutify-url (uri/uri raw-feed-url) base-url))

          feed {:title (-> res :title)
                :language (-> res :language)
                :url feed-url
                :descriptions {"text/plain" (-> res :description)}
                :encoding (-> res :encoding)
                :pub-ts (some->> res :published-date feed-date-to-zoned-date-time)
                :feed-type (-> res :feed-type)}]
      (for [re (:entries res)]
        (let [timestamp (extract-feed-timestamp re http-item)
              authors (extract-feed-authors (:authors re))
              in-feed-contents (extract-feed-content (:contents re))
              contents-url (-> re :link uri/uri)
              contents-base-url (if (uri/absolute? contents-url)
                                  (get-base-url contents-url)
                                  base-url)
              contents-url (absolutify-url contents-url contents-base-url)
              deep-fetch? (and (get-in src [:args :deep?])
                               (some? contents-url))
              contents (if deep-fetch?
                         (http-get-feed-content contents-url)
                         (process-feed-html-contents contents-base-url in-feed-contents))
              descriptions (process-feed-html-contents contents-base-url
                                                       (extract-feed-description (:description re)))
              base-entry {:updated-ts (some-> re :updated-date feed-date-to-zoned-date-time)
                          :pub-ts (some-> re :published-date feed-date-to-zoned-date-time)
                          :url contents-url
                          :categories (some->> re :categories
                                               (map :name) (remove nil?))
                          :title (-> re :title)}]
          (map->FeedItem
           (-> http-item
               (dissoc :hash :hickory :summary :body)
               (merge {:meta (merge (make-meta src)
                                    {:source-name (:title feed)})
                       :raw re
                       :feed feed
                       :entry (merge base-entry
                                     {:authors authors
                                      :contents contents
                                      :descriptions descriptions})
                       :hash (make-item-hash
                              (:title re) (:link re))
                       :summary {:ts timestamp
                                 :title (:title re)}}))))))))

(defn default-selector-feed-extractor [hick]
  (-> hick first :content first))

(defn hick-select-extract [selector extractor hickory]
  (let [extractor (or extractor default-selector-feed-extractor)]
    (when (some? selector)
      (let [tree (hick-s/select selector hickory)]
        (try
          (extractor tree)
          (catch Throwable th
            (log/warn th "Extractor crashed. Selected tree was:" tree)
            (throw+ th)))))))

(defn hick-select-extract-with-source [src k hickory fallback]
  (let [{:keys [selectors extractors]} src
        sel (get selectors k)
        ext (get extractors k)]
    (try
      (let [selected (hick-select-extract sel ext hickory)]
        (when-not (and (some? sel) (some? selected))
          (log/debugf "Hickory selector %s for %s turned up nothing"
                      (str src) k))
        (or selected fallback))
      (catch Throwable th
        (log/warn th "Hick Extract failed :("
                  src k)
        (throw+ {:type ::hickory-select-failed
                 :src src
                 :key k
                 :selector sel
                 :extractor ext})))))

(extend-protocol FetchSource
  u1f596.src.SelectorFeed
  (fetch-source [src]
    (let [{:keys [url selectors extractors args]} src
          user-agent (:user-agent args)
          {:keys [summary hickory]} (fetch url :user-agent user-agent)
          base-url (get-base-url-with-path (uri/uri url))

          meta (make-meta src)
          feed {:title (:title summary)
                :url url
                :feed-type "selector-feed"}
          item-extractor (or (:urls extractors)
                             (fn [l] (map (fn [x] (absolutify-url (-> x :attrs :href) base-url)) l)))
          item-urls (item-extractor (hick-s/select (:urls selectors) hickory))]

      (log/debug (str src) " Parsed URLs: " {:base-url base-url
                                             :urls item-urls})
      (when-not (coll? item-urls)
        (throw+ {:type ::selector-found-shit
                 :extractor item-extractor
                 :urls item-urls
                 :selector (:urls selectors)}))
      (doall
       (for [raw-item-url item-urls
             :let [base-url (get-base-url-with-path raw-item-url)
                   item-url (absolutify-url raw-item-url base-url)
                   item (fetch item-url :user-agent user-agent)
                   {:keys [hickory summary]} item]]

         (try
           (log/debug (str src) " Fetching: " {:base-url base-url
                                           :item-url item-url})
           (let [author (hick-select-extract-with-source src :author hickory nil)
                 title (hick-select-extract-with-source src :title hickory (:title summary))
                 pub-ts (hick-select-extract-with-source src :ts hickory (:ts summary))
                 description (hick-select-extract-with-source src :description hickory nil)
                 sanitized (process-feed-html-contents base-url hickory)
                 content (if (some? (:content selectors))
                           (first (hick-s/select (:content selectors) sanitized))
                           (first (hick-s/select (hick-s/child (hick-s/tag :body)) sanitized)))
                 content-html (hick-r/hickory-to-html content)]

             ;; todo match selected+extracted data to some schema
             (when-not (string? title)
               (throw+ {:type ::selector-or-extractor-broken
                        :item "title"
                        :value title}))

             (map->FeedItem
              (-> item
                  (dissoc :meta :hash :hickory :summary :body)
                  (merge {:meta meta
                          :feed feed
                          :hash (make-item-hash title (str item-url) content-html)
                          :entry {:pub-ts pub-ts
                                  :url item-url
                                  :title title
                                  :authors author
                                  :descriptions {"text/plain" description}
                                  :contents {"text/html" content-html
                                             "text/plain" (conv/html2text content-html)}}
                          :summary {:title title
                                    :ts pub-ts}}))))
           (catch Throwable th
             (log/warn th "SelectorFeed item processing failed. Skipping:"
                       raw-item-url))))))))

(defn- get-posts-url [json]
  (let [field (get-in json [:routes (keyword "/wp/v2/posts") :_links :self])]
    (cond
      (string? field) field
      (vector? field) (-> field first :href))))

(defn- parse-utc-without-timezone [ts]
  (time/zoned-date-time
   (time/local-date-time
    (time/formatter :iso-date-time)
    ts)
   (time/zone-id "UTC")))

(extend-protocol FetchSource
  u1f596.src.WordpressJsonFeed
  (fetch-source [src]
    (let [wp-json-url (str (:url src))
          user-agent (resolve-user-agent
                      (get-in src [:args :user-agent]))
          site (http/get wp-json-url
                         {:as :json
                          :headers {"User-Agent" user-agent}})
          posts-url (get-posts-url (:body site))
          posts (-> (http/get posts-url {:as :reader
                                                   :headers {:user-agent user-agent}})
                    :body (cheshire/parse-stream true))]
      (doall
       (for [post posts]
         (let [authors (try+
                        (doall
                         (for [url (map :href (get-in post [:_links :author]))]
                           (get-in [:body :name]
                                   (http/get url
                                             {:as :json
                                              :headers {:user-agent user-agent}}))))
                        (catch (contains? #{403 404} (get % :status))
                               {:keys [headers body status]}
                          (log/debug "Could not fetch article's authors endpoint:"
                                     headers body status (get-in post [:_links :self]))
                          [""]))
               url (uri/uri (get post :link))
               base-url (get-base-url url)
               title (get-in post [:title :rendered])
               pub-ts (some-> post :date_gmt parse-utc-without-timezone)
               description (get-in post [:excerpt :rendered])
               content-html (get-in post [:content :rendered])
               sanitized-html (-> content-html
                                  hick/parse
                                  hick/as-hickory
                                  (absolutify-links-in-hick base-url)
                                  sanitize
                                  blobify
                                  (hick-r/hickory-to-html))]

           (map->FeedItem
            {:meta (make-meta src)
             :raw {:site (:body site)
                   :post post}
             :feed {:title (get-in site [:body :name])
                    :url (get-in site [:body :home])
                    :feed-type "wp-json"}
             :hash (make-item-hash title pub-ts url)
             :entry {:pub-ts pub-ts
                     :url url
                     :title title
                     :authors authors
                     :descriptions {"text/plain" description}
                     :contents {"text/html" sanitized-html
                                "text/plain" (conv/html2text sanitized-html)}}
             :summary {:title title
                       :ts pub-ts}})))))))

(extend-protocol ItemProcessor
  FeedItem
  (post-process-item [item src state]
    (let [nlp (analysis/analyze-entry (:entry item))

          urls (get-in nlp [:nlp :urls])
          tags (set
                (remove nil?
                        [(when (some #(re-find #"^https?://\w+\.(youtube|vimeo|youtu)" %) urls)
                           :has-video)
                         (when (and (string? (:url item)) (re-find #"^https?://\w+\.(youtube|vimeo|youtu)" urls))
                           :has-video)
                         (when (some #(re-find #"https?://open\.spotify\.com/playlist" %) urls)
                           :has-spotify-playlist)]))]
      (-> item
          (update-in [:meta :tags] into tags)
          (update :entry merge (:entry item) nlp)
          (tag-items src))))
  (filter-item [item src state]
    (let [force-update? (or (get-in src [:args :force-update?])
                            (get state :forced-update?))
          last-fetch (get state :last-successful-fetch-ts)
          feed-pub (get-in item [:feed :pub-ts])]
      (if (or force-update? (nil? last-fetch) (nil? feed-pub))
        false
        (do
          (log/debugf "Filtering out item %s: older than last fetch at %s" (str item) last-fetch)
          (time/before? feed-pub last-fetch))))))

(extend-protocol CouchItem
  FeedItem
  (to-couch [item]
    (-> item
        (assoc :type :feed)
        (dissoc :raw)
        (dissoc :body))))
