(ns llar.fetch.feed
  (:require [llar.fetch :refer [FetchSource
                                item-to-string
                                make-meta
                                make-item-hash
                                tag-items]]
            [llar.fetchutils :as fetchutils]
            [clojure.spec.alpha :as s]
            [llar.src]
            [llar.postproc :refer [ItemProcessor]]
            [llar.persistency :refer [CouchItem]]
            [llar.http :refer [fetch
                               absolutify-links-in-hick
                               absolutify-url
                               get-base-url
                               get-base-url-with-path
                               blobify
                               sanitize
                               resolve-user-agent
                               with-http-exception-handler]]
            [llar.analysis :as analysis]
            [llar.converter :as conv]
            [org.bovinegenius [exploding-fish :as uri]]
            [slingshot.slingshot :refer [throw+ try+]]
            [clojure.tools.logging :as log]
            [hickory.core :as hick]
            [hickory.select :as hick-s]
            [hickory.render :as hick-r]
            [llar.item]
            [clj-rome.reader :as rome]
            [clj-http.client :as http]
            [cheshire.core :as cheshire]
            [java-time.api :as time])
  (:import (llar.src Feed)))

(defrecord FeedItem
           [meta
            summary
            hash
            entry
            raw
            feed]
  Object
  (toString [item] (item-to-string item)))

(defn make-feed-item [meta summary hash entry raw feed]
  {:pre [(s/valid? :irq0/item-metadata meta)
         (s/valid? :irq0/item-summary summary)
         (s/valid? :irq0/item-hash hash)
         (s/valid? :irq0/feed feed)]}
  (->FeedItem meta summary hash entry raw feed))

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
  {:post [(s/valid? :irq0/ts %)]}
  (or (some-> e :published-date feed-date-to-zoned-date-time)
      (some-> e :updated-date feed-date-to-zoned-date-time)
      (get-in http [:summary :ts])))

(defn http-get-feed-content [url]
  (log/debug "fetching feed item content of " url)
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
                         (fetchutils/process-html-contents contents-base-url in-feed-contents))
              descriptions (fetchutils/process-html-contents contents-base-url
                                                             (extract-feed-description (:description re)))
              base-entry {:updated-ts (some-> re :updated-date feed-date-to-zoned-date-time)
                          :pub-ts (some-> re :published-date feed-date-to-zoned-date-time)
                          :url contents-url
                          :categories (some->> re :categories
                                               (map :name) (remove nil?))
                          :title (-> re :title)}]
          (make-feed-item
           (merge (make-meta src)
                  {:source-name (:title feed)})
           {:ts timestamp
            :title (:title re)}
           (make-item-hash
            (:title re) (:link re))
           (merge base-entry
                  {:authors authors
                   :contents contents
                   :descriptions descriptions})
           re
           feed))))))

(defn default-selector-feed-extractor [hick]
  (-> hick first :content first))

(defn hick-select-extract [selector extractor hickory]
  (let [extractor (or extractor default-selector-feed-extractor)]
    (when (some? selector)
      (let [tree (hick-s/select selector hickory)]
        (try
          (extractor tree)
          (catch clojure.lang.ExceptionInfo ex
            (throw+ (assoc (ex-data ex) :tree tree)))
          (catch Throwable th
            (throw+ th)))))))

(defn hick-select-extract-with-source [src k hickory fallback]
  (let [{:keys [selectors extractors]} src
        sel (get selectors k)
        ext (get extractors k)]
    (try
      (if (coll? ext)
        ext
        (let [selected (hick-select-extract sel ext hickory)]
          (when-not (and (some? sel) (some? selected))
            (log/debugf "hickory selector %s for %s turned up nothing"
                        (str src) k))
          (or selected fallback)))
      (catch clojure.lang.ExceptionInfo ex
        (throw+ (assoc {:type ::hickory-select-failed
                        :src (str src)
                        :key k
                        :selector sel
                        :extractor ext}
                       :select-extract-error (ex-data ex))))
      (catch Throwable _
        (throw+ {:type ::hickory-select-failed
                 :src (str src)
                 :key k
                 :selector sel
                 :extractor ext})))))

(extend-protocol FetchSource
  llar.src.SelectorFeed
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

      (log/debug (str src) " parsed URLs: " {:base-url base-url
                                             :urls item-urls})
      (let [conform (s/conform (s/coll-of :irq0/url) item-urls)]
        (when (s/invalid? conform)
          (throw+ {:type ::selector-found-shit
                   :spec-explain (s/explain-str (s/coll-of :irq0/url) item-urls)
                   :extractor item-extractor
                   :urls item-urls
                   :selector (:urls selectors)})))
      (doall
       (for [raw-item-url item-urls
             :let [base-url (get-base-url-with-path raw-item-url)
                   item-url (absolutify-url raw-item-url base-url)
                   item (fetch item-url :user-agent user-agent)
                   {:keys [hickory summary]} item]]

         (try
           (log/debug (str src) " fetching: " {:base-url base-url
                                               :item-url item-url})
           (let [author (hick-select-extract-with-source src :author hickory nil)
                 title (hick-select-extract-with-source src :title hickory (:title summary))
                 pub-ts (hick-select-extract-with-source src :ts hickory (:ts summary))
                 description (hick-select-extract-with-source src :description hickory nil)
                 sanitized (fetchutils/process-html-contents base-url hickory)
                 content (if (some? (:content selectors))
                           (first (hick-s/select (:content selectors) sanitized))
                           (first (hick-s/select (hick-s/child (hick-s/tag :body)) sanitized)))
                 content-html (hick-r/hickory-to-html content)]

             ;; todo match selected+extracted data to some schema
             (when-not (string? title)
               (throw+ {:type ::selector-or-extractor-broken
                        :item "title"
                        :value title}))

             (make-feed-item
              meta
              {:title title
               :ts pub-ts}
              (make-item-hash title (str item-url) content-html)
              {:pub-ts pub-ts
               :url item-url
               :title title
               :authors author
               :descriptions {"text/plain" description}
               :contents {"text/html" content-html
                          "text/plain" (conv/html2text content-html)}}
              nil
              feed))
           (catch clojure.lang.ExceptionInfo ex
             (log/warnf "SelectorFeed processing failed: raw-url:%s url:%s data:%s"
                        raw-item-url item-url (ex-data ex)))
           (catch Throwable th
             (log/warnf th "SelectorFeed processing failed raw-url:%s url:%s with exception"
                        raw-item-url item-url))))))))

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

(s/def :irq0-src-wp-json/rendered string?)
(s/def :irq0-src-wp-json/href :irq0/url-str)
(s/def :irq0-src-wp-json/link :irq0/url-str)
(s/def :irq0-src-wp-json/self (s/coll-of (s/keys :req-un [:irq0-src-wp-json/href])))
(s/def :irq0-src-wp-json/title (s/keys :req-un [:irq0-src-wp-json/rendered]))
(s/def :irq0-src-wp-json/excerpt (s/keys :req-un [:irq0-src-wp-json/rendered]))
(s/def :irq0-src-wp-json/content (s/keys :req-un [:irq0-src-wp-json/rendered]))
(s/def :irq0-src-wp-json/_links (s/keys :req-un [:irq0-src-wp-json/self]))
(s/def :irq0-src-wp-json/date_gmt :irq0/iso-date-time-str)

(s/def :irq0-src-wp-json/post (s/keys :req-un [:irq0-src-wp-json/title
                                               :irq0-src-wp-json/excerpt
                                               :irq0-src-wp-json/content
                                               :irq0-src-wp-json/link
                                               :irq0-src-wp-json/_links
                                               :irq0-src-wp-json/date_gmt]))

(defn- fetch-wp-json-authors [post user-agent]
  (try
    (doall
     (for [url (map :href (get-in post [:_links :author]))
           :let [response (with-http-exception-handler
                            {:url url
                             :request ::wp-json-author
                             :user-agent user-agent}
                            (http/get url
                                      {:as :json
                                       :headers {:user-agent user-agent}}))
                 author (get-in response [:body :name])]]
       (do (log/debugf "wordpress JSON author fetch %s: %s" url author)
           author)))
    (catch Exception _
      (log/debugf "wordpress JSON author fetch failed with endpoint %s."
                  (get-in post [:_links :author])))))

(defn- extract-embedded-authors [post]
  (if-let [authors (get-in post [:_embedded :author])]
    (remove nil? (map :name authors))
    (log/debugf "no embedded authors in post %s" (:_embedded post))))

(extend-protocol FetchSource
  llar.src.WordpressJsonFeed
  (fetch-source [src]
    (let [wp-json-url (str (:url src))
          user-agent (resolve-user-agent
                      (get-in src [:args :user-agent]))
          site (with-http-exception-handler
                 {:url wp-json-url
                  :request :wp-json-site
                  :user-agent user-agent}
                 (http/get wp-json-url
                           {:as :json
                            :headers {"User-Agent" user-agent}}))
          posts-url (str (get-posts-url (:body site)) "?_embed")
          posts (-> (with-http-exception-handler
                      {:url posts-url
                       :request ::wp-json-posts
                       :user-agent user-agent}
                      (http/get posts-url {:as :reader
                                           :headers {:user-agent user-agent}}))
                    :body (cheshire/parse-stream true))]
      (log/debugf "wordpress JSON feed %s / %s fetched: %s posts" wp-json-url posts-url (count posts))
      (doall
       (for [post posts
             :let [conform (s/conform :irq0-src-wp-json/post post)]]
         (if (s/invalid? conform)
           (log/warn "invalid post: " conform (s/explain-str :irq0-src-wp-json/post post))
           (let [authors (some #(when-not (empty? %) %)
                               [(extract-embedded-authors post)
                                (fetch-wp-json-authors post user-agent)
                                [(get-in post [:yoast_head_json :author])]
                                [""]])
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
             (make-feed-item
              (make-meta src)
              {:title title :ts pub-ts}
              (make-item-hash title (str pub-ts) (str url))
              {:pub-ts pub-ts
               :url url
               :title title
               :authors authors
               :descriptions {"text/plain" description}
               :contents {"text/html" sanitized-html
                          "text/plain" (conv/html2text sanitized-html)}}
              {:site (:body site)
               :post post}
              {:title (get-in site [:body :name])
               :url (uri/uri (get-in site [:body :home]))
               :feed-type "wp-json"}))))))))

(extend-protocol ItemProcessor
  FeedItem
  (post-process-item [item src _state]
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
          (log/debugf "filtering out item %s: older than last fetch at %s" (str item) last-fetch)
          (time/before? feed-pub last-fetch))))))

(extend-protocol CouchItem
  FeedItem
  (to-couch [item]
    (-> item
        (assoc :type :feed)
        (dissoc :raw)
        (dissoc :body))))
