(ns llar.fetch.feed
  (:require [llar.appconfig :as appconfig]
            [llar.fetch :refer [FetchSource
                                item-to-string
                                make-meta
                                make-item-hash]]
            [llar.fetchutils :as fetchutils]
            [clojure.string :as string]
            [clojure.spec.alpha :as s]
            [llar.src]
            [llar.postproc :refer [ItemProcessor]]
            [llar.persistency :refer [CouchItem]]
            [llar.http :as llar-http
             :refer [fetch
                     absolutify-links-in-hick
                     absolutify-url
                     get-base-url
                     get-base-url-with-path
                     blobify
                     logged-error?
                     sanitize
                     resolve-user-agent
                     with-http-exception-handler]]
            [llar.analysis :as analysis]
            [llar.commands :refer [html2text]]
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
  (:import (llar.src Feed)
           (org.jdom Element)))

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
     "text/plain" (html2text (:value description))}
    {"text/plain" (:value description)}))

(def rome-content-type-to-mime {"html" "text/html"
                                "text" "text/plain"})

(def ^:private +media-rss-namespace+ "http://search.yahoo.com/mrss")
(def ^:private +max-media-per-item+ 8)

(defn- media-rss-element? [^Element node local-name]
  (and (= local-name (.getName node))
       (= +media-rss-namespace+
          (string/replace (.getNamespaceURI node) #"/$" ""))))

(defn- media-child-text [^Element node local-name]
  (some (fn [^Element child]
          (when (media-rss-element? child local-name)
            (not-empty (.getTextTrim child))))
        (.getChildren node)))

(defn- raw-media-content [^Element node source]
  {:url (.getAttributeValue node "url")
   :mime-type (.getAttributeValue node "type")
   :medium (.getAttributeValue node "medium")
   :size-bytes (.getAttributeValue node "fileSize")
   :description (media-child-text node "description")
   :rating (media-child-text node "rating")
   :player? (boolean (some #(media-rss-element? % "player")
                           (.getChildren node)))
   :source source})

(defn- raw-media-rss-item [entry]
  (let [children (filter #(instance? Element %) (:foreign-markup entry))]
    {:contents (mapv #(raw-media-content % :media-content)
                     (filter #(media-rss-element? % "content") children))
     :thumbnails (mapv #(raw-media-content % :media-thumbnail)
                       (filter #(media-rss-element? % "thumbnail") children))
     :group-count (count (filter #(media-rss-element? % "group") children))
     :player-count (count (filter #(media-rss-element? % "player") children))}))

(def ^:private media-extension-kinds
  {"jpg" :image "jpeg" :image "png" :image "gif" :image "webp" :image
   "mp3" :audio "m4a" :audio "ogg" :audio "oga" :audio "opus" :audio
   "wav" :audio "aac" :audio "flac" :audio
   "mp4" :video "m4v" :video "webm" :video "mov" :video "mkv" :video
   "pdf" :document})

(defn- mime-kind [mime-type]
  (let [family (some-> mime-type string/lower-case (string/split #"[;/]") first)]
    (when (contains? #{"image" "audio" "video"} family)
      (keyword family))))

(defn- medium-kind [medium]
  (case (some-> medium string/lower-case)
    "image" :image
    "audio" :audio
    "video" :video
    "document" :document
    "executable" :executable
    nil))

(defn- extension-kind [url]
  (some->> url
           (re-find #"(?i)\.([a-z0-9]+)(?:[?#]|$)")
           second
           string/lower-case
           (get media-extension-kinds)))

(defn- parse-positive-long [x]
  (try
    (let [n (parse-long (str x))]
      (when (and n (pos? n)) n))
    (catch Exception _ nil)))

(defn- absolute-http-url [raw-url base-url]
  (try
    (when-not (string/blank? raw-url)
      (let [url (absolutify-url raw-url base-url)]
        (when (contains? #{"http" "https"} (uri/scheme url))
          (str url))))
    (catch Exception _ nil)))

(defn- ignored! [diagnostics reason]
  (swap! diagnostics update-in [:ignored reason] (fnil inc 0)))

(defn- accepted! [diagnostics source kind]
  (swap! diagnostics update-in [:accepted source kind] (fnil inc 0)))

(defn- normalize-media [candidate base-url]
  (let [url (absolute-http-url (:url candidate) base-url)
        mime (mime-kind (:mime-type candidate))
        medium (medium-kind (:medium candidate))
        extension (extension-kind (:url candidate))
        conflict? (and mime medium (not= mime medium))
        kind (or mime medium extension)]
    (cond
      (nil? url) {:ignored (if (:player? candidate) :player-only :invalid-url)}
      conflict? {:ignored :conflicting-type}
      (nil? kind) {:ignored :unknown-type}
      (contains? #{:document :executable} kind) {:ignored kind}
      (= "adult" (some-> (:rating candidate) string/lower-case)) {:ignored :adult}
      :else
      {:media (cond-> {:url url
                       :kind kind
                       :source (:source candidate)}
                (not (string/blank? (:mime-type candidate)))
                (assoc :mime-type (string/lower-case (:mime-type candidate)))

                (parse-positive-long (:size-bytes candidate))
                (assoc :size-bytes (parse-positive-long (:size-bytes candidate)))

                (not (string/blank? (:description candidate)))
                (assoc :description (:description candidate))

                (not (string/blank? (:rating candidate)))
                (assoc :rating (string/lower-case (:rating candidate))))})))

(defn- html-image-urls [html base-url]
  (if (string/blank? html)
    #{}
    (try
      (->> (hick/as-hickory (hick/parse html))
           (tree-seq map? :content)
           (filter #(and (= :element (:type %)) (= :img (:tag %))))
           (keep #(absolute-http-url (get-in % [:attrs :src]) base-url))
           set)
      (catch Exception _ #{}))))

(defn- cache-image [media diagnostics]
  (let [cached-url (llar-http/try-blobify-url! (:url media))]
    (if (and (string? cached-url) (string/starts-with? cached-url "/blob/"))
      (assoc media :local-url cached-url)
      (do
        (ignored! diagnostics :cache-failed)
        media))))

(defn- collect-media [entry raw-media base-url description-html diagnostics]
  (let [enclosures (map #(hash-map :url (:url %)
                                   :mime-type (:type %)
                                   :size-bytes (:length %)
                                   :source :enclosure)
                        (:enclosures entry))
        _ (when (> (count enclosures) 1)
            (ignored! diagnostics :multiple-enclosures))
        _ (dotimes [_ (:group-count raw-media)]
            (ignored! diagnostics :grouped))
        _ (dotimes [_ (:player-count raw-media)]
            (ignored! diagnostics :player-only))
        candidates (concat enclosures (:contents raw-media))
        embedded (if (seq candidates)
                   (html-image-urls description-html base-url)
                   #{})]
    (loop [remaining candidates
           seen embedded
           collected []]
      (if-let [candidate (first remaining)]
        (let [{media-item :media ignored :ignored} (normalize-media candidate base-url)]
          (cond
            ignored
            (do (ignored! diagnostics ignored)
                (recur (next remaining) seen collected))

            (contains? seen (:url media-item))
            (do (ignored! diagnostics :duplicate)
                (recur (next remaining) seen collected))

            (>= (count collected) +max-media-per-item+)
            (do (ignored! diagnostics :over-limit)
                (recur (next remaining) (conj seen (:url media-item)) collected))

            :else
            (let [media-item (if (= :image (:kind media-item))
                               (cache-image media-item diagnostics)
                               media-item)]
              (accepted! diagnostics (:source media-item) (:kind media-item))
              (recur (next remaining)
                     (conj seen (:url media-item))
                     (conj collected media-item)))))
        collected))))

(defn- collect-thumbnail [raw-media base-url diagnostics]
  (let [thumbnails (:thumbnails raw-media)]
    (dotimes [_ (max 0 (- (count thumbnails) +max-media-per-item+))]
      (ignored! diagnostics :over-limit))
    (loop [[candidate & remaining] (take +max-media-per-item+ thumbnails)]
      (when candidate
        (let [{media-item :media ignored :ignored}
              (normalize-media (assoc candidate :medium "image") base-url)]
          (cond
            ignored
            (do (ignored! diagnostics ignored)
                (recur remaining))

            (not= :image (:kind media-item))
            (do (ignored! diagnostics :thumbnail-non-image)
                (recur remaining))

            :else
            (let [cached (cache-image media-item diagnostics)]
              (if-let [local-url (:local-url cached)]
                (do
                  (accepted! diagnostics :media-thumbnail :image)
                  local-url)
                (recur remaining)))))))))

(defn- extract-feed-content
  "Extract feed content from rome feed item"
  [contents]
  (let [by-type (into {}
                      (for [{:keys [type value]} contents]
                        [(get rome-content-type-to-mime
                              type "application/octet-stream") value]))]
    ;; Convert non plain text content types
    (condp #(contains? %2 %1) by-type
      "text/html" (assoc by-type "text/plain" (html2text (get by-type "text/html")))
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

(defn- rome-parse-http-response [url resp]
  (try+
   (-> resp :raw :body rome/build-feed)
   (catch Object _
     (log/error (:throwable &throw-context) "rome parse failed" url (:summary resp))
     (throw+ {:type ::rome-failure
              :url url
              :http-item (-> resp
                             (assoc-in [:raw :body] :removed)
                             (assoc-in [:hickory] :removed)
                             (assoc-in [:body] :removed))}))))

(defn- make-rome-feed [url res]
  {:title (or (:title res) (str url))
   :language (-> res :language)
   :url (if (nil? (:link res))
          url
          (absolutify-url (uri/uri (:link res)) (get-base-url url)))
   :descriptions {"text/plain" (-> res :description)}
   :encoding (-> res :encoding)
   :pub-ts (some->> res :published-date feed-date-to-zoned-date-time)
   :feed-type (-> res :feed-type)})

(defn- make-rome-entry-hash [entry]
  (let [{:keys [title link uri description published-date updated-date]} entry]
    ;; Keep the hashes of conventional feeds stable. Some feeds (notably
    ;; Bluesky) omit item titles, but expose a stable GUID as :uri.
    (if (and (string? title) (string? link))
      (make-item-hash title link)
      (if-let [identifier (some #(when (string? %) %)
                                [uri link])]
        (make-item-hash identifier)
        (let [fallback (filter string?
                               [title
                                (:value description)
                                (some-> published-date str)
                                (some-> updated-date str)])]
          (if (seq fallback)
            (apply make-item-hash fallback)
            (throw (ex-info "Feed entry has no stable identity"
                            {:entry entry}))))))))

(defn- get-rome-entry-url [feed-url entry]
  (let [entry-url (-> entry :link uri/uri)
        entry-base-url (if (uri/absolute? entry-url)
                         (get-base-url entry-url)
                         (get-base-url feed-url))]
    (absolutify-url entry-url entry-base-url)))

(defn- rome-entry-to-llar-entry [feed-url entry raw-media diagnostics]
  (let [contents-url (get-rome-entry-url feed-url entry)
        base-url (get-base-url contents-url)
        raw-descriptions (extract-feed-description (:description entry))
        media (collect-media entry raw-media base-url
                             (get raw-descriptions "text/html") diagnostics)
        thumbnail (or (some (fn [{:keys [kind local-url]}]
                              (when (= :image kind) local-url))
                            media)
                      (collect-thumbnail raw-media base-url diagnostics))
        contents (fetchutils/process-html-contents (get-base-url contents-url) (extract-feed-content (:contents entry)))
        descriptions (fetchutils/process-html-contents base-url raw-descriptions)]
    (cond-> {:updated-ts (some-> entry :updated-date feed-date-to-zoned-date-time)
             :pub-ts (some-> entry :published-date feed-date-to-zoned-date-time)
             :url contents-url
             :categories (some->> entry :categories
                                  (map :name) (remove nil?))
             :title (-> entry :title)
             :authors (extract-feed-authors (:authors entry))
             :contents contents
             :descriptions descriptions}
      (seq media) (assoc :media media)
      thumbnail (assoc :thumbnail thumbnail))))

(extend-protocol FetchSource
  Feed
  (fetch-source [src conditional-tokens]
    (let [url (uri/uri (:url src))
          http-response (fetch url
                               :conditionals conditional-tokens
                               :user-agent (get-in src [:args :user-agent]))]
      (if (= :not-modified (:status http-response))
        (with-meta []
          {:conditional-tokens (:conditional-tokens http-response)})
        (let [res (rome-parse-http-response url http-response)
              feed (make-rome-feed url res)
              diagnostics (atom {})
              items (doall
                     (for [entry (:entries res)]
                       (let [raw-media (raw-media-rss-item entry)
                             llar-entry (rome-entry-to-llar-entry url entry raw-media diagnostics)
                             title (or (:title entry)
                                       (get-in llar-entry [:descriptions "text/plain"])
                                       (:uri entry)
                                       (:link entry)
                                       "")]
                         (make-feed-item
                          (merge (make-meta src)
                                 {:source-name (:title feed)})
                          {:ts (extract-feed-timestamp entry http-response)
                           :title title}
                          (make-rome-entry-hash entry)
                          llar-entry
                          entry
                          feed))))
              media-summary @diagnostics]
          (when (seq media-summary)
            (log/infof "feed media %s: %s" url media-summary))
          (with-meta items
            (cond-> {:conditional-tokens (:conditional-tokens http-response)}
              (seq media-summary) (assoc :media-summary media-summary))))))))

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

(defn- hickory-node-text [node]
  (cond
    (string? node) node
    (map? node) (string/join " " (map hickory-node-text (:content node)))
    (sequential? node) (string/join " " (map hickory-node-text node))
    :else ""))

(defn- usable-title [value]
  (when (string? value)
    (let [title (-> value
                    (string/replace #"\s+" " ")
                    string/trim)]
      (when-not (string/blank? title)
        title))))

(defn- selector-item-title [selected summary hickory item-url]
  (or (usable-title selected)
      (usable-title (:title summary))
      (some-> (or (first (hick-s/select (hick-s/tag :h1) hickory))
                  (first (hick-s/select (hick-s/tag :h2) hickory)))
              hickory-node-text
              usable-title)
      ;; A valid URL is stable and preferable to dropping the item altogether when
      ;; an old page has neither title metadata nor a usable heading.
      (str item-url)))

(defn fetch-selector-index
  "Fetch and select the index page of a SelectorFeed. This is also the shared
  diagnostic boundary used by Config Lab, so production and the lab see exactly
  the same sanitized Hickory tree."
  [src & {:keys [trace?] :or {trace? false}}]
  (let [{:keys [url selectors extractors args]} src
        user-agent (:user-agent args)
        http (fetch url :user-agent user-agent :trace? trace?)
        {:keys [summary hickory]} http
        base-url (get-base-url-with-path (uri/uri url))
        matches (vec (hick-s/select (:urls selectors) hickory))
        item-extractor (or (:urls extractors)
                           (fn [nodes]
                             (map (fn [node]
                                    (absolutify-url (-> node :attrs :href) base-url))
                                  nodes)))
        item-urls (vec (item-extractor matches))]
    (log/debug (str src) " parsed URLs: " {:base-url base-url
                                           :urls item-urls})
    (let [conform (s/conform (s/coll-of :irq0/url) item-urls)]
      (when (s/invalid? conform)
        (throw+ {:type ::selector-found-shit
                 :spec-explain (s/explain-str (s/coll-of :irq0/url) item-urls)
                 :extractor item-extractor
                 :urls item-urls
                 :selector (:urls selectors)})))
    {:http http
     :trace? trace?
     :base-url base-url
     :matches matches
     :item-urls item-urls
     :meta (make-meta src)
     :feed {:title (or (:title summary) (str url))
            :url url
            :feed-type "selector-feed"}}))

(defn fetch-selector-item
  "Fetch one URL selected from a SelectorFeed index."
  [src raw-item-url & {:keys [trace?] :or {trace? false}}]
  (let [base-url (get-base-url-with-path raw-item-url)
        item-url (absolutify-url raw-item-url base-url)]
    (log/debug (str src) " fetching: " {:base-url base-url
                                        :item-url item-url})
    {:base-url base-url
     :item-url item-url
     :http (fetch item-url
                  :user-agent (get-in src [:args :user-agent])
                  :trace? trace?)}))

(defn- hickory-preview [match]
  (let [rendered (if (or (map? match) (sequential? match))
                   (hick-r/hickory-to-html match)
                   (str match))]
    (if (> (count rendered) 2000)
      (str (subs rendered 0 2000) "…")
      rendered)))

(defn- selector-field-trace [src k hickory fallback trace?]
  (let [value (hick-select-extract-with-source src k hickory fallback)]
    (if trace?
      (let [selector (get-in src [:selectors k])
            matches (if selector (vec (hick-s/select selector hickory)) [])]
        {:selector (some-> selector str)
         :match-count (count matches)
         :matches (mapv hickory-preview (take 20 matches))
         :selected-hickory (vec (take 20 matches))
         :selected-hickory-truncated? (> (count matches) 20)
         :value value})
      {:value value})))

(defn extract-selector-item
  "Extract one fetched SelectorFeed item. Returns both the production item and a
  compact field trace suitable for diagnostics. With :allow-invalid? true,
  return the field traces and validation errors without constructing an item."
  [src feed-context {:keys [base-url item-url http]}
   & {:keys [allow-invalid?] :or {allow-invalid? false}}]
  (let [{:keys [hickory summary]} http
        trace? (:trace? feed-context)
        author-trace (selector-field-trace src :author hickory nil trace?)
        title-trace (selector-field-trace src :title hickory nil trace?)
        title (selector-item-title (:value title-trace) summary hickory item-url)
        ts-trace (selector-field-trace src :ts hickory (:ts summary) trace?)
        description-trace (selector-field-trace src :description hickory nil trace?)
        pub-ts (:value ts-trace)
        valid-ts? (s/valid? :irq0-fetch-item/ts pub-ts)
        ts-error (when-not valid-ts?
                   {:field :ts
                    :message "SelectorFeed :ts must produce a java.time.ZonedDateTime"
                    :expected "java.time.ZonedDateTime"
                    :actual-type (or (some-> pub-ts type str) "nil")})
        sanitized (fetchutils/process-html-contents base-url hickory)
        content-selector (get-in src [:selectors :content])
        content-matches (if content-selector
                          (vec (hick-s/select content-selector sanitized))
                          (vec (hick-s/select (hick-s/child (hick-s/tag :body)) sanitized)))
        content (first content-matches)
        content-html (if content (hick-r/hickory-to-html content) "")
        fields {:author author-trace
                :title (assoc title-trace :value title)
                :ts (cond-> (assoc ts-trace :valid? valid-ts?)
                      ts-error (assoc :validation-error (:message ts-error)
                                      :expected (:expected ts-error)
                                      :actual-type (:actual-type ts-error)))
                :description description-trace
                :content (cond-> {:selector (some-> content-selector str)
                                  :match-count (count content-matches)
                                  :value content-html}
                           trace? (assoc :matches (mapv hickory-preview
                                                        (take 20 content-matches))
                                         :selected-hickory (vec (take 20 content-matches))
                                         :selected-hickory-truncated? (> (count content-matches)
                                                                         20)))}
        errors (cond-> [] ts-error (conj ts-error))
        _ (when (and (seq errors) (not allow-invalid?))
            (throw (ex-info (:message ts-error)
                            (assoc ts-error :type ::invalid-selector-value))))
        item (when (empty? errors)
               (make-feed-item
                (:meta feed-context)
                {:title title :ts pub-ts}
                (make-item-hash title (str item-url) content-html)
                {:pub-ts pub-ts
                 :url item-url
                 :title title
                 :authors (:value author-trace)
                 :descriptions {"text/plain" (:value description-trace)}
                 :contents {"text/html" content-html
                            "text/plain" (html2text content-html)}}
                nil
                (:feed feed-context)))]
    {:item item
     :valid? (empty? errors)
     :errors errors
     :fields fields}))

(extend-protocol FetchSource
  llar.src.SelectorFeed
  (fetch-source [src _conditional-tokens]
    (let [index (fetch-selector-index src)]
      (doall
       (keep
        (fn [raw-item-url]
          (try
            (:item (extract-selector-item src index
                                          (fetch-selector-item src raw-item-url)))
            (catch clojure.lang.ExceptionInfo ex
              (if (logged-error? ex)
                (log/debugf "SelectorFeed item skipped after logged HTTP error: raw-url:%s"
                            raw-item-url)
                (log/warnf "SelectorFeed item failed: raw-url:%s data:%s"
                           raw-item-url (ex-data ex)))
              nil)
            (catch Throwable th
              (log/warnf th "SelectorFeed item failed: raw-url:%s"
                         raw-item-url)
              nil)))
        (:item-urls index))))))

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
                                      (merge
                                       (appconfig/http-request-timeouts)
                                       {:as :json
                                        :headers {:user-agent user-agent}})))
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
  (fetch-source [src _conditional-tokens]
    (let [wp-json-url (str (:url src))
          user-agent (resolve-user-agent
                      (get-in src [:args :user-agent]))
          site (with-http-exception-handler
                 {:url wp-json-url
                  :request :wp-json-site
                  :user-agent user-agent}
                 (http/get wp-json-url
                           (merge
                            (appconfig/http-request-timeouts)
                            {:as :json
                             :headers {"User-Agent" user-agent}})))
          posts-url (str (get-posts-url (:body site)) "?_embed")
          posts (-> (with-http-exception-handler
                      {:url posts-url
                       :request ::wp-json-posts
                       :user-agent user-agent}
                      (http/get posts-url (merge
                                           (appconfig/http-request-timeouts)
                                           {:as :reader
                                            :headers {:user-agent user-agent}})))
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
                          "text/plain" (html2text sanitized-html)}}
              {:site (:body site)
               :post post}
              {:title (get-in site [:body :name])
               :url (uri/uri (get-in site [:body :home]))
               :feed-type "wp-json"}))))))))

(extend-protocol ItemProcessor
  FeedItem
  (post-process-item [item _src _state]
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
          (update :entry merge (:entry item) nlp))))
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
