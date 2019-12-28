(ns infowarss.fetch.feed
  (:require [infowarss.fetch :refer [FetchSource
                                     item-to-string
                                     make-meta
                                     make-item-hash
                                     tag-items]]
            [infowarss.postproc :refer [ItemProcessor]]
            [infowarss.persistency :refer [CouchItem]]
            [infowarss.http :refer [fetch
                                    absolutify-links-in-hick
                                    absolutify-url
                                    get-base-url
                                    blobify
                                    sanitize
                                    +http-user-agent+]]
            [infowarss.analysis :as analysis]
            [infowarss.schema :as schema]
            [infowarss.converter :as conv]
            [infowarss.src :as src]
            [twitter.api.restful :as twitter]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+ try+]]
            [clojurewerkz.urly.core :as urly]
            [taoensso.timbre :as log]
            [hiccup.core :refer [html]]
            [hickory.core :as hick]
            [hickory.select :as hick-s]
            [hickory.render :as hick-r]
            [clj-rome.reader :as rome]
            [clojure.java.io :as io]
            [clj-http.client :as http]
            [cheshire.core :as cheshire]
            [clj-time.core :as time]
            [clj-time.format :as tf]
            [clojure.string :as string]
            [clj-time.coerce :as tc]))

(def +g+-base-url+
  "https://www.googleapis.com/plus/v1/")

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
        hick/parse hick/as-hickory
        (absolutify-links-in-hick base-url)
        sanitize
        blobify
        (hick-r/hickory-to-html)))
    contents))

(extend-protocol FetchSource
  infowarss.src.Feed
  (fetch-source [src]
    (let [url (urly/url-like (:url src))
          base-url (get-base-url url)
          http-item (fetch url)
          res (try+
               (-> http-item :raw :body rome/build-feed)
               (catch Object _
                 (log/error (:throwable &throw-context) "rome parse failed" http-item)
                 (throw+ {:type ::rome-failure :http-item http-item})))
          raw-feed-url (:link res)
          feed-url (if (nil? raw-feed-url)
                     url
                     (absolutify-url (urly/url-like raw-feed-url) base-url))

          feed {:title (-> res :title)
                :language (-> res :language)
                :url feed-url
                :descriptions {"text/plain" (-> res :description)}
                :encoding (-> res :encoding)
                :pub-ts (some->> res :published-date tc/from-date)
                :feed-type (-> res :feed-type)}]
      (for [re (:entries res)]
        (let [timestamp (extract-feed-timestamp re http-item)
              authors (extract-feed-authors (:authors re))
              in-feed-contents (extract-feed-content (:contents re))
              contents-url (-> re :link urly/url-like)
              contents-base-url (if (urly/absolute? contents-url)
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
              base-entry {:updated-ts (some-> re :updated-date tc/from-date)
                          :pub-ts (some-> re :published-date tc/from-date)
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
    (if (some? selector)
      (let [tree (hick-s/select selector hickory)]
        (extractor tree)))))

(defn hick-select-extract-with-source [src k hickory fallback]
  (let [{:keys [selectors extractors]} src
        sel (get selectors k)
        ext (get extractors k)]
    (try+
      (let [selected (hick-select-extract sel ext hickory)]
        (when-not (and (some? sel) (some? selected))
          (log/debugf "Hickory selector %s for %s turned up nothing"
            (str src) k))
        (or selected fallback))
      (catch Object _
        (log/warn (:throwable &throw-context) "Hick Extract failed :("
          src k)
        (throw+ {:type ::hickory-select-failed :src src :key k
                 :selector sel :extractor ext})))))


(extend-protocol FetchSource
  infowarss.src.SelectorFeed
  (fetch-source [src]
    (let [{:keys [url selectors extractors]} src
          {:keys [summary hickory]} (fetch url)

          meta (make-meta src)
          feed {:title (:title summary)
                :url url
                :feed-type "selector-feed"}
          item-extractor (or (:urls extractors)
                             (fn [l] (map (fn [x] (-> x :attrs :href)) l)))
          item-urls (item-extractor (hick-s/select (:urls selectors) hickory))]

      (log/debug (str src) " Parsed URLs: " (prn-str item-urls))
      (when-not (coll? item-urls)
        (throw+ {:type ::selector-found-shit :extractor item-extractor :urls item-urls :selector (:urls selectors)}))
      (doall
       (for [raw-item-url item-urls
             :let [item-url (urly/url-like raw-item-url)
                   item (fetch item-url)
                   {:keys [hickory summary]} item]]
         (let [author (hick-select-extract-with-source src :author hickory nil)
               title (hick-select-extract-with-source src :title hickory (:title summary))
               pub-ts (hick-select-extract-with-source src :ts hickory (:ts summary))
               description (hick-select-extract-with-source src :description hickory nil)
               content (or
                        (and (some? (:content selectors))
                             (first (hick-s/select (:content selectors) hickory)))
                        (first (hick-s/select (hick-s/child (hick-s/tag :body)) hickory)))
               content-html (hick-r/hickory-to-html content)]
           (map->FeedItem
            (-> item
                (dissoc :meta :hash :hickory :summary :body)
                (merge {:meta meta
                        :feed feed
                        :hash (make-item-hash title pub-ts item-url)
                        :entry {:pub-ts pub-ts
                                :url item-url
                                :title title
                                :authors author
                                :descriptions {"text/plain" description}
                                :contents {"text/html" content-html
                                           "text/plain" (conv/html2text content-html)}}
                        :summary {:title title
                                  :ts pub-ts}})))))))))


(extend-protocol FetchSource
  infowarss.src.WordpressJsonFeed
  (fetch-source [src]
    (let [wp-json-url (str (:url src))
          site (http/get wp-json-url {:as :json :headers {:user-agent +http-user-agent+}})
          posts-url (get-in site [:body :routes (keyword "/wp/v2/posts") :_links :self])
          posts (-> (http/get posts-url {:headers {:user-agent +http-user-agent+}}) :body (cheshire/parse-string true)) ]
      (doall
       (for [post posts]
         (let [
               authors (try+
                        (for [url (map :href (get-in post [:_links :author]))]
                          (get-in [:body :name]
                                  (http/get url
                                            {:as :json
                                             :headers {:user-agent +http-user-agent+}})))
                        (catch Object _
                          [""]))
               url (urly/url-like (get post :link))
               title (get-in post [:title :rendered])
               pub-ts (some-> post :date_gmt tc/from-string)
               description (get-in post [:excerpt :rendered])
               content-html (get-in post [:content :rendered])]
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
                     :contents {"text/html" content-html
                                "text/plain" (conv/html2text content-html)}}
             :summary {:title title
                       :ts pub-ts}})))))))


(defn g+-html-att [att]
  (html
    [:div {:class "attachment-item"}
     [:h4 (string/capitalize (:objectType att)) ": " (or (:displayName att) "Unknown")]
     [:div {:class "content"}
      (when-let [img (:fullImage att)]
        [:img {:src (:url img)}])
      (when-let [cont (:content att)]
        [:p (:content att)])
      [:a {:href (:url att)} "Link"]]]))


(defn g+-html-summary [item score]
  (html
    [:h1 (:title item)]
    [:div {:class "summary"}
     [:ul
      [:li [:span {:class "key"} "Score: "]
       (string/join ", "
         (map (fn [[k v]] (str (name k) "=" v)) score))]
      [:li [:span {:class "key"} "Published: "] (:published item)]
      [:li [:span {:class "key"} "Updated: "] (:updated item)]
      [:div {:class "links"}
       [:ul
        [:li [:span {:class "key"} "URL: "]
         [:a {:href (:url item)} (:url item)]]]]]]
    [:div {:class "content"}
     [:p (get-in item [:object :content])]]
    (when-let [atts (get-in item [:object :attachments])]
      [:div {:class "attachments"}
       (map g+-html-att atts)])))

(extend-protocol FetchSource
  infowarss.src.GooglePlusActivityFeed
  (fetch-source [src]
    (let [{:keys [user-id api-key]} src
          profile-url (format "%speople/%s?key=%s"
                        +g+-base-url+ user-id api-key)
          activity-feed-url (format "%speople/%s/activities/public?key=%s"
                              +g+-base-url+ user-id api-key)
          profile (:body (http/get profile-url {:as :json}))
          activity-feed (:body (http/get activity-feed-url {:as :json}))

          feed {:title (:displayName profile)
                :url (:url profile)
                :feed-type "g+feed"}]
      (doall
        (for [item (:items activity-feed)]
          (let [authors [(get-in item [:actor :displayName])]
                url (urly/url-like (:url item))
                id (:id item)
                title (first
                        (string/split
                          (if (string/blank? (:title item))
                            (or (-> item :object :attachments first :displayName) id)
                            (:title item))
                        #"(\n|\r\n)"))
                pub-ts (tc/from-string (:published item))
                updated-ts (tc/from-string (:updated item))
                comments-feed (:body (http/get (format "%sactivities/%s/comments?key=%s"
                                                 +g+-base-url+ id api-key)
                                       {:as :json}))

                score {:replies (get-in item [:object :replies :totalItems])
                       :plus1 (get-in item [:object :plusoners :totalItems])
                       :reshares (get-in item [:object :resharers :totalItems])
                       :comments (count (:items comments-feed))}

                description (or (get-in item [:object :content])
                              (-> item :object :attachments first :content))
                content-html (g+-html-summary item score)]
            (map->FeedItem
              {:meta (make-meta src)
               :feed feed
               :hash (make-item-hash title pub-ts description)
               :raw {:profile profile
                     :activity item
                     :comments comments-feed}
               :entry {:pub-ts pub-ts
                       :url url
                       :title title
                       :score score
                       :authors authors
                       :descriptions {"text/plain" description}
                       :contents {"text/html" content-html
                                  "text/plain" (conv/html2text content-html)}}
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
                           :has-spotify-playlist)
                         ]))]
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
