(ns llar.fetch.hackernews
  (:require
   [llar.fetch :as fetch]
   [llar.fetchutils :as fetchutils]
   [llar.specs]
   [llar.src]
   [llar.persistency :as persistency]
   [llar.postproc :as postproc]
   [llar.analysis :as analysis]
   [llar.item]
   [llar.http :as llar-http :refer [with-http-exception-handler]]
   [clojure.tools.logging :as log]
   [clojure.spec.alpha :as s]
   [digest]
   [hiccup2.core :refer [html]]
   [clj-http.client :as http]
   [java-time.api :as time]
   [clojure.string :as string]
   [org.bovinegenius [exploding-fish :as uri]]))

;; https://hn.algolia.com/api

(def +hn-item-url-prefix+ "https://news.ycombinator.com/item?id=")
(def +hn-algoia-url+ "https://hn.algolia.com/api/v1/search_by_date?")
(def +hn-user-agent+ "java:llar:23: (https://github.com/irq0/llar")

(defn- hn-item-to-string [item]
  (format "[%s: %s/%s/%s/id:%sscore:%s]"
          (.getSimpleName (class item))
          (str (get-in item [:meta :source]))
          (if-not (nil? (get-in item [:summary :ts]))
            (time/format :iso-instant (get-in item [:summary :ts]))
            "?")
          (str (get-in item [:summary :title]))
          (str (get-in item [:entry :id]))
          (str (get-in item [:entry :score]))))

(defrecord HackerNewsItem
           [meta
            summary
            hash
            entry]
  Object
  (toString [item] (hn-item-to-string item)))

(defn make-hn-item [meta summary hash entry]
  {:pre [(s/valid? :irq0/item-metadata meta)
         (s/valid? :irq0/item-summary summary)
         (s/valid? :irq0/item-hash hash)]}
  (->HackerNewsItem meta summary hash entry))

(extend-protocol postproc/ItemProcessor
  HackerNewsItem
  (post-process-item [item _src _state]
    (let [nlp (analysis/analyze-entry (:entry item))]
      (update item :entry merge (:entry item) nlp)))
  (filter-item [_ _ _] false))

(extend-protocol persistency/CouchItem
  HackerNewsItem
  (to-couch [item]
    (-> item
        (assoc :type :link)
        (dissoc :raw)
        (assoc-in [:meta :source :args] nil))))

(s/def :irq0-hn/numeric-filter-args (s/keys :opt-un [:irq0-hn-filter/min-score :irq0-hn-filter/min-comments :irq0-hn-filter/created-after]))
(s/def :irq0-hn/algolia-query-args (s/keys :opt-un [:irq0-hn-filter/query :irq0-hn-filter/count :irq0-hn-filter/tags :irq0-hn-filter/filters :irq0-hn-filter/count :irq0-hn-filter/min-score :irq0-hn-filter/min-comments :irq0-hn-filter/created-after]))

(defn- make-numeric-filters [{:keys [min-score min-comments created-after] :as filters}]
  {:pre [(s/assert :irq0-hn/numeric-filter-args filters)]}
  (string/join
   ","
   (cond-> []
     min-score (conj (str "points>=" min-score))
     min-comments (conj (str "num_comments>=" min-comments))
     created-after (conj (str "created_at_i>=" (long (/ (time/to-millis-from-epoch created-after) 1000)))))))

(defn- make-algolia-query [& {:keys [count tags query filters min-score min-comments created-after] :as args}]
  {:pre [(s/assert :irq0-hn/algolia-query-args args)]}
  (cond-> {}
    (or min-score min-comments created-after)
    (assoc "numericFilters" (make-numeric-filters args))
    query (assoc "query" query)
    tags (assoc "tags" tags)
    count (assoc "hitsPerPage" count)
    filters (assoc "filters" filters)))

(defn- query-algolia [query]
  (log/debug "HN algolia query: " query)
  (with-http-exception-handler
    {:url +hn-algoia-url+
     :user-agent +hn-user-agent+
     :request ::hn-algolia-query}
    (let [resp  (http/get +hn-algoia-url+
                          {:query-params query
                           :accept :json
                           :as :json
                           :headers {:user-agent +hn-user-agent+}})]
      (:body resp))))

;; tag: front_page, comment, ask_hn, show_hn, job
(defn- get-hn-by-tag [tag options]
  {:pre [(s/assert :irq0-hn/tag tag) (s/assert :irq0-hn/algolia-query-args options)]}
  (query-algolia (make-algolia-query (merge {:tags (name tag)} options))))

(s/def :irq0-hn/type (s/and keyword? #{:job :story :comment :poll :pollopt :unknown}))
(defn hn-type [hit]
  {:post [(s/valid? :irq0-hn/type %)]}
  (condp #(contains? %2 %1) (->> hit :_tags (into #{}))
    "job" :job
    "story" :story
    "comment" :comment
    "poll" :poll
    "pollopt" :pollopt
    :unknown))

(defn hn-html-summary [hit]
  (let [{:keys [points url objectID story_text]} hit]
    (str
     (html
      [:div {:class "summary"}
       [:div
        [:p [:span {:class "key"} "Score: "] points " " [:span {:class "key"} "Type: "] (hn-type hit)]
        (when url [:p [:span {:class "key"} "URL: "] [:a {:href url} url]])
        [:p [:span {:class "key"} "Comments: "]
         [:a {:href (str "https://news.ycombinator.com/item?id=" objectID)}
          (str "https://news.ycombinator.com/item?id=" objectID)]]]]
      [:p story_text]))))

(defn make-hn-entry-from-algolia-hit [hit]
  (let [{:keys [url num_comments title created_at author points objectID story_text]} hit
        hn-url (uri/uri (str +hn-item-url-prefix+ objectID))]
    {:score points
     :num-comments num_comments
     :authors [author]
     :id objectID
     :type (hn-type hit)
     :pub-ts (fetchutils/parse-timestamp :iso-date-time created_at)
     :title title
     :url (if (string? url)
            (llar-http/absolutify-url url)
            hn-url)
     :comments-url hn-url
     :contents {"text/plain" (str title "\n" story_text)
                "text/html" (hn-html-summary hit)}}))

(extend-protocol fetch/FetchSource
  llar.src.HackerNews
  (fetch-source [src _conditional-tokens]
    (let [resp (get-hn-by-tag (:tag src) (:args src))]
      (for [hit (:hits resp)]
        (make-hn-item
         (fetch/make-meta src)
         {:ts (fetchutils/parse-timestamp :iso-date-time (:created_at hit))
          :title (:title hit)}
         (fetch/make-item-hash (:objectID hit))
         (make-hn-entry-from-algolia-hit hit))))))
