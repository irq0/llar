(ns infowarss.fetch
  (:require
   [infowarss.converter :as conv]
   [infowarss.postproc :refer [postproc]]
   [infowarss.src :refer :all]
   [clj-rome.reader :as rome]
   [digest]
   [clj-http.client :as http]
   [hickory.core :as hick]
   [hickory.select :as hick-s]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [clj-time.format :as tf]
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojure.tools.nrepl.server :as nrepl]
   [clojure.java.io :as io]
   [schema.core :as s]
   )
  (:import [java.util.Base64.Encoder]
           [org.joda.time.DateTime]
           [infowarss.src.Feed])
  )

(def Metadata
  "Metadata about an item"
  {:source s/Any
   :app s/Str
   :fetch-ts org.joda.time.DateTime
   :tags (s/pred set?)
   :version s/Int})

(def Summary
  "Summary data about an item"
  {:ts org.joda.time.DateTime
   :title s/Str})

(def Hash
  "Hash value of the item"
  (s/constrained s/Str (partial re-matches #"SHA-256\:[0-9a-f]{64}")))

(def HttpResponse
  {:headers s/Any
   :status s/Int
   :body s/Str})

(s/defn make-item-hash :- Hash
  "Make hash to use in *Item"
  [& args]
  (str "SHA-256:" (-> args string/join digest/sha-256)))

(s/defrecord HttpItem
    [meta :- Metadata
     summary :- Summary
     hash :- Hash
     http :- HttpResponse
     hickory :- s/Any])

(def FeedEntry
  {:url (s/maybe java.net.URL)
   :updated-ts (s/maybe org.joda.time.DateTime)
   :pub-ts (s/maybe org.joda.time.DateTime)
   :title s/Str
   :authors [s/Str]
   :contents {(s/required-key "text/plain") (s/maybe s/Str)
              (s/optional-key "text/html") s/Str}
   :description {(s/required-key "text/plain") (s/maybe s/Str)}})

(def Feed
  {:title s/Str
   :language (s/maybe s/Str)
   :url (s/maybe java.net.URL)
   :description {(s/required-key "text/plain") (s/maybe s/Str)}
   :encoding (s/maybe s/Str)
   :pub-ts (s/maybe org.joda.time.DateTime)
   :feed-type s/Str})

(s/defrecord FeedItem
    [meta :- Metadata
     summary :- Summary
     hash :- Hash
     feed-entry :- FeedEntry
     feed :- Feed])

(s/defn make-meta :- Metadata
  "Make meta entry from source and optional initial tags"
  ([src :- s/Any]
   (make-meta src #{}))
  ([src :- s/Any
    initial-tags :- (s/pred set?)]
  {:source src
   :app "infowarss.fetch"
   :fetch-ts (time/now)
   :tags initial-tags
   :version 0}))

(defn- extract-http-title
  [parsed-html]
  (-> (hick-s/select (hick-s/child
                      (hick-s/tag :title))
       parsed-html)
    first
    :content
    first
    string/trim))

(defn extract-http-timestamp
  [resp]
  (let [{:keys [headers]} resp
        parser (partial
                 tf/parse (tf/formatter "EEE, dd MMM yyyy HH:mm:ss z"))]
    (try+
      (or (parser (get headers "Last-Modified"))
        (parser (get headers "Date")))
      (catch Object _
        (time/now)))))

(defn fetch-http-generic
  "Generic HTTP fetcher"
  [src]
  (try+
    (let [url (-> src :url str)
          response (http/get url)
          parsed-html (-> response :body hick/parse hick/as-hickory)]
      {:meta (make-meta src #{:unread})
       :http (select-keys response [:headers :status :body])
       :hash (make-item-hash (:body response))
       :hickory parsed-html
       :summary {:ts (extract-http-timestamp response)
                 :title (extract-http-title parsed-html)}})

    (catch (contains? #{400 401 402 403 404 405 406 410} (get % :status))
        {:keys [headers body status]}
      (log/errorf "Client error probably due to broken request (%s): %s %s"
        status headers body)
      (throw+ (assoc &throw-context :type ::request-error)))

    (catch (contains? #{500 501 502 503 504} (get % :status))
        {:keys [headers body status] :as orig}
      (log/errorf "Server Error (%s): %s %s" status headers body)
      (throw+ (assoc &throw-context :type ::server-error-retry-later)))

    (catch [:status 408]
        {:keys [headers body status]}
      (log/errorf "Client Error (%s): %s %s" status headers body)
      (throw+ (assoc &throw-context :type ::client-error-retry-later)))

    (catch Object _
      (log/error "Unexpected error: " (:throwable &throw-context))
      (throw+ (assoc &throw-context :type ::unexpected-error)))))

;; Extractors for rome feed items

(defn- extract-feed-authors [authors]
  "Extract feed author from rome feed item"
  (for [{:keys [name email]} authors]
    (str
      (when-not (nil? name)
        name)
      (when-not (nil? email)
        (str " <" email ">")))))


(defn- extract-feed-description [description]
  "Extract feed description from rome reed item"
  (if (= (:type description) "text/html")
    {"text/html" (:value description)
     "text/plain" (conv/html2text (:value description))}
    {"text/plain" (:value description)}))

(def rome-content-type-to-mime {"html" "text/html"
                                "text" "text/plain"})

(defn- extract-feed-content [contents]
  "Extract feed content from rome feed item"
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

;; Fetch source protocol

(defprotocol FetchSource
  "Protocol to work with data sources"
  (fetch-source [src]))

(extend-protocol FetchSource
  infowarss.src.Http
  (fetch-source [src]
    [(map->HttpItem (fetch-http-generic src))])
  infowarss.src.Feed
  (fetch-source [src]
    (let [http-item (fetch-http-generic src)
          res (-> http-item :http :body rome/build-feed)
          feed {:title (-> res :title)
                :language (-> res :language)
                :url (-> res :link  maybe-extract-url)
                :description {"text/plain" (-> res :description)}
                :encoding (-> res :encoding)
                :pub-ts (some-> res :published-date tc/from-date)
                :feed-type (-> res :feed-type)}]

      (for [re (:entries res)]
        (let [timestamp (extract-feed-timestamp re http-item)
              authors (extract-feed-authors (:authors re))
              contents (extract-feed-content (:contents re))
              descriptions (extract-feed-description (:description re))

              base-feed-entry {:updated-ts (some-> re :updated-date tc/from-date)
                               :pub-ts (some-> re :published-date tc/from-date)
                               :url (-> re :link maybe-extract-url)
                               :title (-> re :title)}]
          (map->FeedItem
            (-> http-item
              (dissoc :http :hash :hickory :summary)
              (merge {:feed feed
                      :feed-entry (merge base-feed-entry
                                    {:authors authors
                                     :contents contents
                                     :description descriptions})
                      :hash (make-item-hash
                              (:title re) (:link re)
                              (get contents "text/plain"))
                      :summary {:ts timestamp
                                :title (:title re)}}))))))))

(defn fetch-and-process-source [source]
  "Fetches source, postprocesses results and handles exceptions in the
  process"
  (log/info "Fetching: " (:title source))
  (let [items (fetch-source source)
        processed (map postproc items)]
    processed))
