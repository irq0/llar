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
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojure.tools.nrepl.server :as nrepl]
   [cheshire.generate :refer [add-encoder encode-map]])
  (:import [java.util.Base64.Encoder]
           [infowarss.src.Feed])
  )

(defrecord HttpItem [source meta summary hash http raw])
(defrecord FeedItem [source meta summary hash feed-entry feed])

(defn- make-meta [url]
  "Make meta entry for *Item"
  {:source {:app "infowar-core/fetch-http"
            :address url}
   :fetch-ts (time/now)
   :tags #{:unread}
   :version 0})


(defn- fetch-http-generic [url]
  "Generic HTTP fetcher"

  (try+
    (let [response (http/get url)
          parsed-html (-> response :body hick/parse hick/as-hickory)]
      [{:meta (make-meta url)
        :http (select-keys response [:headers :status])
        :raw (:body response)
        :hash (digest/sha-256 (:body response))
        :summary {:from url
                  :title (-> (hick-s/select (hick-s/child
                                             (hick-s/tag :title))
                              parsed-html)
                           first
                           :content
                           first
                           string/trim)}}])

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
  (or (get e :published-date)
    (get e :updated-date)
    (get-in http [:meta :fetch-ts])))

;; Fetch source protocol

(defprotocol FetchSource
  "Protocol to work with data sources"
  (fetch-source [src]))

(add-encoder org.joda.time.DateTime
  (fn [dt jg]
    (.writeString jg (tc/to-string dt))))

(extend-protocol FetchSource
  infowarss.src.Http
  (fetch-source [src]
    (map map->HttpItem (fetch-http-generic (:url src))))
  infowarss.src.Feed
  (fetch-source [src]
    (map map->FeedItem
      (let [http (-> (fetch-http-generic (:url src)) first)
            feed (rome/build-feed (-> http :raw))]
        (for [e (:entries feed)]
          (let [authors (extract-feed-authors (:authors e))
                content (extract-feed-content (:contents e))
                timestamp (extract-feed-timestamp e http)
                description (extract-feed-description (:description e))]
            (-> http
              (dissoc :raw :parsed-html :http)
              (assoc :feed-entry (select-keys e [:link :updated-date :published-date :title]))
              (assoc-in [:feed-entry :authors] authors)
              (assoc-in [:feed-entry :contents] content)
              (assoc-in [:feed-entry :description] description)
              (assoc :feed (select-keys feed [:title :language :link :description
                                              :encoding :published-date :feed-type]))
              (assoc :hash (digest/sha-256
                             (str
                               (:title e)
                               (:link e)
                               (get content "text/plain"))))
              (assoc :summary {:from authors
                               :ts timestamp
                               :title (:title e)}))))))))

(defn fetch-and-process-source [source]
  "Fetches source, postprocesses results and handles exceptions in the
  process"
  (log/info "Fetching: " (:title source))
  (map #(-> %
         (assoc :source source)
         (postproc))
    (fetch-source source)))
