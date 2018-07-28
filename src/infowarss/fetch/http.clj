(ns infowarss.fetch.http
  (:require [infowarss.fetch :refer [FetchSource item-to-string make-meta make-item-hash tag-items]]
            [infowarss.postproc :refer [ItemProcessor]]
            [infowarss.schema :as schema]
            [infowarss.converter :as conv]
            [infowarss.analysis :as analysis]
            [twitter.api.restful :as twitter]
            [infowarss.persistency :refer [CouchItem]]
            [slingshot.slingshot :refer [throw+ try+]]
            [clj-http.client :as http]
            [hickory.core :as hick]
            [hickory.select :as hick-s]
            [hickory.render :as hick-r]
            [hickory.zip :as hick-z]
            [schema.core :as s]
            [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.zip :as zip]
            [clj-time.format :as tf]
            [clj-time.core :as time]))

(s/defrecord HttpItem
    [meta :- schema/Metadata
     summary :- schema/Summary
     hash :- schema/Hash
     raw :- schema/HttpResponse
     body :- s/Str
     hickory :- s/Any]
  Object
  (toString [item] (item-to-string item)))

(s/defrecord GenericWebsiteItem
    [meta :- schema/Metadata
     summary :- schema/Summary
     hash :- schema/Hash
     entry :- schema/FeedEntry
     feed :- schema/Feed]
  Object
  (toString [item] (item-to-string item)))

(defn- parse-http-ts [ts]
  (when-not (nil? ts)
    (tf/parse (tf/formatter "EEE, dd MMM yyyy HH:mm:ss z") ts)))


(defn- extract-http-title
  [parsed-html]
  (some-> (hick-s/select (hick-s/child
                      (hick-s/tag :title))
       parsed-html)
    first
    :content
    first
    string/trim))

(defn- extract-http-timestamp
  [resp]
  (let [{:keys [headers]} resp
        parser (partial
                 tf/parse (tf/formatter "EEE, dd MMM yyyy HH:mm:ss z"))]
    (try+
      (or (parser (get headers "Last-Modified"))
        (parser (get headers "Date")))
      (catch Object _
        (time/now)))))


(defn absolutify-url [url base-url]
  (cond
    (and (string? url) (string? base-url) (string/starts-with? url "/") (string/ends-with? base-url "/"))
    (str base-url (subs url 1))
    (and (string? url) (string/starts-with? url "/"))
    (str base-url url)
    :default
    url))

(defn parse-img-srcset [str]
  (when (string? str)
    (map #(string/split % #"\s") (string/split str #"\s?,\s?"))))


(defn unparse-img-srcset [parsed]
  (when (coll? parsed)
    (string/join ", " (map #(string/join " " %) parsed))))

(defn edit-img-tag [base-url loc]
  (zip/edit loc update-in [:attrs]
    (fn [attrs]
      (let [{:keys [src srcset]} attrs]
        (-> attrs
          (assoc :src (absolutify-url src base-url))
          (assoc :srcset (some->> (parse-img-srcset srcset)
                           (map (fn [[url descr]] [(absolutify-url url base-url) descr]))
                           unparse-img-srcset)))))))


(defn absolutify-links-in-hick [root base-url]
  (let [zipper (hick-z/hickory-zip root)
        edit-tag (fn [tag loc]
                   (case tag
                     :a (zip/edit loc update-in [:attrs :href] absolutify-url base-url)
                     :img (edit-img-tag base-url loc)
                     loc))]
    (loop [loc zipper]
      (if (zip/end? loc)
        (zip/root loc)
        (let [{:keys [tag type content attrs]} (zip/node loc)]
          (if (= type :element)
            (recur
              (zip/next
                (edit-tag tag loc)))
            (recur (zip/next loc))))))))

(defn- maybe-extract-url
  [s]
  (try+
    (io/as-url s)
    (catch java.net.MalformedURLException _
      nil)))

(defn get-base-url [u]
  (let [url (maybe-extract-url u)]
    (try+
      (java.net.URL. (.getProtocol url) (.getHost url) (.getPort url) "/")
      (catch Object _
        (log/warn "http fetcher: failed to get base url for:" u)
        url))))

(defn fetch-http-generic
  "Generic HTTP fetcher"
  [src]
  (try+
    (let [url (-> src :url str)
          response (http/get url {:headers {:user-agent "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"}})
          base-url (get-base-url url)
          parsed-html (-> response
                        :body
                        hick/parse hick/as-hickory
                        (absolutify-links-in-hick base-url))]
      (log/debugf "Fetched HTTP: %s -> %s bytes body" url (count (get response :body)))
      (map->HttpItem
        {:meta (make-meta src)
         :raw response
         :body (hick-r/hickory-to-html parsed-html)
         :hash (make-item-hash (:body response))
         :hickory parsed-html
         :summary {:ts (extract-http-timestamp response)
                   :title (extract-http-title parsed-html)}}))

    (catch (contains? #{400 401 402 403 404 405 406 410} (get % :status))
        {:keys [headers body status]}
      (log/errorf "Client error probably due to broken request (%s): %s %s"
        status headers body)
      (throw+ {:type ::request-error}))

    (catch (contains? #{500 501 502 503 504} (get % :status))
        {:keys [headers body status] :as orig}
      (log/errorf "Server Error (%s): %s %s" status headers body)
      (throw+ {:type ::server-error-retry-later}))

    (catch [:status 408]
        {:keys [headers body status]}
      (log/errorf "Client Error (%s): %s %s" status headers body)
      (throw+ {:type :client-error-retry-later}))

    (catch Object _
      (log/error "Unexpected error: " (:throwable &throw-context) (-> src :url str))
      (throw+ {:type ::unexpected-error :url (-> src :url str)}))))


(extend-protocol FetchSource
  infowarss.src.Http
  (fetch-source [src]
    [(fetch-http-generic src)]))

(extend-protocol ItemProcessor
  HttpItem
  (post-process-item [item src state] item)
  (filter-item [item src state] false))


(extend-protocol FetchSource
  infowarss.src.GenericWebsite
  (fetch-source [src]
    (let [{:keys [url]} src
          {:keys [summary body raw hickory meta]} (fetch-http-generic src)

          feed {:title "[website]"
                :url ""
                :feed-type "generic-website"}]

      [(map->GenericWebsiteItem
        {:meta meta
         :summary summary
         :hash (make-item-hash (:title summary) body)
         :entry {:pub-ts (:ts summary)
                 :url url
                 :title (:title summary)
                 :authors ""
                 :descriptions {"text/plain" body}
                 :contents {"text/html" body
                            "text/plain" (conv/html2text body)}}
         :feed feed})])))

(extend-protocol ItemProcessor
  GenericWebsiteItem
  (post-process-item [item src state]
    (let [nlp (analysis/analyze-entry (:entry item))
          urls (get-in nlp [:nlp :urls])
          tags (into #{}
                 (remove nil?
                 [(when (some #(re-find #"^https?://\w+\.(youtube|vimeo|youtu)" %) urls)
                    :has-video)
                  (when (and (string? (:url item)) (re-find #"^https?://\w+\.(youtube|vimeo|youtu)" urls))
                    :has-video)

                  ]))]
      (-> item
        (update-in [:meta :tags] into tags)
        (update :entry merge (:entry item) nlp)
        (tag-items src))))
  (filter-item [item src state] false))


(extend-protocol CouchItem
  GenericWebsiteItem
  (to-couch [item]
    (-> item
      (assoc :type :website))))


;; (preview (src/http "https://www.scality.com/blog") :post [(fn [site] (let [hick (:hickory site)
;;                                                                                  article-urls (->>
;;                                                                                (hickory.select/select
;;                                                                                  (hickory.select/descendant
;;                                                                                    (hickory.select/class "read--more--button")
;;                                                                                    (hickory.select/tag :a))
;;                                                                                  hick)
;;                                                                                (map #(get-in % [:attrs :href])))
;;                                                                                  feed {:title (get-in site [:summary :title])
;;                                                                                        :url (get-in site [:meta :src :url])
;;                                                                                        :encoding (-> res :encoding)
;;                                                                                        :pub-ts (get-in site [:summary :ts])
;;                                                                                        :feed-type "http-postproc"}
;;                                                                                  entries-http
;;                                                                                  (map #(infowarss.fetch.http/fetch-http-generic {:url %})
;;                                                                                    article-urls)]
;;                                                                              (for [entry-http entires-http
;;                                                                                    :let [{:keys [summary meta]} entry-http
;;                                                                                          url (get-in meta [:src :url])]
;;                                                                                    ]
;;                                                                                (map->FeedItem
;;                                                                                  (-> site
;;                                                                                  (dissoc :hash :hickory :summary)
;;                                                                                  (merge {:raw nil
;;                                                                                          :feed feed
;;                                                                                          :entry (merge base-entry
;;                                                                                                   {:authors authors
;;                                                                                                    :contents contents
;;                                                                                                    :descriptions descriptions})
;;                                                                                          :hash (infowarss.fetch/make-item-hash
;;                                                                                                  (:title summary) url)
;;                                                                                          :summary summary
;;                                                                                          })))
;;                                                                   (assoc site :body nil) )])
