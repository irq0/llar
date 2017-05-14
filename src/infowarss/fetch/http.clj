(ns infowarss.fetch.http
  (:require [infowarss.fetch :refer [FetchSource item-to-string make-meta make-item-hash]]
            [infowarss.schema :as schema]
            [twitter.api.restful :as twitter]
            [slingshot.slingshot :refer [throw+ try+]]
            [clj-http.client :as http]
            [hickory.core :as hick]
            [hickory.select :as hick-s]
            [hickory.render :as hick-r]
            [schema.core :as s]
            [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clj-time.format :as tf]
            [clj-time.core :as time]))

(s/defrecord HttpItem
    [meta :- schema/Metadata
     summary :- schema/Summary
     hash :- schema/Hash
     raw :- schema/HttpResponse
     hickory :- s/Any]
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

(defn fetch-http-generic
  "Generic HTTP fetcher"
  [src]
  (try+
    (let [url (-> src :url str)
          response (http/get url {:headers {:user-agent "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"}})
          parsed-html (-> response :body hick/parse hick/as-hickory)]
      (log/debugf "Fetched HTTP: %s -> %s bytes body" url (count (get response :body)))
      (map->HttpItem
        {:meta (make-meta src)
         :raw response
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
      (log/error "Unexpected error: " (:throwable &throw-context))
      (throw+ {:type ::unexpected-error}))))


(extend-protocol FetchSource
  infowarss.src.Http
  (fetch-source [src]
    [(fetch-http-generic src)]))
