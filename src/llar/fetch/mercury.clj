(ns llar.fetch.mercury
  (:require
   [llar.converter :as conv]
   [llar.fetch :as fetch :refer [FetchSource]]
   [llar.postproc :refer [ItemProcessor]]
   [llar.persistency :refer [CouchItem]]
   [llar.analysis :as analysis]
   [llar.http :refer [resolve-user-agent absolutify-url absolutify-links-in-hick get-base-url-with-path blobify try-blobify-url! sanitize]]
   [llar.appconfig :as appconfig]
   [org.bovinegenius [exploding-fish :as uri]]
   [hickory.core :as hick]
   [hickory.render :as hick-r]
   [digest]
   [llar.item]
   [java-time.api :as time]
   [clojure.tools.logging :as log]
   [slingshot.slingshot :refer [throw+]]
   [clojure.spec.alpha :as s]
   [clojure.java.shell :as shell]
   [cheshire.core :as json]))

(defrecord MercuryItem
           [meta
            summary
            hash
            entry]
  Object
  (toString [item] (fetch/item-to-string item)))

(defn make-mercury-item [meta summary hash entry]
  {:pre [(s/valid? :irq0/item-metadata meta)
         (s/valid? :irq0/item-summary summary)
         (s/valid? :irq0/item-hash hash)]}
  (->MercuryItem meta summary hash entry))

(extend-protocol ItemProcessor
  MercuryItem
  (post-process-item [item _src _state]
    (let [nlp (analysis/analyze-entry (:entry item))
          tags (set
                (remove nil?
                        [(when (some #(re-find #"^https?://\w+\.(youtube|vimeo|youtu)" %) (:urls nlp))
                           :has-video)
                         (when (and (string? (:url item)) (re-find #"^https?://\w+\.(youtube|vimeo|youtu)" (:url item)))
                           :has-video)]))]

      (-> item
          (update-in [:meta :tags] into tags)
          (update :entry merge (:entry item) nlp))))

  (filter-item [_ _ _] false))

(extend-protocol CouchItem
  MercuryItem
  (to-couch [item]
    (-> item
        (dissoc :raw)
        (dissoc :body)
        (assoc-in [:meta :source :args] nil)
        (assoc :type :bookmark))))

(defn- mercury-local
  [url user-agent]
  {:pre [(s/valid? :irq0/url url)]}
  (let [url (uri/uri url)
        {:keys [exit out err]} (shell/sh (appconfig/command :mercury-parser) (str url)
                                         (str "--header.user-agent=" user-agent))
        base-url (get-base-url-with-path url)
        {:keys [failed message error content] :as json} (json/parse-string out true)]
    (if (and (zero? exit) (not failed) (not error))
      (assoc json :content
             (try
               (-> content
                   hick/parse
                   hick/as-hickory
                   (absolutify-links-in-hick base-url)
                   sanitize
                   blobify
                   hick-r/hickory-to-html)
               (catch Throwable th
                 (log/warn th "mercury local post processing failed. Using vanilla. Url:" url)
                 (log/debug {:content json
                             :url url
                             :mercury {:stdout out
                                       :stderr err
                                       :exit exit}
                             :json json}))))
      (let [status (->> message
                        (re-find #"Resource returned a response status code of (4..) and resource was instructed to reject non-200 status codes.")
                        second
                        parse-long)]
        (cond
          (#{400 401 402 403 404 405 406 410} status)
          (do
            (log/warnf "mercury HTTP client error probably due to broken request (%s): message:%s"
                       status message)
            (throw+ {:type :llar.http/request-error
                     :code status
                     :stdout out
                     :stderr err
                     :exit exit
                     :message message
                     :request ::mercury-cli
                     :url url}))
          (#{500 501 502 503 504} status)
          (do (log/warnf "server Error (%s): %s" status message)
              (throw+ {:type :llar.http/server-error-retry-later
                       :code status
                       :stdout out
                       :stderr err
                       :exit exit
                       :message message
                       :request ::mercury-cli
                       :url url}))
          (#{408 429} status)
          (do
            (log/warnf "client Error (overloaded?) (%s): %s" status message)
            (throw+ {:type :llar.http/client-error-retry-later
                     :code status
                     :stdout out
                     :stderr err
                     :exit exit
                     :message message
                     :request ::mercury-cli
                     :url url}))
          :else
          (do
            (log/warn "mercury Error: " url)
            (throw+ {:type ::not-parsable
                     :url url
                     :stdout out
                     :request ::mercury-cli
                     :stderr err
                     :exit exit
                     :message message})))))))

(extend-protocol FetchSource
  llar.src.MercuryWebParser
  (fetch-source [src _conditional-tokens]
    (let [url (uri/uri (:url src))
          base-url (get-base-url-with-path url)
          mercu (mercury-local url (resolve-user-agent (get src :user-agent :default)))
          pub-ts (or (when (string? (:date_published mercu)) (time/zoned-date-time (time/formatter :iso-zoned-date-time)
                                                                                   (:date_published mercu)))
                     (time/zoned-date-time))
          title (cond
                  (string? (:title mercu)) (:title mercu)
                  (vector? (:title mercu)) (first (:title mercu))
                  :else "")]
      [(make-mercury-item
        (fetch/make-meta src)
        {:ts pub-ts :title title}
        (fetch/make-item-hash (:content mercu))
        {:url (absolutify-url (uri/uri (:url mercu)) base-url)
         :lead-image-url (some-> (:lead_image_url mercu)
                                 uri/uri
                                 (absolutify-url base-url)
                                 try-blobify-url!)
         :pub-ts pub-ts
         :title title
         :authors [(or (:author mercu) (:domain mercu))]
         :descriptions {"text/plain" (:excerpt mercu)}
         :contents {"text/html" (:content mercu)
                    "text/plain" (conv/html2text (:content mercu))}})])))
