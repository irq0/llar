(ns infowarss.fetch.mercury
  (:require
   [infowarss.converter :as conv]
   [infowarss.fetch :as fetch :refer [FetchSource]]
   [infowarss.postproc :refer [ItemProcessor]]
   [infowarss.schema :as schema]
   [infowarss.persistency :refer [CouchItem]]
   [infowarss.analysis :as analysis]
   [infowarss.http :refer [absolutify-url absolutify-links-in-hick get-base-url-with-path blobify try-blobify-url! sanitize]]
   [clojurewerkz.urly.core :as urly]
   [hickory.core :as hick]
   [hickory.render :as hick-r]
   [digest]
   [clj-http.client :as http]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [schema.core :as s]
   [clojure.java.shell :as shell]
   [cheshire.core :as json]))

(s/defrecord MercuryItem
             [meta :- schema/Metadata
              summary :- schema/Summary
              hash :- schema/Hash
              entry :- schema/MercuryEntry]
  Object
  (toString [item] (fetch/item-to-string item)))

(extend-protocol ItemProcessor
  MercuryItem
  (post-process-item [item src state]
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

  (filter-item [item src state] false))

(extend-protocol CouchItem
  MercuryItem
  (to-couch [item]
    (-> item
        (dissoc :raw)
        (dissoc :body)
        (assoc-in [:meta :source :args] nil)
        (assoc :type :bookmark))))

(s/defn mercury-get
  [url :- schema/URLType
   api-key :- s/Str]
  (try+
   (let [url (urly/url-like url)
         resp (http/get "https://mercury.postlight.com/parser"
                        {:accept :json
                         :as :json
                         :content-type :json
                         :headers {:x-api-key api-key}
                         :query-params {:url (str url)}})
         base-url (get-base-url-with-path url)
         body (try+
               (assoc (:body resp) :content
                      (-> resp
                          :body
                          :content
                          hick/parse hick/as-hickory
                          (absolutify-links-in-hick base-url)
                          blobify
                          hick-r/hickory-to-html))
               (catch Object _
                 (log/warn &throw-context "Mercury post processing failed. Using vanilla")
                 (:body resp)))]
     body)
    ;; Mercury sends 502 when it's unable to parse the url
   (catch [:status 502] {:keys [headers body status]}
     (log/error "Mercury Error: " url status body)
     (throw+ {:type ::not-parsable}))

   (catch Object _
     (log/error (:throwable &throw-context) "Unexpected error. URL: " url)
     (throw+))))

(s/defn mercury-local
  [url :- schema/URLType]
  (try+
   (let [url (urly/url-like url)
         {:keys [exit out err]} (shell/sh "/home/seri/opt/mercury-parser/cli.js" (str url))
         base-url (get-base-url-with-path url)
         json (json/parse-string out true)]
     (if (zero? exit)
       (assoc json :content
              (try
                (-> json
                    :content
                    hick/parse
                    hick/as-hickory
                    (absolutify-links-in-hick base-url)
                    sanitize
                    blobify
                    hick-r/hickory-to-html)
                (catch Throwable th
                  (log/warn th "Mercury post processing failed. Using vanilla")
                  (log/debug (:content json)))))
       (do
         (log/error "Mercury Error: " url err)
         (throw+ {:type ::not-parsable}))))
   (catch Object _
     (log/error (:throwable &throw-context) "Unexpected error. URL: " url)
     (throw+))))

(extend-protocol FetchSource
  infowarss.src.MercuryWebParser
  (fetch-source [src]
    (let [url (urly/url-like (:url src))
          base-url (get-base-url-with-path url)
          mercu (mercury-local url)
          pub-ts (or (tc/from-string (:date_published mercu)) (time/now))
          title (cond
                  (string? (:title mercu)) (:title mercu)
                  (vector? (:title mercu)) (first (:title mercu))
                  :else "")]
      [(->MercuryItem
        (fetch/make-meta src)
        {:ts pub-ts :title title}
        (fetch/make-item-hash (:content mercu))
        {:url (absolutify-url (urly/url-like (:url mercu)) base-url)
         :lead-image-url (some-> (:lead_image_url mercu)
                                 urly/url-like
                                 (absolutify-url base-url)
                                 try-blobify-url!)
         :pub-ts pub-ts
         :title title
         :authors [(or (:author mercu) (:domain mercu))]
         :descriptions {"text/plain" (:excerpt mercu)}
         :contents {"text/html" (:content mercu)
                    "text/plain" (conv/html2text (:content mercu))}})])))
