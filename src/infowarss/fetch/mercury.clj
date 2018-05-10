(ns infowarss.fetch.mercury
  (:require
   [infowarss.converter :as conv]
   [infowarss.src :as src]
   [infowarss.fetch :as fetch]
   [infowarss.schema :as schema]
   [infowarss.persistency :as persistency]
   [infowarss.postproc :as postproc]
   [infowarss.analysis :as analysis]
   [infowarss.fetch.http :refer [absolutify-links-in-hick get-base-url]]
   [hickory.core :as hick]
   [hickory.render :as hick-r]
   [digest]
   [clj-http.client :as http]
   [hickory.core :as hick]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojure.java.io :as io]
   [schema.core :as s]
   ))

(s/defrecord MercuryItem
    [meta :- schema/Metadata
     summary :- schema/Summary
     hash :- schema/Hash
     entry :- schema/MercuryEntry]
  Object
  (toString [item] (fetch/item-to-string item)))


(extend-protocol postproc/ItemProcessor
  MercuryItem
  (post-process-item [item src state]
    (let [nlp (analysis/analyze-entry (:entry item))
          tags (into #{}
                 (remove nil?
                 [(when (some #(re-find #"^https?://\w+\.(youtube|vimeo|youtu)" %) (:urls nlp))
                    :has-video)
                  (when (and (string? (:url item)) (re-find #"^https?://\w+\.(youtube|vimeo|youtu)" (:url item)))
                    :has-video)

                  ]))]
      (log/spy tags)
      (-> item
        (update-in [:meta :tags] into tags)
        (update :entry merge (:entry item) nlp))))

  (filter-item [item src state] false))

(extend-protocol persistency/CouchItem
  MercuryItem
  (to-couch [item]
    (-> item
      (dissoc :raw)
      (dissoc :body)
      (assoc-in [:meta :source :args] nil)
      (assoc :type :bookmark))))

(defn mercury-get [url api-key]
  (try+
    (let [resp (http/get "https://mercury.postlight.com/parser"
                  {:accept :json
                   :as :json
                   :content-type :json
                   :headers {:x-api-key api-key}
                   :query-params {:url url}})
          base-url (get-base-url url)
          body (try+
                 (assoc (:body resp) :content
                   (-> resp
                     :body
                     :content
                     hick/parse hick/as-hickory
                     (absolutify-links-in-hick base-url)
                     hick-r/hickory-to-html))
                 (catch Object _
                   (log/warn &throw-context "Mercury post processing failed. Using vanilla")
                   (:body resp)))]
      body)
    ;; Mercury sends 502 when it's unable to parse the url
    (catch [:status 502] {:keys [headers body status]}
      (log/errorf "Mercury Error (%s): %s %s" status headers body)
      (throw+ {:type ::not-parsable}))

    (catch Object _
      (log/error (:throwable &throw-context) "Unexpected error. URL: " url)
      (throw+))))

(extend-protocol fetch/FetchSource
  infowarss.src.MercuryWebParser
  (fetch-source [src]
    (let [mercu (mercury-get (:url src) (:api-key src))
          pub-ts (or (tc/from-string (:date_published mercu)) (time/now))
          title (cond
                  (string? (:title mercu)) (:title mercu)
                  (vector? (:title mercu)) (first :title mercu)
                  :else "")]


      [(->MercuryItem
        (fetch/make-meta src)
        {:ts pub-ts :title title}
        (fetch/make-item-hash (:content mercu))
        {:url (io/as-url (:url mercu))
         :lead-image-url (io/as-url (:lead_image_url mercu))
         :next-page-url (io/as-url (:next_page_url mercu))
         :pub-ts pub-ts
         :title title
         :authors [(or (:author mercu) (:domain mercu))]
         :descriptions {"text/plain" (:excerpt mercu)}
         :contents {"text/html" (:content mercu)
                    "text/plain" (conv/html2text (:content mercu))}})])))
