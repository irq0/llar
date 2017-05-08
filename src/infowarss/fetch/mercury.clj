(ns infowarss.fetch.mercury
  (:require
   [infowarss.converter :as conv]
   [infowarss.src :as src]
   [infowarss.fetch :as fetch]
   [infowarss.schema :as schema]
   [infowarss.persistency :as persistency]
   [infowarss.postproc :as postproc]
   [infowarss.analysis :as analysis]
   [digest]
   [clj-http.client :as http]
   [hickory.core :as hick]
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
    (let [nlp (analysis/analyze-entry (:entry item))]
      (-> item
        (update :entry merge (:entry item) nlp))))

  (filter-item [item src state] false))

(extend-protocol persistency/CouchItem
  MercuryItem
  (to-couch [item]
    (let [atts (persistency/to-couch-atts "content" (get-in item [:entry :contents]))]
      (cond->
          (-> item
            (dissoc :raw)
            (assoc-in [:meta :source :args] nil)
            (assoc :type :mercury)
            (assoc-in [:entry :contents] nil))
        (seq atts) (assoc "_attachments" atts)))))


(defn mercury-get [url api-key]
  (try+
    (let [resp  (http/get "https://mercury.postlight.com/parser"
                  {:accept :json
                   :as :json
                   :content-type :json
                   :headers {:x-api-key api-key}
                   :query-params {:url url}})]
      (:body resp))
    (catch (not= 200 (get % :status))
        {:keys [headers body status]}
      (log/errorf "Client error probably due to broken request (%s): %s %s"
        status headers body)
      (throw+ {:type ::request-error}))
    (catch Object _
      (log/error "Unexpected error: " (:throwable &throw-context))
      (throw+ {:type ::unexpected-error}))))

(extend-protocol fetch/FetchSource
  infowarss.src.MercuryWebParser
  (fetch-source [src]
    (let [mercu (mercury-get (:url src) (:api-key src))
          pub-ts (tc/from-string (:date_published mercu))]
      (->MercuryItem
        (fetch/make-meta src)
        {:ts pub-ts :title (:title mercu)}
        (fetch/make-item-hash (:content mercu))
        {:url (io/as-url (:url mercu))
         :lead-image-url (io/as-url (:lead_image_url mercu))
         :next-page-url (io/as-url (:next_page_url mercu))
         :pub-ts pub-ts
         :title (:title mercu)
         :authors [(or (:author mercu) (:domain mercu))]
         :description {"text/plain" (:excerpt mercu)}
         :contents {"text/html" (:content mercu)
                    "text/plain" (conv/html2text (:content mercu))}}))))
