(ns infowarss.fetch.http
  (:require
   [infowarss.http :as http]
   [infowarss.fetch :refer [FetchSource item-to-string make-meta make-item-hash tag-items]]
   [infowarss.postproc :refer [ItemProcessor]]
   [infowarss.schema :as schema]
   [infowarss.converter :as conv]
   [infowarss.analysis :as analysis]
   [twitter.api.restful :as twitter]
   [infowarss.persistency :refer [CouchItem]]
   [slingshot.slingshot :refer [throw+ try+]]
   [schema.core :as s]
   [taoensso.timbre :as log]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [clj-time.format :as tf]
   [clj-time.core :as time]))

(s/defrecord GenericWebsiteItem
    [meta :- schema/Metadata
     summary :- schema/Summary
     hash :- schema/Hash
     entry :- schema/FeedEntry
     feed :- schema/Feed]
  Object
  (toString [item] (item-to-string item)))

(extend-protocol FetchSource
  infowarss.src.GenericWebsite
  (fetch-source [src]
    (let [{:keys [url]} src
          {:keys [summary body raw hickory meta]} (http/fetch url)

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

;; (extend-protocol ItemProcessor
;;   ChangeDetectionWebsiteItem
;;   (post-process-item [item src state] item)
;;   (filter-item [item src state] false))


;; (extend-protocol FetchSource
;;   infowarss.src.ChangeDetectionWebsite
;;   (fetch-source [src]
;;     (let [{:keys [url]} src
;;           {:keys [summary body raw hickory meta]} (fetch url)

;;           feed {:title (:title summary)
;;                 :url url
;;                 :feed-type "generic-website"}]

;;       [(map->GenericWebsiteItem
;;         {:meta meta
;;          :summary summary
;;          :hash (make-item-hash (:title summary) body)
;;          :entry {:pub-ts (:ts summary)
;;                  :url url
;;                  :title (:title summary)
;;                  :authors ""
;;                  :descriptions {"text/plain" body}
;;                  :contents {"text/html" body
;;                             "text/plain" (conv/html2text body)}}
;;          :feed feed})])))
