(ns infowarss.fetch.http
  (:require
   [infowarss.http :as http]
   [infowarss.fetch :refer [FetchSource item-to-string make-meta make-item-hash tag-items]]
   [infowarss.postproc :refer [ItemProcessor]]
   [infowarss.schema :as schema]
   [infowarss.converter :as conv]
   [infowarss.analysis :as analysis]
   [infowarss.persistency :refer [CouchItem]]
   [slingshot.slingshot :refer [try+]]
   [schema.core :as s]
   [taoensso.timbre :as log]
   [clj-http.client :as http-client]
   [hickory.core :as hick]
   [hickory.render :as hick-r]
   [clojurewerkz.urly.core :as urly]))

(s/defrecord GenericWebsiteItem
    [meta :- schema/Metadata
     summary :- schema/Summary
     hash :- schema/Hash
     entry :- schema/FeedEntry
     hickory :- s/Any
     feed :- schema/Feed]
  Object
  (toString [item] (item-to-string item)))

(extend-protocol FetchSource
  infowarss.src.GenericWebsite
  (fetch-source [src]
    (let [{:keys [url args]} src
          {:keys [summary body hickory]} (http/fetch
                                              url :user-agent (:user-agent args))
          feed {:title "[website]"
                :url ""
                :feed-type "generic-website"}]

      [(map->GenericWebsiteItem
        {:meta (make-meta src)
         :summary summary
         :hash (make-item-hash (:title summary) body)
         :hickory hickory
         :entry {:pub-ts (:ts summary)
                 :url url
                 :title (:title summary)
                 :authors ""
                 :descriptions {"text/plain" body}
                 :contents {"text/html" body
                            "text/plain" (conv/html2text body)}}
         :feed feed})])))

(extend-protocol FetchSource
  infowarss.src.PaywalledWebsite
  (fetch-source [src]
    (let [{:keys [url cookie-getter args]} src
          url (urly/url-like url)
          base-url (http/get-base-url url)
          cookie-jar (cookie-getter)]
      (try+
       (let [response (http-client/get (str url)
                                       {:headers {:user-agent (http/resolve-user-agent
                                                               (:user-agent args))}
                                        :cookie-store cookie-jar})
             parsed-html (-> response
                             :body
                             hick/parse hick/as-hickory
                             (http/absolutify-links-in-hick base-url)
                             (http/sanitize :remove-css? true)
                             http/blobify)

             summary {:ts (http/extract-http-timestamp response)
                      :title (http/extract-http-title parsed-html)}
             body (hick-r/hickory-to-html parsed-html)
             feed {:title "[website]"
                   :url ""
                   :feed-type "generic-website"}]
         (log/debugf "Fetched HTTP: %s -> %s bytes body" url (count (get response :body)))
         [(map->GenericWebsiteItem
           {:meta (make-meta src)
            :summary summary
            :hickory parsed-html
            :hash (make-item-hash (:title summary) body)
            :entry {:pub-ts (:ts summary)
                    :url url
                    :title (:title summary)
                    :authors ""
                    :descriptions {"text/plain" body}
                    :contents {"text/html" body
                               "text/plain" (conv/html2text body)}}
            :feed feed})])))))


(extend-protocol ItemProcessor
  GenericWebsiteItem
  (post-process-item [item src state]
    (let [nlp (analysis/analyze-entry (:entry item))
          urls (get-in nlp [:nlp :urls])
          tags (set
                (remove nil?
                        [(when (some #(re-find #"^https?://\w+\.(youtube|vimeo|youtu)" %) urls)
                           :has-video)
                         (when (some #(re-find #"https?://open\.spotify\.com/playlist" %) urls)
                           :has-spotify-playlist)
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
    (assoc item :type :website)))
