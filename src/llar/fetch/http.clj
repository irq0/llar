(ns llar.fetch.http
  (:require
   [llar.http :as http]
   [llar.fetch :refer [FetchSource item-to-string make-meta make-item-hash]]
   [llar.postproc :refer [ItemProcessor]]
   [llar.commands :refer [html2text]]
   [llar.analysis :as analysis]
   [llar.persistency :refer [CouchItem]]
   [llar.item]
   [clojure.spec.alpha :as s]
   [clojure.tools.logging :as log]
   [clj-http.client :as http-client]
   [hickory.core :as hick]
   [hickory.render :as hick-r]
   [org.bovinegenius [exploding-fish :as uri]]))

(defrecord GenericWebsiteItem
           [meta
            summary
            hash
            entry
            hickory
            feed]
  Object
  (toString [item] (item-to-string item)))

(defn make-website-item [meta summary hash entry hickory feed]
  {:pre [(s/valid? :irq0/item-metadata meta)
         (s/valid? :irq0/item-summary summary)
         (s/valid? :irq0/item-hash hash)]}
  (->GenericWebsiteItem meta summary hash entry hickory feed))

(extend-protocol FetchSource
  llar.src.GenericWebsite
  (fetch-source [src _conditional-tokens]
    (let [{:keys [url args]} src
          {:keys [summary body hickory]} (http/fetch
                                          url :user-agent (:user-agent args))
          feed {:title "[website]"
                :url ""
                :feed-type "generic-website"}]

      [(make-website-item
        (make-meta src)
        summary
        (make-item-hash (:title summary) body)
        {:pub-ts (:ts summary)
         :url url
         :title (:title summary)
         :authors [""]
         :descriptions {"text/plain" body}
         :contents {"text/html" body
                    "text/plain" (html2text body)}}
        hickory
        feed)])))

(extend-protocol FetchSource
  llar.src.PaywalledWebsite
  (fetch-source [src _conditional-tokens]
    (let [{:keys [url cookie-getter args]} src
          url (uri/uri url)
          base-url (http/get-base-url url)
          cookie-jar (cookie-getter)
          response (http-client/get (str url)
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
      [(make-website-item
        (make-meta src)
        summary
        (make-item-hash (:title summary) body)
        {:pub-ts (:ts summary)
         :url url
         :title (:title summary)
         :authors [""]
         :descriptions {"text/plain" body}
         :contents {"text/html" body
                    "text/plain" (html2text body)}}
        parsed-html
        feed)])))

(extend-protocol ItemProcessor
  GenericWebsiteItem
  (post-process-item [item _src _state]
    (let [nlp (analysis/analyze-entry (:entry item))
          urls (get-in nlp [:nlp :urls])
          tags (set
                (remove nil?
                        [(when (some #(re-find #"^https?://\w+\.(youtube|vimeo|youtu)" %) urls)
                           :has-video)
                         (when (some #(re-find #"https?://open\.spotify\.com/playlist" %) urls)
                           :has-spotify-playlist)
                         (when (and (string? (:url item)) (re-find #"^https?://\w+\.(youtube|vimeo|youtu)" urls))
                           :has-video)]))]
      (-> item
          (update-in [:meta :tags] into tags)
          (update :entry merge (:entry item) nlp))))
  (filter-item [_ _ _] false))

(extend-protocol CouchItem
  GenericWebsiteItem
  (to-couch [item]
    (assoc item :type :website)))
