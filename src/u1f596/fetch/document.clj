(ns u1f596.fetch.document
  (:require
   [u1f596.fetch :as fetch]
   [u1f596.schema :as schema]
   [u1f596.persistency :as persistency]
   [u1f596.postproc :as postproc]
   [u1f596.analysis :as analysis]
   [u1f596.converter :as converter]
   [digest]
   [clj-http.client :as http]
   [hiccup.core :refer [html]]
   [taoensso.timbre :as log]
   [clojure.string :as string]
   [org.bovinegenius [exploding-fish :as uri]]
   [clojure.java.io :as io]
   [schema.core :as s]
   [pantomime.web :refer [mime-type-of]]
   [pantomime.extract :as extract]
   [java-time :as time]))

;;;; Document Fetcher
;;;; A Document is something like a pdf or docx. It needs some transformation
;;;; to be machine readable.

(s/defrecord DocumentItem
             [meta :- schema/Metadata
              summary :- schema/Summary
              hash :- schema/Hash
              entry :- schema/MercuryEntry]
  Object
  (toString [item] (fetch/item-to-string item)))

(defn description-html [i]
  (html
   [:h1 (get-in i [:summary :title])]
   [:div {:class "summary"}
    [:ul
     [:li [:span {:class "key"} "URL: "]
      [:a {:href (get-in i [:entry :url])} (get-in i [:entry :url])]]
     [:li [:span {:class "key"} "Authors: "] (string/join ", " (get-in i [:entry :authors]))]
     [:li [:span {:class "key"} "Published: "] (time/format (get-in i [:summary :ts]))]
     [:li [:span {:class "key"} "Mime Type (orig): "] (get-in i [:entry :orig-mime-type])]
     [:li [:span {:class "key"} "Mime Types Stored: "]
      (string/join ", " (keys (get-in i [:entry :contents])))]
     [:li [:span {:class "key"} "Pages: "] (get-in i [:entry :npages])]
     [:li [:span {:class "key"} "Size (orig): "] (get-in i [:entry :orig-size])]]]
   [:div {:class "nlp"}
    [:h2 "Names / Places"]
    [:p (map (fn [name] [:span [:a {:href (str "https://www.startpage.com/do/search?query=" name)} (str " " name " ")] "&nbsp;"]) (get-in i [:entry :nlp :names]))]]
   (when-let [thumb (get-in i [:entry :thumbs "image/png"])]
     [:div {:class "preview"}
      [:h2 "Preview"]
      [:img {:src (converter/data-uri thumb :mime-type "image/png")}]])))

(extend-protocol postproc/ItemProcessor
  DocumentItem
  (post-process-item [item src state]
    (let [nlp (analysis/analyze-entry (:entry item))
          with-nlp (update item :entry merge (:entry item) nlp)
          descr (description-html with-nlp)]
      (-> with-nlp
          (assoc-in [:entry :descriptions] {"text/plain" (converter/html2text descr)
                                            "text/html" descr})
          (assoc-in [:meta :view-hints :html] [:entry :descriptions "text/html"]))))

  (filter-item [item src state] false))

(extend-protocol persistency/CouchItem
  DocumentItem
  (to-couch [item]
    (-> item
        (dissoc :raw)
        (dissoc :body)
        (assoc :type :document))))

(extend-protocol fetch/FetchSource
  u1f596.src.Document
  (fetch-source [src]
    (let [url (-> src :url str)
          resp (http/get url {:as :byte-array
                              :headers {:user-agent "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"}})
          data (:body resp)
          mime-type (mime-type-of data (:headers resp))]

      (log/debug src "Retrieved with mime: " mime-type)

      (let [html (converter/convert-to-html data :mime-type mime-type)
            thumb (converter/thumbnail data)
            meta (extract/parse data)
            ts (or (some->> (:creation-date meta) (time/zoned-date-time (time/formatter :iso-zoned-date-time)))
                   (time/zoned-date-time))
            title (or (first (:title meta)) (some-> (:url src) uri/uri uri/path io/as-file .getName))]

        [(->DocumentItem
          (fetch/make-meta src)
          {:ts ts :title title}
          (fetch/make-item-hash (str (:url src)) (:text meta))
          {:url (uri/uri (:url src))
           :title title
           :authors  [(:author meta)]
           :pub-ts (some->> (:creation-date meta) (time/zoned-date-time (time/formatter :iso-zoned-date-time)))  
           :npages (first (:xmptpg/npages meta))
           :orig-mime-type mime-type
           :orig-size (count data)
           :thumbs thumb
           :descriptions nil
           :contents {"text/html" html
                      "text/plain" (:text meta)
                      mime-type (bytes data)}})]))))