(ns infowarss.fetch.goose
  (:require
   [infowarss.persistency :as persistency]
   [infowarss.fetch :as fetch]
   [infowarss.analysis :as analysis]
   [infowarss.schema :as schema]
   [infowarss.postproc :as postproc]
   [hiccup.core :refer [html]]
   [clj-time.coerce :as tc]
   [clojure.string :as string]
   [clj-time.core :as time]
   [clojure.java.io :as io]
   [schema.core :as s]
   [clj-http.client :as http])
  (:import [com.gravity.goose Goose Configuration]))


(defn goose-html-content [a]
  (html
    [:h1 (.title a)]
    [:div {:class "summary"}
     [:h2 "Summary"]
     [:ul
      [:li [:span {:class "key"} "URL: "] [:a {:href (.canonicalLink a)}
                                           (.canonicalLink a)]]
      [:li [:span {:class "key"} "Published: "] (tc/to-string (.publishDate a))]]
     [:p (.metaDescription a)]]

    [:div {:class "article"}
     [:img {:src (.imageSrc (.topImage a))}]
     (for [p (re-seq #"(.*)\n\n" (.cleanedArticleText a))]
       [:p p])]))

(defn article-to-map [a]
  (let [img (some-> (.topImage a) .getImageSrc)]
    {:url (some-> (.canonicalLink a) io/as-url)
     :contents {"text/html" (goose-html-content a)
                "text/plain" (.cleanedArticleText a)}
     :title (.title a)
     :pub-ts (some-> (.publishDate a) tc/from-date)
     :entities {:movies []}
     :lead-image-url (when-not (string/blank? img) (io/as-url img))
     :descriptions {"text/plain" (.metaDescription a)}
     :authors [(.domain a)]}))

(def goose-default-config
  (doto (Configuration.)
    (.setImagemagickIdentifyPath "/usr/bin/identify")
    (.setImagemagickConvertPath "/usr/bin/convert")
    (.setBrowserUserAgent "Mozilla/5.0 (X11; CrOS x86_64 7262.57.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.98 Safari/537.36")))

(defn get-goose-extraction
  ([url html]
   (doto (Goose. goose-default-config)
     (.extractContent url html)))
  ([url]
   (-> (Goose. goose-default-config)
     (.extractContent url))))

(defn goose-extract [url]
  (article-to-map (get-goose-extraction (str url))))


(s/defrecord GooseItem
    [meta :- schema/Metadata
     summary :- schema/Summary
     hash :- schema/Hash
     entry :- schema/GooseEntry]
  Object
  (toString [item] (fetch/item-to-string item)))

(extend-protocol postproc/ItemProcessor
  GooseItem
  (post-process-item [item src state]
    (let [nlp (analysis/analyze-entry (:entry item))]
      (-> item
        (update :entry merge (:entry item) nlp))))

  (filter-item [item src state] false))

(extend-protocol persistency/CouchItem
  GooseItem
  (to-couch [item]
    (-> item
      (dissoc :raw)
      (assoc :type :bookmark))))


(extend-protocol fetch/FetchSource
  infowarss.src.GooseArticleExtractor
  (fetch-source [src]
    (let [entry (goose-extract (:url src))]
      [(->GooseItem
         (fetch/make-meta src)
         {:ts (or (:pub-ts entry) (time/now)) :title (:title entry)}
         (fetch/make-item-hash (:url entry) (get-in entry [:contents "text/plain"]))
         entry)])))
