(ns llar.postproc
  (:require
   [clojure.set :refer [intersection union]]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [llar.commands :refer [download-subtitles]]
   [llar.converter :as conv]
   [slingshot.slingshot :refer [throw+ try+]]))

;;;; Postprocessing and Filtering

;;; Processing data structure

;; Every source may have a processing record
(defrecord Processing
           [post
            pre
            filter])

(defn make
  "Make new Processing record"
  [& {:keys [post pre filter]
      :or {post []
           pre []
           filter (constantly false)}}]
  (Processing. post pre filter))

;; new api
(defn new
  "Make new Processing record"
  [{:keys [post pre rm]
    :or {post []
         pre []
         rm (constantly false)}}]
  (Processing. post pre rm))

(declare process-feedless-item)

(def +fetch-subtitles-publisher+ #{"Youtube"})

(defn subtitle-fetch [url]
  (log/info "subtitle fetch" url)
  (try+
   (let [{:keys [format subtitles]} (download-subtitles url)]
     (cond
       (= format :ttml)
       (conv/ttml2text subtitles)
       :default nil))
   (catch Object e
     (log/errorf e "subtitle fetch %s failed" url)
     nil)))

;; All item processors
;; first = first in chain, last = last in chain

(defonce highlight-matches
  (atom {}))

(defn video-url?
  [url]
  (some? (re-find  #"(youtu\.be\/|youtube\.com\/(watch\?(.*&)?v=|(embed|v)\/))([^\?&\"'>]+)" (str url))))

(defn all-items-process-first [item _ state]
  (log/trace "all items processor (first)" (str item))
  (let [url (get-in item [:entry :url])
        transscript (when (video-url? url)
                      (subtitle-fetch url))
        item (-> item
                 (update-in [:meta :tags] conj :unread)
                 (assoc-in [:meta :source-key] (:key state)))]
    (cond-> item
      (and (video-url? url) (nil? (get-in item [:entry :contents "text/plain"])))
      (assoc-in [:entry :contents "text/plain"] (subtitle-fetch url))
)))

(defn highlight-item? [item]
  (let [names-and-nouns (union (get-in item [:entry :nlp :names])
                               (get-in item [:entry :nlp :nouns])
                               (->> (get-in item [:entry :nlp :top :words])
                                    (keys)
                                    (into #{})))
        author-matches (intersection (into #{} (->>
                                                (get-in item [:entry :authors])
                                                (remove nil?)
                                                (map string/lower-case)))
                                     (:authors @highlight-matches))
        words-matches (intersection names-and-nouns (:words @highlight-matches))]
    (log/debugf "item %s highlight: authors:%s words:%s" (str item) author-matches words-matches)
    (cond
      (seq words-matches)
      {:type :words
       :matches words-matches}
      (seq author-matches)
      {:type :authors
       :matches author-matches}
      :else
      false)))

(defn all-items-process-last [item _ _]
  (log/trace "all items processor (last)" (str item))
  (let [url (get-in item [:entry :url])
        highlight-info (highlight-item? item)
        item (cond-> item
               (some? highlight-info)
               (assoc-in [:entry :highlight] highlight-info)

               (some? highlight-info)
               (update-in [:meta :tags] conj :highlight))]
    item))

;;; Item postprocessing protocol

;; The ItemProcessor protocol allows processing hooks
;; per item type

(defprotocol ItemProcessor
  (post-process-item [item src state] "Postprocess item")
  (filter-item [item src state] "Filter items"))

;;; Postprocessing utilities

(defn- wrap-proc-fn [item func hint]
  (fn [& args]
    (try+
     (let [new (apply func args)]
       (log/tracef "proc %s: (%s %s)" (str item) func (count args))
       new)
     (catch Object e
       (log/warnf (:throwable &throw-context) "proc %s: (%s %s %s) FAILED: %s %s"
                  (str item) func (count args) hint e (ex-message e))
       nil))))

(defn- apply-filter [item f]
  (if-not (nil? f)
    (let [out? (boolean (f item))]
      (log/tracef "filter: (%s, %s) -> %s"
                  f item out?)
      (when-not out? item))
    item))

;;; API

(defn process-feedless-item
  "Postprocess and filter item produced without a feed"
  [src item]
  (let [state {}
        all-proc-first #(all-items-process-first % src state)
        all-proc-last #(all-items-process-last % src state)
        proto-feed-proc (wrap-proc-fn item
                                      #(post-process-item % src state)
                                      "proto-feed-proc")]
    (log/debugf "processing feedless %s"
                (str item))

    (let [processed (some-> item
                            (all-proc-first)
                            (apply-filter
                             #(filter-item % src state))
                            (proto-feed-proc)
                            (all-proc-last))]
      (when (nil? processed)
        (log/debugf "Filtered out: %s"
                    (str item)))
      processed)))

;; TODO spec validate item
(defn check-intermediate [item where]
  (if (satisfies? ItemProcessor item)
    item
    (log/errorf "processing pipeline failure after %s. intermediate result is not a processable item: type:%s value:%s"
                where (type item) item)))

(defn check-intermediate-maybe-coll [items where]
  (if (or
       (and
        (sequential? items)
        (every? (partial satisfies? ItemProcessor) items))
       (satisfies? ItemProcessor items))
    items
    (log/errorf "processing pipeline failure after %s. intermediate result garbage: %s %s"
                where (type items) items)))

(defn check-pre-multiple [items]
  (let [unique-hashes (set (map :hash items))]
    (when (< (count unique-hashes) (count items))
      (throw+ {:type ::pre-proc-into-multipe-made-duplicates :unique-hashes unique-hashes :item-count (count items)}))))

(defn process-item
  "Postprocess and filter a single item"
  [feed state item]
  (let [{:keys [src]} feed
        all-proc-first #(all-items-process-first % src state)
        all-proc-last #(all-items-process-last % src state)
        per-feed-proc-pre (apply comp
                                 (->> feed
                                      :proc :pre
                                      (map #(wrap-proc-fn item % "per-feed-proc-pre"))))
        per-feed-proc-post (apply comp
                                  (->> feed
                                       :proc :post
                                       (map #(wrap-proc-fn item % "per-feed-proc-post"))))
        proto-feed-proc (wrap-proc-fn item
                                      #(post-process-item % src state)
                                      "proto-feed-proc")

        per-feed-filter (-> feed :proc :filter)

        pre-chain (fn [item] (some-> item
                                     (all-proc-first)
                                     (check-intermediate :all-proc-first)
                                     (per-feed-proc-pre)
                                     (check-intermediate-maybe-coll :per-feed-pre-processor)))

        main-chain (fn [item] (some-> item
                                      (apply-filter
                                       #(filter-item % src state))
                                      (check-intermediate :protocol-filter)
                                      (proto-feed-proc)
                                      (check-intermediate :protocol-processor)
                                      (apply-filter per-feed-filter)
                                      (check-intermediate :per-feed-filter)
                                      (per-feed-proc-post)
                                      (check-intermediate :per-feed-post-processor)
                                      (all-proc-last)
                                      (check-intermediate :all-proc-last)))]

    (log/debugf "processing %s"
                (str item))

    (let [pre-chain-processed (pre-chain item)

          processed (if (sequential? pre-chain-processed)
                      (do
                        (log/debug "Pre chain produced " (count pre-chain-processed) " extra items")
                        (map main-chain pre-chain-processed))
                      (main-chain pre-chain-processed))]
      (when (nil? processed)
        (log/debugf "Filtered out: %s"
                    (str item)))
      processed)))

(defn process [feed state items]
  (let [{:keys [src]} feed]
    (log/debugf "processing feed: %s (%s items)" (str src) (count items))
    (try+
     (if (some? items)
       (doall
        (->>
         items
         (pmap  #(process-item feed state %))
         (remove nil?)
         (flatten)))
       (do
         (log/warn "postprocess with empty items called" (str src))
         items))
     (catch [:type :llar.http/client-error-retry-later] ex
       (throw+ {:type ::postproc-temp-fail :items-count (count items)
                :feed feed
                :error ex}))
     (catch Object _
       (log/warn (:throwable &throw-context) "postprocessing failed during parallel item proc: " (str src)
                 feed state items)
       (throw+ {:type ::postproc-fail :itemsc (count items) :feed feed})))))
