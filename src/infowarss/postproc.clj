(ns infowarss.postproc
  (:require
   [schema.core :as s]
   [taoensso.timbre :as log]
   [clojure.test :refer [function?]]
   [taoensso.timbre.appenders.core :as appenders]))


;; Schemas and data structures

(def Func
  "Function"
  (s/pred function?))

(def FuncList
  "List of functions"
  [Func])

;; Every source may have a processing record
(s/defrecord Processing
    [post :- FuncList
     filter :- Func])

(defn make
  [& {:keys [post filter] :or {post [] filter (constantly false)}}]
  (Processing. post filter))

;; The ItemProcessor protocol allows processing hooks
;; per item type
(defprotocol ItemProcessor
  (post-process-item [item] "Postprocess item")
  (filter-item [item] "Filter items"))

(extend-protocol ItemProcessor
  infowarss.fetch.FeedItem
  (post-process-item [item]
    (update-in item [:meta :tags] conj :unread))
  (filter-item [item] false)

  infowarss.fetch.HttpItem
  (post-process-item [item]
    (update-in item [:meta :tags] conj :unread))
  (filter-item [item] false))

;; postproc utility functions

(defn add-tag [tag]
  (fn [item]
    (update-in item [:meta :tags] conj tag)))

(defn copy [src dst]
  (fn [item]
    (let [src-val (get-in item src)]
      (-> item
        (assoc-in dst src-val)))))

(defn move [src dst]
  (fn [item]
    (let [src-val (get-in item src)]
      (-> item
        (assoc-in dst src-val)
        (assoc-in src nil)))))

(defn exchange [src dst]
  (fn [item]
    (let [src-val (get-in item src)
          dst-val (get-in item dst)]
      (-> item
        (assoc-in dst src-val)
        (assoc-in src dst-val)))))


;; Postprocessing
(defn- make-proc-chain [fs]
  (let [with-logging (map (fn [fun]
                            (fn [x]
                              (log/debugf "proc chain fn: (%s, %s)" fun x)
                              (fun x)))
                       fs)]
    (apply comp with-logging)))

(defn- apply-filter [item f]
  (if-not (nil? f)
    (let [out? (f item)]
      (log/debugf "filter fn: (%s, %s) -> %s"
        f item out?)
      (if out? nil item))
    item))

(defn- apply-proc [item f]
  (when-not (nil? f)
    (log/debugf "proc fn: (%s, %s)"
      f item)
    (post-process-item item)))

(defn process-item [feed item]
  (log/infof "Processing item: %s/\"%s\""
    (-> feed :src :title)
    (-> item :summary :title))

  (let [per-feed-proc (make-proc-chain (-> feed :proc :post))
        per-feed-filter (-> feed :proc :filter)

        processed (some-> item
                    (apply-proc post-process-item)
                    (apply-filter filter-item)
                    (per-feed-proc)
                    (apply-filter per-feed-filter))]
    (when (nil? processed)
      (log/infof "Filtered out: %s/\"%s\""
        (-> feed :src :title)
        (-> item :summary :title)))
    processed))

(defn process [feed items]
  (log/infof "Processing feed: %s"
    (-> feed :src :title))
  (remove nil?
    (for [item items]
      (process-item feed item))))
