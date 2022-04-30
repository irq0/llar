(ns u1f596.postproc
  (:require
   [u1f596.schema :as schema]
   [schema.core :as s]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.set :refer [union intersection]]
   [clojure.string :as string]
   [clojure.java.shell :as shell]))

;;;; Postprocessing and Filtering

;;; Processing data structure

;; Every source may have a processing record
(s/defrecord Processing
             [post :- schema/FuncList
              pre :- schema/FuncList
              filter :- schema/Func])

(defn make
  "Make new Processing record"
  [& {:keys [post pre filter]
      :or {post []
           pre []
           filter (constantly false)}}]
  (Processing. post pre filter))


;;;; Postprocessing utility functions


(defn add-tag [tag]
  (fn [item]
    (update-in item [:meta :tags] conj tag)))

(defn add-tag-filter [tag fltr]
  (fn [item]
    (if (fltr item)
      (update-in item [:meta :tags] conj tag)
      item)))

(defn copy [src dst]
  (fn [item]
    (let [src-val (get-in item src)]
      (assoc-in item dst src-val))))

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

(declare process-feedless-item)

;; All item processors
;; first = first in chain, last = last in chain

(def +highlight-words+
  #{"quobyte" "marcel lauhoff" "ceph" "ionos" "fscrypt" "suse" "irq0"})

(def +highlight-authors+
  (into #{} (map string/lower-case
                 ["Sascha Lobo"
                  "Sibylle Berg"
                  "Scott Galloway"
                  "Kara Swisher"])))


(defn all-items-process-first [item _ state]
  (log/trace "All items processor (first)" (str item))
  (-> item
      (update-in [:meta :tags] conj :unread)
      (assoc-in [:meta :source-key] (:key state))))

(defn all-items-process-last [item _ _]
  (log/trace "All items processor (last)" (str item))
  (let [names-and-nouns (union (get-in item [:entry :nlp :names])
                               (get-in item [:entry :nlp :nouns]))

        highlight (or
                   (> (count (intersection (into #{} (->>
                                                      (get-in item [:entry :authors])
                                                      (remove nil?)
                                                      (map string/lower-case)))
                                           +highlight-authors+)) 0)
                   (> (count (intersection names-and-nouns +highlight-words+)) 0))]
    (cond-> item highlight (update-in [:meta :tags] conj :highlight))))

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
                  (str item) func (count args) hint e (.getMessage e))
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
    (log/debugf "Processing feedless %s"
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

(defn check-intermediate [item where]
  (if (satisfies? ItemProcessor item)
    item
    (log/errorf "Processing pipeline failure after %s. Intermediate result garbage: %s %s"
                where (type item) item)))

(defn check-intermediate-maybe-coll [items where]
  (if (or
       (and
        (sequential? items)
        (every? (partial satisfies? ItemProcessor) items))
       (satisfies? ItemProcessor items))
    items
    (log/errorf "Processing pipeline failure after %s. Intermediate result garbage: %s %s"
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

    (log/debugf "Processing %s"
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
    (log/debugf "Processing feed: %s (%s items)" (str src) (count items))
    (try+
     (if (not-empty items)
       (->>
        items
        (pmap  #(process-item feed state %))
        (remove nil?)
        (flatten))
       (do
         (log/warn "Postprocess with empty items called" (str src))
         items))
     (catch Object _
       (log/warn (:throwable &throw-context) "Postprocessing failed during parallel item proc: " (str src)
                 feed state items)
       (throw+ {:type ::postproc-failed :itemsc (count items) :feed feed})))))
