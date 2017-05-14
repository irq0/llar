(ns infowarss.postproc
  (:require
   [infowarss.fetch :as fetch]
   [infowarss.src :as src]
   [infowarss.schema :as schema]
   [schema.core :as s]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.java.shell :as shell]
   [postal.core :as postal]
   [clojure.string :as string]
   [clj-time.core :as time]
   [infowarss.analysis :as analysis]
   [taoensso.timbre.appenders.core :as appenders]))

;;;; Postprocessing and Filtering

;;; Processing data structure

;; Every source may have a processing record
(s/defrecord Processing
    [post :- schema/FuncList
     filter :- schema/Func])

(defn make
  "Make new Processing record"
  [& {:keys [post filter] :or {post [] filter (constantly false)}}]
  (Processing. post filter))

;;; Item postprocessing protocol

;; The ItemProcessor protocol allows processing hooks
;; per item type

(defn all-items-process [item src state]
  (log/trace "All items processor" (str item))
  (-> item
    (update-in [:meta :tags] conj :unread)
    (assoc-in [:meta :source-key] (:key state))))

(defprotocol ItemProcessor
  (post-process-item [item src state] "Postprocess item")
  (filter-item [item src state] "Filter items"))

(extend-protocol ItemProcessor
  infowarss.fetch.FeedItem
  (post-process-item [item src state]
    (let [nlp (analysis/analyze-entry (:entry item))]
      (-> item
        (update :entry merge (:entry item) nlp))))
  (filter-item [item src state]
    (let [last-fetch (get state :last-successful-fetch-ts)
          feed-pub (get-in item [:feed :pub-ts])]
      (if-not (or (nil? last-fetch) (nil? feed-pub))
        (do
          (log/tracef "Filtering out item %s: older than last fetch"
            (str item))
          (time/before? feed-pub last-fetch))
        false)))

  infowarss.fetch.HttpItem
  (post-process-item [item src state] item)
  (filter-item [item src] false)

  infowarss.fetch.TweetItem
  (post-process-item [item src state]
    (-> item
      (update :entry merge (:entry item) (analysis/analyze-entry (:entry item)))
    ))
  (filter-item [item src state] false))


;;;; Postprocessing utility functions

(defn add-tag [tag]
  (fn [item]
    (update-in item [:meta :tags] conj tag)))

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

(defn mercury-contents
  [creds & {:keys [keep-orig]
                :or [keep-orig false]}]
  (fn [item]
    (let [url (get-in item [:entry :url])
          src (src/mercury (str url) (get-in creds [:api-key]))
          mercu (process-feedless-item src (first (fetch/fetch-source src)))
          html (if keep-orig
                 (str "<div class=\"orig-content\">" (get-in item [:entry :contents "text/html"]) "</div>"
                   "<div class=\"mercury\">" (get-in mercu [:entry :contents "text/html"]) "</div>")
                 (get-in mercu [:entry :contents "text/html"]))
          text (if keep-orig
                 (str (get-in item [:entry :contents "text/plain"])
                   "\n"
                   (get-in mercu [:entry :contents "text/plain"]))
                 (get-in mercu [:entry :contents "text/plain"]))]

      (-> item
        (assoc-in [:entry :nlp] (get-in mercu [:entry :nlp]))
        (assoc-in [:entry :contents "text/plain"] text)
        (assoc-in [:entry :contents "text/html"] html)))))

(def sa-to-bool
  {"Yes" true
   "No" false})

(defn spamassassin
  [msg]
  (let [{:keys [exit out]} (shell/sh "spamassassin" "--local" "--test-mode" :in msg)]
    (if (zero? exit)
      (let [[_ sa-bool score] (re-find #"X-Spam-Status: (.+), score=(\d+\.\d+)" out)]
        {:status (get sa-to-bool sa-bool)
         :score (Float/parseFloat score)})
      "")))

;;; Postprocessing utilities

(defn- wrap-proc-fn [func]
  (fn [& args]
    (try+
      (let [new (apply func args)]
        (log/tracef "proc: (%s %s)" func (count args))
        new)
      (catch Object  e
        (log/warn e "proc function failed")
        args))))

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
        all-proc #(all-items-process % src state)
        proto-feed-proc (wrap-proc-fn
                          #(post-process-item % src state))]
    (log/debugf "Processing feedless %s"
      (str item))

    (let [processed (some-> item
                      (all-proc)
                      (apply-filter
                        #(filter-item % src state))
                      (proto-feed-proc))]
      (when (nil? processed)
        (log/debugf "Filtered out: %s"
          (str item)))
      processed)))

(defn process-item
  "Postprocess and filter a single item"
  [feed state item]
  (let [{:keys [src]} feed
        all-proc #(all-items-process % src state)
        per-feed-proc (apply comp
                        (->> feed
                          :proc :post
                          (map wrap-proc-fn)))
        proto-feed-proc (wrap-proc-fn
                          #(post-process-item % src state))

        per-feed-filter (-> feed :proc :filter)]

    (log/debugf "Processing %s"
      (str item))

    (let [processed (some-> item
                      (all-proc)
                      (apply-filter
                        #(filter-item % src state))
                      (proto-feed-proc)
                      (apply-filter per-feed-filter)
                      (per-feed-proc))]
      (when (nil? processed)
        (log/debugf "Filtered out: %s"
          (str item)))
      processed)))

(defn process [feed state items]
  (let [{:keys [src]} feed]
    (log/debugf "Processing feed: %s" (str src))
    (remove nil?
      (pmap #(process-item feed state %) items))))
