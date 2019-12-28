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


(def +mercury-site-blacklist+
  #"www\.washingtonpost\.com|semiaccurate\.com|gitlab\.com|youtube|vimeo|reddit|redd\.it|open\.spotify\.com|news\.ycombinator\.com|www\.amazon\.com")

(defn replace-contents-with-mercury [creds item keep-orig?]
  (let [url (get-in item [:entry :url])
        src (src/mercury (str url))
        mercu (process-feedless-item src (first (fetch/fetch-source src)))
        html (if keep-orig?
               (str "<div class=\"orig-content\">" (get-in item [:entry :contents "text/html"]) "</div>"
                   "<div class=\"mercury\">" (get-in mercu [:entry :contents "text/html"]) "</div>")
               (get-in mercu [:entry :contents "text/html"]))
        text (if keep-orig?
               (str (get-in item [:entry :contents "text/plain"])
                 "\n"
                 (get-in mercu [:entry :contents "text/plain"]))
               (get-in mercu [:entry :contents "text/plain"]))]
    (-> item
      (assoc-in [:entry :nlp] (get-in mercu [:entry :nlp]))
      (assoc-in [:entry :contents "text/plain"] text)
      (assoc-in [:entry :lead-image-url] (get-in mercu [:entry :lead-image-url]))
      (assoc-in [:entry :contents "text/html"] html))))


(defn mercury-contents
  [creds & {:keys [keep-orig?]
            :or {keep-orig? false}}]
  (fn [item]
    (let [site (some-> item :entry :url .getHost)
          path (some-> item :entry :url .getPath)]
      (cond
        ;; images
        (or (re-find #"i\.imgur\.com|i\.redd\.it|twimg\.com" site)
          (re-find #"\.(jpg|jpeg|gif|png)$" path))
        (update-in item [:entry :contents "text/html"]
          str "<img src=\"" (get-in item [:entry :url]) "\"/>")

        ;; blacklisted sites
        (re-find +mercury-site-blacklist+ site)
        item

        ;; rest: replace with mercury
        :else
        (try+
          (replace-contents-with-mercury creds item keep-orig?)
          (catch [:type :infowarss.fetch.mercury/not-parsable] _
            (log/errorf (str item) "Mercury Error. Not replacing content with mercury")
            item))))))

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


;;; Postprocessing utilities

(defn- wrap-proc-fn [item func]
  (fn [& args]
    (try+
      (let [new (apply func args)]
        (log/tracef "proc %s: (%s %s)" (str item) func (count args))
        new)
      (catch Object e
        (log/warnf (:throwable &throw-context) "proc %s: (%s %s) FAILED: %s" (str item) func (count args) e)
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
        all-proc #(all-items-process % src state)
        proto-feed-proc (wrap-proc-fn item
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
  (let [unique-hashes (hash-set (map :hash items))]
  (when (< (count unique-hashes) (count items))
    (throw+ {:type ::pre-proc-into-multipe-made-duplicates :unique-hashes unique-hashes :item-count (count items)}))))

(defn process-item
  "Postprocess and filter a single item"
  [feed state item]
  (let [{:keys [src]} feed
        all-proc #(all-items-process % src state)
        per-feed-proc-pre (apply comp
                            (->> feed
                              :proc :pre
                              (map #(wrap-proc-fn item %))))
        per-feed-proc-post (apply comp
                             (->> feed
                               :proc :post
                               (map #(wrap-proc-fn item %))))
        proto-feed-proc (wrap-proc-fn item
                          #(post-process-item % src state))

        per-feed-filter (-> feed :proc :filter)


        pre-chain (fn [item] (some-> item
                           (all-proc)
                           (check-intermediate :all-proc)
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
                            (check-intermediate :per-feed-post-processor)))]

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
    ;; (let [processed (some-> item
    ;;                   (all-proc)
    ;;                   (check-intermediate :all-proc)
    ;;                   (per-feed-proc-pre)
    ;;                   (check-intermediate-maybe-coll :per-feed-pre-processor)
    ;;                   (apply-filter
    ;;                     #(filter-item % src state))
    ;;                   (check-intermediate :protocol-filter)
    ;;                   (proto-feed-proc)
    ;;                   (check-intermediate :protocol-processor)
    ;;                   (apply-filter per-feed-filter)
    ;;                   (check-intermediate :per-feed-filter)
    ;;                   (per-feed-proc-post)
    ;;                   (check-intermediate-maybe-coll :per-feed-post-processor))]

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
        (throw+ {:type ::postproc-failed :itemsc (count items) :feed feed })))))
