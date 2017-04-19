(ns infowarss.postproc
  (:require
   [infowarss.fetch]
   [schema.core :as s]
   [taoensso.timbre :as log]
   [clojure.test :refer [function?]]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.java.shell :as shell]
   [postal.core :as postal]
   [clojure.string :as string]
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
  (post-process-item [item src] "Postprocess item")
  (filter-item [item src] "Filter items"))

(extend-protocol ItemProcessor
  infowarss.fetch.FeedItem
  (post-process-item [item src]
    (update-in item [:meta :tags] conj :unread))
  (filter-item [item src] false)

  infowarss.fetch.HttpItem
  (post-process-item [item src]
    (update-in item [:meta :tags] conj :unread))
  (filter-item [item src] false)

  infowarss.fetch.TweetItem
  (post-process-item [item src]
    (update-in item [:meta :tags] conj :unread))
  (filter-item [item src] false))

;; postproc utility functions

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

(defn dummy-mail [body]
  (string/join "\n"
    ["Received: from ks3366964.kimsufi.com (Postfix [37.187.2.217])"
     "	by ks3366964.kimsufi.com (Postfix) with ESMTPS id 18DCF8013E"
     "	for <ml@irq0.org>; Mon, 17 Apr 2017 22:12:32 +0200 (CEST)"
     "Message-Id: <4043aad0-25ae-4ded-a261-eff3e8ed063f@irq0.org>"
     "From: Marcel Lauhoff <ml@irq0.org>"
     "Delivered-To: seri@ks3366964.kimsufi.com"
     "To: test@irq0.org"
     "Content-Type: text/plain;"
     "	charset=UTF-8"
     "Content-Transfer-Encoding: 7bit"
     "Subject: Please check this tweet"
     "Date: Mon, 17 Apr 2017 20:12:23 +0000 (UTC)"
     ""
     body]))

(def sa-to-bool
  {"Yes" true
   "No" false})

(defn spamassassin
  [text]
  (let [msg (dummy-mail text)
        {:keys [exit out]} (shell/sh "spamassassin" "--local" "--test-mode" :in msg)]

    (if (zero? exit)
      (let [[_ sa-bool score] (re-find #"X-Spam-Status: (.+), score=(\d+\.\d+)" out)]
        {:status (get sa-to-bool sa-bool)
         :score (Float/parseFloat score)})
      "")))

;; Postprocessing


(defn- wrap-proc-fn [func]
  (fn [& args]
    (try+
      (let [new (apply func args)]
        (log/debugf "proc: (%s %s) -> "
          func (count args))
        (log/spy :trace "before:" args)
        (log/spy :trace "after:" new)
        new)
      (catch Object  _
        (log/errorf "proc function failed: %s"
          (:throwable &throw-context))
        (log/spy :error &throw-context)
        args))))

(defn- apply-filter [item f]
  (if-not (nil? f)
    (let [out? (boolean (f item))]
      (log/debugf "filter: (%s, %s) -> %s"
        f item out?)
      (when-not out? item))
    item))

(defn process-item [feed item]
  (let [{:keys [src]} feed
        per-feed-proc (apply comp
                        (->> feed
                          :proc :post
                          (map wrap-proc-fn)))
        proto-feed-proc (wrap-proc-fn
                          #(post-process-item % src))

        per-feed-filter (-> feed :proc :filter)]

    (comp #(log/info "foo %s" %) (partial + 3))

    (log/infof "Processing %s: %s/\"%s\""
      (type item)
      (str src)
      (-> item :summary :title))

    (let [processed (some-> item
                      (proto-feed-proc)
                      (apply-filter
                        #(filter-item % src))
                      (per-feed-proc)
                      (apply-filter per-feed-filter))]
      (when (nil? processed)
        (log/infof "Filtered out: %s/\"%s\""
          (str src)
          (-> item :summary :title)))
      processed)))

(defn process [feed items]
  (let [{:keys [src]} feed]
    (log/infof "Processing feed: %s" (str src))
    (remove nil?
      (for [item items]
        (process-item feed item)))))
