(ns infowarss.repl
  (:require
   [infowarss.core :as core :refer [*srcs* state config]]
   [infowarss.persistency :as persistency :refer [store-items! duplicate?]]
   [infowarss.couchdb :as couch]
   [infowarss.update :refer :all]
   [infowarss.webapp :as webapp]
   [infowarss.src :as src]
   [infowarss.fetch :as fetch]
   [infowarss.postproc :as proc]
   [infowarss.live :as live]
   [infowarss.schema :as schema]
   [infowarss.analysis :as analysis]
   [infowarss.converter :as converter]
   [clj-http.client :as http]
   [slingshot.slingshot :refer [throw+ try+]]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [clj-time.format :as tf]
   [taoensso.timbre :as log]
   [table.core :refer [table]]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [postal.core :as postal]
   [schema.core :as s]
   [cheshire.core :as json]
   [twitter.oauth :as twitter-oauth]
   [twitter.api.restful :as twitter]
   [hara.io.scheduler :as sched]
   [clojure.java.shell :as shell]
   [opennlp.nlp :as nlp]
   [clojure.core.async :refer [>!! <!!] :as async]
   [clojure.tools.namespace.repl :refer [refresh]]
   [taoensso.timbre.appenders.core :as appenders]))

;;;; Namespace to interact with infowarss from the REPL

(s/set-fn-validation! true)

(defn format-interval [period]
  (let [formatter (some-> (org.joda.time.format.PeriodFormatterBuilder.)
                    .printZeroNever
                    .appendDays
                    (.appendSuffix "d")
                    .appendHours
                    (.appendSuffix "h")
                    .appendMinutes
                    (.appendSuffix "m")
                    .printZeroAlways
                    .appendSeconds
                    (.appendSuffix "s")
                    .toFormatter)]
    (.print formatter period)))

(defn period-since-now [ts]
  (.toPeriod (time/interval ts (time/now))))

(defn since-now-str [ts]
  (some-> ts
    period-since-now
    format-interval
    (str " ago")))

(defn- human-src
  "Extract interesting informations from source data structure"
  [[k v]]
  (let [state (if (instance? clojure.lang.Atom (get @state k))
                @(get @state k) (get @state k))

        base (cond->
                 {:key k
                  :name (str (get v :src))
                  :status (:status state)}
               (instance? org.joda.time.DateTime (:last-update-ts state)) ; live feeds have a last update
               (assoc :last-update (since-now-str (:last-update-ts state)))
               (instance? org.joda.time.DateTime (:last-attempt-ts state)) ; others have last success / attempt
               (assoc :last-attempt (since-now-str (:last-attempt-ts state)))
               (instance? org.joda.time.DateTime (:last-successful-fetch-ts state))
               (assoc :last-success (since-now-str (:last-successful-fetch-ts state))))]

    (if (#{:perm-fail :temp-fail} (:status base))
      (let [exception (:last-exception state)]
        (assoc base :last-exception-msg (:message exception)))
      base)))

(defn sources
  "Return list of sources for human consumption"
  []
  (map human-src *srcs*))

(defn- human-feed-item [i]
  {:src-title (get-in i [:source :title])
   :title (get-in i [:entry :title])
   :link (get-in i [:entry :link])
   :content (get-in i [:entry :contents "text/plain"])})

(defn items-with-tag [tag & {:keys [group]}]
  (let [items (for [id (couch/doc-ids-with-tag tag)]
                (let [doc (couch/get-document-with-attachments id)]
                  (human-feed-item doc)))]
    (if group
      (group-by :src-title items)
      items)))

(defn get-feed [key]
  (get-in *srcs* [key]))

(defn get-src [key]
  (get-in *srcs* [key :src]))


;;; Preview - Try out filters, processing, fetch

(defn preview
  [src & {:keys [limit post filter skip-postproc]
          :or [limit nil
               post []
               filter (constantly false)
               skip-postproc false]}]
  (try+
    (let [fetched (fetch/fetch-source src)
          postproc (proc/make
                     :post post
                     :filter filter)
          processed (cond->> fetched
                      limit (take limit)
                      (not skip-postproc) (proc/process {:src src
                                                         :proc postproc} {}))]

      (log/infof "Preview of %s: fetched: %d, limit: %d, after processing: %d"
        (str src) (count fetched) limit (count processed))
      processed)

    (catch Object _
      (log/errorf "Error fetching %s: %s" (str src)  (:message &throw-context))
      (:message &throw-context))))

;;; Update Scheduling

(defn make-sched-from-feeds [feeds]
  (into {}
    (for [[k feed] feeds
          :when (and (seq (get feed :cron))
                  (seq (get feed :src)))]
      {k {:handler (fn [t] (log/debugf "Cron start on %s: Update %s"
                             t k)
                     (update! k))
          :schedule (get feed :cron)}})))

(defn make-feed-sched
  []
  (sched/scheduler
    (make-sched-from-feeds *srcs*)
    {}
    {:clock {:type "org.joda.time.DateTime"
             :timezone "Europe/Berlin"
             :interval 2}}))

(defn start []
  (let [scheduler (sched/start! (make-feed-sched))]
    (merge
      (webapp/start)
      {:scheduler {:feed scheduler}})))


(defn stop [app]
  (let [scheduler (get-in app [:scheduler :feeds])]
    (when-not (nil? scheduler)
      (sched/stop! scheduler))
    (webapp/stop app)))

;;; Toy around area

(comment
  (defonce jetty (run-jetty #'webapp/fever-app {:port 8765 :join? false}))
  (defonce jetty (run-jetty #'feedbin-app {:port 8765 :join? false}))
  (.start jetty)
  (.stop jetty))

;; twitter spam filter training data

(def twitter-spam-accounts
  ["butler746_grace"
   "freecams_live"
   "FrontPageCelebs"
   "dfayvazovskaya"])

(def twitter-ham-accounts
  ["c3roc_" "ChaosLady90" "PicardEbooks" "c3soc" "grauhut" "paulg"
   "chaosupdates" "sama" "Ceph" "usenix"])

(defonce twitter-spam (atom #{}))
(defonce twitter-ham (atom #{}))

(defn my-timeline []
  (->>
    (twitter/statuses-home-timeline
      :oauth-creds (:oauth-creds (get-src :twit-augsburg-pics)))
    :body
    (map fetch/tweet-to-entry)
    (map #(merge % (analysis/analyze-entry %)))))

(defn my-following-list []
  (->>
    (twitter/friends-list
      :oauth-creds (:oauth-creds (get-src :twit-augsburg-pics))
      :params {:count 200})
    :body :users (map :screen_name)))

(defn timeline-of [user]
  (->>
    (twitter/statuses-user-timeline
      :oauth-creds (:oauth-creds (get-src :twit-augsburg-pics))
      :params {:count 200
               :screen_name user})
    :body
    (map fetch/tweet-to-entry)
    (map #(merge % (analysis/analyze-entry %)))))

(def lang-to-n
  {:en 0
   :de 1})


(defn to-training-list [x & flag]
  (let [data [(count (get-in x [:contents "text/plain"]))
              (count (get-in x [:entities :hashtags]))
              (count (get-in x [:entities :mentions]))
              (count (get-in x [:entities :photos]))
              (get-in x [:score :retweets])
              (get-in x [:score :favs])
              (or (get lang-to-n (get-in x [:language])) -1)
              (count (get-in x [:nlp :names]))
              (count (get-in x [:nlp :nouns]))
              (count (get-in x [:nlp :verbs]))]]
    (if-not (nil? flag)
      (conj data flag)
      data)))

(defonce training-data (atom []))


(defn browse-url [url]
  (shell/sh "chromium-browser" url))

(defn browse-with-tag [tag]
  (->> (couch/doc-ids-with-tag :saved)
    (map couch/get-document)
    (map #(get-in % [:entry :url]))
    (map browse-url)))




;; populate lists
(comment
  (do (swap! training-data concat (map #(to-training-list % :ham) @twitter-ham)) (swap! training-data concat (map #(to-training-list % :spam) @twitter-spam)) :ok)


  (def fit (-> (bayes/make-naive-bayes) (bayes/naive-bayes-fit (random-sample 0.8 @training-data))))

(def fit (-> (bayes/make-naive-bayes) (bayes/naive-bayes-fit (random-sample 0.8 (concat (map #(to-training-list % :ham) @twitter-ham) (map #(to-training-list % :spam) @twitter-spam))))))


  (swap! twitter-ham concat (my-timeline))
  (doseq [x (my-following-list)] (swap! twitter-ham concat (timeline-of x)))
  (doseq [x twitter-spam-accounts] (swap! twitter-spam concat (timeline-of x))))
