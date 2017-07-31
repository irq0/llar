(ns infowarss.repl
  (:require
   [infowarss.core :as core :refer [*srcs* config]]
   [infowarss.persistency :as persistency :refer [store-items! duplicate?]]
   [infowarss.couchdb :as couch]
   [infowarss.update :refer :all :as update]
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
   [infowarss.fetch.twitter]
   [postal.core :as postal]
   [schema.core :as s]
   [cheshire.core :as json]
   [twitter.oauth :as twitter-oauth]
   [twitter.api.restful :as twitter]
   [clojure.xml :as xml]
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

(defn- get-state [k]
  (if (instance? clojure.lang.Atom (get @update/state k))
    @(get @update/state k) (get @update/state k)))

(defn- human-src
  "Extract interesting informations from source data structure"
  [[k v]]
  (let [state (get-state k)
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
  [& {:keys [by-state]}]
  (let [srcs (cond->> *srcs*
               (keyword? by-state) (filter (fn [[k v]] (= (:status (get-state k)) by-state))))]
    (map human-src srcs)))

(defn failed-sources
  "Return list of sources for human consumption"
  []
  (concat
    (sources :by-state :temp-fail)
    (sources :by-state :perm-fail)
    (sources :by-state :failed)))

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


(defn get-feeds
  []
  (let [dbfs (couch/get-feeds)
        confs *srcs*]
    (into {}
      (for [dbf dbfs]
        [(get dbf :source-key)
         (merge dbf (get confs (get dbf :source-key)))]))))




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

      (log/infof "Preview of %s: fetched: %s, limit: %s, after processing: %s"
        (str src) (count fetched) limit (count processed))
      processed)

    (catch Object e
      (log/error e "Error fetching " (str src))
      (:message &throw-context))))


(comment
  (->> yt first :raw :foreign-markup (some #(when (= (.getName %) "group") %))))

(defn youtube-media [dom]
  (->> dom
    .getChildren
    (map (fn [x] [(keyword (.getName x))
                  (or (.getAttributeValue x "url") (.getText x))]))))

;;.getChildren (map (fn [x] [(keyword (.getName x)) (or (.getAttributeValue x "url") (.getText x))]))))


;;; Update Scheduling


;;; Toy around area


;; TODO mydealz


(defn mydealz-extract-extra [item]
  (let [raw (:raw item)
        merchant (->> raw
                   :foreign-markup
                   (some #(when (= (.getName %) "merchant") %)))]
  (-> item
    (assoc-in [:entry :merchant] (some-> merchant (.getAttribute "name") .getValue))
    (assoc-in [:entry :price] (some-> merchant (.getAttribute "price") .getValue)))))




(comment
  (def foo (preview (src/feed "https://www.mydealz.de/rss/alle") :limit 1))
;;  (-> foo first :raw :foreign-markup first .getName) = merchant:

  (-> foo first :raw :foreign-markup first (.getAttribute "name") .getValue)
  (-> foo first :raw :foreign-markup first (.getAttribute "price") .getValue)

  )
;;  filter by categories, price, merchant
;; amazon



;; WIP: youtube dl music as mp3


;;; move to couch someday

(defn update-tags
  [id f]
  (couch/swap-document! id
    (fn [doc] (update-in doc [:meta :tags] f))))

(defn remove-tag
  [id tag]
  (update-tags id (fn [m] (-> m set (disj tag)))))

(defn set-tag
  [id tag]
  (update-tags id (fn [m] (-> m (conj tag) set))))


(defn youtube-dl-music [url]
  (let [{:keys [exit out err]} (shell/sh
                                 "sudo" "ip" "netns" "exec" "privacy" "sudo" "-u" "seri"
                                 "youtube-dl"
                                 "--no-progress"
                                 "-f" "bestaudio"
                                 "--extract-audio"
                                 "--ignore-errors"
                                 "--audio-format" "mp3"
                                 "--audio-quality" "320"
                                 url
                                 :dir "/tank/media/Music/Miner")
        youtube-err (second (re-find #"ERROR: (.+)" err))]
    (log/trace "Youtube-dl OUT: " out)
    (log/trace "Youtube-dl ERR: " err)
    (cond (zero? exit)
          (let [[_ out-filename] (re-find #"Destination:\s(.+\.mp3)" out)]
            out-filename)
          (some? youtube-err)
          (throw+ {:type :youtube-error :msg youtube-err})
          :else
          (throw+ {:type :error :stderr err :stdout out :exit exit}))))

(defn youtube-dl-video [url]
  (let [{:keys [exit out err]} (shell/sh
                                 "sudo" "ip" "netns" "exec" "privacy" "sudo" "-u" "seri"
                             "youtube-dl"
                             "--format" "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best"
                             "--recode-video" "mp4"
                             "--embed-subs"
                             "--embed-thumbnail"
                             url
                             :dir "/tank/scratch/youtube-dl")
        youtube-err (second (re-find #"YouTube said: (.+)$ err"))]
    (log/trace "Youtube-dl OUT: " out)
    (log/trace "Youtube-dl ERR: " err)
    (if (zero? exit)
      (let [[_ out-filename] (re-find #"Destination:\s(.+\.mp4)" out)]
        out-filename)
      (log/error "Youtube-dl error: " youtube-err))))



(defn music-links []
  (let [music-feeds (filter #(contains? (:tags (val %)) :music) (get-feeds))]
    (apply concat
      (for [[k feed] music-feeds]
        (let [ids (couch/query-ids {:meta {:source-key (:source-key feed)
                                           :tags {"$not" {"$elemMatch" {"$eq" "music-mined"}}}}
                                    :entry {:url {"$regex" (str #"^(https?\:\/\/)?(www\.)?(youtube\.com|youtu\.?be)\/.+$")}}})]
          (for [id ids]
            [id (get-in (couch/get-document id) [:entry :url])]))))))

(defn music-miner []
  (doseq [[id url] (music-links)]
    (try+
      (let [filename (youtube-dl-music url)]
        (log/info "Music Miner downloaded: " filename)
        (log/debug "Music mined" id)
        (set-tag id "music-mined")
        (remove-tag id "unread"))
      (catch [:type :youtube-error] {:keys [msg]}
        (set-tag id "music-mined")
        (set-tag id "music-miner-failed")
        (remove-tag id "unread")
        (log/errorf "Music miner failed for \"%s\": %s" url msg))
      (catch Object e
        (log/error e "Unexpected error")))))



(defn podcast-item [filename]
  (let [file (io/as-file filename)
        size (.length file)
        meta (pantomime.extract/parse file)
        mime-type (pantomime.mime/mime-type-of file)]
    (string/join "\n"
      ["<item>"
       (format "<title><![CDATA[%s]]</title>" (first (:title meta)))
       "<link></link"
       (format "<description><![CDATA[%s]]</description>" (:text meta))
       (format "<enclosure url=\"http://10.23.1.23:7654%s\" length=\"%s\" type=\"%s\" />"
         (str "/files/videos/" (.getName file)) size mime-type)
       "</item>"])))

(defn podcast-feed [directory]
  (let [path (io/as-file directory)
        files (seq (.list path (reify java.io.FilenameFilter
                                 (^boolean accept [_ ^java.io.File dir ^String name]
                                  (some? (re-find #".+\.mp4" name))))))]
    (string/join "\n"
      (concat
        ["<?xml version=\"1.0\" encoding=\"UTF-8\"?><rss version=\"2.0\""
         "xmlns:itunes=\"http://www.itunes.com/dtds/podcast-1.0.dtd\""
         "<channel>"
         "<title>infowarss podcast feed</title>"
         "<description>All the extracted videos!!1111!1!elf</description>"
         "<generator>infowarss</generator>"]
        (map #(podcast-item (str directory "/" %)) files)))))



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
    (map infowarss.fetch.twitter/tweet-to-entry)
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
    (map infowarss.fetch.twitter/tweet-to-entry)
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
