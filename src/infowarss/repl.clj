(ns infowarss.repl
  (:require
   [infowarss.core :as core :refer [*srcs* config]]
   [infowarss.persistency :as persistency :refer [store-items!]]
   [infowarss.update :refer :all :as update]
   [infowarss.src :as src]
   [infowarss.db :as db]
   [infowarss.fetch :as fetch]
   [infowarss.postproc :as proc]
   [infowarss.live :as live]
   [infowarss.schema :as schema]
   [infowarss.analysis :as analysis]
   [infowarss.converter :as converter]
   [clojure.java.jdbc :as j]
   [clj-http.client :as http]
   [slingshot.slingshot :refer [throw+ try+]]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [clj-time.format :as tf]
   [taoensso.timbre :as log]
   [table.core :refer [table]]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [infowarss.fetch.twitter]
   [postal.core :as postal]
   [schema.core :as s]
   [cheshire.core :as json]
   [twitter.oauth :as twitter-oauth]
   [twitter.api.restful :as twitter]
   [clojure.xml :as xml]
   [clojure.java.shell :as shell]
   [opennlp.nlp :as nlp]
   [hickory.core :as hick]
   [hickory.select :as S]
   [hickory.render :as hick-r]
   [hickory.zip :as hick-z]
   [me.raynes.clhue.lights :as lights]
   [mount.core :refer [defstate]]
   [clojure.tools.nrepl.server :refer [start-server stop-server]]
   [cider.nrepl :refer (cider-nrepl-handler)]
   [clojure.core.async :refer [>!! <!!] :as async]
   [clojure.tools.namespace.repl :refer [refresh]]
   [taoensso.timbre.appenders.core :as appenders]))

;;;; Namespace to interact with infowarss from the REPL

(s/set-fn-validation! true)

(defstate nrepl-server
  :start (start-server :port 42000 :handler cider-nrepl-handler)
  :stop (stop-server nrepl-server))

(def +current-fetch-preview+ (atom nil))

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

(def hue-config (:hue core/creds))

;;; Preview - Try out filters, processing, fetch

(defn preview
  [src & {:keys [limit post filter skip-postproc]
          :or {limit nil
               post []
               filter (constantly false)
               skip-postproc false}}]
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
      (reset! +current-fetch-preview+ processed)
      {:info "Open http://localhost:8023/preview"
       :n (count processed)
       :limited-preview (map #(select-keys % [:summary :feed :meta :hash]) processed)})

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


(comment
(defn readability-scores [s]
  (let [{:keys [exit out err]}
        (shell/sh "style" :in s :env {"LANG" "c"})]
    {:flesch-index (when-let [[_ x] (re-find #"Flesch Index: ([\d\.-]+)/" out)]
                     (Float/parseFloat x))
     :smog-grade (when-let [[_ x] (re-find #"SMOG-Grading: ([\d\.-]+)" out)]
                   (Float/parseFloat x))}))


(j/with-db-transaction [t db/*db*]
  (let [xs (pmap (fn [x]
                (assoc x :readability
                  (readability-scores (:text x))))
           (j/query t "select items.id, item_data.text from items inner join item_data on items.id = item_data.item_id where item_data.type = 'content' and item_data.mime_type = 'text/plain'"))]
  (doseq [{:keys [id readability]} (take 1 xs)]
    (log/info id)
    (j/execute! t ["update items set entry = entry || ? where id = ?" {:readability readability} id]))))
)



;; TODO mydealz

(defn call-method
  [obj method & args]
  (-> (.getClass obj)
    (.getDeclaredMethod (str (:name method))
      (into-array Class (map resolve (:parameter-types method))))
    (doto (.setAccessible true))
    (.invoke obj (into-array Object args))))

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
  (let [sources (-> (db/get-sources)
                  (db/sources-merge-in-config)
                  vals)
        music-srcs (->> sources
                     (filter #(contains? (:tags %) :music))
                     (map :key))]
    (let [items (db/get-items-recent
                  {:with-source-keys music-srcs
                   :limit 512
                   :with-tag :unread})]
      (->> items
        (map (juxt :id :title :url))
        (filter (fn [[_ _ url]]
                  (re-find #"^(https?\:\/\/)?(www\.)?(youtube\.com|youtu\.?be)\/.+$" url)))))))

(defn music-miner []
  (doseq [[id title url] (music-links)]
    (try+
      (log/info "Music Mining: " title url)
      (let [filename (youtube-dl-music url)]
        (log/infof "Music Miner downloaded %s/%s to %s"
          title url filename)
        (db/item-set-tags id :music-mined)
        (db/item-remove-tags id :unread))
      (catch [:type :youtube-error] {:keys [msg]}
        (db/item-set-tags id :music-mined :music-miner-failed)
        (db/item-remove-tags id :unread)
        (log/errorf "Music miner failed for %s/%s: %s" title url msg))
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

;; twitter spam filter training data

;; Twitter timeline - spam filter toying around
(comment
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

  (defonce training-data (atom [])))


(defn browse-url [url]
  (shell/sh "chromium-browser" url))

;; populate lists
(comment
  (do (swap! training-data concat (map #(to-training-list % :ham) @twitter-ham)) (swap! training-data concat (map #(to-training-list % :spam) @twitter-spam)) :ok)


  (def fit (-> (bayes/make-naive-bayes) (bayes/naive-bayes-fit (random-sample 0.8 @training-data))))

(def fit (-> (bayes/make-naive-bayes) (bayes/naive-bayes-fit (random-sample 0.8 (concat (map #(to-training-list % :ham) @twitter-ham) (map #(to-training-list % :spam) @twitter-spam))))))


  (swap! twitter-ham concat (my-timeline))
  (doseq [x (my-following-list)] (swap! twitter-ham concat (timeline-of x)))
  (doseq [x twitter-spam-accounts] (swap! twitter-spam concat (timeline-of x))))
