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
   [nrepl.server :refer [start-server stop-server]]
   [cider.nrepl :refer [cider-nrepl-handler]]
   [clojure.core.async :refer [>!! <!!] :as async]
   [clojure.tools.namespace.repl :refer [refresh]]
   [pantomime.mime :as pm]
   [clj-ml.clusterers :as ml-clusterers]
   [clj-ml.data :as ml-data]
   [clj-ml.filters :as ml-filters]
   [taoensso.timbre.appenders.core :as appenders]))

;;;; Namespace to interact with infowarss from the REPL

(s/set-fn-validation! true)

(defn nrepl-handler []
  (require 'cider.nrepl)
  (ns-resolve 'cider.nrepl 'cider-nrepl-handler))

(defstate nrepl-server
  :start (start-server :port 42000 :handler (nrepl-handler))
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
  [src & {:keys [limit post pre filter skip-postproc]
          :or {limit nil
               post []
               pre []
               filter (constantly false)
               skip-postproc false}}]
  (try
    (let [fetched (fetch/fetch-source src)
          postproc (proc/make
                    :post post
                    :pre pre
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

    (catch Throwable th
      (log/error th "Error fetching " (str src))
      )))


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


;; clustering

;; (defn clustertest []
;;   (let [terms (into #{} (db/saved-itesmtf-idf-terms))
;;         items (db/saved-items-tf-idf)
;;         ds (ml-data/make-sparse-dataset "saved-items"
;;                                         terms
;;                                         items)]
;;     ds))

(def current-clustered-saved-items (atom {}))

(defn make-saved-dataset []
  ;; must ensure that there are no '/' in terms - messes up keyword/name
  (let [attributes (vec (conj (into #{} (db/saved-items-tf-idf-terms)) "item_id"))
        term-tf-idf-maps (db/saved-items-tf-idf)
        weka-attributes (map (fn [s]
                               (let [new-attrib (weka.core.Attribute. s)]
                                 (if (= s :item_id)
                                   (.setWeight new-attrib 0.0)
                                   (.setWeight new-attrib 1.0))
                                 new-attrib
                                                          ))
                               attributes)
        ds (weka.core.Instances. "saved-items" (java.util.ArrayList. weka-attributes)
                                          (count term-tf-idf-maps))]
        ;; ds (ml-data/make-dataset "saved-items" attributes (count term-tf-idf-maps))]
    (doseq [m term-tf-idf-maps]
      (let [inst (weka.core.SparseInstance. (count attributes))]
        (.setDataset inst ds)
        (doall (map-indexed (fn [i attrib]
                              (try
                                (.setValue inst i (or (get m attrib) 0.0))
                                (catch Throwable th
                                  (log/info th i attrib (get m attrib) (type (get m attrib)))
                                  (throw th))))
                            attributes))
        (.setWeight inst 1.0)
        (.add ds inst)))

    ds))

(comment
  (def ds2 (ml-filters/make-apply-filter :remove-attributes {:attributes [:item_id]} ds)))


(defn improvise-cluster-name [attributes centroid]
  (let [take-this (nth (sort centroid) (- (count centroid) 5) )]
    (string/join "+"
                 (take 5
               (remove nil?
                       (map (fn [att val]
                              (let [name (.name att)]
                                (when (and (>= val take-this)
                                           (not (re-find #"\W" name))
                                           (not= "item_id" name)
                                           )
                                  name)))
                            attributes
                            centroid))))))

(defn cluster-saved []
  (let [ds (make-saved-dataset)
        clst (ml-clusterers/make-clusterer :k-means {:number-clusters (int (/ (ml-data/dataset-count ds) 5))})]
    (ml-clusterers/clusterer-build clst ds)
    (let [ds-clst (ml-clusterers/clusterer-cluster clst ds)
          centroids (:centroids (ml-clusterers/clusterer-info clst))
          names (into {}
                      (map (fn [[k cent]]
                             [(keyword (str k))
                              (if-let [human (improvise-cluster-name
                                              (ml-data/dataset-attributes ds)
                                              (-> cent
                                                  ml-data/instance-to-vector))]
                                human
                                (keyword (str k))
                               )])
                           centroids))]
      (log/info names)
      (->> ds-clst
           ml-data/dataset-as-maps
           (map (fn [{:keys [item_id class]}]
                  (let [id (int item_id)
                        title (->
                               (db/get-items-by-id [id])
                               first
                               :title)
                        class-name (get names class)]
                    {:title title
                     :class class-name
                     :id id})))
           (group-by :class)))))

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

(defn youtube-dl-video [url dest-dir]
  (let [{:keys [exit out err]} (shell/sh
                                 ;; "sudo" "ip" "netns" "exec" "privacy" "sudo" "-u" "seri"
                             "/home/seri/.local/bin/youtube-dl"
                             "--format" "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best"
                             "--recode-video" "mp4"
                             "--embed-subs"
                             "--embed-thumbnail"
                             "--no-call-home"
                             "--no-progress"
                             "--no-continue"
                             "--no-part"
                             "--no-playlist"
                             "--dump-json"
                             url
                             :dir dest-dir)
        youtube-err (second (re-find #"YouTube said: (.+)$" err))]
    (if (zero? exit)
      (let [j (json/parse-string out true)]
        (log/info "Youtube-dl success: " (:_filename j))
        (select-keys j [:_filename :id :uploader :title]))
      (do
        (log/error "Youtube-dl error: "
                   exit
                   out
                   err)
        (throw+ {:type :youtube-error :exit exit})))))


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

(defn download-tagged-stuff []
  (let [items (db/get-items-by-tag :download)]
    (doall
    (for [item items]
      (let [url (get-in item [:entry :url])
            dest-dir (str "/home/seri/Desktop/MEDIA/"
                          (name (:key item)))]
        (when (re-find #"^(https?\:\/\/)?(www\.)?(youtube\.com|youtu\.?be)\/.+$" url)
          (log/info "Downloading " url " to " dest-dir)
          (-> (io/as-file dest-dir) .mkdirs)
          (future
            (try+
             (let [filename (youtube-dl-video url dest-dir)]
               (db/item-remove-tags (:id item) :download)
               filename)
             (catch [:type :youtube-error] {:keys [msg]}
               (log/errorf "Miner failed for %s: %s" item msg))
             (catch Object e
               (log/error e "Unexpected error")))
            )
          ))))))

(defn copy-wallpapers-to-home []
  (doseq [[source destination]
          (some->> (db/get-items-by-tag :wallpaper)
                   (map (fn [item]
                          (when-let [blob-url (get-in item [:entry :lead-image-url])]
                            [(string/replace blob-url "/blob/" "")
                             (:key item)])))
                   (map (fn [[blob source-key]]
                          (let [local-fn (infowarss.blobstore/get-local-filename blob)
                                mime (pantomime.mime/mime-type-of local-fn)
                                extension (pantomime.mime/extension-for-name mime)]
                            [local-fn (str source-key "_" blob extension)]))))]
    (let [dst-file (io/file "/home/seri/Pictures/wallpaper" destination)]
      (when-not (.exists dst-file)
        (io/copy source dst-file)
        dst-file))))

;;; toying around with urban sports club

(defn get-usc-venues []
  (:body (http/get "https://urbansportsclub.com/studios-map?city=1" {:as :json})))

(defn usc-l-only [usc-venues]
  (->> usc-venues :data :venues
    (filter #(= 3 (apply min (:planTypeIds %))))
    (remove #(some (partial = "Massage") (map :name (:categories %))))
    (map #(merge (select-keys % [:district :name :location])
            {:categories (string/join ", " (map :name (:categories %)))}))
    ))



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
