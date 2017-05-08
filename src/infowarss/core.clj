(ns infowarss.core
  (:require
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]
   [infowarss.src :as src]
   [clj-time.periodic :refer [periodic-seq]]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [infowarss.postproc :as proc]
   [hara.io.scheduler :as sched]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [hara.time.joda]))

;;;; Core configuration and data structures

;;; Logger

(log/merge-config!
  {:appenders {:spit (appenders/spit-appender {:fname "/tmp/inforwarss.log"})}
   :ns-blacklist  ["org.apache.http.*"
                   "org.eclipse.jetty.io.*"
                   "org.eclipse.jetty.http.*"
                   "org.eclipse.jetty.server.*"
                   "com.ning.http.client.providers.netty.handler.*"
                   "com.ning.http.client.providers.netty.channel.*"
                   ]
   :level :trace})

(log/merge-config!
  {:appenders {:println {:min-level :info
                         :stream :std-err}}})

;;; Config

(def config (edn/read-string (slurp (io/resource "config.edn"))))
(def creds (edn/read-string (slurp (io/resource "credentials.edn"))))
(defonce state (atom {}))

;;; State

(defmethod print-method org.joda.time.DateTime
  [v ^java.io.Writer w]
  (.write w "#datetime \"")
  (.write w (tc/to-string v))
  (.write w "\""))

(defmethod print-method java.net.URL
  [v ^java.io.Writer w]
  (.write w "#url \"")
  (.write w (str v))
  (.write w "\""))

(defmethod print-method clojure.lang.Atom
  [v ^java.io.Writer w]
  (.write w "#atom ")
  (.write w (prn-str @v)))


(defmethod print-method java.lang.Object
  [v ^java.io.Writer w]
  (.write w "#object nil"))

(defrecord TaggedValue [tag value])

(defn read-edn-string [s]
  (try
    (edn/read-string
      {:readers {'datetime tc/from-string
                 'url io/as-url
                 'error (fn [_] nil)  ; Throw away error details
                 'object (fn [_] (Object.))}
       :default ->TaggedValue}
      s)
    (catch RuntimeException e
      (log/error e "Failed to read EDN")
      {})))

(defn- persist-state! [_ _ _ new]
  (spit (io/resource "state.edn") (prn-str new)))


(defn -init []
  (reset! state (read-edn-string (slurp (io/resource "state.edn"))))
  (add-watch state :persist persist-state!)
  (log/info "Loaded state for keys: " (keys @state)))

;;; Sources

(def cron-hourly "23 42 * * * * *")
(def cron-daily "0 42 23 * * * *")


(defn make-hacker-news-filter [min-score min-score-match]
  (fn [item]
    (let [site (some-> item :entry :url .getHost)
          score (get-in item [:entry :score])
          title (get-in item [:summary :title])
          type (get-in item [:entry :type])]
      (not
        (or
          (and (= :story type)
            (>= score min-score))
          (and (= :story type)
            (re-find #"clojure" title)
            (>= score min-score-match))
          (and
            (some? site)
            (re-find #"theatlantic|medium|youtube|nytimes|theguardian|washingtonpost|99percentinvisible|theverge|phys.org|bbc.com"
              site)
            (>= score min-score-match)))))))

(defn make-reddit-proc [min-score]
  (proc/make
    :filter (fn [item]
              (let [site (some-> item :entry :url .getHost)
                    score (get-in item [:entry :score])
                    title (get-in item [:summary :title])]
                (< score min-score)))
    :post [(proc/mercury-contents (:mercury creds) :keep-orig true)]))


(def ^:dynamic *srcs*
  {:twit-c3pb {:src (src/twitter-search "c3pb" (:twitter-api creds))
               :proc (proc/make
                       :filter (fn [item]
                                 (->> item
                                   :entry
                                   :type
                                   #{:retweet})))
               :tags #{}
               :cron cron-hourly}
   :twit-augsburg-pics {:src (src/twitter-search "augsburg filter:images"
                               (:twitter-api creds))
                        :proc (proc/make
                                :filter (fn [item]
                                          (let [type (get-in item [:entry :type])
                                                text (get-in item [:entry :contents "text/plain"])]
                                            (or (#{:retweet} type)
                                              (re-find #"pussy|porn|camsex|webcam" text)))))
                        :tags #{}
                        :cron cron-hourly}

   :fefe {:src (src/feed "http://blog.fefe.de/rss.xml?html")
          :proc (proc/make
                  :post [(proc/exchange
                           [:entry :description]
                           [:entry :contents])]
                  :filter (fn [item]
                            (->> item
                              :summary
                              :title
                              (re-find #"Zu Sarin"))))
          :tags #{:tech}
          :cron cron-hourly}

   :upwork-personal {:src (src/feed
"https://www.upwork.com/ab/feed/topics/atom?securityToken=c037416c760678f3b3aa058a7d31f4a0dc32a269dd2f8f947256d915b19c8219029b5846f9f18209e6890ca6b72be221653cf275086926945f522d934a200eec&userUid=823911365103362048&orgUid=823911365107556353")
                     :tags #{:jobs}
                     :cron cron-hourly
                     :proc (proc/make
                             :filter (fn [item]
                                       (let [wants (get-in item [:entry :nlp :nouns])
                                             haves #{"python" "clojure" "ceph" "c++" "c" "logstash" "kibana" "java" "linux"}
                                             dontwants #{"php" "node.js" "javascript" "ecmascript" "wordpress" "scrapy" "django"}]
                                         (not
                                           (and (< (count (clojure.set/intersection wants dontwants)) 2)
                                             (>= (count (clojure.set/intersection wants haves)) 1))))))}

   :irq0 {:src (src/feed "http://irq0.org/news/index.atom")
          :proc (proc/make
                  :post [(proc/add-tag :personal)])
          :tags #{:personal}
          :cron cron-hourly}
   :oreilly-ideas {:src (src/feed "https://www.oreilly.com/ideas/feed.atom")
                   :cron cron-daily}

   :danluu {:src (src/feed "https://danluu.com/atom.xml" :deep? true)
            :cron cron-daily}
   :ridiculousfish {:src (src/feed "http://ridiculousfish.com/blog/atom.xml")
                    :cron cron-daily}
   :rachelbythebay {:src (src/feed "https://rachelbythebay.com/w/atom.xml")
                    :cron cron-daily}
   :chneukirchen {:src (src/feed "http://chneukirchen.org/trivium/index.atom")
                  :cron cron-daily}
   :codinghorror {:src (src/feed "http://feeds.feedburner.com/codinghorror")
                  :cron cron-daily}
   :joel-on-software {:src (src/feed "https://www.joelonsoftware.com/feed/")
                      :cron cron-daily}
   :summit-route {:src (src/feed "http://summitroute.com/blog/feed.xml")
                  :proc (proc/make
                          :post [(proc/exchange
                                   [:entry :description]
                                   [:entry :contents])])
                  :cron cron-daily}
   :joe-duffy {:src (src/feed "http://joeduffyblog.com/feed.xml")
               :cron cron-daily}

   :aphyr {:src (src/feed "https://aphyr.com/posts.atom")
           :cron cron-daily}

   :99pi {:src (src/feed "https://feeds.feedburner.com/99pi")
          :cron cron-daily}

   :kottke {:src (src/feed "http://feeds.kottke.org/main")
            :cron cron-daily}

   :stackoverflow-engineering {:src (src/feed "https://stackoverflow.blog/engineering/feed/")
                               :cron cron-daily}

   :spotify-insights {:src (src/feed "https://insights.spotify.com/us/feed/")
                      :cron cron-daily}

   :weekly-programming-digest {:src (src/feed "http://feeds.feedburner.com/digest-programming")
                               :proc (proc/make
                                       :post [(proc/exchange
                                                [:entry :description]
                                                [:entry :contents])])
                               :cron cron-daily}

   :usenix-conferences {:src (src/feed "https://www.usenix.org/upcoming-conferences-feed")
                        :tags #{:events}
                        :cron cron-daily}

   :acm-queue {:src (src/feed "https://queue.acm.org/rss/feeds/queuecontent.xml" :deep? true)
               :cron cron-daily}

   :reddit-diy {:src (src/reddit "DIY" :hot)
                :cron cron-daily
                :tags #{:reddit}
                :proc (make-reddit-proc 500)}

   :reddit-techsupportgore {:src (src/reddit "techsupportgore" :hot)
                            :cron cron-daily
                            :tags #{:reddit}
                            :proc (make-reddit-proc 500)}

   :reddit-techsupportmacgyver {:src (src/reddit "techsupportmacgyver" :hot)
                                :cron cron-daily
                                :tags #{:reddit}
                                :proc (make-reddit-proc 300)}

   :reddit-somethingimade {:src (src/reddit "somethingimade" :hot)
                           :cron cron-daily
                           :tags #{:reddit}
                           :proc (make-reddit-proc 300)}

   :reddit-Justrolledintotheshop {:src (src/reddit "Justrolledintotheshop" :hot)
                                  :cron cron-daily
                                  :tags #{:reddit}
                                  :proc (make-reddit-proc 400)}

   :reddit-fixit {:src (src/reddit "fixit" :hot)
                  :cron cron-daily
                  :tags #{:reddit}
                  :proc (make-reddit-proc 300)}

   :reddit-lifeprotips {:src (src/reddit "LifeProTips" :hot)
                        :cron cron-daily
                        :tags #{:reddit}
                        :proc (make-reddit-proc 300)}


   :reddit-depthhub {:src (src/reddit "DepthHub" :hot)
                     :cron cron-daily
                     :tags #{:reddit}
                     :proc (make-reddit-proc 500)}

   :reddit-fascinating {:src (src/reddit "fascinating" :hot)
                        :cron cron-daily
                        :tags #{:reddit}
                        :proc (make-reddit-proc 100)}

   :reddit-learnuselesstalents {:src (src/reddit "LearnUselessTalents" :hot)
                                :cron cron-daily
                                :tags #{:reddit}
                                :proc (make-reddit-proc 1000)}

   :reddit-todayilearned {:src (src/reddit "todayilearned" :hot)
                          :cron cron-daily
                          :tags #{:reddit}
                          :proc (make-reddit-proc 2000)}

   :reddit-clojure {:src (src/reddit "Clojure" :hot)
                    :cron cron-daily
                    :tags #{:reddit}
                    :proc (make-reddit-proc 80)}

   :reddit-listentothis {:src (src/reddit "listentothis" :hot)
                         :cron cron-daily
                         :tags #{:reddit}
                         :proc (make-reddit-proc 150)}

   :reddit-popular {:src (src/reddit "popular" :hot)
                    :cron cron-daily
                    :tags #{:reddit}
                    :proc (make-reddit-proc 30000)}

   :hn-top {:src (src/hn "topstories" :throttle-secs (* 23 60))
            :proc (proc/make
                    :post [(proc/mercury-contents (:mercury creds) :keep-orig true)]
                    :filter (make-hacker-news-filter 600 200))}

   :hn-best {:src (src/hn "beststories" :throttle-secs (* 23 60))
             :proc (proc/make
                     :post [(proc/mercury-contents (:mercury creds) :keep-orig true)]
                     :filter (make-hacker-news-filter 800 300))}

   :xkcd {:src (src/feed "https://xkcd.com/rss.xml")
          :proc (proc/make
                  :post [(proc/exchange [:entry :description] [:entry :contents])])
          :cron cron-daily
          :tags #{:comics}}

   :fail {:src
          (src/feed "http://irq0.org/404")}
   })
