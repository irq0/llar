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
  (.write w (str @v)))


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
   :upwork-personal {:src (src/feed "https://www.upwork.com/ab/feed/topics/atom?securityToken=c037416c760678f3b3aa058a7d31f4a0dc32a269dd2f8f947256d915b19c8219029b5846f9f18209e6890ca6b72be221653cf275086926945f522d934a200eec&userUid=823911365103362048&orgUid=823911365107556353")
                     :tags #{:jobs}
                     :cron []}

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
   :weekly-programming-digest {:src (src/feed "http://feeds.feedburner.com/digest-programming")
                               :proc (proc/make
                                       :post [(proc/exchange
                                                [:entry :description]
                                                [:entry :contents])])
                               :cron cron-daily}

   :hn-best {:src (src/hn "beststories" :throttle-secs (* 23 60))
             :proc (proc/make
                     :filter (fn [item]
                               (let [site (some-> item :entry :url .getHost)
                                     score (get-in item [:entry :score])
                                     type (get-in item [:entry :type])]
                                 (not
                                   (or
                                     (and (= :story type)
                                       (>= score 800))
                                     (and
                                       (some? site)
                                       (re-find #"theatlantic|medium|youtube|nytimes|theguardian|washingtonpost|99percentinvisible|theverge|phys.org|bbc.com"
                                            site)
                                       (>= score 250)))))))}

   :fail {:src
          (src/feed "http://irq0.org/404")}
   })
