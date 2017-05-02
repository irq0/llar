(ns infowarss.core
  (:require
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]
   [infowarss.src :as src]
   [clj-time.periodic :refer [periodic-seq]]
   [clj-time.core :as time]
   [infowarss.postproc :as proc]
   [hara.io.scheduler :as sched]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [hara.time.joda]))

;;;; Core configuration and data structures

;;; Logger

(log/merge-config!
  {:appenders {:spit (appenders/spit-appender {:fname "/tmp/inforwarss.log"})}
   :ns-blacklist  [] #_["org.apache.http.*"]
   :level :debug})

(log/merge-config!
  {:appenders {:println {:min-level :info
                         :stream :std-err}}})

;;; Config

(def config (edn/read-string (slurp (io/resource "config.edn"))))
(def creds (edn/read-string (slurp (io/resource "credentials.edn"))))

;;; State

(defonce ^:dynamic *state* (atom {}))

;;; Sources

(def cron-hourly "23 42 * * * * *")
(def cron-daily "0 42 23 * * * *")

(def ^:dynamic *srcs*
  (atom
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
     :weekly-programming-digest {:src (src/feed "http://feeds.feedburner.com/digest-programming")
                                 :proc (proc/make
                                         :post [(proc/exchange
                                                  [:entry :description]
                                                  [:entry :contents])])
                                 :cron cron-daily}

     :fail {:src
            (src/feed "http://irq0.org/404")}
     }))
