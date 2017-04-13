(ns infowarss.core
  (:require
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]
   [infowarss.src :as src]
   [infowarss.postproc :as postproc]
   ))

;; Log config

(log/merge-config!
  {:appenders {:spit (appenders/spit-appender {:fname "/tmp/inforwarss.log"})}
   :ns-blacklist  [] #_["org.apache.http.wire"]
   :level :debug})

(log/merge-config!
  {:appenders {:println {:min-level :info
                         :stream :std-err}}})

;; app config

(defonce ^:dynamic *update-max-retires* 5)


;; sources

(defonce ^:dynamic *srcs*
  (atom
    {:fefe {:src
            (src/feed "http://blog.fefe.de/rss.xml?html" "fefe"
              [(postproc/exchange [:feed-entry :description] [:feed-entry :contents])])
            :tags #{:tech}
            :cron []}

     :irq0 {:src
            (src/feed "http://irq0.org/news/index.atom" "irq0.org feed"
              [(postproc/add-tag :personal)])
            :tags #{:personal}
            :cron []}
     :fail {:src
            (src/feed "http://irq0.org/404" "404")}
     }))
