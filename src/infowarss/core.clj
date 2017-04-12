(ns infowarss.core
  (:require
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]
   [infowarss.src]
   [infowarss.postproc :as postproc]
   )
  (:import [infowarss.src Feed]))

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
            (Feed. "http://blog.fefe.de/rss.xml?html" "fefe"
              [(postproc/move [:feed-entry] :description :contents)])
            :tags #{:tech}
            :cron []}

     :irq0 {:src
            (Feed. "http://irq0.org/news/index.atom" "irq0.org feed"
              [(postproc/add-tag :personal)]
              )
            :tags #{:personal}
            :cron []}
     :fail {:src
            (Feed. "http://irq0.org/404" "404" nil)}
     }))
