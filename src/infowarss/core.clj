(ns infowarss.core
  (:require
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]
   [infowarss.src :as src]
   [infowarss.postproc :as proc]
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

(def ^:dynamic *srcs*
  (atom
    {:fefe {:src (src/feed "http://blog.fefe.de/rss.xml?html" "fefe")
            :proc (proc/make
                    :post [(postproc/exchange
                             [:feed-entry :description]
                             [:feed-entry :contents])]
                    :filter (fn [item]
                              (->> item
                                :summary
                                :title
                                (re-find #"Zu Sarin"))))
            :tags #{:tech}
            :cron []}
     :upwork-personal {:src (src/feed "https://www.upwork.com/ab/feed/topics/atom?securityToken=c037416c760678f3b3aa058a7d31f4a0dc32a269dd2f8f947256d915b19c8219029b5846f9f18209e6890ca6b72be221653cf275086926945f522d934a200eec&userUid=823911365103362048&orgUid=823911365107556353" "upwork-personal")
                       :tags #{:jobs}
                       :cron []}

     :irq0 {:src (src/feed "http://irq0.org/news/index.atom" "irq0.org feed")
            :proc (proc/make
                    :post [(postproc/add-tag :personal)])
            :tags #{:personal}
            :cron []}
     :fail {:src
            (src/feed "http://irq0.org/404" "404")}
     }))
