(ns llar.sched
  #_{:clj-kondo/ignore [:unused-namespace]}
  (:require
   [chime.core :as chime]
   [llar.appconfig :refer [appconfig]]
   [mount.core :refer [defstate]]
   [llar.metrics :as metrics]
   [clojure.tools.logging :as log]
   [llar.src])
  (:import
   [java.time Duration ZoneId ZonedDateTime]))

(defstate canned-scheds
  :start {:during-daytime
          (->>
           (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 9 0))
                                                 (ZoneId/of (:timezone appconfig)))
                               (Duration/ofHours 1))
           (filter (comp #{10 12 13 14 16 18} #(.getHour %)))
           (chime/without-past-times))
          :sundays
          (->>
           (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 5 0))
                                                 (ZoneId/of (:timezone appconfig)))
                               (Duration/ofDays 1))
           (filter (comp #{java.time.DayOfWeek/SUNDAY} #(.getDayOfWeek %)))
           (chime/without-past-times))
          :early-morning
          (->>
           (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 7 0))
                                                 (ZoneId/of (:timezone appconfig)))
                               (Duration/ofDays 1))
           (chime/without-past-times))
          :hourly
          (->>
           (chime/periodic-seq  (-> (java.time.LocalDate/now)
                                    (.atStartOfDay (ZoneId/of (:timezone appconfig))))
                                (Duration/ofHours 1))
           (chime/without-past-times))})

(defmacro defsched [sched-name chime-times-or-keyword & body]
  `(defstate ~sched-name
     :start (let [chime-times# (if (keyword? ~chime-times-or-keyword)
                                 (get canned-scheds ~chime-times-or-keyword)
                                 ~chime-times-or-keyword)]
              (chime/chime-at
               chime-times#
               (fn [~'$TIME]
                 (metrics/with-log-exec-time
                   (do ~@body)))))
     :stop (.close ~sched-name)))
