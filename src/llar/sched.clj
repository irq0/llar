(ns llar.sched
  (:require
   [chime.core :as chime]
   [llar.appconfig :as appconfig]
   [mount.core :refer [defstate]]
   [llar.metrics :as metrics]
   [llar.src])
  (:import
   [java.time Duration ZonedDateTime]))

(defmacro defsched [sched-name chime-times & body]
  `(defstate ~sched-name
     :start (chime/chime-at
             ~chime-times
             (fn [~'$TIME]
               (metrics/with-log-exec-time
                 (do ~@body))))
     :stop (.close ~sched-name)))

(defstate canned-scheds
  :start {:during-daytime
          (->>
           (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 9 0))
                                                 (appconfig/timezone))
                               (Duration/ofHours 1))
           (filter (comp #{10 12 13 14 16 18} #(.getHour %)))
           (chime/without-past-times))
          :sundays
          (->>
           (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 5 0))
                                                 (appconfig/timezone))
                               (Duration/ofDays 1))
           (filter (comp #{java.time.DayOfWeek/SUNDAY} #(.getDayOfWeek %)))
           (chime/without-past-times))
          :early-morning
          (->>
           (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 7 0))
                                                 (appconfig/timezone))
                               (Duration/ofDays 1))
           (chime/without-past-times))
          :hourly
          (->>
           (chime/periodic-seq  (-> (java.time.LocalDate/now)
                                    (.atStartOfDay
                                     (appconfig/timezone)))
                                (Duration/ofHours 1))
           (chime/without-past-times))})
