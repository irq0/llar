(ns llar.sched
  #_{:clj-kondo/ignore [:unused-namespace]}
  (:require
   [chime.core :as chime]
   [mount.core :refer [defstate]]
   [slingshot.slingshot :refer [throw+]]
   [llar.metrics :as metrics]
   [clojure.tools.logging :as log]
   [java-time.api :as time]
   [llar.src])
  (:import
   [java.time Duration ZoneId ZonedDateTime]))

(defn canned-scheds [kw]
  (case kw
    :during-daytime
    (->>
     (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 9 0))
                                           (ZoneId/systemDefault))
                         (Duration/ofHours 1))
     (filter (comp #{10 12 13 14 16 18} #(.getHour %)))
     (chime/without-past-times))
    :sundays
    (->>
     (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 5 0))
                                           (ZoneId/systemDefault))
                         (Duration/ofDays 1))
     (filter (comp #{java.time.DayOfWeek/SUNDAY} #(.getDayOfWeek %)))
     (chime/without-past-times))
    :early-morning
    (->>
     (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 7 0))
                                           (ZoneId/systemDefault))
                         (Duration/ofDays 1))
     (chime/without-past-times))
    :now-and-early-morning
    (->
     (->>
      (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 7 0))
                                            (ZoneId/systemDefault))
                          (Duration/ofDays 1))
      (chime/without-past-times))
     (conj (time/plus (time/zoned-date-time) (time/seconds 10))))
    :hourly
    (->>
     (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 0 0))
                                           (ZoneId/systemDefault))
                         (Duration/ofHours 1))
     (chime/without-past-times))))

(defn resolve-chime-times [chime-times-or-keyword]
  (if (keyword? chime-times-or-keyword)
    (if-let [canned (canned-scheds chime-times-or-keyword)]
      canned
      (throw+ {:type ::unknown-canned-sched
               :keyword chime-times-or-keyword}))
    chime-times-or-keyword))

(defmacro defsched [sched-name chime-times-or-keyword & body]
  `(defstate ~sched-name
     :start (vary-meta (chime/chime-at
                        (resolve-chime-times ~chime-times-or-keyword)
                        (fn [~'$TIME]
                          (metrics/with-log-exec-time
                            (do ~@body))))
                       merge
                       {:sched-name (str '~sched-name)
                        :chime-times (when (keyword? ~chime-times-or-keyword) ~chime-times-or-keyword)
                        :sched-type :defsched
                        :pred (quote ~body)})
     :stop (.close ~sched-name)))
