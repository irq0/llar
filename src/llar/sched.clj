(ns llar.sched
  #_{:clj-kondo/ignore [:unused-namespace]}
  (:require
   [chime.core :as chime]
   [llar.converter :as converter]
   [mount.core :refer [defstate] :as mount]
   [slingshot.slingshot :refer [throw+]]
   [llar.metrics :as metrics]
   [clojure.tools.logging :as log]
   [java-time.api :as time]
   [iapetos.core :as prometheus]
   [llar.src])
  (:import
   [java.io Closeable]
   [java.time Duration ZoneId ZonedDateTime]))

(declare resolve-chime-times)

(defrecord Schedule [key mount-state sched-name sched-type chime-times schedule-times pred run-fn state* closeable*]
  Closeable
  (close [_]
    (when-let [closeable @closeable*]
      (.close closeable))))

(defn schedule? [x]
  (and (map? x)
       (contains? x :run-fn)
       (contains? x :state*)
       (contains? x :closeable*)))

(defn make-schedule [opts]
  (map->Schedule
   (merge {:state* (atom {:running? false})
           :closeable* (atom nil)}
          opts)))

(defn attach-closeable! [schedule closeable]
  (reset! (:closeable* schedule) closeable)
  schedule)

(defn- throwable->exception-context [^Throwable throwable]
  (converter/->ExceptionContext
   (.getCause throwable)
   (ex-message throwable)
   (.getStackTrace throwable)
   throwable
   (ex-data throwable)))

(defn next-run-at [schedule]
  (let [now (time/zoned-date-time)]
    (->> (:schedule-times schedule)
         (drop-while #(time/before? % now))
         first)))

(defn snapshot
  "Return dashboard/API-safe schedule metadata plus runtime state."
  [schedule]
  (merge (select-keys schedule [:key :mount-state :sched-name :sched-type :chime-times :pred])
         {:next-run-at (next-run-at schedule)}
         @(:state* schedule)))

(defn- with-mount-state [schedule state-name]
  (if (:mount-state schedule)
    schedule
    (assoc schedule :mount-state state-name)))

(defn run-schedule!
  ([schedule trigger]
   (run-schedule! schedule trigger {}))
  ([schedule trigger {:keys [skip-if-running?]
                      :or {skip-if-running? true}}]
   (let [state* (:state* schedule)
         started-at (time/zoned-date-time)]
     (if (and skip-if-running? (:running? @state*))
       :already-running
       (do
         (swap! state* assoc
                :running? true
                :last-started-at started-at
                :last-trigger trigger)
         (try
           (prometheus/set-to-current-time metrics/prom-registry :llar-sched/last-run
                                           {:schedule (:sched-name schedule)})
           (let [result (metrics/with-log-exec-time-named (:sched-name schedule)
                          ((:run-fn schedule)))
                 finished-at (time/zoned-date-time)]
             (swap! state* merge
                    {:running? false
                     :last-finished-at finished-at
                     :last-duration (time/duration started-at finished-at)
                     :last-result result
                     :last-exception nil})
             result)
           (catch Throwable t
             (let [finished-at (time/zoned-date-time)]
               (swap! state* merge
                      {:running? false
                       :last-finished-at finished-at
                       :last-duration (time/duration started-at finished-at)
                       :last-exception (throwable->exception-context t)})
               (throw t)))))))))

(defn trigger-schedule! [schedule]
  (let [state* (:state* schedule)]
    (if (:running? @state*)
      :already-running
      (let [fut (future
                  (try
                    (run-schedule! schedule :manual)
                    (finally
                      (swap! state* dissoc :current-future))))]
        (swap! state* assoc :current-future fut)
        :triggered))))

(defn find-schedules []
  (->> (mount/find-all-states)
       (keep (fn [state-name]
               (let [state (mount/current-state state-name)]
                 (when (schedule? state)
                   (with-mount-state state state-name)))))))

(defn find-schedule [schedule-name]
  (let [k (keyword schedule-name)
        matches (->> (find-schedules)
                     (filter (fn [schedule]
                               (let [{:keys [key sched-name mount-state]} schedule]
                                 (or (= k key)
                                     (= (name k) sched-name)
                                     (= schedule-name mount-state)))))
                     vec)]
    (case (count matches)
      0 {:error :not-found}
      1 {:schedule (first matches)}
      {:error :ambiguous
       :matches (mapv snapshot matches)})))

(defn start-schedule! [schedule chime-times]
  (let [schedule-times (resolve-chime-times chime-times)
        schedule (assoc schedule :schedule-times schedule-times)]
    (attach-closeable!
     schedule
     (chime/chime-at
      schedule-times
      (fn [_time]
        (run-schedule! schedule :scheduled))))))

(defn- nowish []
  (time/plus (time/zoned-date-time) (time/seconds (rand-int 120))))

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
     (conj (nowish)))
    :now-and-hourly
    (->
     (->>
      (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 0 0))
                                            (ZoneId/systemDefault))
                          (Duration/ofHours 1))
      (chime/without-past-times))
     (conj (nowish)))
    :hourly
    (->>
     (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 0 0))
                                           (ZoneId/systemDefault))
                         (Duration/ofHours 1))
     (chime/without-past-times))
    :now-and-every-5-minutes
    (->
     (->>
      (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 0 0))
                                            (ZoneId/systemDefault))
                          (Duration/ofMinutes 5))
      (chime/without-past-times))
     (conj (nowish)))))

(defn resolve-chime-times [chime-times-or-keyword]
  (if (keyword? chime-times-or-keyword)
    (if-let [canned (canned-scheds chime-times-or-keyword)]
      canned
      (throw+ {:type ::unknown-canned-sched
               :keyword chime-times-or-keyword}))
    chime-times-or-keyword))

(defmacro defsched [sched-name chime-times-or-keyword & body]
  `(defstate ~sched-name
     :start (let [schedule# (make-schedule
                             {:key (keyword '~sched-name)
                              :mount-state ~(str "#'" (ns-name *ns*) "/" sched-name)
                              :sched-name (str '~sched-name)
                              :chime-times (when (keyword? ~chime-times-or-keyword) ~chime-times-or-keyword)
                              :sched-type :defsched
                              :pred (quote ~body)
                              :run-fn (fn [] ~@body)})
                  started# (start-schedule! schedule# ~chime-times-or-keyword)]
              started#)
     :stop (.close ~sched-name)))
