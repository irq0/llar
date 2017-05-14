(ns infowarss.sched
  (:require
   [infowarss.core :refer [*srcs*]]
   [infowarss.update :refer [update!]]
   [hara.io.scheduler :as sched]
   [taoensso.timbre :as log]
   [mount.core :refer [defstate]]))



(defn make-sched-from-feeds [feeds]
  (into {}
    (for [[k feed] feeds
          :when (and (seq (get feed :cron))
                  (seq (get feed :src)))]
      {k {:handler (fn [t] (log/debugf "Cron start on %s: Update %s"
                             t k)
                     (update! k))
          :schedule (get feed :cron)}})))

(defn make-feed-sched
  []
  (sched/scheduler
    (make-sched-from-feeds *srcs*)
    {}
    {:clock {:type "org.joda.time.DateTime"
             :timezone "Europe/Berlin"
             :interval 2}}))


(defstate feed-sched
  :start (sched/start! (make-feed-sched))
  :stop (sched/stop! feed-sched))
