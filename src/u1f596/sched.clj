(ns u1f596.sched
  (:require
   [java-time :as time]
   [u1f596.config :as config]
   [u1f596.update :refer [update!]]
   [u1f596.lab :as u1f596-lab]
   [u1f596.persistency :as persistency]
   [u1f596.store :refer [backend-db]]
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
   (make-sched-from-feeds config/*srcs*)
   {}
   {:clock {:type "java.time.ZonedDateTime"
            :timezone "Europe/Berlin"
            :interval 2}}))

;;  second minute hour day-of-week day-of-month month year

(defstate db-sched
  :start (sched/start!
          (sched/scheduler
           {:update-db-search-indices {:handler (fn [_] (log/info "Refreshing search index:"
                                                                  (persistency/update-index! backend-db)))
                                       :schedule "0 42 3 * * * *"}
            :update-clustered-saved-items {:handler (fn [_]
                                                      (log/info "Updating saved items cluster")
                                                      (reset!
                                                       u1f596-lab/current-clustered-saved-items
                                                       (u1f596-lab/cluster-saved)))
                                           :schedule "0 5 * * * * *"}}))
  :stop (sched/stop! db-sched))

(defstate misc-sched
  :start (sched/start!
          (sched/scheduler
           {:download-tagged-items {:handler (fn [_] (log/info "Downloaded tagged links:"
                                                               (u1f596-lab/download-tagged-stuff)))
                                    :schedule "0 0 */5 * * * *"}
            :copy-wallpapers {:handler (fn [_] (log/info "New Wallpaper: "
                                                         (u1f596-lab/copy-wallpapers-to-home)))
                              :schedule "0 42 23 * * * *"}
            :remove-unread {:handler (fn [_]
                                       ;; 4 weeks
                                       (persistency/remove-unread-for-items-of-source-older-then! backend-db
                                        [:golem :hn-top :thenewstack
                                         :hn-best :reddit-berlin
                                         :comingsoon :reddit-games
                                         :netzpolitik :economist-scitech
                                         :wired :recode :infoq-articles :nerdcore
                                         :longform :gruenderszene-de :blocksandfiles
                                         :theregister-storage
                                         :inside-hpc
                                         :reddit-albumoftheday
                                         :reddit-albumaday
                                         :reddit-clojure
                                         :reddit-dataisbeautiful
                                         :reddit-indie
                                         :reddit-diy
                                         :reddit-berlin
                                         :reddit-jwd
                                         :reddit-listentothis
                                         :reddit-listentoconcerts
                                         :reddit-postrock
                                         :reddit-ifyoulikeblank
                                         :reddit-berlin]
                                        (time/minus (time/zoned-date-time) (time/weeks 4)))

                                       ;; 1 week
                                       (persistency/remove-unread-for-items-of-source-older-then! backend-db
                                        [:mydealz-hot :screenrant :wired :theverge :vox :kottke
                                         :vice :humblebundle]
                                        (time/minus (time/zoned-date-time) (time/weeks 1)))

                                       ;; 2 weeks
                                       (persistency/remove-unread-for-items-of-source-older-then! backend-db
                                        [:weekly-programming-digest :oreilly-fourshortlinks
                                         :nasa-image-of-the-day :atlantic-best-of
                                         :reddit-dataisbeautiful
                                         :reddit-europe]
                                        (time/minus (time/zoned-date-time) (time/weeks 2))))
                            :schedule "0 42 23 * * * *"}}))

  :stop (sched/stop! misc-sched))

(defstate feed-sched
  :start (sched/start! (make-feed-sched))
  :stop (sched/stop! feed-sched))

(defn get-sched-info [k]
  (some #(when (= (:name %) k) %) (sched/list-tasks feed-sched)))
