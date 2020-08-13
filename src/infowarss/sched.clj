(ns infowarss.sched
  (:require
   [clj-time.core :as time]
   [infowarss.core :refer [*srcs*]]
   [infowarss.update :refer [update!]]
   [infowarss.db :as infowarss-db]
   [infowarss.lab :as infowarss-lab]
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

;;  second minute hour day-of-week day-of-month month year

(defstate db-sched
  :start (sched/start!
          (sched/scheduler
           {:refresh-search-index {:handler (fn [_] (log/info "Refreshing search index:"
                                                              (infowarss-db/refresh-search-index)))
                                   :schedule "0 42 3 * * * *"}
            :update-clustered-saved-items {:handler (fn [_]
                                                      (log/info "Updating saved items cluster")
                                                      (reset!
                                                       infowarss-lab/current-clustered-saved-items
                                                       (infowarss-lab/cluster-saved)))
                                           :schedule "0 5 * * * * *"}

            :refresh-idf {:handler (fn [_] (log/info "Refreshing search index:"
                                                     (infowarss-db/refresh-idf)))
                          :schedule "0 42 3 * * * *"}}))
  :stop (sched/stop! db-sched))

(defstate sched
  :start (sched/start!
          (sched/scheduler
           {:download-tagged-items {:handler (fn [_] (log/info "Downloaded tagged links:"
                                                               (infowarss-lab/download-tagged-stuff)))
                                    :schedule "0 0 */5 * * * *"}
            :copy-wallpapers {:handler (fn [_] (log/info "New Wallpaper: "
                                                         (infowarss-lab/copy-wallpapers-to-home)))
                              :schedule "0 42 23 * * * *"}
            :remove-unread {:handler (fn [_]
                                       (infowarss-db/remove-unread-for-items-of-source-older-than
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
                                        (time/minus (time/now) (time/weeks 4)))
                                       (infowarss-db/remove-unread-for-items-of-source-older-than
                                        [:mydealz-hot :screenrant :wired :theverge :vox :kottke
                                         :vice :humblebundle]
                                        (time/minus (time/now) (time/weeks 1)))
                                       (infowarss-db/remove-unread-for-items-of-source-older-than
                                        [:weekly-programming-digest :oreilly-fourshortlinks
                                         :nasa-image-of-the-day :atlantic-best-of
                                         :reddit-dataisbeautiful
                                         :reddit-europe]
                                        (time/minus (time/now) (time/weeks 2))))
                            :schedule "0 42 23 * * * *"}}))

  :stop (sched/stop! sched))

(defstate feed-sched
  :start (sched/start! (make-feed-sched))
  :stop (sched/stop! feed-sched))

(defn get-sched-info [k]
  (some #(when (= (:name %) k) %) (sched/list-tasks feed-sched)))
