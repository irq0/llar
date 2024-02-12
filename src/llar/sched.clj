(ns llar.sched
  (:require
   [chime.core :as chime]
   [clojure.tools.logging :as log]
   [java-time.api :as time]
   [mount.core :refer [defstate]]
   [llar.lab :as llar-lab]
   [llar.metrics :as metrics]
   [llar.persistency :as persistency]
   [llar.src :as src]
   [llar.store :refer [backend-db]]
   [llar.update :as feed-update])
  (:import
   [java.time Duration ZoneId ZonedDateTime]))

(defn- today-at [hour minute]
  (.toInstant
   (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime hour minute))
                     (ZoneId/of "Europe/Berlin"))))

(defmacro defsched [sched-name chime-times & body]
  `(defstate ~sched-name
     :start (chime/chime-at
             ~chime-times
             (fn [~'$TIME]
               (metrics/with-log-exec-time
                 (do ~@body))))
     :stop (.close ~sched-name)))

(defmacro defsched-feed-by-filter [sched-name chime-times pred]
  `(defstate ~sched-name
     :start (chime/chime-at
             ~chime-times
             (fn [~'$TIME]
               (metrics/with-log-exec-time-named ~sched-name
                 (let [sources# (feed-update/updateable-sources)
                       filtered# (filter (fn [[k# source#]]
                                           (let [~'$KEY k#
                                                 ~'$SRC (:src source#)
                                                 ~'$TAGS (:tags source#)]
                                             ~pred))
                                         sources#)
                       keys# (mapv first filtered#)
                       result# (pmap feed-update/update! keys#)]
                   (log/infof "Scheduled feed update %s: %s"
                              '~sched-name (vec (interleave keys# result#)))))))
     :stop (.close ~sched-name)))

(defsched update-db-search-indices (chime/periodic-seq (today-at 5 0) (Duration/ofDays 1))
  (persistency/update-index! backend-db))

(defsched update-clustered-saved-items (chime/periodic-seq (.plusSeconds (chime/now) 42) (Duration/ofHours 12))
  (log/info "Updating saved items cluster")
  (reset!
   llar-lab/current-clustered-saved-items
   (llar-lab/cluster-saved)))

(defsched download-tagged-items (chime/periodic-seq (.plusSeconds (chime/now) 42) (Duration/ofMinutes 10))
  (log/info
   "Downloaded tagged links:"
   (vec (llar-lab/download-tagged-stuff))))

(defsched copy-wallpapers (chime/periodic-seq (.plusSeconds (chime/now) 42) (Duration/ofHours 1))
  (log/info
   "New Wallpaper: "
   (llar-lab/copy-wallpapers-to-home)))

(defsched remove-unread-tags (chime/periodic-seq (today-at 5 0) (Duration/ofDays 1))
  ;; 4 weeks
  (persistency/remove-unread-for-items-of-source-older-then!
   backend-db
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
  (persistency/remove-unread-for-items-of-source-older-then!
   backend-db
   [:mydealz-hot :screenrant :wired :theverge :vox :kottke
    :vice :humblebundle]
   (time/minus (time/zoned-date-time) (time/weeks 1)))

  ;; 2 weeks
  (persistency/remove-unread-for-items-of-source-older-then!
   backend-db
   [:weekly-programming-digest :oreilly-fourshortlinks
    :nasa-image-of-the-day :atlantic-best-of
    :reddit-dataisbeautiful
    :reddit-europe]
   (time/minus (time/zoned-date-time) (time/weeks 2))))

(def during-daytime
  (->>
   (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 9 0))
                                         (ZoneId/of "Europe/Berlin"))
                       (Duration/ofHours 1))
   (filter (comp #{10 12 13 14 16 18} #(.getHour %)))
   (chime/without-past-times)))

(def sundays
  (->>
   (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 5 0))
                                         (ZoneId/of "Europe/Berlin"))
                       (Duration/ofDays 1))
   (filter (comp #{java.time.DayOfWeek/SUNDAY} #(.getDayOfWeek %)))
   (chime/without-past-times)))

(def early-morning
  (->>
   (chime/periodic-seq (ZonedDateTime/of (-> (java.time.LocalDate/now) (.atTime 7 0))
                                         (ZoneId/of "Europe/Berlin"))
                       (Duration/ofDays 1))
   (chime/without-past-times)))

(def hourly
  (->>
   (chime/periodic-seq  (-> (java.time.LocalDate/now)
                            (.atStartOfDay
                             (ZoneId/of "Europe/Berlin")))
                        (Duration/ofHours 1))
   (chime/without-past-times)))

(defsched-feed-by-filter twitter
  early-morning
  (src/twitter? $SRC))

(defsched-feed-by-filter reddit
  during-daytime
  (src/reddit? $SRC))

(defsched-feed-by-filter tech-blogs-sci
  during-daytime
  (and (src/feed? $SRC) (some #{:tech :sci :deep-tech :blog} $TAGS)))

(defsched-feed-by-filter shopping-sites
  hourly
  (some #{:shopping} $TAGS))

(defsched-feed-by-filter mailboxes
  early-morning
  (src/mailbox? $SRC))

(defsched-feed-by-filter magazines
  early-morning
  (some #{:magazine :youtube-channel :comics :movies :gaming} $TAGS))

(defsched-feed-by-filter picture-of-the-day
  early-morning
  (some #{:pics} $TAGS))

(defsched-feed-by-filter all-feeds-weekly
  sundays
  (src/feed? $SRC))

(defsched-feed-by-filter local-news
  during-daytime
  (and (src/feed? $SRC) (some #{:berlin} $TAGS)))

(defsched-feed-by-filter alerts
  hourly
  (some #{:google-alert} $TAGS))

(def db-sched [#'update-db-search-indices
               #'update-clustered-saved-items])

(def misc-sched [#'copy-wallpapers
                 #'download-tagged-items
                 #'remove-unread-tags])

(def feed-sched [#'twitter
                 #'reddit
                 #'tech-blogs-sci
                 #'shopping-sites
                 #'mailboxes
                 #'magazines
                 #'picture-of-the-day
                 #'all-feeds-weekly
                 #'local-news
                 #'alerts])
