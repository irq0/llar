(ns infowarss.update
  (:require
   [infowarss.core :refer [*srcs* *update-max-retires*]]
   [infowarss.fetch :refer [fetch-and-process-source]]
   [infowarss.persistency :refer [store-items! duplicate?]]
   [clj-time.core :as time]
   [slingshot.slingshot :refer [throw+ try+]]
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]))


(def src-state-template
  (atom  {:last-successful-fetch-ts nil
          :status :new
          :last-exception nil
          :retry-count 0}))

(defn add-feed-src! [key src]
  (swap! *srcs* (fn [cur]
                  (assoc cur key {:src src}))))


(defn- merge-state [src new]
  (let [old (:state src)
        merged (merge old new)]
    (assoc src :state merged )))

(defn- update-feed! [feed]
  "Update feed. Return new state"
  (try+
    (let [items (fetch-and-process-source (:src feed))
          dbks (store-items! items)]
      (log/infof "[%s] Fetched %d, %d new stored to db"
        (get-in feed [:src :title])  (count items) (count dbks))

      (merge-state feed
        {:last-successful-fetch-ts (time/now)
         :status :ok
         :retry-count 0}))

    (catch [:type ::server-error-retry-later] _
      (merge-state feed
        {:status :temp-fail
         :last-exception &throw-context
         :retry-count (inc (get-in feed [:state :retry-count]))}))

    (catch [:type ::request-error] _
      (merge-state feed
        {:status :perm-fail
         :last-exception &throw-context
         :retry-count 0}))

    (catch [:type ::unexpected-error] _
      (merge-state feed
        {:status :perm-fail
         :last-exception &throw-context
         :retry-count 0}))
    (catch Object _
      (log/error "Unexpected error: " (:throwable &throw-context))
      (merge-state feed
        {:status :perm-fail
         :last-exception &throw-context
         :retry-count 0}))))

(defn set-state! [k new-status]
  (let [src (get @*srcs* k)]
    (when (contains? src :state)
      (swap! *srcs* (fn [current]
                      (assoc-in current [k :state :status] new-status))))))

(defn reset-all-failed! []
  (doseq [[k v] @*srcs*]
    (set-state! k :new)))


(defn update! [k]
  "Update feed by id (see: *srcs*)"
  (let [v (get @*srcs* k)
        feed (if (contains? v :state)
               v
               (assoc v :state @src-state-template))
        src (:src feed)
        old-state (:state feed)]

    (condp = (:status old-state)
      :new
      (log/info "New feed: " k)
      :ok
      (log/info "Working feed: " k)
      :temp-fail
      (log/info "Temporary failing feed %d/%d: %s"
        (:retry-count old-state) *update-max-retires* k)
      :perm-fail
      (log/info "Skipping perm fail feed: " k)
      (log/infof "Unknown status \"%s\": %s" (:status old-state) k))

    (when (or
            (#{:ok :new} (:status old-state))
            (and
              (= (:status old-state) :temp-fail)
              (< (:retry-count old-state) *update-max-retires* )))
      (let [new-feed (update-feed! feed)
            new-state (:state new-feed)]
        (log/infof "[%s] State: %s -> %s " k
          (:state old-state) (:state new-state))
        (swap! *srcs* (fn [current]
                        (assoc-in current [k :state] new-state)))))))

(defn update-all! []
  (doseq [[k v] @*srcs*]
    (update! k)))
