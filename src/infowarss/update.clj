(ns infowarss.update
  (:require
   [infowarss.core :refer [*srcs* *update-max-retires*]]
   [infowarss.fetch :as fetch]
   [infowarss.persistency :refer [store-items! duplicate?]]
   [infowarss.postproc :as proc]
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
    (let [fetched (fetch/fetch feed)
          processed (proc/process feed fetched)
          dbks (store-items! processed)]
      (log/infof "[%s] fetched: %d, after processing: %d, new in db: %d"
        (-> feed :src :title) (count fetched) (count processed) (count dbks))

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
      (log/error "Unexpected error: "  &throw-context)
      (merge-state feed
        {:status :perm-fail
         :last-exception &throw-context
         :retry-count 0}))))

(defn set-status! [k new-status]
  (let [src (get @*srcs* k)]
    (when (contains? src :state)
      (swap! *srcs* (fn [current]
                      (assoc-in current [k :state :status] new-status))))))

(defn reset-all-failed! []
  (doseq [[k v] @*srcs*]
    (set-status! k :new)))


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
          (:status old-state) (:status new-state))
        (swap! *srcs* (fn [current]
                        (assoc-in current [k :state] new-state)))))))

(defn update-all! []
  (doseq [[k v] @*srcs*]
    (update! k)))
