(ns u1f596.live
  (:require
   [u1f596.config :as config]
   [u1f596.postproc :as proc]
   [u1f596.store :refer [store-items!]]
   [u1f596.live.common :refer :all]
   [clojure.core.async :as async]
   [mount.core :refer [defstate]]
   [slingshot.slingshot :refer [try+]]
   [taoensso.timbre :as log]))

;;;; Live source logic. Similar to update, but running continuously in background


;; newstories
;; topstories
;; beststories
;; showstories
;; askstories

(declare live)

(defn process [item]
  (let [{:keys [_]} item
        k (get-in item [:meta :source-key])
        feed (config/get-source k)
        state (get-in live [:sources k :state])]

    (when (or (= k :unknown) (nil? k) (nil? @state))
      (log/error "Implementation error: item -> :meta"
                 "-> :source-key must be set during collection"))
    (try+
     (let [processed (proc/process feed @state [item])
           dbks (when (some? processed) (store-items! processed))]
       (log/debugf "Live processing for %s/%s: processed: %s, db: %s"
                   (str feed) (str item) (count processed) (count dbks)))

     (catch Object e
       (log/error e "Live: Exception in process - persist run")
       (:entry item)))))

(defn processor [input-ch term-ch]
  (async/thread
    (log/debug "Starting live processor")
    (loop []
      (let [[v ch] (async/alts!! [input-ch term-ch])]
        (when (identical? ch input-ch)
          (when (some? v)
            (process v)
            (recur)))))
    (log/debug "Stopping live processor")))

(defn live-feeds []
  (into {}
        (for [[k feed] (config/get-sources)
              :when (satisfies? LiveSource (:src feed))]
          [k feed])))

(defstate live
  :start (let [mix-ch (async/chan (async/sliding-buffer 1000))
               mix (async/mix mix-ch)
               term-ch (async/chan)]

           {:sources (into {}
                           (doall (for [[k feed] (live-feeds)]
                                    (let [state (atom (assoc state-template :key k))
                                          ch (async/chan (async/sliding-buffer 1000))]
                                      (start-collecting! (:src feed) state ch)
                                      (async/admix mix ch)
                                      (async/toggle mix {ch {:mute false
                                                             :pause false
                                                             :solo false}})
                                      [k {:state state
                                          :feed feed
                                          :ch ch}]))))
            :mix-ch mix-ch
            :mix mix
            :proc {:term-ch term-ch
                   :result (processor mix-ch term-ch)}})
  :stop (do
          (async/put! (get-in live [:proc :term-ch]) :terminate)
          (doall (for [[k {:keys [state ch feed]}] (:sources live)]
                   (do
                     (async/toggle (:mix live) {ch {:mute true
                                                    :pause false
                                                    :solo false}})
                     (stop-collecting! (:src feed) state))))))
