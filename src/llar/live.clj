(ns llar.live
  (:require
   [llar.config :as config]
   [llar.postproc :as proc]
   [llar.store :refer [store-items!]]
   [llar.src :as src]
   [llar.live.common :refer [start-collecting! state-template stop-collecting!]]
   [clojure.core.async :as async]
   [mount.core :refer [defstate]]
   [slingshot.slingshot :refer [try+]]
   [clojure.tools.logging :as log]))

;;;; Live source logic. Similar to update, but running continuously in background

(declare live)

(defn process [item]
  (let [{:keys [_]} item
        k (get-in item [:meta :source-key])
        feed (config/get-source k)
        state (get-in live [:sources k :state])]

    (when (or (= k :unknown) (nil? k) (nil? @state))
      (log/error "implementation error: item -> :meta"
                 "-> :source-key must be set during collection"))
    (try+
     (let [processed (proc/process feed @state [item])
           dbks (when (some? processed) (store-items! processed))]
       (log/debugf "live processing for %s/%s: processed: %s, db: %s"
                   (str feed) (str item) (count processed) (count dbks)))

     (catch Object e
       (log/error e "live: exception in process - persist run")
       (:entry item)))))

(defn processor [input-ch term-ch]
  (async/thread
    (log/debug "starting live processor")
    (loop []
      (let [[v ch] (async/alts!! [input-ch term-ch])]
        (when (identical? ch input-ch)
          (when (some? v)
            (process v)
            (recur)))))
    (log/debug "stopping live processor")))

(defn live-feeds []
  (into {} (filter (fn [[_ {:keys [src]}]] (and (some? src) (= (src/source-type src) :llar.src/live))) (config/get-sources))))

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
          (doall (for [[_k {:keys [state ch feed]}] (:sources live)]
                   (do
                     (async/toggle (:mix live) {ch {:mute true
                                                    :pause false
                                                    :solo false}})
                     (stop-collecting! (:src feed) state))))))
