(ns infowarss.live
  (:require
   [infowarss.core :as core]
   [infowarss.schema :as schema]
   [infowarss.postproc :as proc]
   [infowarss.persistency :as persistency]
   [infowarss.live.common :refer :all]
   [schema.core :as s]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [clj-time.format :as tf]
   [clojure.java.io :as io]
   [clojure.core.async :refer [>!! <!!] :as async]
   [clojure.set :refer [map-invert]]
   [slingshot.slingshot :refer [throw+ try+]]
   [taoensso.timbre :as log])
  (:import (com.firebase.client Firebase ValueEventListener DataSnapshot FirebaseError Logger Logger$Level)))

;;;; Live source logic. Similar to update, but running continuously in background


;; newstories
;; topstories
;; beststories
;; showstories
;; askstories

(defonce live-chans (atom {}))
(defonce live-chan (async/chan))
(defonce live-mix (async/mix live-chan))

(defonce processor-terminate (async/chan))
(defonce processor-thread (atom nil))

(defn process [item]
  (let [{:keys [src]} item
        k (get-in item [:meta :source-key])
        feed (get core/*srcs* k)
        state @(get @core/state k)]

    (when (or (= k :unknown) (nil? k))
      (log/error "Implementation error: item -> :meta"
        "-> :source-key must be set during collection"))
    (try+
      (let [processed (proc/process feed state [item])
            dbks (when-not (nil? processed) (persistency/store-items! processed))]
        (log/debugf "Live processing for %s/%s: processed: %s, db: %s"
          (str feed) (str item) (count processed) (count dbks)))

      (catch Object e
        (log/error e "Live: Exception in process - persist run")
        (log/spy (:entry item))))))

(defn processor [input-ch term-ch]
  (async/thread
    (log/debug "Starting live processor")
    (loop []
      (let [[v ch] (async/alts!! [input-ch term-ch])]
        (if (identical? ch input-ch)
          (when (some? v)
            (process v)
            (recur)))))))

(defn start-processor []
  (when (some? @processor-thread)
    (throw+ {:type ::processor-running}))
  (reset! processor-thread (processor live-chan processor-terminate)))


(defn stop-processor []
  (log/debug "Sending terminate msg")
  (async/put! processor-terminate :terminate)
  (let [result (<!! @processor-thread)]
    (reset! processor-thread nil)
    result))


(defn start
  "Start live source by id (see: *srcs*)"
  [k]

  (when (nil? (get core/*srcs* k))
    (throw+ {:type ::unknown-source-key :key k :known-keys (keys core/*srcs*)}))

  (when-not (satisfies? LiveSource (get-in core/*srcs* [k :src]))
    (throw+ {:type ::not-a-live-source :key k}))

  (when (nil? (get @core/state k))
    (swap! core/state assoc k (atom state-template)))

  (when (nil? (get live-chans k))
    (swap! live-chans assoc k (async/chan 1000)))

  (let [feed (get core/*srcs* k)
        src (get feed :src)
        state (get @core/state k)
        chan (get @live-chans k)]
    (when (= :running (:status @state))
      (throw+ {:type ::already-running :state @state}))

    (when (nil? (:key @state))
      (swap! state assoc :key k))
    (async/admix live-mix chan)
    (async/toggle live-mix {chan {:mute false
                                  :pause false
                                  :solo false}})
    (start-collecting! src state chan))
  (:status @(get @core/state k)))


(defn stop
 "Stop live source"
  [k]

  (when (nil? (get core/*srcs* k))
    (throw+ {:type ::unknown-source-key :key k :known-keys (keys core/*srcs*)}))

  (when-not (satisfies? LiveSource (get-in core/*srcs* [k :src]))
    (throw+ {:type ::not-a-live-source :key k}))

  (let [feed (get core/*srcs* k)
        src (get feed :src)
        state (get @core/state k)]

    (when-not (= :running (:status @state))
      (throw+ {:type ::not-running :status (:status @state) :state @state}))

    (let [chan (get @live-chans k)
          new-state (stop-collecting! src state)]
      (async/toggle live-mix {chan {:mute true
                                    :pause false
                                    :solo false}})))
  (:status @(get @core/state k)))



;; :hn-best {:src (src/hn "beststories")
;;           :proc (proc/make
;;                      :filter (fn [item]
;;                                (->> item
;;                                    :hn :score (> 200)))
;;
