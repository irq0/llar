(ns infowarss.live
  (:require
   [infowarss.core :as core]
   [infowarss.schema :as schema]
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

(def live-chans (atom {}))
(def live-chan (async/chan))
(def live-mix (async/mix live-chan))

(def processor-thread (atom nil))

(defn start-processor []
  (when (nil? @processor-thread)
    (reset! processor-thread
      (async/thread
        (loop []
          (let [item (<!! live-chan)]
            (if-not (= item :stop)
              (do
                (log/trace (str item))
                (recur))
              (log/debug "Stopping live processor"))))))))

(defn stop-processor []
  (>!! live-chan :stop))


(defn start
  "Start live source by id (see: *srcs*)"
  [k]

  (when (nil? (get core/*srcs* k))
    (throw+ {:type ::unknown-source-key :key k :known-keys (keys core/*srcs*)}))

  (when-not (satisfies? LiveSource (get-in core/*srcs* [k :src]))
    (throw+ {:type ::not-a-live-source :key k}))

  (when (nil? (get @core/state k))
    (swap! core/state assoc k (atom nil)))

  (when (nil? (get live-chans k))
    (swap! live-chans assoc k (async/chan 1000)))

  (let [feed (get core/*srcs* k)
        src (get feed :src)
        state (get @core/state k)
        chan (get @live-chans k)]
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
        state (get @core/state k)
        chan (get @live-chans k)
        new-state (stop-collecting! src state)]
    (async/toggle live-mix {chan {:mute true
                                  :pause false
                                  :solo false}}))
  (:status @(get @core/state k)))



;; :hn-best {:src (src/hn "beststories")
;;           :proc (proc/make
;;                      :filter (fn [item]
;;                                (->> item
;;                                    :hn :score (> 200)))
;;
