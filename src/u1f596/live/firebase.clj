(ns u1f596.live.firebase
  (:require
   [clojure.set :refer [map-invert]]
   [clojure.tools.logging :as log])
  (:import (com.firebase.client Firebase ValueEventListener DataSnapshot FirebaseError Logger Logger$Level)))


;;;; Firebase API clojure wrapper


(defn make-firebase-logger
  "Return a com.firebase.client.Logger that forwards log entries to
  timbre"
  [level]
  (let [firebase-to-taoensso {Logger$Level/DEBUG :debug
                              Logger$Level/INFO :info
                              Logger$Level/WARN :warn
                              Logger$Level/ERROR :error
                              Logger$Level/NONE :info}
        taoensso-to-firebase (map-invert firebase-to-taoensso)
        firebase-level (get taoensso-to-firebase level)]

    (reify Logger
      (^void onLogMessage [_
                           ^Logger$Level level
                           ^String tag
                           ^String message
                           ^long ts]
        (log/log (get firebase-to-taoensso level) tag message))
      (^Logger$Level getLogLevel [_]
        firebase-level))))

(defn init-firebase
  "Initialize and configure logging for firebase API"
  []
  (Firebase/setDefaultConfig
   (doto (Firebase/getDefaultConfig)
     (.setLogger (make-firebase-logger :debug)))))

(defn make-value-event-listener
  "Make com.firebase.client.ValueEventListener. Calls function
  on-data-change on new data and logs errors to timbre"
  [on-data-change]
  (reify ValueEventListener
    (^void onDataChange [_ ^DataSnapshot snap]
      (on-data-change (.getValue snap)))
    (^void onCancelled [_ ^FirebaseError err]
      (log/error "Firebase error:" err))))

(defn get-value
  "Dereference ref and return value"
  [ref]
  (let [p (promise)
        listener (make-value-event-listener (partial deliver p))]
    (.addListenerForSingleValueEvent ref listener)
    @p))

(defn listen-value-changes
  "Register value event listener for ref. Call f on data updates"
  [ref f]
  (let [listener (make-value-event-listener f)]
    (.addValueEventListener ref listener)))

(defn make-ref
  "Make firebase ref from url"
  [base]
  (Firebase. base))

(defn make-path
  "Make firebase ref path"
  [ref & path]
  (reduce #(.child %1 (str %2)) ref path))
