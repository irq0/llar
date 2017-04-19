(ns infowarss.live
  (:require
   [infowarss.fetch :refer [Hash Metadata Summary]]
   [schema.core :as s]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [clj-time.format :as tf]
   [clojure.java.io :as io]
   [clojure.core.async :refer [>!! <!!] :as async]
   [clojure.set :refer [map-invert]]
   [taoensso.timbre :as log])
  (:import (com.firebase.client Firebase ValueEventListener DataSnapshot FirebaseError Logger Logger$Level)))



(defn make-firebase-logger [level]
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

(defn init-firebase []
  (Firebase/setDefaultConfig
    (doto (Firebase/getDefaultConfig)
      (.setLogger (make-firebase-logger :debug)))))

(def hacker-news-base-url "https://hacker-news.firebaseio.com/v0/")

(defn make-value-event-listener [on-data-change]
  (reify ValueEventListener
    (^void onDataChange [_ ^DataSnapshot snap]
     (log/spy snap)
     (on-data-change (.getValue snap)))
    (^void onCancelled [_ ^FirebaseError err]
     (log/error "Firebase error:" err))))

(defn get-value [ref]
  (let [p (promise)
        listener (make-value-event-listener (partial deliver p))]
    (.addListenerForSingleValueEvent ref listener)
    @p))

(defn listen-value-changes [ref f]
  (let [listener (make-value-event-listener f)]
    (.addValueEventListener ref listener)))

(defn make-ref [base]
  (Firebase. base))

(defn make-path [ref & path]
  (reduce #(.child %1 (str %2)) ref path))

(def HackerNewsEntry
  {:score s/Int
   :author s/Str
   :id s/Int
   :pub-ts (s/maybe org.joda.time.DateTime)
   :title s/Str
   :type s/Str
   :url (s/maybe java.net.URL) ;; something
   :hn-url (s/maybe java.net.URL) ;; https://news.ycombinator.com/item?id=
   :contents {(s/required-key "text/plain") (s/maybe s/Str)
              (s/optional-key "text/html") s/Str}})

(s/defrecord HackerNewsItem
    [meta :- Metadata
     summary :- Summary
     hash :- Hash
     entry :- HackerNewsEntry])

(defn make-meta [src]
  {:source src
   :app "infowarss.live"
   :fetch-ts (time/now)
   :tags #{}
   :version 0})

(s/defn make-hn-summary :- Summary
  [hn-entry :- HackerNewsEntry]
  (let [{:keys [title pub-ts]} hn-entry]
    {:ts pub-ts
     :title title}))

(defn make-hn-entry [item]
  {:score (get item "score")
     :author (get item "by")
     :id (get item "id")
     :pub-ts (tc/from-long (get item "time"))
     :title (get item "title")
     :type (keyword (get item "type"))
     :url (io/as-url (get item "url"))
     :hn-url (io/as-url (str "https://news.ycombinator.com/item?id=" (get item "id")))
     :contents {"text/plain" (get item "text")}})


(s/defn get-hn-entry :- HackerNewsEntry
  [id :- s/Int]
  (let [ref (-> (make-ref hacker-news-base-url)
              (make-path "item" id))
        raw (get-value ref)]
    (make-hn-entry raw)))

(defprotocol LiveSource
  "Protocol to work with live sources"
  (start-collecting! [src chan])
  (status? [src])
  (stop-collecting! [src]))

(defn- hn-item-resolver
  "Deref item to HackerNewsEntry"
  [hn-ref id]
  (log/debug "Resolving id:" id)
  (let [entry (-> (make-path hn-ref "item" id)
                get-value
                make-hn-entry)]
    (log/spy entry)))

(defn- hn-resolver-thread
  "Resolve ids from in-chan to HackerNewsEntries on out-chan"
  [hn-ref in-chan out-chan]
  (async/thread
    (loop []
      (let [id (<!! in-chan)]
        (if-not (= id :stop)
          (do
            (async/put! out-chan
              (hn-item-resolver hn-ref id))
            (recur))
          (log/debug "Stopping resolver"))))))

(extend-protocol LiveSource
  infowarss.src.HackerNews
  (start-collecting! [src item-chan]
    (let [story-feed (-> src :story-feed)
          state @(get src :state)
          hn (make-ref hacker-news-base-url)
          ref (make-path hn story-feed)]
      (when-not (= (:status state) :running)
        (let [resolve-chan (async/chan 1000)
              resolver-thread (hn-resolver-thread hn resolve-chan item-chan)

              listener-f (fn [ids]
                           (log/infof "HackerNews feed %s update: %s items"
                             story-feed (count ids))
                           (swap! (:state src)
                             (fn [s] (assoc s :last-update-ts (time/now))))
                           (doseq [id ids]
                             (async/put! resolve-chan id)))
              listener (listen-value-changes ref listener-f)]

          (reset! (:state src) {:firebase {:listener listener
                                           :hn-ref hn
                                           :resolve-chan resolve-chan
                                           :story-ref ref}
                         :start-ts (time/now)
                         :status :running
                         :last-update-ts nil})))))
  (status? [src]
    (let [state @(get src :state)]
      (:status state)))

  (stop-collecting! [src]
    (let [state @(get src :state)
          listener (get-in state [:firebase :listener])
          ref (get-in state [:firebase :story-ref])]
      (when (= (:status state) :running)
        (.removeEventListener ref listener)
        (>!! (get-in state [:firebase :resolve-chan]) :stop)
        (swap! (:state src) (fn [s] (merge s {:status :stopped})))))))

;; newstories
;; topstories
;; beststories
;; showstories
;; askstories

(defn start-live [feed]

  )

;; :hn-best {:live (firebase/hn "beststories")
;;           :proc (proc/make
;;                      :filter (fn [item]
;;                                (->> item
;;                                    :hn :score (> 200)))
;;
