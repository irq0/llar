(ns infowarss.live.hackernews
  (:require
   [infowarss.schema :as schema]
   [infowarss.core :refer [state]]
   [infowarss.live.firebase :refer :all]
   [infowarss.live.common :refer :all]
   [schema.core :as s]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [clj-time.format :as tf]
   [clojure.java.io :as io]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.core.async :refer [>!! <!!] :as async]
   [taoensso.timbre :as log]))


;;;; Hacker News Firebase API live source

(def hacker-news-base-url "https://hacker-news.firebaseio.com/v0/")

(s/defrecord HackerNewsItem
    [meta :- schema/Metadata
     summary :- schema/Summary
     hash :- schema/Hash
     raw :- s/Any
     entry :- schema/HackerNewsEntry]
  Object
  (toString [item] (item-to-string item)))


(s/defn make-hn-summary :- schema/Summary
  [hn-entry :- schema/HackerNewsEntry]
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

(s/defn get-hn-entry :- schema/HackerNewsEntry
  [id :- schema/PosInt]
  (let [ref (make-path (make-ref hacker-news-base-url) "item" id)
        raw (get-value ref)]
    (make-hn-entry raw)))

(defn- hn-item-resolver
  "Deref item to HackerNewsEntry"
  [hn-ref src id]
  (log/tracef "HackerNews (%s): resolving id %s" hn-ref id)
  (let [path (make-path hn-ref "item" id)
        raw (get-value path)
        entry (make-hn-entry raw)]
    {:meta (make-meta src)
     :summary (make-hn-summary entry)
     :hash (str "SHA-256:" (digest/sha-256 (str id)))
     :raw raw
     :entry entry}))

(defn- hn-resolver-thread
  "Resolve ids from in-chan to HackerNewsEntries on out-chan"
  [resolver in-chan out-chan]
  (async/thread
    (loop []
      (let [id (<!! in-chan)]
        (if-not (= id :stop)
          (do
            (async/put! out-chan (resolver id))
            (recur))
          (log/debug "HackerNews: Stopping resolver" resolver))))))

(extend-protocol LiveSource
  infowarss.src.HackerNews
  (start-collecting! [src state item-chan]
    (let [story-feed (:story-feed src)
          status (:status @state)
          hn (make-ref hacker-news-base-url)
          ref (make-path hn story-feed)]
      (when (nil? status)
        (reset! state state-template))

      (if-not (= status :running)
        (let [resolve-chan (async/chan 1000)
              resolver-thread (hn-resolver-thread (partial hn-item-resolver hn src) resolve-chan item-chan)

              listener-f (fn [ids]
                           (try+
                             (let [next-update-ok (time/plus (get @state :last-update-ts)
                                                  (time/seconds (or (:throttle-secs src) 60)))]
                             (if (time/after? (time/now) next-update-ok)
                               (do
                                 (log/debugf "HackerNews feed %s update (last: %s): %s items"
                                   story-feed (get @state :last-update-ts) (count ids))
                                 (swap! state assoc :last-update-ts (time/now))
                                 (doseq [id ids]
                                   (async/put! resolve-chan id)))
                               (log/debugf "HackerNews feed %s update skipped (last: %s)"
                                 story-feed (get @state :last-update-ts) (count ids))))
                             (catch Object e
                               (swap! state assoc :last-exception e)
                               (log/error e "Exception in listener"))))

              listener (listen-value-changes ref listener-f)]

          (swap! state merge {:firebase {:listener listener
                                         :resolve-chan resolve-chan
                                         :story-ref ref}
                              :start-ts (time/now)
                              :status :running}))
        @state)))

  (stop-collecting! [src state]
    (let [status (:status @state)
          listener (get-in @state [:firebase :listener])
          ref (get-in @state [:firebase :story-ref])]
      (if (= status :running)
        (do
          (.removeEventListener ref listener)
          (>!! (get-in @state [:firebase :resolve-chan]) :stop)
          (swap! state merge {:status :stopped}))
        @state))))
