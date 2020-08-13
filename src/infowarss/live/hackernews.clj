(ns infowarss.live.hackernews
  (:require
   [infowarss.schema :as schema]
   [infowarss.postproc :as postproc]
   [infowarss.persistency :as persistency]
   [infowarss.live.common :refer :all]
   [infowarss.live.firebase :refer :all]
   [hiccup.core :refer [html]]
   [schema.core :as s]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [clojure.java.io :as io]
   [slingshot.slingshot :refer [try+]]
   [clojure.core.async :refer [>!! <!!] :as async]
   [taoensso.timbre :as log]))


;;;; Hacker News Firebase API live source

(def hacker-news-base-url "https://hacker-news.firebaseio.com/v0/")

(defn hn-item-to-string [item]
  (format "[%s: %s/%s/%s/%s]"
    (.getSimpleName (class item))
    (str (get-in item [:meta :source]))
    (if-not (nil? (get-in item [:summary :ts]))
      (tc/to-string (get-in item [:summary :ts]))
      "?")
    (str (get-in item [:summary :title]))
    (str (get-in item [:entry :id]))))


(s/defrecord HackerNewsItem
    [meta :- schema/Metadata
     summary :- schema/Summary
     hash :- schema/Hash
     raw :- s/Any
     entry :- schema/HackerNewsEntry]
  Object
  (toString [item] (hn-item-to-string item)))


(extend-protocol postproc/ItemProcessor
  HackerNewsItem
  (post-process-item [item src state]
    (assoc-in item [:meta :canary] :fooo))
  (filter-item [item src state] false))

(extend-protocol persistency/CouchItem
  HackerNewsItem
  (to-couch [item]
    (-> item
      (dissoc :raw)
      (assoc-in [:meta :source :args] nil)
      (assoc :type :link))))

(s/defn make-hn-summary :- schema/Summary
  [hn-entry :- schema/HackerNewsEntry]
  (let [{:keys [title pub-ts]} hn-entry]
    {:ts pub-ts
     :title title}))

(defn hn-html-summary [item]
  (html
    [:h1 (get item "title")]
    [:div {:class "summary"}
     [:ul
      [:li [:span {:class "key"} "Score: "] (get item "score")]
      [:li [:span {:class "key"} "Time: "] (tc/to-string (tc/from-long (* 1000 (get item "time"))))]
      [:li [:span {:class "key"} "Type: "] (get item "type")]]]
    [:div {:class "links"}
     [:ul
      [:li [:span {:class "key"} "URL: "]
       [:a {:href (get item "url")} (get item "url")]]
      [:li [:span {:class "key"} "Comments: "]
       [:a {:href (str "https://news.ycombinator.com/item?id=" (get item "id"))}
        (str "https://news.ycombinator.com/item?id=" (get item "id"))]]]]))

(defn make-hn-entry [item]
  (let [hn-url(io/as-url (str "https://news.ycombinator.com/item?id=" (get item "id")))]
    {:score (get item "score")
     :author (get item "by")
     :id (get item "id")
     :pub-ts (tc/from-long (* 1000 (get item "time")))
     :title (get item "title")
     :type (keyword (get item "type"))
     :url (if (some? (get item "url"))
            (io/as-url (get item "url"))
            hn-url)
     :hn-url hn-url
     :contents {"text/plain" (str (get item "title") "\n"  (get item "text"))
                "text/html" (hn-html-summary item)}}))

(s/defn get-hn-entry :- schema/HackerNewsEntry
  [id :- schema/PosInt]
  (let [ref (make-path (make-ref hacker-news-base-url) "item" id)
        raw (get-value ref)]
    (make-hn-entry raw)))

(defn- hn-item-resolver
  "Deref item to HackerNewsEntry"
  [hn-ref src state id]
  (log/tracef "HackerNews (%s): resolving id %s" hn-ref id)
  (let [path (make-path hn-ref "item" id)
        raw (get-value path)
        entry (make-hn-entry raw)]
    (try+
      (->HackerNewsItem
        (make-meta src @state)
        (make-hn-summary entry)
        (str "SHA-256:" (digest/sha-256 (str id)))
        raw
        entry)
      (catch Object e
        (log/error e "HackerNews resolver failed -> terminating source")
        (async/put! (get-in @state [:firebase :resolve-term-chan]) :terminate)
        (swap! state assoc :status :failed)
        (swap! state assoc-in [:firebase :last-exception] e)))))

(defn- hn-resolver-thread
  "Resolve ids from in-chan to HackerNewsEntries on out-chan"
  [resolver in-chan term-chan out-chan]
  (async/thread
    (log/debug "HackerNews: Starting resolver thread")
    (loop []
      (let [[v ch] (async/alts!! [in-chan term-chan])]
        (if (identical? ch in-chan)
          (when (number? v)
            (async/put! out-chan (resolver v))
            (recur)))))
    (log/debug "HackerNews: Stopping resolver" resolver)))

(extend-protocol LiveSource
  infowarss.src.HackerNews
  (start-collecting! [src state item-chan]
    (let [story-feed (:story-feed src)
          status (:status @state)
          hn (make-ref hacker-news-base-url)
          ref (make-path hn story-feed)]

      (if-not (= status :running)
        (let [resolve-chan (async/chan (async/sliding-buffer 1000))
              resolve-term-chan (async/chan)
              resolver-thread (hn-resolver-thread (partial hn-item-resolver hn src state) resolve-chan resolve-term-chan item-chan)

              listener-f (fn [ids]
                           (try+
                             (let [next-update-ok (when-not (nil? (get @state :last-update-ts))
                                                    (time/plus (get @state :last-update-ts)
                                                      (time/seconds (or (get-in src [:args :throttle-secs]) 60))))]
                               (if (or (nil? next-update-ok)
                                     (time/after? (time/now) next-update-ok))
                                 (do
                                   (log/debugf "HackerNews feed %s update (last: %s): %s items"
                                     story-feed (get @state :last-update-ts) (count ids))
                                   (swap! state assoc :last-update-ts (time/now))
                                   (doseq [id ids]
                                     (async/put! resolve-chan id)))
                                 (log/debugf "HackerNews feed %s update skipped (last: %s, next: %s)"
                                   story-feed (get @state :last-update-ts) next-update-ok)))
                             (catch Object e
                               (swap! state assoc :last-exception e)
                               (log/error e "Exception in listener"))))

              listener (listen-value-changes ref listener-f)]

          (swap! state merge {:firebase {:listener listener
                                         :resolve-term-chan resolve-term-chan
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
          (>!! (get-in @state [:firebase :resolve-term-chan]) :stop)
          (swap! state merge {:status :stopped}))
        @state))))
