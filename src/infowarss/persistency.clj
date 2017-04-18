(ns infowarss.persistency
  (:require
   [infowarss.couchdb :as couch]
   [infowarss.fetch]
   [digest]
   [clj-time.coerce :as tc]
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]
   [slingshot.slingshot :refer [throw+ try+]]
   [cheshire.generate :refer [add-encoder encode-map]]
   [clojure.string :as string])
  (:import [java.util.Base64.Encoder]
           [infowarss.fetch FeedItem TweetItem]
           ))

(add-encoder org.joda.time.DateTime
  (fn [dt jg]
    (.writeString jg (tc/to-string dt))))

(add-encoder java.net.URL
  (fn [dt jg]
    (.writeString jg (str dt))))

(defprotocol StorableItem
  "Protocol to work with items (fetched by a source)"
  (store-item! [item] "Process new item" )
  (duplicate? [item] "Already processed?"))

(defprotocol CouchItem
  (to-couch [item] "Convert item to database form"))

(defn- base64-enc [s]
  (.encodeToString
    (java.util.Base64/getMimeEncoder)
    (.getBytes s)))

(defn- to-couch-atts
    "Convert feed entry contents to couch attachments"
  [contents]
  (into {} (for [[content-type data] contents]
             (when-not (nil? data)
               [(str "content" "." (get couch/attachment-extensions content-type))
                {:content_type content-type
                 :data (base64-enc data)}]))))

(extend-protocol CouchItem
  FeedItem
  (to-couch [item]
    (let [atts (to-couch-atts (get-in item [:feed-entry :contents]))]
      (cond->
          (-> item
            (assoc :type :feed)
            (assoc-in [:feed-entry :contents] nil))
        (not (empty? atts)) (assoc "_attachments" atts))))
  TweetItem
  (to-couch [item]
    (let [atts (to-couch-atts (get-in item [:entry :contents]))]
      (cond->
          (-> item
            (assoc :type :tweet)
            (assoc-in [:meta :source :oauth-creds] nil)
            (assoc-in [:entry :contents] nil))
        (not (empty? atts)) (assoc "_attachments" atts)))))

(extend-protocol StorableItem
  FeedItem
  (duplicate? [item]
    (let [resp (couch/lookup-hash (:hash item))]
      (seq (get-in resp [:body :rows]))))

  (store-item! [item]
    (let [doc (to-couch item)]
      (log/spy doc)
      (couch/add-document! doc)))

  TweetItem
  (duplicate? [item]
    (let [resp (couch/lookup-hash (:hash item))]
      (seq (get-in resp [:body :rows]))))

  (store-item! [item]
    (let [doc (to-couch item)]
      (log/spy doc)
      (couch/add-document! doc))))


(defn- store-item-skip-duplicate! [item]
  (let [name (get-in item [:meta :source-name])
        title (get-in item [:summary :title])]
    (if (duplicate? item)
      (log/infof "Skipping item %s/\"%s\": duplicate" name title)
      (let [{:keys [id]} (store-item! item)]
        (log/infof "Stored item %s/\"%s\": %s" name title id)
        id))))

(defn store-items! [mixed-items]
  ;; Each vector may contain multiple item types.
  ;; -> Group them by type and call the store method
  (let [by-type (group-by type mixed-items)]
    (log/infof "Persisting %d items with types: %s"
      (count mixed-items) (keys by-type))
    (doall
      (apply concat
        (for [[type items] by-type]
          (do (log/infof "Persisting %s items" type)
              (remove nil? (map #(store-item-skip-duplicate! %) items ))))))))
