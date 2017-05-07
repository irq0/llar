(ns infowarss.persistency
  (:require
   [infowarss.couchdb :as couch]
   [infowarss.fetch]
   [digest]
   [clj-time.coerce :as tc]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [cheshire.generate :refer [add-encoder encode-map]]
   [pantomime.mime :as pm]
   [clojure.string :as string])
  (:import [java.util.Base64.Encoder]
           [infowarss.fetch FeedItem TweetItem]
           ))

;;;; Persist *Items to couchdb

;;; Extra cheshire encoders

(add-encoder org.joda.time.DateTime
  (fn [dt jg]
    (.writeString jg (tc/to-string dt))))

(add-encoder java.net.URL
  (fn [dt jg]
    (.writeString jg (str dt))))

;;; Item -> Persistency abstraction
;;; Split into two parts:
;;; CouchItem proto: Convert item to couchdb document
;;; StorableItem: store / overwrite / check if duplicate

(defprotocol StorableItem
  "Protocol to work with items (fetched by a source)"
  (store-item! [item] "Process new item" )
  (overwrite-item! [item] "Overwrite existing item")
  (duplicate? [item] "Already processed?"))

(defprotocol CouchItem
  (to-couch [item] "Convert item to database form"))

(defn- base64-enc [s]
  (.encodeToString
    (java.util.Base64/getMimeEncoder)
    (.getBytes s)))

(defn extension-for-mimetype [m]
  (let [by-mime (pm/extension-for-name m)]
    (if (string/blank? by-mime)
      ".bin"
      by-mime)))

(defn to-couch-atts
  "Convert feed entry contents to couch attachments. Name will be prefix +
  extension based on content type"
  [prefix contents]
  (into {} (for [[content-type data] contents
                 :let [extension (extension-for-mimetype content-type)]]
             (when-not (nil? data)
               [(str prefix extension)
                {:content_type content-type
                 :data (base64-enc data)}]))))

(extend-protocol CouchItem
  FeedItem
  (to-couch [item]
    (let [atts (to-couch-atts "content" (get-in item [:entry :contents]))]
      (cond->
          (-> item
            (dissoc :raw)
            (assoc :type :feed)
            (assoc-in [:entry :contents] nil))
        (seq atts) (assoc "_attachments" atts))))
  TweetItem
  (to-couch [item]
    (let [atts (to-couch-atts "content" (get-in item [:entry :contents]))]
      (cond->
          (-> item
            (dissoc :raw)
            (assoc :type :tweet)
            (assoc-in [:meta :source :oauth-creds] nil)
            (assoc-in [:entry :contents] nil))
        (seq atts) (assoc "_attachments" atts)))))

(extend-protocol StorableItem
  FeedItem
  (duplicate? [item]
    (let [resp (couch/lookup-hash (:hash item))]
      (seq (get-in resp [:body :rows]))))

  (overwrite-item! [item]
    (let [doc (to-couch item)
          resp (couch/lookup-hash (:hash item))
          id (-> (get-in resp [:body :rows]) first :id)]
      (when (string? id)
        (couch/swap-document! id (fn [_] doc)))))


  (store-item! [item]
    (let [doc (to-couch item)]
      (couch/add-document! doc)))

  TweetItem
  (duplicate? [item]
    (let [resp (couch/lookup-hash (:hash item))]
      (seq (get-in resp [:body :rows]))))

  (overwrite-item! [item]
    (let [doc (to-couch item)
          resp (couch/lookup-hash (:hash item))
          id (-> (get-in resp [:body :rows]) first :id)]
      (when (string? id)
        (couch/swap-document! id (fn [_] doc)))))

  (store-item! [item]
    (let [doc (to-couch item)]

      (couch/add-document! doc))))


(defn- store-item-skip-duplicate!
  [item & {:keys [overwrite?] :or [overwrite? false]}]
  (let [name (get-in item [:meta :source-name])
        title (get-in item [:summary :title])
        dup? (duplicate? item)]
    (cond
      (and dup? overwrite?)
      (let [{:keys [id]} (overwrite-item! item)]
        (log/debugf "Overwritten item %s/\"%s\": %s" name title id)
        id)
      dup?
      (log/debugf "Skipping item %s/\"%s\": duplicate" name title)
      :else
      (let [{:keys [id]} (store-item! item)]
        (log/debugf "Stored item %s/\"%s\": %s" name title id)
        id)
    )))

;;; API

(defn store-items!
  "Store items (may be a mixture of different types). Pass overwrite? true to
  overwrite existing items"
  [mixed-items & {:keys [overwrite?]
                  :or [overwrite? false]
                  :as args}]
  ;; Each vector may contain multiple item types.
  ;; -> Group them by type and call the store method
  (let [by-type (group-by type mixed-items)]
    (log/debugf "Persisting %d items with types: %s"
      (count mixed-items) (keys by-type))
    (doall
      (apply concat
        (for [[type items] by-type]
          (do (log/debugf "Persisting %s items" type)
              (remove nil? (map #(store-item-skip-duplicate! % :overwrite? overwrite?) items ))))))))
