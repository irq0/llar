(ns infowarss.persistency
  (:require
   [infowarss.couchdb :as couch]
   [infowarss.fetch]
   [digest]
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string])
  (:import [java.util.Base64.Encoder]
           [infowarss.fetch FeedItem]
           ))

(defprotocol StorableItem
  "Protocol to work with items (fetched by a source)"
  (store-item! [item] "Process new item")
  (duplicate? [item] "Already processed?"))

(extend-protocol StorableItem
  FeedItem
  (duplicate? [item]
    (let [resp (couch/lookup-hash (str "SHA-256:" (:hash item)))]
      (seq (get-in resp [:body :rows]))))

  (store-item! [item]
    (let [contents (get-in item [:feed-entry :contents])
          source (get item :source)
          atts (into {} (for [[content-type data] contents]
                          (when-not (nil? data)
                            [(str "content" "."
                               (get couch/attachment-extensions content-type))
                             {:content_type content-type
                              :data (.encodeToString
                                      (java.util.Base64/getMimeEncoder)
                                      (.getBytes data))}])))
          doc (-> {:meta (:meta item)
                  :summary (:summary item)
                  :type :feed
                  :hash (str "SHA-256:" (:hash item))
                  :feed-entry (:feed-entry item)
                  :feed (:feed item)
                  :source {:url (:url source)
                           :title (:title source)
                           :postproc (some? (:postproc source))}}
                (assoc-in [:feed-entry :contents] nil))]
      (log/spy doc)
      (log/spy atts)
      (couch/add-document! (if (empty? atts)
                             doc
                             (assoc doc "_attachments" atts))))))

(defn- store-item-skip-duplicate! [item]
  (if (duplicate? item)
    (log/infof "Skipping item \"%s\": duplicate"
      (get-in item [:summary :title]))
    (let [{:keys [id]} (store-item! item)]
      (log/infof "Stored item \"%s\": %s"
        (get-in item [:summary :title]) id)
      id)))

(defn store-items! [mixed-items]
  ;; Each vector may contain multiple item types.
  ;; -> Group them by type and call the store method
  (let [by-type (group-by type mixed-items)]
    (log/infof "Processing %d items with types: %s"
      (count mixed-items) (keys by-type))
    (doall
      (apply concat
        (for [[type items] by-type]
          (do (log/infof "Processing %s items" type)
              (remove nil? (map #(store-item-skip-duplicate! %) items ))))))))
