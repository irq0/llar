(ns infowarss.persistency
  (:require
   [infowarss.db :as db]
   [infowarss.fetch]
   [infowarss.converter :as conv]
   [digest]
   [clj-time.coerce :as tc]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [cheshire.generate :refer [add-encoder encode-map]]
   [pantomime.mime :as pm]
   [clojure.string :as string]))


;;; Couch is a historical artifact. Before postgresql there was couch :)

;;; Extra cheshire encoders

(add-encoder org.joda.time.DateTime
  (fn [dt jg]
    (.writeString jg (tc/to-string dt))))

(add-encoder java.net.URL
  (fn [dt jg]
    (.writeString jg (str dt))))

(add-encoder java.net.URI
  (fn [dt jg]
    (.writeString jg (str dt))))

;;; Item -> Persistency abstraction
;;; Split into two parts:
;;; CouchItem proto: Convert item to database document
;;; StorableItem: store / overwrite / check if duplicate

(defprotocol CouchItem
  (to-couch [item] "Convert item to database form"))

;;; Attachment Converter

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
                 :data (conv/base64-encode data)}]))))

;;; Write functions

(defn overwrite-item! [item]
  (try+
    (let [doc (to-couch item)]
      (db/inplace-update-document doc))
    (catch java.lang.IllegalAccessException e
      (log/error e "Failed to store item. Probably called with an unsupported record "
        (type item) ":" item)
      (throw+))))


(def last-items (atom []))

(defn store-item! [item]
  (let [doc (to-couch item)]
    (db/add-document doc)))

(defn- store-item-skip-duplicate!
  [item & {:keys [overwrite?] :or {overwrite? false}}]
  (let [name (get-in item [:meta :source-name])
        title (get-in item [:summary :title])]
    (try+
      (swap! last-items conj item)

      (let [{:keys [id]} (store-item! item)]
        (log/debugf "Stored item %s/\"%s\": %s" name title id)
        id)
      (catch [:type :infowarss.db/duplicate] _
        (if overwrite?
          (do
            (let [ret (overwrite-item! item)]
              (log/debugf "Item overwritten %s/\"%s\": %s" name title ret)
              ret))
          (log/debugf "Skipping item %s/\"%s\": duplicate" name title)))
      (catch Object _
        (log/errorf (:throwable &throw-context) "Unexpected exception while storing item %s/\"%s\"" name title)))))
;;; API

(defn store-items!
  "Store items (may be a mixture of different types). Pass overwrite? true to
  overwrite existing items"
  [mixed-items & {:keys [overwrite?]
                  :or {overwrite? false}
                  :as args}]
  ;; Each vector may contain multiple item types.
  ;; -> Group them by type and call the store method
  (let [by-type (group-by type mixed-items)]
    (when (>= (count mixed-items) 1)
      (log/debugf "Persisting %d items with types: %s"
        (count mixed-items) (keys by-type))
      (doall
        (apply concat
          (for [[type items] by-type]
            (do (log/debugf "Persisting %s items" type)
                (remove nil? (map #(store-item-skip-duplicate! % :overwrite? overwrite?) items )))))))))
