(ns infowarss.repl
  (:require
   [infowarss.core :refer :all]
   [infowarss.postproc :refer [postproc]]
   [infowarss.persistency :as persistency :refer [store-items! duplicate?]]
   [infowarss.couchdb :as couch]
   [infowarss.update :refer :all]
   [infowarss.webapp :as webapp]
   [infowarss.src :as src]
   [infowarss.fetch :as fetch]
   [infowarss.postproc :as postproc]
   [clj-http.client :as http]
   [slingshot.slingshot :refer [throw+ try+]]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [clj-time.format :as tf]
   [taoensso.timbre :as log]
   [table.core :refer [table]]
   [clojure.java.io :as io]
   [schema.core :as s]
   [cheshire.core :as json]
   [ring.adapter.jetty :refer [run-jetty]]
   [taoensso.timbre.appenders.core :as appenders]))

(s/set-fn-validation! true)

(defn- human-src [[k v]]
  "Extract interesting informations from source data structure"
  (let [base {:key k
              :title (get-in v [:src :title])
              :status (get-in v [:state :status])
              :last-success (tc/to-string (get-in v [:state :last-successful-fetch-ts]))}]
    (if (#{:perm-fail :temp-fail} (:status base))
      (assoc base :last-exception-msg
        (get-in v [:state :last-exception :message]))
      base)))

(defn sources []
  "Return list of sources for human consumption"
  (map human-src @*srcs*))

(defn- human-feed-item [i]
  {:src-title (get-in i [:source :title])
   :title (get-in i [:feed-entry :title])
   :link (get-in i [:feed-entry :link])
   :content (get-in i [:feed-entry :contents "text/plain"])})

(defn items-with-tag [tag & {:keys [group]}]
  (let [items (for [id (couch/doc-ids-with-tag tag)]
                (let [doc (couch/get-document-with-attachments id)]
                  (human-feed-item doc)))]
    (if group
      (group-by :src-title items)
      items)))


(comment
  (defonce jetty (run-jetty #'webapp/fever-app {:port 8765 :join? false}))
  (defonce jetty (run-jetty #'feedbin-app {:port 8765 :join? false}))
  (.start jetty)
  (.stop jetty))
