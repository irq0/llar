(ns infowarss.repl
  (:require
   [infowarss.core :refer :all]
   [infowarss.persistency :as persistency :refer [store-items! duplicate?]]
   [infowarss.couchdb :as couch]
   [infowarss.update :refer :all]
   [infowarss.webapp :as webapp]
   [infowarss.src :as src]
   [infowarss.fetch :as fetch]
   [infowarss.postproc :as proc]
   [infowarss.live :as live]
   [infowarss.schema :as schema]
   [clj-http.client :as http]
   [slingshot.slingshot :refer [throw+ try+]]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [clj-time.format :as tf]
   [taoensso.timbre :as log]
   [table.core :refer [table]]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [postal.core :as postal]
   [schema.core :as s]
   [cheshire.core :as json]
   [twitter.oauth :as twitter-oauth]
   [twitter.api.restful :as twitter]
   [ring.adapter.jetty :refer [run-jetty]]
   [taoensso.timbre.appenders.core :as appenders]))

(s/set-fn-validation! true)

(defn format-interval [period]
  (let [formatter (some-> (org.joda.time.format.PeriodFormatterBuilder.)
                    .printZeroNever
                    .appendDays
                    (.appendSuffix "d")
                    .appendHours
                    (.appendSuffix "h")
                    .appendMinutes
                    (.appendSuffix "m")
                    .printZeroAlways
                    .appendSeconds
                    (.appendSuffix "s")
                    .toFormatter)]
    (.print formatter period)))

(defn period-since-now [ts]
  (.toPeriod (time/interval ts (time/now))))


(defn- human-src
  "Extract interesting informations from source data structure"
  [[k v]]
  (let [base {:key k
              :name (str (get v :src))
              :status (get-in @*state* [k :status])
              :last-attempt (some-> (get-in @*state* [k :last-attempt-ts])
                              period-since-now
                              format-interval
                              (str " ago"))
              :last-success (some-> (get-in @*state* [k :last-successful-fetch-ts])
                              period-since-now
                              format-interval
                              (str " ago"))}]

    (if (#{:perm-fail :temp-fail} (:status base))
      (assoc base :last-exception-msg
        (get-in @*state* [k :last-exception :message]))
      base)))

(defn sources
  "Return list of sources for human consumption"
  []
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
