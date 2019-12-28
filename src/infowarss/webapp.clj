(ns infowarss.webapp
  (:require
   [taoensso.timbre :as log]
   [infowarss.apis.status :as status]
   [infowarss.apis.dataworkbench :as datawb]
   [slingshot.slingshot :refer [throw+ try+]]
   [ring.adapter.jetty :refer [run-jetty]]
   [mount.core :refer [defstate]]
   [infowarss.apis.fever :as fever]
   [ring.middleware params gzip keyword-params json stacktrace lint basic-authentication not-modified]))
;;;; Web APIs

;;; Fever API - https://feedafever.com/api
(def fever-app
  (->
    fever/fever-api
    fever/wrap-auth
    ring.middleware.json/wrap-json-response
    ring.middleware.keyword-params/wrap-keyword-params
    ring.middleware.params/wrap-params
    ring.middleware.stacktrace/wrap-stacktrace-log
    ring.middleware.lint/wrap-lint))

(def status-app
  (->
    status/app
    ring.middleware.json/wrap-json-response
    ring.middleware.keyword-params/wrap-keyword-params
    ring.middleware.params/wrap-params
    ring.middleware.json/wrap-json-params
    ring.middleware.gzip/wrap-gzip
    ring.middleware.not-modified/wrap-not-modified
    ring.middleware.stacktrace/wrap-stacktrace-log
    ring.middleware.lint/wrap-lint))

(def data-app
  (->
    datawb/app
    ring.middleware.json/wrap-json-response
    ring.middleware.keyword-params/wrap-keyword-params
    ring.middleware.params/wrap-params
    ring.middleware.json/wrap-json-params
    ring.middleware.gzip/wrap-gzip
    ring.middleware.not-modified/wrap-not-modified
    ring.middleware.stacktrace/wrap-stacktrace-log
    ring.middleware.lint/wrap-lint))


(defn try-start-app [app port]
  (try+
    (run-jetty app {:port port :join? false})
    (catch java.net.BindException e
      (log/error e "Failed to start jetty" app port))))

(defn try-stop-app [jetty]
  (when-not (nil? jetty)
    (.stop jetty)))

(defstate fever
  :start (try-start-app #'fever-app 8765)
  :stop (try-stop-app fever))

(defstate status
  :start (try-start-app status-app 8023)
  :stop (try-stop-app status))

(defstate datawb
  :start (try-start-app data-app 8042)
  :stop (try-stop-app data-app))
