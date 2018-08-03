(ns infowarss.webapp
  (:require
   [infowarss.apis.infowarss :as infowarss]
   [infowarss.apis.status :as status]
   [ring.adapter.jetty :refer [run-jetty]]
   [mount.core :refer [defstate]]
   [infowarss.apis.fever :as fever]
   [ring.middleware params gzip keyword-params json stacktrace lint basic-authentication]))
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

(def infowarss-app
  (->
    infowarss/app
    (ring.middleware.basic-authentication/wrap-basic-authentication infowarss/api-authenticated? "Infowarss API")
    ring.middleware.keyword-params/wrap-keyword-params
    ring.middleware.params/wrap-params
    ring.middleware.stacktrace/wrap-stacktrace-log
    ring.middleware.lint/wrap-lint))

(def status-app
  (->
    status/app
    ring.middleware.keyword-params/wrap-keyword-params
    ring.middleware.params/wrap-params
    ring.middleware.gzip/wrap-gzip
    ring.middleware.stacktrace/wrap-stacktrace-log
    ring.middleware.lint/wrap-lint))


(defstate fever
  :start (run-jetty #'fever-app {:port 8765 :join? false})
  :stop (.stop fever))


(defstate infowarss
  :start (run-jetty #'infowarss-app {:port 7654 :join? false})
  :stop (.stop infowarss))

(defstate status
  :start (run-jetty #'status-app {:port 8023 :join? false})
  :stop (.stop status))
