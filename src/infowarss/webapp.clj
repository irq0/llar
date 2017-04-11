(ns infowarss.webapp
  (:require
   [infowarss.apis.feedbin :as feedbin]
   [ring.middleware params keyword-params json stacktrace lint reload basic-authentication]
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]
   [slingshot.slingshot :refer [throw+ try+]]
   [compojure.route :as route]
   [ring.adapter.jetty :refer [run-jetty]]
   [cheshire.generate :refer [add-encoder encode-map]])
  (:import [java.util.Base64.Encoder]))

(def app
  (->
    ;; fever-api-handler
    ;;  wrap-json-response
    ;;  wrap-fever-auth
    feedbin/feedbin-api
    (ring.middleware.basic-authentication/wrap-basic-authentication feedbin/api-authenticated? "API")
    ring.middleware.reload/wrap-reload
    ring.middleware.json/wrap-json-response
    ring.middleware.keyword-params/wrap-keyword-params
    ring.middleware.params/wrap-params
;      ring.middleware.stacktrace/wrap-stacktrace
      ring.middleware.lint/wrap-lint))


(comment
  (defonce jetty (run-jetty #'app {:port 8765 :join? false}))
  (.start jetty)
  (.stop jetty))
