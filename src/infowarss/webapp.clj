(ns infowarss.webapp
  (:require
   [infowarss.apis.feedbin :as feedbin]
   [infowarss.apis.fever :as fever]
   [ring.middleware params keyword-params json stacktrace lint reload basic-authentication]
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]
   [slingshot.slingshot :refer [throw+ try+]]
   [compojure.route :as route]
   [cheshire.generate :refer [add-encoder encode-map]])
  (:import [java.util.Base64.Encoder]))

(def feedbin-app
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

(def fever-app
  (->
    fever/fever-api
    fever/wrap-auth
    ring.middleware.reload/wrap-reload
    ring.middleware.json/wrap-json-response
    ring.middleware.keyword-params/wrap-keyword-params
    ring.middleware.params/wrap-params
    ring.middleware.stacktrace/wrap-stacktrace
    ring.middleware.lint/wrap-lint))



(comment
  (defonce jetty (run-jetty #'fever-app {:port 8765 :join? false}))
  (defonce jetty (run-jetty #'feedbin-app {:port 8765 :join? false}))
  (.start jetty)
  (.stop jetty))
