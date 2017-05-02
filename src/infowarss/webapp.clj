(ns infowarss.webapp
  (:require
   [infowarss.apis.feedbin :as feedbin]
   [infowarss.apis.fever :as fever]
   [ring.middleware params keyword-params json stacktrace lint reload basic-authentication]))

;;;; Web APIs

;;; Rudimentary feedbin api - does not really work
(def feedbin-app
  (->
    feedbin/feedbin-api
    (ring.middleware.basic-authentication/wrap-basic-authentication feedbin/api-authenticated? "API")
    ring.middleware.json/wrap-json-response
    ring.middleware.keyword-params/wrap-keyword-params
    ring.middleware.params/wrap-params
    ring.middleware.lint/wrap-lint))


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
