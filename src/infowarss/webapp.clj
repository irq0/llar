(ns infowarss.webapp
  (:require
   [taoensso.timbre :as log]
   [infowarss.apis.status :as status]
   [infowarss.apis.reader :as reader]
   [infowarss.apis.dataworkbench :as datawb]
   [slingshot.slingshot :refer [throw+ try+]]
   [ring.adapter.jetty :refer [run-jetty]]
   [mount.core :refer [defstate]]
   [hiccup.core :refer [html]]
   [infowarss.apis.fever :as fever]
   [ring.middleware params gzip keyword-params json stacktrace lint basic-authentication not-modified]))
;;;; Web APIs

(defn exception-response [ex]
  (let [th (Throwable->map ex)]
    {:status 500

     :body (html
            [:head
             [:meta {:charset "utf-8"}]
             [:title "500 Internal Server Error"]
             [:link {:rel "stylesheet" :href "/static/css/bootstrap.min.css"}]
             [:link {:rel "stylesheet" :href "/static/fonts/fira/fira.css"}]
             [:link {:rel "stylesheet" :href "/static/fonts/charter/webfonts/stylesheet.css"}]
             [:link {:rel "stylesheet" :href "/static/css/my.css"}]]
            [:body
             [:h1 "😭"]
             [:h2 "Internal Server Error"]
             [:h4 "Exception Chain"]
             (status/html-exception-chain th)
             [:h4 "Stack Trace"]
             (status/html-stack-trace th)])}))


(defn wrap-exception [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception ex
        (log/error ex "Exception during request" request)
        (exception-response ex)))))

;;; Fever API - https://feedafever.com/api
(def fever-app
  (->
   fever/fever-api
   fever/wrap-auth
   ring.middleware.json/wrap-json-response
   ring.middleware.keyword-params/wrap-keyword-params
   ring.middleware.params/wrap-params
   wrap-exception
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
   wrap-exception
   ring.middleware.lint/wrap-lint))

(def reader-app
  (->
   reader/app
   ring.middleware.json/wrap-json-response
   ring.middleware.keyword-params/wrap-keyword-params
   ring.middleware.params/wrap-params
   ring.middleware.json/wrap-json-params
   ring.middleware.gzip/wrap-gzip
   ring.middleware.not-modified/wrap-not-modified
   wrap-exception))


(def data-app
  (->
   datawb/app
   ring.middleware.json/wrap-json-response
   ring.middleware.keyword-params/wrap-keyword-params
   ring.middleware.params/wrap-params
   ring.middleware.json/wrap-json-params
   ring.middleware.gzip/wrap-gzip
   ring.middleware.not-modified/wrap-not-modified
   wrap-exception
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
  :start (try-start-app status-app 9999)
  :stop (try-stop-app status))

(defstate reader
  :start (try-start-app reader-app 8023)
  :stop (try-stop-app reader))

(defstate datawb
  :start (try-start-app data-app 8042)
  :stop (try-stop-app data-app))
