(ns llar.webapp
  (:require
   [clojure.tools.logging :as log]
   [llar.metrics :as metrics]
   [llar.apis.dashboard :as status]
   [llar.apis.reader :as reader]
   [slingshot.slingshot :refer [try+]]
   [ring.adapter.jetty :refer [run-jetty]]
   [clj-stacktrace.core :as stacktrace]
   [clj-stacktrace.repl :as stacktrace-repl]
   [clojure.string :as string]
   [mount.core :refer [defstate]]
   [hiccup.core :refer [html]]
   [iapetos.collector.ring :refer [wrap-instrumentation]]
   [ring.middleware params gzip keyword-params json stacktrace lint not-modified]))

(defn exception-response [request ex]
  (let [th (stacktrace/parse-exception ex)]
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
             [:h4 "Request"]
             (status/pprint-html request)
             [:h4 "Exception"]
             [:p "Message: " [:pre (ex-message ex)]]
             [:p "Cause: " [:pre (ex-cause ex)]]
             [:p "Exception Class: " [:pre (class ex)]]
             [:p "Data: " (status/pprint-html (ex-data ex))]
             [:h4 "Exception Chain"]
             [:pre (get-in ex [:object :message])]
             [:ol
              (for [s (:trace-elems th)
                    :let [formatted (stacktrace-repl/pst-elem-str false s 70)]]
                [:li [:pre formatted]])]])}))

(defn wrap-exception [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception ex
        (log/error ex "Exception during request" request)
        (exception-response request ex)))))

(defn dashboard-app []
  (->
   status/app
   ring.middleware.json/wrap-json-body
   ring.middleware.json/wrap-json-response
   ring.middleware.keyword-params/wrap-keyword-params
   ring.middleware.params/wrap-params
   ring.middleware.json/wrap-json-params
   ring.middleware.gzip/wrap-gzip
   ring.middleware.not-modified/wrap-not-modified
   wrap-exception
   ring.middleware.lint/wrap-lint))

(def +prom-shorten-path-of+
  ["/blob" "/static" "/reader/item/by-id/"])

(defn prom-path-fn [request]
  (if-let [prefix (some #(when (string/starts-with? (:uri request) %) %) +prom-shorten-path-of+)]
    prefix
    (:uri request)))

(defn reader-app [prom-registry]
  (->
   reader/app
   ring.middleware.json/wrap-json-response
   ring.middleware.keyword-params/wrap-keyword-params
   ring.middleware.params/wrap-params
   ring.middleware.json/wrap-json-params
   ring.middleware.gzip/wrap-gzip
   ring.middleware.not-modified/wrap-not-modified
   wrap-exception
   (wrap-instrumentation prom-registry {:path-fn prom-path-fn})))

(defn try-start-app [app port]
  (try+
   (run-jetty app {:port port :join? false})
   (catch java.net.BindException e
     (log/error e "Failed to start jetty" app port))))

(defn try-stop-app [jetty]
  (when-not (nil? jetty)
    (.stop jetty)))

(defstate dashboard
  :start (try-start-app (dashboard-app) 9999)
  :stop (try-stop-app dashboard))

(defstate reader
  :start (try-start-app (reader-app metrics/prom-registry) 8023)
  :stop (try-stop-app reader))
