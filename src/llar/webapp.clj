(ns llar.webapp
  (:require
   [mount.core :refer [defstate]]
   [clojure.tools.logging :as log]
   [llar.metrics :as metrics]
   [llar.apis.dashboard :as dashboard]
   [llar.apis.reader :as reader]
   [llar.appconfig :refer [appconfig]]
   [slingshot.slingshot :refer [try+]]
   [ring.adapter.jetty :refer [run-jetty]]
   [clj-stacktrace.core :as stacktrace]
   [clj-stacktrace.repl :as stacktrace-repl]
   [clojure.string :as string]
   [hiccup2.core :as h]
   [iapetos.collector.ring :refer [wrap-instrumentation]]
   [ring.middleware params gzip keyword-params json stacktrace lint not-modified]))

(defn exception-response [request ex]
  (let [th (stacktrace/parse-exception ex)]
    {:status 500
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (str (h/html
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
                  (dashboard/pprint-html request)
                  [:h4 "Version"]
                  [:pre (str "llar " (get appconfig :version))]
                  [:h4 "Exception"]
                  [:p "Message: " [:pre (ex-message ex)]]
                  [:p "Cause: " [:pre (ex-cause ex)]]
                  [:p "Exception Class: " [:pre (class ex)]]
                  [:p "Data: " (dashboard/pprint-html (ex-data ex))]
                  [:h4 "Exception Chain"]
                  [:pre (get-in ex [:object :message])]
                  [:ol
                   (for [s (:trace-elems th)
                         :let [formatted (stacktrace-repl/pst-elem-str false s 70)]]
                     [:li [:pre formatted]])]]))}))

(defn wrap-exception [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception ex
        (log/error ex "Exception during request" request)
        (exception-response request ex)))))

(defn dashboard-app []
  (->
   dashboard/app
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

(defn try-start-jetty [app port]
  (try+
   (run-jetty app {:port port :join? false})
   (catch java.net.BindException e
     (log/error e "failed to start jetty" app port))))

(defn try-stop-app [jetty]
  (when-not (nil? jetty)
    (.stop jetty)))

(defstate dashboard
  :start (if-let [port (get-in appconfig [:api :dashboard :port])]
           (do (try-start-jetty (dashboard-app) port)
               (log/infof "dashboard started on http://localhost:%s prometheus endpoint http://localhost:%s/metrics" port port))
           (log/info "dashboard disabled in appconfig"))
  :stop (try-stop-app dashboard))

(defstate ^{:depends-on [metrics/prom-registry reader/frontend-db]} reader
  :start (if-let [port (get-in appconfig [:api :reader :port])]
           (do (try-start-jetty (reader-app metrics/prom-registry) (get-in appconfig [:api :reader :port]))
               (log/infof "reader started: http://localhost:%s/reader" port))
           (log/info "reader disabled in appconfig"))
  :stop (try-stop-app reader))
