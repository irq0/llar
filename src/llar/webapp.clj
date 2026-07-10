(ns llar.webapp
  (:require
   [mount.core :refer [defstate]]
   [clojure.tools.logging :as log]
   [llar.metrics :as metrics]
   [llar.apis.dashboard :as dashboard]
   [llar.apis.fever :as fever]
   [llar.apis.podcast :as podcast]
   [llar.apis.reader :as reader]
   [llar.appconfig :as appconfig :refer [appconfig]]
   [slingshot.slingshot :refer [try+]]
   [ring.adapter.jetty :refer [run-jetty]]
   [clj-stacktrace.core :as stacktrace]
   [clj-stacktrace.repl :as stacktrace-repl]
   [clojure.string :as string]
   [hiccup2.core :as h]
   [iapetos.collector.ring :refer [wrap-instrumentation]]
   [ring.middleware params gzip keyword-params json stacktrace lint not-modified]))

(defn- error-page-head []
  [:head
   [:meta {:charset "utf-8"}]
   [:title "500 Internal Server Error"]
   [:link {:rel "stylesheet" :href "/static/bootstrap/css/bootstrap.min.css"}]
   [:link {:rel "stylesheet" :href "/static/ibmplex/Web/css/ibm-plex.min.css"}]
   [:link {:rel "stylesheet" :href "/static/llar.css"}]])

(defn- exception-details [request ex]
  [[:h4 "Request"]
   (dashboard/pprint-html request)
   [:h4 "Version"]
   [:pre (str "llar " (get appconfig :version))]
   [:h4 "Exception"]
   [:p "Message: " [:pre (ex-message ex)]]
   [:p "Cause: " [:pre (ex-cause ex)]]
   [:p "Exception Class: " [:pre (class ex)]]
   [:p "Data: " (dashboard/pprint-html (ex-data ex))]
   [:h4 "Exception Chain"]
   [:pre (get-in ex [:object :message])]])

(defn- stacktrace-list [th]
  [:ol
   (for [s (:trace-elems th)
         :let [formatted (stacktrace-repl/pst-elem-str false s 70)]]
     [:li [:pre formatted]])])

(defn- error-page-body [request ex th]
  [:body
   (concat
    [[:h1 "😭"]
     [:h2 "Internal Server Error"]]
    (exception-details request ex)
    [(stacktrace-list th)])])

(defn exception-response [request ex]
  (let [th (stacktrace/parse-exception ex)]
    {:status 500
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (str (h/html
                 (error-page-head)
                 (error-page-body request ex th)))}))

(defn wrap-exception [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception ex
        (log/error ex "Exception during request" request)
        (exception-response request ex)))))

(def +prom-shorten-path-of+
  ["/blob" "/static" "/reader/item/by-id/"])

(def +prom-route-patterns+
  [[#"^/api/source/[^/]+$" "/api/source/:source-key"]
   [#"^/api/update/[^/]+$" "/api/update/:source-key"]
   [#"^/source-details/[^/]+$" "/source-details/:key"]
   [#"^/api/podcast/retry/[^/]+$" "/api/podcast/retry/:item-id"]
   [#"^/api/podcast/[^/]+$" "/api/podcast/:item-id"]])

(defn prom-path-fn [request]
  (let [uri (:uri request)]
    (or
     (some #(when (string/starts-with? uri %) %) +prom-shorten-path-of+)
     (some (fn [[re replacement]]
             (when (re-matches re uri)
               replacement))
           +prom-route-patterns+)
     uri)))

(def ^:private reader-content-security-policy
  (string/join
   "; "
   ["default-src 'self'"
    "script-src 'self'"
    "style-src 'self' 'unsafe-inline'"
    "img-src 'self' data: https:"
    "font-src 'self'"
    "connect-src 'self'"
    "frame-src https://www.youtube-nocookie.com https://www.youtube.com"
    "media-src 'self' https: blob:"
    "object-src 'none'"
    "base-uri 'none'"
    "form-action 'self'"
    "frame-ancestors 'none'"]))

(defn wrap-security-headers [handler & {:keys [content-security-policy]}]
  (fn [request]
    (let [response (handler request)]
      (cond-> response
        (map? response)
        (assoc-in [:headers "Referrer-Policy"] "no-referrer")

        (and (map? response) content-security-policy)
        (assoc-in [:headers "Content-Security-Policy"] content-security-policy)))))

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
   wrap-security-headers
   (wrap-instrumentation metrics/prom-registry {:path-fn prom-path-fn})
   ring.middleware.lint/wrap-lint))

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
   (wrap-security-headers :content-security-policy reader-content-security-policy)
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
           (let [server (try-start-jetty (dashboard-app) port)]
             (log/infof "dashboard started on http://localhost:%s prometheus endpoint http://localhost:%s/metrics" port port)
             server)
           (log/info "dashboard disabled in appconfig"))
  :stop (try-stop-app dashboard))

(defstate ^{:depends-on [metrics/prom-registry reader/frontend-db]} reader
  :start (if-let [port (get-in appconfig [:api :reader :port])]
           (let [server (try-start-jetty (reader-app metrics/prom-registry) port)]
             (log/infof "reader started: http://localhost:%s/reader" port)
             server)
           (log/info "reader disabled in appconfig"))
  :stop (try-stop-app reader))

(defn podcast-app []
  (->
   podcast/app
   podcast/wrap-token-auth
   ring.middleware.not-modified/wrap-not-modified
   ring.middleware.params/wrap-params
   wrap-exception
   (wrap-instrumentation metrics/prom-registry {:path-fn prom-path-fn})))

(defn fever-app []
  (->
   (fever/handler reader/frontend-db (appconfig/fever))
   ring.middleware.json/wrap-json-response
   ring.middleware.params/wrap-params
   ring.middleware.gzip/wrap-gzip
   wrap-exception
   wrap-security-headers
   (wrap-instrumentation metrics/prom-registry {:path-fn prom-path-fn})))

(defstate podcast
  :start (when-let [port (appconfig/podcast :port)]
           (log/infof "Starting podcast server on port %d" port)
           (try-start-jetty (podcast-app) port))
  :stop (try-stop-app podcast))

(defstate ^{:depends-on [metrics/prom-registry reader/frontend-db]} fever
  :start (if-let [port (appconfig/fever :port)]
           (let [server (try-start-jetty (fever-app) port)]
             (log/infof "Fever API started on port %d" port)
             server)
           (log/info "Fever API disabled in appconfig"))
  :stop (try-stop-app fever))
