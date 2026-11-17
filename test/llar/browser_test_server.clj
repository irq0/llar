(ns llar.browser-test-server
  "A real Reader for browser tests. Control messages use stdin, never HTTP."
  (:require
   [cheshire.core :as json]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [java-time.api :as time]
   [llar.apis.reader :as reader]
   [llar.appconfig :as appconfig]
   [llar.config :as config]
   [llar.db.core :as db]
   [llar.db.modify]
   [llar.db.query]
   [llar.db.test-fixtures :as fixtures]
   [llar.fetch :as fetch]
   [llar.fetch.demo :as demo]
   [llar.metrics :as metrics]
   [llar.persistency :as persistency]
   [llar.postproc :as postproc]
   [llar.rc :as rc]
   [llar.src :as src]
   [llar.store :as store]
   [llar.update :as update]
   [llar.webapp :as webapp]
   [migratus.core :as migratus]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]))

(def reference-time (time/zoned-date-time 2026 11 17 12 0 0 0 "UTC"))

(defn demo-items []
  (with-redefs [demo/now (constantly reference-time)]
    (vec
     (for [publication (sort src/demo-publications)
           :let [source (src/demo publication :count 8 :seed 4242)]
           item (fetch/fetch-source source {})]
       (-> item
           (postproc/all-items-process-first source {:key publication})
           (postproc/post-process-item source {})
           persistency/to-couch)))))

(defn reset-data! [database items]
  (fixtures/clear-test-data! database)
  (fixtures/seed-default-tags! database)
  (reset! rc/rc-overrides* {})
  (reset! update/state {})
  (doseq [item items]
    (persistency/store-item! database item {}))
  {:items (jdbc/execute! database ["SELECT id, title, hash FROM items ORDER BY id"]
                         {:builder-fn rs/as-unqualified-lower-maps})})

(defn reply! [value]
  (println (str "LLAR_BROWSER_TEST " (json/generate-string value)))
  (flush))

(defn -main [port blob-dir]
  (java.util.Locale/setDefault java.util.Locale/ENGLISH)
  (let [port (parse-long port)
        database-config {:dbtype "postgresql" :dbname "llar_browser_test"
                         :host "127.0.0.1" :port port :user "postgres"}
        migration-config {:store :database :db database-config
                          :migration-dir "migrations/" :init-script "init.sql"
                          :command-separator "--;;"}
        settings (assoc (edn/read-string (slurp (io/resource "config.edn")))
                        :blob-store-dir blob-dir :credentials {})]
    (migratus/init migration-config)
    (migratus/migrate migration-config)
    (let [database (db/make-postgresql-pooled-datastore
                    {:jdbc-url (str "jdbc:postgresql://127.0.0.1:" port "/llar_browser_test")
                     :username "postgres" :maximum-pool-size 5})]
      (try
        ;; The production handler and persistence run unchanged. No scheduler or watcher starts.
        ;; fetch updates the source registry, but lint-as def reports inline definitions here.
        #_{:clj-kondo/ignore [:inline-def]}
        (with-redefs [appconfig/appconfig settings
                      reader/frontend-db database
                      store/backend-db database
                      update/state (atom {})]
          (config/fetch signal-wire (src/demo :signal-wire) :tags #{:demo})
          (config/fetch field-notes (src/demo :field-notes) :tags #{:demo})
          (config/fetch common-ground (src/demo :common-ground) :tags #{:demo})
          (config/fetch after-hours (src/demo :after-hours) :tags #{:demo})
          (let [items (demo-items)
                server (webapp/try-start-jetty (webapp/reader-app metrics/prom-registry)
                                               0 {:host "127.0.0.1"})]
            (try
              (reply! {:url (str "http://127.0.0.1:" (.getLocalPort (first (.getConnectors server))))})
              (doseq [line (line-seq (java.io.BufferedReader. *in*))]
                (try
                  (let [{:keys [command]} (json/parse-string line true)]
                    (case command
                      "reset" (reply! (reset-data! database items))
                      (throw (ex-info "Unknown browser test command" {:command command}))))
                  (catch Exception ex
                    (reply! {:error (str ex)}))))
              (finally (.stop server)))))
        (finally (.close (:datasource database)))))
    (shutdown-agents)))
