(ns llar.core
  (:require
   [mount.core :as mount]
   [clojure.tools.logging :as log]
   [llar.logging]
   [llar.appconfig :as appconfig]
   [llar.persistency :as persistency]
   [llar.store :as store]
   [llar.db.core :as db]
   [llar.db.query]
   [llar.db.modify]
   [llar.db.search]
   [llar.repl :as repl]
   [llar.update :as update]
   [llar.metrics :as metrics]
   [llar.sched :as sched]
   [llar.apis.reader :as api-reader]
   [llar.http :as http]
   [llar.blobstore :as blobstore]
   [llar.live :as live]
   [llar.notifier :as notifier]
   [clojure.string :as string]
   [clojure.tools.cli :refer [parse-opts]]
   [migratus.core :as migratus]
   [llar.webapp :as webapp])
  (:gen-class))

(def cli-options
  [[nil "--init-db" "Initialize new database"]
   [nil "--dry" "Start without live, schedulers, etc"]
   [nil "--nrepl" "Start nrepl server"]
   ["-h" "--help"]])

(defn -main [& args]
  ;; otherwise date time parsers will fail!
  (java.util.Locale/setDefault java.util.Locale/ENGLISH)

  (let [{:keys [options summary errors]} (parse-opts args cli-options)]
    (when (or errors (:help options))
      (log/info "Usage:")
      (doseq [line (string/split summary #"\n")]
        (log/info line))
      (when errors
        (log/error errors))
      (System/exit 1))

    (mount/in-clj-mode)

    (when (:nrepl options)
      (mount/start #'repl/nrepl-server))

    (cond
      (:init-db options)
      (mount/start
       #'appconfig/appconfig
       #'store/backend-db)

      (:dry options)
      (mount/start
       #'appconfig/appconfig

       #'api-reader/frontend-db
       #'store/backend-db

       #'blobstore/locks

       #'update/state
       #'metrics/prom-registry

       #'notifier/telegram-bot

       #'webapp/status
       #'webapp/reader)

      :else
      (mount/start
       #'appconfig/appconfig

       #'api-reader/frontend-db
       #'store/backend-db

       #'blobstore/locks

       #'update/state
       #'metrics/prom-registry

       #'live/live

       #'notifier/telegram-bot

       #'webapp/status
       #'webapp/reader

       #'sched/db-sched
       #'sched/misc-sched
       #'sched/feed-sched))

    (cond
      (:init-db options)
      (do
        (let [config {:store :database
                      :db store/backend-db
                      :migration-dir "migrations/"
                      :init-script "init.sql"}]
          (log/info "Initializing database" config)
          (log/info (migratus/init config))
          (log/info (migratus/migrate config)))
        (log/info "Finished DB migrations. Exiting")
        (System/exit 0))

      (not (:dry options))
      (doseq [[key db-config] (:postgresql appconfig/appconfig)
              :let [db-spec (db/make-postgresql-dbspec db-config)
                    store (db/make-postgresql-datastore db-spec)]]
        (log/info "Smoke testing database: "
                  key
                  (vec (persistency/get-table-row-counts store)))))

    (let [config {:store :database
                  :db store/backend-db
                  :migration-dir "migrations/"}]
      (log/info "Running database migrations" (migratus/migrate config)))

    (when-not (:dry options)
      (http/update-domain-blocklist!))

    (log/info "🖖")
    (log/debug "Startup options: " options)))