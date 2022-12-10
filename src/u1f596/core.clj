(ns u1f596.core
  (:require
   [mount.core :as mount]
   [clojure.tools.logging :as log]
   [u1f596.logging :as logging]
   [u1f596.appconfig :as appconfig]
   [u1f596.persistency :as persistency]
   [u1f596.store :as store]
   [u1f596.db.core :as db]
   [u1f596.db.query]
   [u1f596.db.modify]
   [u1f596.db.search]
   [u1f596.repl :as repl]
   [u1f596.update :as update]
   [u1f596.metrics :as metrics]
   [u1f596.sched :as sched]
   [u1f596.apis.reader :as api-reader]
   [u1f596.http :as http]
   [u1f596.blobstore :as blobstore]
   [u1f596.live :as live]
   [u1f596.notifier :as notifier]
   [clojure.string :as string]
   [clojure.tools.cli :refer [parse-opts]]
   [nextjournal.clerk :as clerk]
   [migratus.core :as migratus]
   [u1f596.webapp :as webapp])
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

      :default
      (mount/start
       #'appconfig/appconfig

       #'api-reader/frontend-db
       #'store/backend-db

       #'http/domain-blocklist
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
         (log/info "Starting DB migrations" config)
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

   (when-not (:dry options)
     (http/update-domain-blocklist!))

   (when (:clerk options)
     (clerk/serve! {:browse? false
                    :watch-paths ["src/notebooks"]}))

   (log/info "🖖")))
