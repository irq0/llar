(ns llar.core
  (:require
   [mount.core :as mount]
   [mount.tools.graph :as mount-graph]
   [mount-up.core :as mount-up]
   [clojure.tools.logging :as log]
   [llar.appconfig :as appconfig]
   [llar.persistency :as persistency]
   [llar.store :as store]
   [llar.db.core :as db]
   [llar.db.query]
   [llar.db.modify]
   [llar.db.search]
   [llar.repl :as repl]
   [llar.update :as update]
   [llar.metrics]
   [llar.lab :as lab]
   [llar.apis.reader]
   [llar.http :as http]
   [llar.blobstore]
   [llar.live :as live]
   [llar.config :as config]
   [clojure.string :as string]
   [clojure.tools.cli :refer [parse-opts]]
   [migratus.core :as migratus]
   [llar.webapp])
  (:gen-class))

(def cli-options
  [[nil "--init-db" "Initialize new database and exit"]
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
    (mount-up/on-upndown :info mount-up/log :before)
    (mount-up/on-up
     :guard (mount-up/try-catch
             (fn [ex state] (log/error ex "!! Error bringing up state " state)))
     :wrap-in)
    (mount/start #'appconfig/appconfig)
    (when (:nrepl options)
      (mount/start #'repl/nrepl-server))

    (cond
      (:init-db options)
      (mount/start
       #'store/backend-db)

      (:dry options)
      (->
       (mount/except [#'live/live #'lab/lab-sched #'config/change-watcher #'update/remove-unread-tags #'update/persist-state])
       (mount/except lab/lab-sched)
       mount/start)

      :else
      (mount/start))

    (cond
      (:init-db options)
      (do
        (let [config {:store :database
                      :db store/backend-db
                      :migration-dir "migrations/"
                      :init-script "init.sql"}]
          (log/info "initializing database" config)
          (log/info (migratus/init config))
          (log/info (migratus/migrate config)))
        (log/info "finished DB migrations. exiting")
        (System/exit 0))

      (not (:dry options))
      (doseq [[key db-config] (:postgresql appconfig/appconfig)
              :let [db-spec (db/make-postgresql-dbspec db-config)
                    store (db/make-postgresql-datastore db-spec)]]
        (log/info "smoke testing database: "
                  key
                  (vec (persistency/get-table-row-counts store)))))

    (let [config {:store :database
                  :db store/backend-db
                  :migration-dir "migrations/"}
          result (migratus/migrate config)]
      (log/info "database migrations: " (if (nil? result) "ok" result)))

    (when-not (:dry options)
      (http/update-domain-blocklist!)
      (config/load-all))

    (log/info "🖖")
    (log/debug "Running states: " (mount-graph/states-with-deps))
    (log/debug "Startup options: " options)))
