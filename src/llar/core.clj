(ns llar.core
  (:require
   [mount.core :as mount]
   [mount.tools.graph :as mount-graph]
   [mount-up.core :as mount-up]
   [clojure.tools.logging :as log]
   [clojure.spec.alpha :as s]
   [llar.appconfig :as appconfig]
   [llar.blobstore :as blobstore]
   [llar.metrics]
   [llar.store :as store]
   [llar.apis.reader :as reader]
   [llar.webapp :as webapp]
   [llar.update :as update]
   [llar.persistency :as persistency]
   [llar.db.core]
   [llar.db.query]
   [llar.db.modify]
   [llar.db.search]
   [llar.http :as http]
   [llar.config :as config]
   [llar.lab :as lab]
   [clojure.string :as string]
   [clojure.tools.cli :refer [parse-opts]]
   [migratus.core :as migratus]
   [llar.repl :as repl])
  (:gen-class))

;; Note: order of requires is important. Mount infers component load
;; sequence from it

(def cli-options
  [[nil "--init-db" "Initialize new database and exit"]
   [nil "--dry" "Start without live, schedulers, etc"]
   [nil "--nrepl" "Start nrepl server"]
   ["-h" "--help"]])

(def essential-states
  [#'appconfig/appconfig])

(def dry-states
  [#'store/backend-db
   #'reader/frontend-db
   #'blobstore/locks
   #'update/state

   #'webapp/dashboard
   #'webapp/reader])

(def wet-states
  [#'config/change-watcher
   #'update/remove-unread-tags
   #'lab/update-db-search-indices])

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

    (->
     essential-states
     (mount/swap {#'appconfig/appconfig (appconfig/read-config-or-die)})
     (mount/start))

    (when (:nrepl options)
      (mount/start #'repl/nrepl-server))

    (s/check-asserts true)

    (cond
      (:init-db options)
      (mount/start #'store/backend-db)

      (:dry options)
      (mount/start dry-states)

      :else
      (mount/start (concat dry-states wet-states)))

    (.addShutdownHook
     (Runtime/getRuntime)
     (Thread.
      (fn []
        (log/info "shutting down...")
        (mount/stop))))

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
      (log/info "smoke testing database: backend"
                (vec (persistency/get-table-row-counts store/backend-db))))

    (let [config {:store :database
                  :db store/backend-db
                  :migration-dir "migrations/"}
          result (migratus/migrate config)]
      (log/info "database migrations: " (if (nil? result) "ok" result)))

    (when-not (:dry options)
      (http/update-domain-blocklist!)
      (config/load-all))

    (log/info "🖖")
    (log/debug "Mount load order: \n" (string/join "\n" (mount-graph/states-with-deps)))
    (log/debug "Startup options: " options)))
