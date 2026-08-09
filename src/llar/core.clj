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
   [llar.commands :as commands]
   [llar.fetch.streaming :as streaming]
   [llar.pool :as pool]
   [llar.config :as config]
   [llar.docs.config :as docs.config]
   [llar.lab :as lab]
   [llar.podcast :as podcast]
   [llar.bookmark-capture :as bookmark-capture]
   [llar.digest :as digest]
   [clojure.string :as string]
   [clojure.tools.cli :refer [parse-opts]]
   [migratus.core :as migratus]
   [llar.repl :as repl])
  (:gen-class))

;; Note: order of requires is important. Mount infers component load
;; sequence from it

(def cli-options
  [[nil "--init-db" "Initialize new database and exit"]
   [nil "--rollback-db" "Rollback the last applied database migration and exit"]
   [nil "--dry" "Start without live, schedulers, etc"]
   [nil "--nrepl" "Start nrepl server"]
   [nil "--write-docs DIR" "Write static documentation to DIR and exit"]
   ["-h" "--help"]])

(def essential-states
  [#'appconfig/appconfig])

(def dry-states
  [#'reader/frontend-db
   #'blobstore/locks
   #'commands/command-throttle
   #'commands/av-download-throttle
   #'streaming/streaming-throttle
   #'pool/source-pool
   #'pool/item-pool
   #'update/state

   #'webapp/dashboard
   #'webapp/reader
   #'webapp/capture
   #'webapp/podcast
   #'webapp/fever])

(def wet-states
  [#'config/change-watcher
   #'update/remove-unread-tags
   #'lab/update-db-search-indices
   #'lab/update-saved-item-clusters
   #'lab/update-todays-vibe
   #'podcast/podcast-scanner
   #'podcast/podcast-retention-enforcer
   #'bookmark-capture/bookmark-capture-scheduler
   #'digest/digest-scheduler])

(defn write-docs! [docs-dir]
  (doseq [path (docs.config/write-static! docs-dir)]
    (log/info "wrote documentation file" (str path))))

(defn db-migration-config [db]
  {:store :database
   :db db
   :migration-dir "migrations/"
   :command-separator "--;;"})

(defn init-db! [db]
  (let [config (assoc (db-migration-config db) :init-script "init.sql")]
    (log/info "initializing database" config)
    (log/info (migratus/init config))
    (log/info (migratus/migrate config))))

(defn rollback-db! [db]
  (let [config (db-migration-config db)]
    (log/warn "rolling back the last applied database migration")
    (migratus/rollback config)))

(defn migrate-db! [db]
  (let [result (migratus/migrate (db-migration-config db))]
    (log/info "database migrations: " (if (nil? result) "ok" result))))

(defn start-runtime! [states]
  ;; No state that can issue application queries may start until migrations
  ;; have completed. In particular, Jetty and scheduled jobs are in `states`.
  (mount/start #'store/backend-db)
  (migrate-db! store/backend-db)
  (mount/start states))

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

    (when-let [docs-dir (:write-docs options)]
      (write-docs! docs-dir)
      (System/exit 0))

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

    (s/check-asserts true)

    (.addShutdownHook
     (Runtime/getRuntime)
     (Thread.
      (fn []
        (log/info "shutting down...")
        (mount/stop))))

    (cond
      (:init-db options)
      (mount/start #'store/backend-db)

      (:rollback-db options)
      (mount/start #'store/backend-db)

      (:dry options)
      (start-runtime! dry-states)

      :else
      (start-runtime! (concat dry-states wet-states)))

    (when (:nrepl options)
      (mount/start #'repl/nrepl-server))

    (cond
      (:init-db options)
      (do
        (init-db! store/backend-db)
        (log/info "finished DB migrations. exiting")
        (System/exit 0))

      (:rollback-db options)
      (do
        (rollback-db! store/backend-db)
        (log/info "finished rolling back the last DB migration. exiting")
        (System/exit 0))

      (not (:dry options))
      (log/info "smoke testing database: backend"
                (vec (persistency/get-table-row-counts store/backend-db))))

    (when-not (:dry options)
      (http/update-domain-blocklist!)
      (config/load-all))

    (log/info "🖖")
    (log/debug "Mount load order: \n" (string/join "\n" (mount-graph/states-with-deps)))
    (log/debug "Startup options: " options)))
