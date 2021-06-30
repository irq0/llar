(ns u1f596.core
  (:require
   [mount.core :as mount]
   [taoensso.timbre :as log]
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
   [u1f596.webapp :as webapp])
  (:gen-class))

(defn -main [& args]
  ;; otherwise date time parsers will fail!
  (java.util.Locale/setDefault java.util.Locale/ENGLISH)
  (logging/setup)
  (mount/in-clj-mode)
  (mount/start
   #'appconfig/appconfig
   #'api-reader/annotations

   #'api-reader/frontend-db
   #'store/backend-db

   #'http/domain-blocklist
   #'blobstore/locks

   #'repl/nrepl-server
   #'update/state
   #'metrics/prom-registry

   #'sched/db-sched
   #'sched/misc-sched

   #'sched/feed-sched
   #'live/live

   #'webapp/status
   #'webapp/reader)
  (doseq [[key db-config] (:postgresql appconfig/appconfig)
          :let [db-spec (db/make-postgresql-dbspec db-config)
                store (db/make-postgresql-datastore db-spec)]]
    (log/info "Testing database connection: "
              key
              (vec (persistency/get-table-row-counts store))))
  (log/info "🖖"))
