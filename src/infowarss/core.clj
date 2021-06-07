(ns infowarss.core
  (:require
   [mount.core :as mount]
   [taoensso.timbre :as log]
   [infowarss.logging :as logging]
   [infowarss.appconfig :as appconfig]
   [infowarss.persistency :as persistency]
   [infowarss.store :as store]
   [infowarss.db.core :as db]
   [infowarss.db.query]
   [infowarss.db.modify]
   [infowarss.db.search]
   [infowarss.repl :as repl]
   [infowarss.update :as update]
   [infowarss.metrics :as metrics]
   [infowarss.sched :as sched]
   [infowarss.apis.reader :as api-reader]
   [infowarss.http :as http]
   [infowarss.blobstore :as blobstore]
   [infowarss.live :as live]
   [infowarss.webapp :as webapp])
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
