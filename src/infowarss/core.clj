(ns infowarss.core
  (:require
   [mount.core :as mount]
   [taoensso.timbre :as log]
   [infowarss.logging :as logging]
   [infowarss.db.core :as db]
   [infowarss.repl :as repl]
   [infowarss.update :as update]
   [infowarss.metrics :as metrics]
   [infowarss.sched :as sched]
   [infowarss.apis.reader :as api-reader]
   [infowarss.http :as http]
   [infowarss.blobstore :as blobstore]
   [infowarss.live :as live]
   [infowarss.webapp :as webapp]))

(defn -main [& args]
  ;; otherwise date time parsers will fail!
  (java.util.Locale/setDefault java.util.Locale/ENGLISH)
  (logging/setup)
  (mount/in-clj-mode)
  (mount/start
   #'db/db
   #'api-reader/annotations
   #'http/domain-blocklist
   #'blobstore/locks
   #'repl/nrepl-server
   #'update/state
   #'metrics/prom-registry
   #'sched/db-sched
   #'sched/misc-sched
   
   ;; #'sched/feed-sched
   ;; #'live/live

   #'webapp/status
   #'webapp/reader)
  (db/check-connectivity)
  (log/info "🖖"))
