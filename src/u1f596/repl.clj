(ns u1f596.repl
  (:require
   [schema.core :as s]
   [mount.core :refer [defstate]]
   [nrepl.server :refer [start-server stop-server]]))

;;;; Namespace to interact with u1f596 from the REPL

(s/set-fn-validation! true)

(defn nrepl-handler []
  (require 'cider.nrepl)
  (ns-resolve 'cider.nrepl 'cider-nrepl-handler))

(defstate nrepl-server
  :start (start-server :port 42000 :handler (nrepl-handler))
  :stop (stop-server nrepl-server))
