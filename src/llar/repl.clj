(ns llar.repl
  (:require
   [mount.core :refer [defstate]]
   [nrepl.server :refer [start-server stop-server]]))

;;;; Namespace to interact with llar from the REPL

(defn nrepl-handler []
  (require 'cider.nrepl)
  (ns-resolve 'cider.nrepl 'cider-nrepl-handler))

(defstate nrepl-server
  :start (start-server :port 42000 :handler (nrepl-handler))
  :stop (stop-server nrepl-server))
