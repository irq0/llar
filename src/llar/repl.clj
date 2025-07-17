(ns llar.repl
  (:require
   [mount.core :refer [defstate]]
   [portal.nrepl :refer [wrap-repl wrap-portal]]
   [portal.api :as portal]
   [cider.nrepl :refer [cider-middleware]]
   [nrepl.server :refer [start-server stop-server default-handler]]))

;;;; Namespace to interact with llar from the REPL

(add-tap #'portal/submit)

(defn nrepl-handler []
  (apply default-handler (concat cider-middleware [#'wrap-portal #'wrap-repl])))

(defstate nrepl-server
  :start (start-server :port 42000 :handler (nrepl-handler))
  :stop (stop-server nrepl-server))
