(ns u1f596.logging
  (:require
   [mount.core]
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [robert.hooke :refer [add-hook clear-hooks]]))

(alter-meta! *ns* assoc ::load false)

;; from mount examples

(defn- f-to-action [f {:keys [status]}]
  (let [fname (-> (str f)
                  (string/split #"@")
                  first)]
    (case fname
      "mount.core$up" (when-not (:started status) :up)
      "mount.core$down" (when-not (:stopped status) :down)
      :noop)))

(defn whatcha-doing? [action]
  (case action
    :up ">> starting"
    :down "<< stopping"
    false))

(defn log-status [f & args]
  (let [[state-name state] args
        action (f-to-action f state)]
    (when-let [taking-over-the-world (whatcha-doing? action)]
      (log/info (str taking-over-the-world ".. " state-name)))
    (apply f args)))

(defonce lifecycle-fns
  #{#'mount.core/up
    #'mount.core/down})

(defn without-logging-status []
  (doall (map clear-hooks lifecycle-fns)))

(defn with-logging-status []
  (without-logging-status)
  (doall (map #(add-hook % log-status) lifecycle-fns)))
