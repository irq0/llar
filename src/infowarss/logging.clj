(ns infowarss.logging
  (:require
   [mount.core]
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]
   [taoensso.timbre.appenders.3rd-party.rotor]
   [clojure.string :as string]
   [robert.hooke :refer [add-hook clear-hooks]]))

(alter-meta! *ns* assoc ::load false)

;;; Logger

(log/merge-config!
  {:appenders {:rotating (taoensso.timbre.appenders.3rd-party.rotor/rotor-appender
                           {:path "/tmp/infowarss_all.log"
                            :max-size (* 42 1024 1024) })}})

(log/merge-config!
  {:ns-blacklist  ["org.apache.http.*"
                   "org.eclipse.jetty.io.*"
                   "org.eclipse.jetty.http.*"
                   "org.eclipse.jetty.server.*"
                   "org.eclipse.jetty.util.*"
                   "org.apache.pdfbox.io.*"
                   "com.ning.http.client.providers.netty.handler.*"
                   "com.ning.http.client.providers.netty.channel.*"
                   ]
   :level :trace})

(log/merge-config!
  {:appenders {:spit (assoc (appenders/spit-appender
                              {:fname "/tmp/infowarss_info.log"})
                       :min-level :info)}})

(log/merge-config!
  {:appenders {:println {:min-level :error
                         :stream :std-err}}})


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
  (doall (map #(clear-hooks %) lifecycle-fns)))

(defn with-logging-status []
  (without-logging-status)
  (doall (map #(add-hook % log-status) lifecycle-fns)))
