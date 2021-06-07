(ns infowarss.appconfig
  (:require
   [mount.core :refer [defstate]]
   [clojure.edn :as edn]
   [taoensso.timbre :as log]
   [clojure.java.io :as io]))

(def +config-locations+
  [(io/resource "config.edn")
   (io/file (System/getenv "INFOWARSS_CONFIG"))
   (io/file (System/getProperty "config"))])

;; TODO run config through schema
(defn try-read-config [file]
  (log/info "Reading config from" file)
  (try
    (edn/read-string (slurp file))
    (catch Exception e
      (log/error e "[Config] Failed to read config from " file)
      {})))

(defn read-config []
  (apply merge (map try-read-config (remove nil? +config-locations+))))

(defstate appconfig
  :start (read-config))

(defn update-max-retry []
  (get appconfig :update-max-retry))

(defn state-dir []
  (-> (get appconfig :state-dir)
      io/file
      .toPath))

(defn annotations-file []
  (-> (get appconfig :annotations-file)
      io/file))

(defn postgresql-config [pool]
  (get-in appconfig [:postgresql pool]))
