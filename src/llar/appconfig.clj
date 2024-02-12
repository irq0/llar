(ns llar.appconfig
  (:require
   [mount.core :refer [defstate]]
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]
   [clojure.java.io :as io]))

(def +config-locations+
  [(io/resource "config.edn")
   (io/file (System/getenv "LLAR_CONFIG"))
   (io/file (System/getProperty "config"))])

;; TODO run config through schema
;; TODO check that :commands all exists and work!
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

(defn postgresql-config [pool]
  (get-in appconfig [:postgresql pool]))

(defn blob-store-dir []
  (get appconfig :blob-store-dir))

(defn blob-store-url-index-dir []
  (str (get appconfig :blob-store-dir) "/url-index"))

(defn blob-store-dupes-dir []
  (str (get appconfig :blob-store-dir) "/url-dupe-content-index"))

(defn runtime-config-dir []
  (get appconfig :runtime-config-dir))

(defn command [name]
  (get-in appconfig [:commands name]))

(defn credentials [name]
  (try
    (let [credentials (edn/read-string (slurp (get appconfig :credentials-file)))]
      (get credentials name))
    (catch Exception e
      (log/error e "[Config] Failed to read credentials from " (get appconfig :credentials-file))
      {})))
