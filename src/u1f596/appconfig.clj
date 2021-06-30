(ns u1f596.appconfig
  (:require
   [mount.core :refer [defstate]]
   [clojure.edn :as edn]
   [taoensso.timbre :as log]
   [clojure.java.io :as io]))

(def +config-locations+
  [(io/resource "config.edn")
   (io/file (System/getenv "U_1F596_CONFIG"))
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

(defn annotations-file []
  (-> (get appconfig :annotations-file)
      io/file))

(defn postgresql-config [pool]
  (get-in appconfig [:postgresql pool]))

(defn blob-store-dir []
  (get appconfig :blob-store-dir))

(defn blob-store-url-index-dir []
  (str (get appconfig :blob-store-dir) "/url-index"))

(defn blob-store-dupes-dir []
  (str (get appconfig :blob-store-dir) "/url-dupe-content-index"))

(defn command [name]
  (get-in appconfig [:commands name]))
