(ns infowarss.appconfig
  (:require
   [mount.core :refer [defstate]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]))

(defn read-config []
  (edn/read-string (slurp (io/resource "config.edn"))))

(defstate appconfig
  :start (read-config))
