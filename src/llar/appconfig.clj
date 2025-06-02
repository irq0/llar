(ns llar.appconfig
  (:require
   [mount.core :refer [defstate]]
   [clojure.spec.alpha :as s]
   [llar.specs]
   [slingshot.slingshot :refer [throw+]]
   [hikari-cp.core :as hikari]
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]
   [clojure.java.io :as io]))

(def +config-locations+
  [(io/resource "config.edn")
   (io/file (System/getenv "LLAR_CONFIG"))
   (io/file (System/getProperty "config"))])

(defn try-read-config [file]
  (log/info "reading config from" file)
  (try
    (edn/read-string (slurp file))
    (catch Exception e
      (log/errorf "failed to read config from %s: %s" file (ex-message e))
      {})))

(s/def :irq0-appconfig/port pos-int?)
(s/def :irq0-appconfig/jetty-config (s/keys :req-un [:irq0-appconfig/port]))
(s/def :irq0-appconfig/dashboard :irq0-appconfig/jetty-config)
(s/def :irq0-appconfig/reader :irq0-appconfig/jetty-config)
(s/def :irq0-appconfig/api (s/keys :opt-un [:irq0-appconfig/reader :irq0-appconfig/dashboard]))
(s/def :irq0-appconfig/blob-store-dir :irq0/path-writable-dir)
(s/def :irq0-appconfig/credentials-file :irq0/path-exists)
(s/def :irq0-appconfig/runtime-config-dir :irq0/path-exists-is-dir)
(s/def :irq0-appconfig/command-max-concurrent pos-int?)
(s/def :irq0-appconfig/throttle (s/keys :req-un [:irq0-appconfig/command-max-concurrent]))
(s/def :irq0-appconfig/timeouts (s/map-of keyword pos-int?))
(s/def :irq0-appconfig/command :irq0/path)
(s/def :irq0-appconfig/commands (s/map-of keyword :irq0-appconfig/command))
(s/def :irq0-appconfig/postgresql-pool hikari/validate-options)
(s/def :irq0-appconfig/frontend :irq0-appconfig/postgresql-pool)
(s/def :irq0-appconfig/backend :irq0-appconfig/postgresql-pool)
(s/def :irq0-appconfig/postgresql (s/keys :req-un [:irq0-appconfig/frontend
                                                   :irq0-appconfig/backend]))
(s/def :irq0-appconfig/list-view #{:headlines :gallery})
(s/def :irq0-appconfig/default-list-view (s/map-of keyword? :irq0-appconfig/list-view))
(s/def :irq0-appconfig/view-group #{:default :item-tags :source-tag :type})
(s/def :irq0-appconfig/favorites (s/coll-of (s/tuple keyword? :irq0-appconfig/view-group)))
(s/def :irq0-appconfig/ui (s/keys :req-un [:irq0-appconfig/default-list-view :irq0-appconfig/favorites]))

(s/def :irq0-llar/appconfig
  (s/keys :req-un [:irq0-appconfig/blob-store-dir
                   :irq0-appconfig/credentials-file
                   :irq0-appconfig/runtime-config-dir
                   :irq0-appconfig/commands
                   :irq0-appconfig/api
                   :irq0-appconfig/ui
                   :irq0-appconfig/postgresql]))

(defn verify-config [config]
  (let [conform (s/conform :irq0-llar/appconfig config)]
    (if (s/invalid? conform)
      (let [err {:spec conform
                 :explain (s/explain-str :irq0-llar/appconfig config)
                 :type ::config-verification-failed
                 :config config}]
        (log/error "Config verification error: " err)
        (throw+ err))
      (do
        (log/info "application config successfully verified")
        (log/debug "application config: " config)
        config))))

(defn read-version []
  (edn/read-string (slurp (io/resource "version.edn"))))

;; TODO check that :commands all exists and work!
(defn read-config []
  (->
   (->> (remove nil? +config-locations+)
        (map try-read-config)
        (apply merge)
        (verify-config))
   (assoc :version (read-version))))

(defn read-config-or-die []
  (try
    (read-config)
    (catch java.lang.Throwable _
      (log/error "failed to read config. exiting")
      (System/exit 1))))

(defstate appconfig
  :start (read-config))

(defn appconfig-redact-secrets []
  (-> appconfig
      (assoc-in [:postgresql :frontend :password] "--secret removed--")
      (assoc-in [:postgresql :backend :password] "--secret removed--")))

(defn update-max-retry []
  (get appconfig :update-max-retry))

(defn postgresql-config [pool]
  (get-in appconfig [:postgresql pool]))

(defn blob-store-dir []
  (get appconfig :blob-store-dir))

(defn blob-store-url-index-dir []
  (str (get appconfig :blob-store-dir) "/url-index"))

(defn blob-store-dupes-dir []
  (str (get appconfig :blob-store-dir) "/url-dupe-content-index"))

(defn command [name]
  (let [cmd (get-in appconfig [:commands name])]
    (when (nil? cmd)
      (throw+ {:type ::command-not-defined
               :command name
               :available-commands (:commands appconfig)}))
    cmd))

(defn credentials [name]
  (try
    (let [credentials (edn/read-string (slurp (get appconfig :credentials-file)))]
      (get credentials name))
    (catch Exception e
      (log/error e "[Config] Failed to read credentials from " (get appconfig :credentials-file))
      {})))
