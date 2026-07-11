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
(s/def :irq0-appconfig/enabled boolean?)
(s/def :irq0-appconfig/base-url string?)
(s/def :irq0-appconfig/username string?)
(s/def :irq0-appconfig/credentials keyword?)
(s/def :irq0-appconfig/source-tag keyword?)
(s/def :irq0-appconfig/initial-days pos-int?)
(s/def :irq0-appconfig/recent-read-days pos-int?)
(s/def :irq0-appconfig/max-content-bytes pos-int?)
(s/def :irq0-appconfig/jetty-config
  (s/keys :req-un [:irq0-appconfig/port]
          :opt-un [:irq0-appconfig/enabled]))
(s/def :irq0-appconfig/dashboard :irq0-appconfig/jetty-config)
(s/def :irq0-appconfig/reader
  (s/keys :req-un [:irq0-appconfig/port]
          :opt-un [:irq0-appconfig/enabled
                   :irq0-appconfig/base-url]))
(s/def :irq0-appconfig/fever
  (s/keys :req-un [:irq0-appconfig/port
                   :irq0-appconfig/username
                   :irq0-appconfig/credentials]
          :opt-un [:irq0-appconfig/source-tag
                   :irq0-appconfig/initial-days
                   :irq0-appconfig/recent-read-days
                   :irq0-appconfig/max-content-bytes]))
;; Digest delivery (e-reader magazines). Optional. Lives under :api :digest.
;; Uses the general top-level :mail config for sending.
(s/def :irq0-appconfig/to string?)
(s/def :irq0-appconfig/from string?)
(s/def :irq0-appconfig/schedule keyword?)
(s/def :irq0-appconfig/inline-images? boolean?)
(s/def :irq0-appconfig/keep-unread-issues nat-int?)
(s/def :irq0-appconfig/digest
  (s/keys :req-un [:irq0-appconfig/to]
          :opt-un [:irq0-appconfig/from
                   :irq0-appconfig/schedule
                   :irq0-appconfig/inline-images?
                   :irq0-appconfig/keep-unread-issues]))
(s/def :irq0-appconfig/default-episode-limit pos-int?)
(s/def :irq0-appconfig/sources (s/map-of keyword? pos-int?))
(s/def :irq0-appconfig/retention
  (s/keys :req-un [:irq0-appconfig/default-episode-limit]
          :opt-un [:irq0-appconfig/sources]))
(s/def :irq0-appconfig/video-format string?)
(s/def :irq0-appconfig/av-downloader-extra-args
  (s/coll-of string? :kind vector?))
(s/def :irq0-appconfig/podcast
  (s/keys :req-un [:irq0-appconfig/port]
          :opt-un [:irq0-appconfig/base-url
                   :irq0-appconfig/video-format
                   :irq0-appconfig/av-downloader-extra-args
                   :irq0-appconfig/retention]))
(s/def :irq0-appconfig/api (s/keys :opt-un [:irq0-appconfig/reader
                                            :irq0-appconfig/dashboard
                                            :irq0-appconfig/podcast
                                            :irq0-appconfig/fever
                                            :irq0-appconfig/digest]))

;; General outgoing mail config. Optional, top-level. :from is the default From
;; address. With :host set, mail is relayed over SMTP to that server (:credentials
;; names a credentials.edn entry holding :user/:pass; :starttls or :tls for
;; encryption); otherwise it is handed to the host's local MTA via the sendmail
;; binary. STARTTLS on the submission port:
;;   :mail {:from "llar@example.org"
;;          :host "mail.example.org" :port 587
;;          :credentials :smtp :starttls true}
;; with credentials.edn: {:smtp {:user "llar@example.org" :pass "secret"}}
;; Implicit TLS instead: :mail {... :port 465 :credentials :smtp :tls true}
(s/def :irq0-appconfig/host string?)
(s/def :irq0-appconfig/tls boolean?)
(s/def :irq0-appconfig/starttls boolean?)
(s/def :irq0-appconfig/mail
  (s/keys :opt-un [:irq0-appconfig/from
                   :irq0-appconfig/host
                   :irq0-appconfig/port
                   :irq0-appconfig/credentials
                   :irq0-appconfig/tls
                   :irq0-appconfig/starttls]))
(s/def :irq0-appconfig/blob-store-dir :irq0/path-writable-dir)
(s/def :irq0-appconfig/credentials-file :irq0/path-exists)
(s/def :irq0-appconfig/runtime-config-dir :irq0/path-exists-is-dir)
(s/def :irq0-appconfig/update-max-retry nat-int?)
(s/def :irq0-appconfig/command-max-concurrent pos-int?)
(s/def :irq0-appconfig/av-downloader-max-concurrent pos-int?)
(s/def :irq0-appconfig/streaming-max-concurrent pos-int?)
(s/def :irq0-appconfig/throttle
  (s/keys :req-un [:irq0-appconfig/command-max-concurrent]
          :opt-un [:irq0-appconfig/av-downloader-max-concurrent
                   :irq0-appconfig/streaming-max-concurrent]))
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

(s/def :irq0-appconfig/highlight-boost-hours number?)
(s/def :irq0-appconfig/rarity-boost-cap-hours number?)
(s/def :irq0-appconfig/ranking (s/keys :opt-un [:irq0-appconfig/highlight-boost-hours
                                                :irq0-appconfig/rarity-boost-cap-hours]))
(s/def :irq0-appconfig/name string?)
(s/def :irq0-appconfig/icon string?)
(s/def :irq0-appconfig/template string?)
(s/def :irq0-appconfig/url-handler
  (s/nilable (s/keys :req-un [:irq0-appconfig/template]
                     :opt-un [:irq0-appconfig/name
                              :irq0-appconfig/icon])))
(s/def :irq0-appconfig/export
  (s/keys :opt-un [:irq0-appconfig/url-handler]))
(s/def :irq0-appconfig/enabled? boolean?)
(s/def :irq0-appconfig/limit pos-int?)
(s/def :irq0-appconfig/runtime-digest
  (s/and (s/keys :opt-un [:irq0-appconfig/enabled?
                          :irq0-appconfig/to
                          :irq0-appconfig/from
                          :irq0-appconfig/limit
                          :irq0-appconfig/inline-images?
                          :irq0-appconfig/keep-unread-issues])
         #(or (not (:enabled? %))
              (string? (:to %)))))
(s/def :irq0-appconfig/podcast-retention
  (s/keys :req-un [:irq0-appconfig/default-episode-limit]
          :opt-un [:irq0-appconfig/sources]))
(s/def :irq0-appconfig/video-format string?)
(s/def :irq0-appconfig/extra-args (s/coll-of string? :kind vector?))
(s/def :irq0-appconfig/max-attempts pos-int?)
(s/def :irq0-appconfig/retry-cooldown-minutes pos-int?)
(s/def :irq0-appconfig/podcast-download
  (s/keys :opt-un [:irq0-appconfig/video-format
                   :irq0-appconfig/extra-args
                   :irq0-appconfig/max-attempts
                   :irq0-appconfig/retry-cooldown-minutes]))
(s/def :irq0-appconfig/podcast-enabled boolean?)
(s/def :irq0-appconfig/podcast-scan
  (s/keys :req-un [:irq0-appconfig/limit]))
(s/def :irq0-appconfig/max-retry nat-int?)
(s/def :irq0-appconfig/update
  (s/keys :req-un [:irq0-appconfig/max-retry]))
(s/def :irq0-appconfig/max-body-bytes pos-int?)
(s/def :irq0-appconfig/max-blob-body-bytes pos-int?)
(s/def :irq0-appconfig/http
  (s/keys :opt-un [:irq0-appconfig/max-body-bytes
                   :irq0-appconfig/max-blob-body-bytes]))

(s/def :irq0-llar/appconfig
  (s/keys :req-un [:irq0-appconfig/blob-store-dir
                   :irq0-appconfig/credentials-file
                   :irq0-appconfig/runtime-config-dir
                   :irq0-appconfig/commands
                   :irq0-appconfig/api
                   :irq0-appconfig/ui
                   :irq0-appconfig/postgresql]
          :opt-un [:irq0-appconfig/update-max-retry
                   :irq0-appconfig/throttle
                   :irq0-appconfig/timeouts
                   :irq0-appconfig/ranking
                   :irq0-appconfig/export
                   :irq0-appconfig/http
                   :irq0-appconfig/mail]))

(def fever-defaults
  "Effective defaults for an enabled Fever service. Activation and credentials
  deliberately have no defaults."
  {:source-tag :mobile
   :initial-days 30
   :recent-read-days 10
   :max-content-bytes 1048576})

(defn documented-defaults
  "Shipped system defaults plus effective defaults for optional services.
  This reads packaged data only; it never reads deployment config or credentials."
  []
  (assoc-in (edn/read-string (slurp (io/resource "config.edn")))
            [:api :fever]
            fever-defaults))

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

(defn podcast
  ([] (get-in appconfig [:api :podcast]))
  ([key] (get-in appconfig [:api :podcast key])))

(defn digest
  ([] (get-in appconfig [:api :digest]))
  ([key] (get-in appconfig [:api :digest key])))

(defn mail
  ([] (get appconfig :mail))
  ([key] (get-in appconfig [:mail key])))

(defn reader
  ([] (get-in appconfig [:api :reader]))
  ([key] (get-in appconfig [:api :reader key])))

(defn fever
  ([] (get-in appconfig [:api :fever]))
  ([key] (get-in appconfig [:api :fever key])))

(defn http
  ([] (get appconfig :http))
  ([key] (get-in appconfig [:http key])))

(defn http-max-body-bytes []
  (or (http :max-body-bytes) 15728640))

(defn http-max-blob-body-bytes []
  (or (http :max-blob-body-bytes) 52428800))

(defn credentials [name]
  (try
    (let [credentials (edn/read-string (slurp (get appconfig :credentials-file)))]
      (get credentials name))
    (catch Exception e
      (log/error e "[Config] Failed to read credentials from " (get appconfig :credentials-file))
      {})))
