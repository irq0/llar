(ns llar.rc
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [llar.appconfig :as appconfig]))

(def rc-registry
  [{:path [:reader :favorites]
    :appconfig-path [:ui :favorites]
    :spec :irq0-appconfig/favorites
    :doc "Favorite reader navigation entries."}
   {:path [:reader :default-list-view]
    :appconfig-path [:ui :default-list-view]
    :spec :irq0-appconfig/default-list-view
    :doc "Default reader list style by group item."}
   {:path [:reader :ranking]
    :appconfig-path [:ranking]
    :spec :irq0-appconfig/ranking
    :doc "Ranking query tuning."}
   {:path [:reader :export :url-handler]
    :appconfig-path [:export :url-handler]
    :spec :irq0-appconfig/url-handler
    :doc "External URL handler used for reader annotation export."}
   {:path [:podcast :retention]
    :appconfig-path [:api :podcast :retention]
    :spec :irq0-appconfig/podcast-retention
    :doc "Podcast episode retention policy."}
   {:path [:digest]
    :appconfig-path [:api :digest]
    :default {:enabled? false
              :limit 200
              :inline-images? true
              :keep-unread-issues 1}
    :appconfig->rc #(assoc % :enabled? true)
    :spec :irq0-appconfig/runtime-digest
    :doc "Digest delivery and rendering policy."}
   {:path [:podcast :enabled?]
    :default false
    :spec :irq0-appconfig/podcast-enabled
    :doc "Enable podcast media downloading and retention jobs."}
   {:path [:podcast :download]
    :appconfig-path [:api :podcast]
    :appconfig->rc #(cond-> {}
                      (:video-format %) (assoc :video-format (:video-format %))
                      (:av-downloader-extra-args %) (assoc :extra-args (:av-downloader-extra-args %)))
    :default {:video-format "bestvideo[height<=1080][ext=mp4][vcodec^=avc1]+bestaudio[ext=m4a]/bestvideo[height<=1080][ext=mp4]+bestaudio[ext=m4a]/best[height<=1080]"
              :extra-args ["--embed-metadata"
                           "--embed-chapters"
                           "--write-subs"
                           "--write-auto-subs"
                           "--sub-langs" "en,en-orig,en-en,-live_chat"]
              :max-attempts 3
              :retry-cooldown-minutes 30}
    :spec :irq0-appconfig/podcast-download
    :doc "Podcast media downloader policy."}
   {:path [:podcast :scan]
    :default {:limit 200}
    :spec :irq0-appconfig/podcast-scan
    :doc "Podcast scanner policy."}
   {:path [:update]
    :appconfig-path [:update-max-retry]
    :appconfig->rc #(hash-map :max-retry %)
    :default {:max-retry 5}
    :spec :irq0-appconfig/update
    :doc "Source update retry policy."}])

(defn- shipped-system-config-defaults []
  (edn/read-string (slurp (io/resource "config.edn"))))

(def ^:private shipped-appconfig-defaults
  (shipped-system-config-defaults))

(defn- appconfig-value [entry config]
  (let [value (get-in config (:appconfig-path entry) ::missing)]
    (if (= ::missing value)
      ::missing
      (cond-> value
        (:appconfig->rc entry) ((:appconfig->rc entry))))))

(def rc-defaults
  (reduce
   (fn [defaults {:keys [path] :as entry}]
     (let [value (if (contains? entry :default)
                   (:default entry)
                   (appconfig-value entry shipped-appconfig-defaults))]
       (if (= ::missing value)
         defaults
         (assoc-in defaults path value))))
   {}
   rc-registry))

(defonce rc-overrides* (atom {}))

(s/def :llar.rc/path (s/coll-of keyword? :kind vector? :min-count 1))
(s/def :llar.rc.registry/path :llar.rc/path)
(s/def :llar.rc.registry/appconfig-path :llar.rc/path)
(s/def :llar.rc.registry/spec keyword?)
(s/def :llar.rc.registry/doc string?)
(s/def :llar.rc.registry/appconfig->rc fn?)
(s/def :llar.rc/registry-entry
  (s/keys :req-un [:llar.rc.registry/path
                   :llar.rc.registry/spec
                   :llar.rc.registry/doc]
          :opt-un [:llar.rc.registry/appconfig-path
                   :llar.rc.registry/appconfig->rc]))

(defn- deep-merge [& maps]
  (letfn [(merge-entry [a b]
            (if (and (map? a) (map? b))
              (merge-with merge-entry a b)
              b))]
    (apply merge-with merge-entry maps)))

(defn- starts-with-path? [path prefix]
  (= prefix (vec (take (count prefix) path))))

(defn- registry-entry-for-path [path]
  (->> rc-registry
       (filter #(starts-with-path? path (:path %)))
       (sort-by (comp count :path) >)
       first))

(defn- child-path? [path entry]
  (> (count path) (count (:path entry))))

(defn- assert-known-path! [path]
  (let [entry (registry-entry-for-path path)]
    (when-not (and entry
                   (or (not (child-path? path entry))
                       (map? (get-in rc-defaults (:path entry)))))
      (throw (ex-info "Unknown runtime config path"
                      {:type ::unknown-path
                       :path path
                       :supported-paths (mapv :path rc-registry)})))
    entry))

(defn- assert-valid-registry! []
  (doseq [entry rc-registry]
    (when-not (s/valid? :llar.rc/registry-entry entry)
      (throw (ex-info "Invalid runtime config registry entry"
                      {:type ::invalid-registry-entry
                       :entry entry
                       :explain (s/explain-data :llar.rc/registry-entry entry)})))))

(defn- assert-valid-value! [entry path value]
  (let [spec (:spec entry)
        root-path (:path entry)
        value-to-check (if (= path root-path)
                         value
                         (assoc-in (get-in rc-defaults root-path)
                                   (subvec path (count root-path))
                                   value))]
    (when-not (s/valid? spec value-to-check)
      (throw (ex-info "Invalid runtime config value"
                      {:type ::invalid-value
                       :path path
                       :value value
                       :spec spec
                       :explain (s/explain-data spec value-to-check)})))
    value))

(defn verify-rc-defaults! []
  (assert-valid-registry!)
  (doseq [{:keys [path] :as entry} rc-registry]
    (assert-valid-value! entry path (get-in rc-defaults path)))
  rc-defaults)

(verify-rc-defaults!)

(defn- appconfig-map []
  (when (map? appconfig/appconfig)
    appconfig/appconfig))

(defn rc-appconfig []
  (reduce
   (fn [config-values {:keys [path appconfig-path] :as entry}]
     (if (and appconfig-path (appconfig-map))
       (let [value (appconfig-value entry (appconfig-map))
             shipped-value (appconfig-value entry shipped-appconfig-defaults)]
         (if (or (= ::missing value)
                 (= value shipped-value))
           config-values
           (do
             (assert-valid-value! entry path value)
             (assoc-in config-values path value))))
       config-values))
   {}
   rc-registry))

(defn rc-overrides []
  @rc-overrides*)

(defn rc-baseline []
  (deep-merge rc-defaults (rc-appconfig)))

(defn rc-effective []
  (deep-merge (rc-baseline) (rc-overrides)))

(defn rc-entries []
  (for [{:keys [path appconfig-path spec doc]} rc-registry]
    {:path path
     :appconfig-path appconfig-path
     :spec spec
     :doc doc
     :default (get-in rc-defaults path)
     :appconfig (when appconfig-path
                  (get-in (rc-appconfig) path))
     :override (get-in (rc-overrides) path)
     :effective (get-in (rc-effective) path)
     :example (format "(rc %s VALUE)" (pr-str path))}))

(defn reset-rc! []
  (reset! rc-overrides* {}))

(defn- kvs->map [form kvs]
  (when (odd? (count kvs))
    (throw (ex-info (str form " expects key/value pairs")
                    {:type ::invalid-key-value-form
                     :form form
                     :args kvs})))
  (apply hash-map kvs))

(defn- assert-known-keys! [form allowed-keys opts]
  (doseq [k (keys opts)]
    (when-not (contains? allowed-keys k)
      (throw (ex-info (str form " received an unknown option")
                      {:type ::unknown-option
                       :form form
                       :option k
                       :supported allowed-keys})))))

(defn ^{:llar.config/kind :construct
        :llar.config/form "(rc PATH VALUE)"
        :llar.config/order 80
        :llar.config/keys ["PATH is a vector under a supported runtime config root"
                           "VALUE is validated with the path's runtime config spec"
                           "Supported roots include [:reader ...], [:podcast ...], [:digest], and [:update]"]
        :llar.config/example "(rc [:reader :ranking :highlight-boost-hours] 48)\n(rc [:reader :default-list-view :storage] :headlines)"}
  rc
  "Read or set dynamic runtime behavior config.

  Reads resolve runtime overrides first, system config values that differ from
  shipped defaults second, and shipped defaults last."
  ([path]
   (let [path (vec path)]
     (assert-known-path! path)
     (get-in (rc-effective) path)))
  ([path value]
   (let [path (vec path)
         entry (assert-known-path! path)]
     (assert-valid-value! entry path value)
     (swap! rc-overrides* assoc-in path value)
     value)))

(defn ^{:llar.config/kind :construct
        :llar.config/form "(digest KEY VALUE ...)"
        :llar.config/order 86
        :llar.config/keys ["Required key: :to"
                           "Optional keys: :from, :limit, :inline-images?, :keep-unread-issues"
                           "Defining digest enables digest delivery."]
        :llar.config/example "(digest :to \"you_abc123@kindle.com\"\n        :from \"llar@example.org\"\n        :limit 200\n        :inline-images? true\n        :keep-unread-issues 1)"}
  digest
  "Configure and enable digest delivery."
  [& kvs]
  (let [opts (assoc (kvs->map "digest" kvs) :enabled? true)]
    (assert-known-keys! "digest"
                        #{:enabled? :to :from :limit :inline-images? :keep-unread-issues}
                        opts)
    (rc [:digest] (merge (rc [:digest]) opts))))

(defn ^{:llar.config/kind :construct
        :llar.config/form "(podcast-download KEY VALUE ...)"
        :llar.config/order 87
        :llar.config/keys ["Optional keys: :video-format, :extra-args, :max-attempts, :retry-cooldown-minutes"
                           "Defining podcast-download enables podcast media downloading and retention jobs."]
        :llar.config/example "(podcast-download :video-format \"bestvideo[height<=1080][ext=mp4]+bestaudio[ext=m4a]/best[height<=1080]\"\n                  :extra-args [\"--embed-metadata\" \"--embed-chapters\"]\n                  :max-attempts 3\n                  :retry-cooldown-minutes 30)"}
  podcast-download
  "Configure and enable podcast media downloads."
  [& kvs]
  (let [opts (kvs->map "podcast-download" kvs)]
    (assert-known-keys! "podcast-download"
                        #{:video-format :extra-args :max-attempts :retry-cooldown-minutes}
                        opts)
    (rc [:podcast :download] (merge (rc [:podcast :download]) opts))
    (rc [:podcast :enabled?] true)
    (rc [:podcast :download])))

(defn ^{:llar.config/kind :construct
        :llar.config/form "(reader-favorites FAVORITES)"
        :llar.config/order 81
        :llar.config/keys ["FAVORITES is the complete ordered list of [KEY GROUP] pairs"
                           "KEY is a source tag, item tag, source key, or view key"
                           "GROUP is one of :default, :item-tags, :source-tag, or :type"
                           "Omit entries from FAVORITES to remove them from the reader navigation"]
        :llar.config/example "(reader-favorites\n [[:all :default]\n  [:saved :item-tags]\n  [:highlight :item-tags]\n  [:bookmark :type]])"}
  reader-favorites
  "Set the complete ordered reader favorite navigation list."
  [favorites]
  (rc [:reader :favorites] (vec favorites)))

(defn ^{:llar.config/kind :construct
        :llar.config/form "(reader-default-list-view KEY STYLE)"
        :llar.config/order 82
        :llar.config/keys ["KEY is a source tag, item tag, source key, or view key"
                           "STYLE is one of :headlines or :gallery"]
        :llar.config/example "(reader-default-list-view :storage :headlines)\n(reader-default-list-view :tweet :gallery)"}
  reader-default-list-view
  "Set the default reader list style for one group item."
  [key style]
  (rc [:reader :default-list-view key] style))

(defn ^{:llar.config/kind :construct
        :llar.config/form "(reader-ranking KEY VALUE ...)"
        :llar.config/order 83
        :llar.config/keys ["KEY is :highlight-boost-hours or :rarity-boost-cap-hours"
                           "VALUE is numeric"
                           "Multiple key/value pairs can be set in one form"]
        :llar.config/example "(reader-ranking :highlight-boost-hours 48\n                :rarity-boost-cap-hours 168)"}
  reader-ranking
  "Set reader ranking tuning values."
  [& kvs]
  (when (odd? (count kvs))
    (throw (ex-info "reader-ranking expects key/value pairs"
                    {:type ::invalid-reader-ranking
                     :form kvs})))
  (doseq [[k v] (partition 2 kvs)]
    (rc [:reader :ranking k] v))
  (rc [:reader :ranking]))

(defn ^{:llar.config/kind :construct
        :llar.config/form "(reader-url-handler CONFIG)"
        :llar.config/order 84
        :llar.config/keys ["CONFIG is nil, a map, or key/value pairs"
                           "Required key: :template"
                           "Optional keys: :name, :icon"
                           "The template supports {title}, {url}, {id}, {source}, and {body}"]
        :llar.config/example "(reader-url-handler :name \"Org-roam\"\n                    :icon \"fas fa-brain\"\n                    :template \"org-protocol://roam-ref?ref={url}&title={title}&body={body}\")"}
  reader-url-handler
  "Configure reader annotation export through an external URL handler."
  [& args]
  (let [config (cond
                 (and (= 1 (count args)) (nil? (first args)))
                 nil

                 (and (= 1 (count args)) (map? (first args)))
                 (first args)

                 (odd? (count args))
                 (throw (ex-info "reader-url-handler expects a map, nil, or key/value pairs"
                                 {:type ::invalid-reader-url-handler
                                  :form args}))

                 :else
                 (apply hash-map args))]
    (rc [:reader :export :url-handler] config)))

(defmacro ^{:llar.config/kind :construct
            :llar.config/form "(podcast-retention SOURCE-KEY LIMIT)"
            :llar.config/order 85
            :llar.config/keys ["SOURCE-KEY is the configured source key"
                               "LIMIT is the episode count to retain"]
            :llar.config/example "(podcast-retention my-video-feed 10)"}
  podcast-retention
  "Override podcast retention for a source."
  [source-key limit]
  `(rc [:podcast :retention :sources ~(keyword source-key)] ~limit))
