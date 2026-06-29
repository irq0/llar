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
    :doc "Ranking query tuning."}])

(defn- shipped-system-config-defaults []
  (edn/read-string (slurp (io/resource "config.edn"))))

(def rc-defaults
  (let [shipped-defaults (shipped-system-config-defaults)]
    (reduce
     (fn [defaults {:keys [path appconfig-path]}]
       (assoc-in defaults path (get-in shipped-defaults appconfig-path)))
     {}
     rc-registry)))

(defonce rc-overrides* (atom {}))

(s/def :llar.rc/path (s/coll-of keyword? :kind vector? :min-count 1))
(s/def :llar.rc.registry/path :llar.rc/path)
(s/def :llar.rc.registry/appconfig-path :llar.rc/path)
(s/def :llar.rc.registry/spec keyword?)
(s/def :llar.rc.registry/doc string?)
(s/def :llar.rc/registry-entry
  (s/keys :req-un [:llar.rc.registry/path
                   :llar.rc.registry/spec
                   :llar.rc.registry/doc]
          :opt-un [:llar.rc.registry/appconfig-path]))

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
       (let [value (get-in (appconfig-map) appconfig-path ::missing)]
         (if (= ::missing value)
           config-values
           (do
             (assert-valid-value! entry path value)
             (assoc-in config-values path value))))
       config-values))
   {}
   rc-registry))

(defn rc-overrides []
  @rc-overrides*)

(defn rc-effective []
  (deep-merge rc-defaults (rc-appconfig) (rc-overrides)))

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

(defn ^{:llar.config/kind :construct
        :llar.config/form "(rc PATH VALUE)"
        :llar.config/order 80
        :llar.config/keys ["PATH is a vector under a supported runtime config root"
                           "VALUE is validated with the path's runtime config spec"
                           "Supported roots: [:reader :favorites], [:reader :default-list-view], [:reader :ranking]"]
        :llar.config/example "(rc [:reader :ranking :highlight-boost-hours] 48)\n(rc [:reader :default-list-view :storage] :headlines)"}
  rc
  "Read or set dynamic runtime behavior config.

  Reads resolve runtime overrides first, system config values second, and
  code defaults last."
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
        :llar.config/form "(reader-favorite KEY GROUP)"
        :llar.config/order 81
        :llar.config/keys ["KEY is a source tag, item tag, source key, or view key"
                           "GROUP is one of :default, :item-tags, :source-tag, or :type"
                           "Repeated definitions replace the favorite for KEY and append it at the end"]
        :llar.config/example "(reader-favorite :blog :source-tag)\n(reader-favorite :saved :item-tags)"}
  reader-favorite
  "Add or replace one reader favorite navigation entry."
  [key group]
  (let [favorites (remove #(= key (first %)) (rc [:reader :favorites]))
        updated (conj (vec favorites) [key group])]
    (rc [:reader :favorites] updated)))

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
