(ns llar.docs.config
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [hiccup2.core :as h]
   [llar.config :as config]
   [llar.rc :as rc]
   [llar.sched :as sched]
   [llar.src]
   [llar.update])
  (:import
   [java.nio.file Files Path StandardCopyOption]))

(def static-assets
  [{:resource "status/bootstrap/css/bootstrap.min.css"
    :target "static/bootstrap/css/bootstrap.min.css"}
   {:resource "status/ibmplex/Web/css/ibm-plex.min.css"
    :target "static/ibmplex/Web/css/ibm-plex.min.css"}
   {:resource "status/llar.css"
    :target "static/llar.css"}])

(defn- resolve-var [entry]
  (cond
    (var? (:var entry)) (:var entry)
    (:var entry) (requiring-resolve (:var entry))
    (var? (:doc-var entry)) (:doc-var entry)
    (:doc-var entry) (requiring-resolve (:doc-var entry))))

(defn- doc-meta [{:keys [resolved-var] :as entry}]
  (merge (when resolved-var (meta resolved-var))
         (select-keys entry [:llar.config/kind
                             :llar.config/form
                             :llar.config/order
                             :llar.config/keys
                             :llar.config/specs
                             :llar.config/defaults
                             :llar.config/example
                             :doc])))

(defn- documented-config-symbols [kinds]
  (->> (config/config-engine-environment)
       (remove :llar.config/private?)
       (keep (fn [entry]
               (when-let [v (resolve-var entry)]
                 (let [m (doc-meta (assoc entry :resolved-var v))]
                   (when (contains? kinds (:llar.config/kind m))
                     (assoc entry :resolved-var v))))))
       (sort-by (juxt (comp #(or % 1000) :llar.config/order doc-meta)
                      (comp str :symbol)))))

(defn runtime-constructs []
  (documented-config-symbols #{:construct :special-form}))

(defn helper-bindings []
  (documented-config-symbols #{:helper}))

(defn namespace-bindings []
  (->> (config/config-engine-environment)
       (remove :llar.config/private?)
       (filter :namespace)))

(defn processing-context-bindings []
  config/config-processing-context)

(defn source-predicate-context-bindings []
  config/config-source-predicate-context)

(defn source-constructors []
  (->> (config/config-engine-environment)
       (filter #(= :source (:docs-kind %)))
       (mapcat (fn [{alias :symbol ns-name :namespace}]
                 (for [[source-name v] (ns-publics ns-name)
                       :when (= :source (:llar.config/kind (meta v)))]
                   {:symbol (symbol (str alias "/" source-name))
                    :resolved-var v})))
       (sort-by (juxt (comp #(or % 1000) :llar.config/order meta :resolved-var)
                      (comp str :symbol)))))

(defn examples []
  (->> (concat (runtime-constructs) (source-constructors))
       (keep (fn [v]
               (when-let [example (:llar.config/example (doc-meta v))]
                 {:title (str (:symbol v))
                  :code example})))))

(defn- predicate-name [x]
  (cond
    (identical? x string?) "string?"
    (identical? x keyword?) "keyword?"
    (identical? x fn?) "fn?"
    :else nil))

(defn- value-label [x]
  (cond
    (keyword? x) (str x)
    (symbol? x) (str x)
    (string? x) x
    (set? x) (str "#{" (string/join " " (sort (map str x))) "}")
    (map? x) (pr-str x)
    (predicate-name x) (predicate-name x)
    :else (pr-str x)))

(defn- arglists-form [display-name arglists]
  (->> arglists
       (map #(str "(" display-name
                  (when (seq %) " ")
                  (string/join " " (map pr-str %))
                  ")"))
       (string/join "\n")))

(defn- doc-entry [{:keys [symbol] :as entry}]
  (let [m (doc-meta entry)
        display-name (str symbol)]
    {:name display-name
     :form (or (:llar.config/form m)
               (arglists-form display-name (:arglists m)))
     :description (:doc m)
     :keys (:llar.config/keys m)
     :specs (:llar.config/specs m)
     :defaults (:llar.config/defaults m)
     :example (:llar.config/example m)}))

(defn- code [s]
  [:code s])

(defn- code-block [s]
  [:pre {:class "bg-light border rounded p-3"} [:code s]])

(defn- section [id title & body]
  (into [:section {:id id :class "mb-5"}
         [:h2 {:class "border-bottom pb-2"} title]]
        body))

(defn- pair-table [rows]
  [:table {:class "table table-sm align-middle"}
   [:tbody
    (for [[k description] rows]
      [:tr
       [:th {:scope "row"} (code (value-label k))]
       [:td description]])]])

(defn rc-path-table []
  [:table {:class "table table-sm align-middle"}
   [:thead
    [:tr
     [:th {:scope "col"} "Path"]
     [:th {:scope "col"} "Description"]
     [:th {:scope "col"} "Spec"]
     [:th {:scope "col"} "System config path"]
     [:th {:scope "col"} "Example"]]]
   [:tbody
    (for [{:keys [path doc spec appconfig-path example]} (rc/rc-entries)]
      [:tr
       [:th {:scope "row"} (code (pr-str path))]
       [:td doc]
       [:td (code (pr-str spec))]
       [:td (when appconfig-path
              (code (pr-str appconfig-path)))]
       [:td (code example)]])]])

(defn- construct-card [v]
  (let [{:keys [name form description keys specs defaults example]} (doc-entry v)]
    [:div {:class "card mb-3"}
     [:div {:class "card-body"}
      [:h3 {:class "h5 card-title"} (code name)]
      (when description
        [:p {:class "card-text"} description])
      (when (seq form)
        (code-block form))
      (when (seq keys)
        [:div
         [:h4 {:class "h6 mt-3"} "Options and notes"]
         [:ul
          (for [k keys]
            [:li (value-label k)])]])
      (when (seq specs)
        [:div
         [:h4 {:class "h6 mt-3"} "Schemas"]
         [:ul
          (for [s specs]
            [:li (code (value-label s)) " " (value-label (clojure.spec.alpha/describe s))])]])
      (when (seq defaults)
        [:div
         [:h4 {:class "h6 mt-3"} "Defaults"]
         (code-block (pr-str defaults))])
      (when example
        [:div
         [:h4 {:class "h6 mt-3"} "Example"]
         (code-block example)])]]))

(defn docs-fragment []
  [:main {:class "llar-docs"}
   [:div {:class "mb-5"}
    [:h1 "LLAR Configuration"]
    [:p {:class "lead"}
     "Runtime configuration is written in " (code ".llar")
     " files. These files define sources, schedules, processing hooks, highlights, and related behavior."]
    [:p
     "This page is generated from metadata attached to LLAR's config and source vars. The same content is rendered in the dashboard and exported for "
     [:a {:href "https://docs.llar.dev/config.html"} "docs.llar.dev"] "."]]

   (section
    "quick-start"
    "Quick Start"
    [:p "A minimal config defines one or more sources and a schedule that updates them."]
    (code-block "(fetch github-llar-releases (src/feed \"https://github.com/irq0/llar/releases.atom\") :tags #{:my-first-feed :github})\n(fetch hn-frontpage (src/hn :front_page) :tags #{:my-first-feed :hackernews})\n(sched-fetch my-first-feeds :now-and-hourly (some #{:my-first-feed} $TAGS))"))

   (section
    "runtime-constructs"
    "Runtime Constructs"
    [:p "Top-level forms accepted in " (code ".llar") " files."]
    (for [construct (runtime-constructs)]
      (construct-card construct)))

   (section
    "sources"
    "Source Constructors"
    [:p "Source constructors live in the " (code "src") " namespace inside " (code ".llar") " files."]
    (for [source (source-constructors)]
      (construct-card source)))

   (section
    "schedules"
    "Canned Schedules"
    [:p "These keywords can be passed as the schedule argument to " (code "sched-fetch") " and internal LLAR schedulers."]
    (pair-table
     (for [[k {:keys [description]}] (sched/canned-schedule-metadata)]
       [k description])))

   (section
    "processing"
    "Processing Hooks"
    [:p
     (code ":pre") ", " (code ":rm") ", and " (code ":post")
     " forms are evaluated with these bindings."]
    (pair-table (map (juxt :symbol :doc) (processing-context-bindings)))
    [:h3 {:class "h5 mt-4"} "Source Predicate Bindings"]
    [:p
     (code "sched-fetch") " and " (code "autoread")
     " predicates are evaluated with these bindings."]
    (pair-table (map (juxt :symbol :doc) (source-predicate-context-bindings)))
    [:h3 {:class "h5 mt-4"} "Helper Bindings"]
    (pair-table (map (fn [{:keys [symbol] :as entry}]
                       [symbol (:doc (doc-meta entry))])
                     (helper-bindings)))
    [:h3 {:class "h5 mt-4"} "Available Namespaces"]
    (pair-table (map (fn [{:keys [symbol namespace]}]
                       [symbol (str namespace)])
                     (namespace-bindings))))

   (section
    "examples"
    "Examples"
    (for [{:keys [title code]} (examples)]
      [:div {:class "mb-4"}
       [:h3 {:class "h5"} title]
       (code-block code)]))

   (section
    "runtime-config-settings"
    "Runtime Config Settings"
    [:p
     (code "rc") " controls dynamic runtime behavior settings. It reads runtime overrides first, system config values second, and shipped defaults from "
     (code "resources/config.edn") " last."]
    (rc-path-table))

   (section
    "system-config"
    "System Config"
    [:p
     "System config is EDN, loaded at startup before runtime " (code ".llar")
     " files. It configures paths, commands, API ports, PostgreSQL pools, mail transport, credentials location, and other service-level settings."]
    [:p
     "Runtime behavior settings such as reader favorites, default list views, and ranking tuning are available through "
     (code "rc") ". Existing system config keys for those settings remain supported through their system config paths."]
    [:p
     "Use " (code "resources/config.edn") " as the complete default example and "
     (code "docker/docker-config.edn") " for Docker Compose deployments. Secrets belong in "
     (code "credentials.edn") "."])])

(defn docs-page []
  [:html {:lang "en"}
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
    [:title "LLAR Configuration"]
    [:link {:rel "stylesheet" :href "static/bootstrap/css/bootstrap.min.css"}]
    [:link {:rel "stylesheet" :href "static/ibmplex/Web/css/ibm-plex.min.css"}]
    [:link {:rel "stylesheet" :href "static/llar.css"}]]
   [:body
    [:div {:class "container my-4"}
     (docs-fragment)]]])

(defn render-static-html []
  (str "<!doctype html>\n" (h/html (docs-page))))

(defn- copy-resource! [{:keys [resource target]} out-dir]
  (let [src (io/resource resource)
        target-path (.resolve ^Path out-dir target)]
    (when (nil? src)
      (throw (ex-info "Documentation asset not found" {:resource resource})))
    (Files/createDirectories (.getParent target-path) (make-array java.nio.file.attribute.FileAttribute 0))
    (with-open [in (io/input-stream src)]
      (Files/copy in target-path
                  (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING])))
    target-path))

(defn write-static! [out-dir]
  (let [out-path (.toPath (io/file out-dir))
        config-path (.resolve out-path "config.html")]
    (Files/createDirectories out-path (make-array java.nio.file.attribute.FileAttribute 0))
    (spit (.toFile config-path) (render-static-html))
    (into [config-path]
          (map #(copy-resource! % out-path))
          static-assets)))

(defn -main [& [out-dir]]
  (when (string/blank? out-dir)
    (binding [*out* *err*]
      (println "Usage: clojure -M -m llar.docs.config OUT_DIR"))
    (System/exit 1))
  (doseq [path (write-static! out-dir)]
    (println (str path))))
