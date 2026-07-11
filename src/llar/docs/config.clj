(ns llar.docs.config
  (:require
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [hiccup2.core :as h]
   [llar.appconfig :as appconfig]
   [llar.config :as config]
   [llar.rc :as rc]
   [llar.sched :as sched]
   [llar.export.zotero]
   [llar.src]
   [llar.update])
  (:import
   [java.nio.file Files]))

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

(defn feature-examples []
  (->> ['llar.export.zotero]
       (mapcat ns-publics)
       (keep (fn [[feature-name v]]
               (when (= :feature-example (:llar.config/kind (meta v)))
                 {:symbol feature-name
                  :resolved-var v})))
       (sort-by (juxt (comp #(or % 1000) :llar.config/order meta :resolved-var)
                      (comp str :symbol)))))

(defn examples []
  (->> (concat (runtime-constructs) (source-constructors) (feature-examples))
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

(defn- spec-description [spec]
  (try
    (value-label (s/describe spec))
    (catch Exception _
      nil)))

(def ^:private no-default ::no-default)

(defn- spec-op [form]
  (when (seq? form)
    (some-> form first name keyword)))

(defn- child-specs [spec]
  (let [form (s/form spec)]
    (when (and (seq? form) (= :keys (spec-op form)))
      (let [args (apply hash-map (rest form))]
        (concat
         (for [k (:req args)] [k k true])
         (for [k (:req-un args)] [(keyword (name k)) k true])
         (for [k (:opt args)] [k k false])
         (for [k (:opt-un args)] [(keyword (name k)) k false]))))))

(defn appconfig-spec-rows
  "Flatten the appconfig spec into documentable paths."
  ([] (appconfig-spec-rows :irq0-llar/appconfig (appconfig/documented-defaults)))
  ([root-spec defaults]
   (letfn [(walk [path spec required?]
             (let [value (get-in defaults path no-default)]
               (cons {:path path :spec spec :required? required? :default value}
                     (mapcat (fn [[config-k child-spec child-required?]]
                               (walk (conj path config-k) child-spec child-required?))
                             (child-specs spec)))))]
     (rest (walk [] root-spec true)))))

(defn appconfig-reference-table []
  [:table {:class "table table-sm align-middle"}
   [:thead
    [:tr
     [:th {:scope "col"} "Path"]
     [:th {:scope "col"} "Status"]
     [:th {:scope "col"} "Schema"]
     [:th {:scope "col"} "Default"]]]
   [:tbody
    (for [{:keys [path spec required? default]} (appconfig-spec-rows)]
      [:tr
       [:th {:scope "row"} (code (pr-str path))]
       [:td (if required? "required" "optional")]
       [:td (code (or (spec-description spec) (pr-str spec)))]
       [:td (when-not (= no-default default) (code (pr-str default)))]])]])

(defn- service-card [title config-path description example & notes]
  [:div {:class "card mb-3"}
   [:div {:class "card-body"}
    [:h3 {:class "h5 card-title"} title]
    [:p [:strong "Config path: "] (code (pr-str config-path))]
    [:p description]
    (when (seq notes) (into [:ul] (map (fn [note] [:li note]) notes)))
    [:h4 {:class "h6 mt-3"} "Example"]
    (code-block example)]])

(defn- services-docs []
  [(service-card
    "Dashboard" [:api :dashboard]
    "Administrative UI and Prometheus metrics endpoint. It starts when :port is present."
    ":dashboard {:port 9999}")
   (service-card
    "Reader" [:api :reader]
    "The browser-based reader. It starts when :port is present; :base-url is used when LLAR builds absolute reader links."
    ":reader {:port 8023\n         :base-url \"https://reader.example.org\"}")
   (service-card
    "Podcast" [:api :podcast]
    "HTTP service for downloaded podcast and video media. It starts when :port is present."
    ":podcast {:port 8024\n          :base-url \"https://media.example.org\"\n          :retention {:default-episode-limit 25}}")
   (service-card
    "Fever-compatible sync" [:api :fever]
    "Mobile sync endpoint for Fever clients such as Fiery Feeds and ReadKit. It starts when :port is present."
    ":fever {:port 8025\n        :base-url \"https://reader.example.org/api/fever\"\n        :username \"llar\"\n        :credentials :mobile-sync\n        :source-tag :mobile}\n\n;; credentials.edn\n{:mobile-sync {:password \"replace-me\"}}"
    "Only sources carrying :source-tag (default :mobile) are exposed."
    "Clients authenticate with the Fever MD5 API key derived from username and the dedicated password."
    "The working-set defaults are 30 initial days, 10 recent-read days, and 1048576 content bytes."
    [:span
     [:strong ":base-url is important. "]
     "Set it to the externally reachable URL of this Fever endpoint, including any reverse-proxy path. "
     "Privacy-rewritten images and media are stored in the local blobstore and served by this endpoint at "
     (code "<base-url>/blob/<hash>") ". Content is rewritten to those absolute URLs so mobile clients can load them; "
     "without a correct :base-url the relative " (code "/blob/") " URLs break on the phone. The endpoint must be exposed over HTTPS."])
   (service-card
    "Digest delivery" [:api :digest]
    "Scheduled EPUB delivery to an e-reader address. This is not an HTTP API and requires top-level :mail configuration."
    ":digest {:to \"you_abc123@kindle.com\"\n         :from \"llar@example.org\"\n         :schedule :sundays\n         :keep-unread-issues 1\n         :inline-images? true}")])

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
       [:td
        (code (pr-str spec))
        (when-let [description (spec-description spec)]
          [:div {:class "small text-muted mt-1"} (code description)])]
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
            [:li (code (value-label s)) " " (value-label (s/describe s))])]])
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
     [:a {:href "https://docs.llar.dev/config.html"} "docs.llar.dev"] "."]
    [:div {:class "card mt-4"}
     [:div {:class "card-body"}
      [:h2 {:class "h5 card-title"} "Three configuration sources"]
      [:dl {:class "row mb-0"}
       [:dt {:class "col-sm-3"} "System config"]
       [:dd {:class "col-sm-9"}
        "Startup-only EDN for paths, databases, services, and host settings. LLAR reads the packaged "
        (code "resources/config.edn") ", then " (code "LLAR_CONFIG") ", then the JVM "
        (code "-Dconfig=...") " file; later files replace complete top-level values."]
       [:dt {:class "col-sm-3"} "Runtime config"]
       [:dd {:class "col-sm-9"}
        (code ".llar") " files under " (code ":runtime-config-dir") ", watched and reloaded while LLAR runs. They define sources, schedules, processing, and runtime overrides."]
       [:dt {:class "col-sm-3"} "Credentials"]
       [:dd {:class "col-sm-9"}
        "Secrets in the EDN file selected by " (code ":credentials-file") ". Runtime config and service settings refer to entries by keyword; PostgreSQL passwords are currently configured directly in the system-config pool maps."]]]]]

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
    "feature-examples"
    "Feature Examples"
    [:p "Feature-specific configuration examples that are not top-level " (code ".llar") " constructs."]
    (for [feature (feature-examples)]
      (construct-card feature)))

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
     (code "rc") " controls dynamic runtime behavior settings. It reads runtime overrides first, system config values that differ from shipped defaults second, and shipped defaults from "
     (code "resources/config.edn") " last."]
    (rc-path-table))

   (section
    "system-config"
    "System Config"
    [:p
     "System config is EDN, loaded at startup before runtime " (code ".llar")
     " files. It configures paths, commands, API ports, PostgreSQL pools, mail transport, credentials location, and other service-level settings."]
    [:p
     "Runtime behavior settings such as reader favorites, default list views, ranking tuning, and podcast retention are available through "
     (code "rc") ". Existing system config keys for those settings remain supported through their system config paths."]
    [:p
     "Use " (code "resources/config.edn") " as the complete default example and "
     (code "docker/docker-config.edn") " for Docker Compose deployments. Secrets belong in "
     (code "credentials.edn") "."]
    [:div {:class "alert alert-warning"}
     [:strong "Overrides are shallow. "]
     "Later config files replace complete top-level values. For example, an "
     (code ":api") " override must repeat every API entry that should remain enabled; it is not merged into the shipped "
     (code ":api") " map."])

   (section
    "postgresql"
    "PostgreSQL Connection Pools"
    [:p
     (code "[:postgresql :frontend]") " and " (code "[:postgresql :backend]")
     " are independent HikariCP pool maps. LLAR passes both maps directly to "
     [:a {:href "https://github.com/tomekw/hikari-cp#configuration-options"} "hikari-cp"]
     ", whose configuration reference defines the accepted kebab-case pool keys and their defaults."]
    [:p
     "Connection fields such as " (code ":server-name") ", " (code ":database-name") ", "
     (code ":username") ", and " (code ":password") " select the PostgreSQL database. See the official "
     [:a {:href "https://jdbc.postgresql.org/documentation/datasource/"} "pgJDBC data-source documentation"]
     " for PostgreSQL connection properties. Start with the shipped Docker example and tune pool sizes only when deployment load requires it."]
    (code-block ":postgresql\n{:frontend {:adapter \"postgresql\"\n            :server-name \"db\"\n            :database-name \"llar\"\n            :username \"llar\"\n            :password \"replace-me\"\n            :maximum-pool-size 5\n            :pool-name \"frontend\"}\n :backend  {:adapter \"postgresql\"\n            :server-name \"db\"\n            :database-name \"llar\"\n            :username \"llar\"\n            :password \"replace-me\"\n            :maximum-pool-size 10\n            :pool-name \"backend\"}}")
    [:div {:class "alert alert-secondary"}
     "Database passwords are currently part of system config, unlike named application credentials. Restrict access to the system-config file and avoid committing deployment passwords."])

   (section
    "services-and-apis"
    "Services and APIs"
    [:p "Service settings are part of the top-level " (code ":api") " map. The examples below are entries inside that map; preserve the other shipped entries when overriding it."]
    (for [service (services-docs)] service))

   (section
    "system-config-reference"
    "System Configuration Reference"
    [:p "This reference is generated by walking " (code ":irq0-llar/appconfig") ". Required and optional status comes from the Clojure specs; displayed defaults come from packaged configuration and shared runtime defaults."]
    (appconfig-reference-table))])

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

(defn write-static! [out-dir]
  (let [out-path (.toPath (io/file out-dir))
        config-path (.resolve out-path "config.html")]
    (Files/createDirectories out-path (make-array java.nio.file.attribute.FileAttribute 0))
    (spit (.toFile config-path) (render-static-html))
    [config-path]))

(defn -main [& [out-dir]]
  (when (string/blank? out-dir)
    (binding [*out* *err*]
      (println "Usage: clojure -M -m llar.docs.config OUT_DIR"))
    (System/exit 1))
  (doseq [path (write-static! out-dir)]
    (println (str path))))
