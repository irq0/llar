(ns llar.config
  (:require
   [llar.appconfig :refer [appconfig]]
   [clojure.spec.alpha :as s]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [llar.converter :as converter]
   [mount.core :refer [defstate] :as mount]
   [nextjournal.beholder :as beholder]
   [slingshot.slingshot :refer [try+]]
   [llar.fetchutils :refer [make-reddit-proc]]
   [llar.human :as human]
   [llar.src :as src]
   [llar.rc]
   [llar.fetch.custom]
   [llar.fetch.feed]
   [llar.fetch.http]
   [llar.fetch.imap]
   [llar.fetch.readability]
   [llar.fetch.reddit]
   [llar.fetch.twitter]
   [llar.fetch.hackernews]
   [llar.fetch.streaming]
   [llar.fetch.github]
   [llar.postproc :as proc]))

;; contains all loaded sources
(defonce srcs (atom {}))

;; contains all schedulers created from configuration
;; see llar.sched for other schedulers
(defonce fetch-scheds (atom {}))

;; contains per-tag sort order defaults loaded from .llar files
(defonce sort-order-defaults (atom {}))

(defn get-sources [] @srcs)

(defn get-sort-order-defaults [] @sort-order-defaults)

(defn get-source [k] (get @srcs k))

(defn remove-source [k] (keys (swap! srcs dissoc k)))

(def config-processing-context
  [{:symbol '$item :doc "The full item."}
   {:symbol '$key :doc "Source key as keyword."}
   {:symbol '$title :doc "Item title, or empty string."}
   {:symbol '$authors :doc "Item authors, or empty string."}
   {:symbol '$tags :doc "Source tags configured on the fetch definition."}
   {:symbol '$raw :doc "Raw fetched data when supported by the source."}
   {:symbol '$url :doc "Item URL, or empty string."}
   {:symbol '$html :doc "HTML content, or empty string."}
   {:symbol '$text :doc "Plain text content, or empty string."}
   {:symbol '$score :doc "Score for sources such as Reddit or Hacker News, or -1."}
   {:symbol '$options :doc "Source options configured on the fetch definition."}
   {:symbol '$entry :doc "The item entry map."}])

(def config-source-predicate-context
  [{:symbol '$KEY :doc "Source key as keyword."}
   {:symbol '$SRC :doc "Source constructor value."}
   {:symbol '$TAGS :doc "Source tags configured on the fetch definition."}])

(defn- processing-hook-let-bindings [item src-key tags options]
  (mapcat (fn [{:keys [symbol]}]
            [symbol
             (case symbol
               $item item
               $key src-key
               $title `(or (get-in ~item [:summary :title]) "")
               $authors `(or (get-in ~item [:entry :authors]) "")
               $tags tags
               $raw `(get ~item :raw {})
               $url `(or (get-in ~item [:entry :url]) "")
               $html `(or (get-in ~item [:entry :contents "text/html"]) "")
               $text `(or (get-in ~item [:entry :contents "text/plain"]) "")
               $score `(or (get-in ~item [:entry :score]) -1)
               $options options
               $entry `(:entry ~item))])
          config-processing-context))

(defn source-predicate-let-bindings [source-key source]
  (mapcat (fn [{:keys [symbol]}]
            [symbol
             (case symbol
               $KEY source-key
               $SRC `(:src ~source)
               $TAGS `(:tags ~source))])
          config-source-predicate-context))

(defmacro wrap-proc
  "Evaluate a processing hook with item context bindings."
  [src-key tags options & body]
  (when-not (nil? body)
    (let [item (gensym "item")]
      `(fn [~item]
         (let [~@(processing-hook-let-bindings item src-key tags options)]
           (do ~@body))))))

;; TODO validate filter code. get dummy from source; pass through see if item exists and things
;; TODO validate pre / post
(defmacro fetch
  "Define a source, its source tags, UI options, and optional item processing hooks."
  {:llar.config/kind :construct
   :llar.config/order 10
   :llar.config/keys [":tags tags applied to the source. a set of keywords"
                      ":options reader behavior changes. a set of keywords. supported: :mark-read-on-view, :main-list-use-description, "
                      ":pre pre-processing function body"
                      ":pre-fns a list of functions to pre-process items"
                      ":rm a filter function body"
                      ":rm-fn a list of filter functions"
                      ":post a post-processing function body"
                      ":post-fns a list of post-processing functions"]
   :llar.config/example "(fetch github-llar-releases\n  (src/feed \"https://github.com/irq0/llar/releases.atom\")\n  :tags #{:github :release})"}
  [src-key src & body]
  (s/valid? symbol? src-key)
  (s/valid? src/source? src)
  (let [{:keys [options tags post pre rm post-fns pre-fns rm-fn]
         :or {options #{} tags #{}}}
        (apply hash-map body)

        src-kw (keyword src-key)

        pre (cond (some? pre-fns) pre-fns
                  (some? pre) [`(wrap-proc ~src-kw ~tags ~options ~pre)]
                  :else nil)

        post (cond (some? post-fns) post-fns
                   (some? post) [`(wrap-proc ~src-kw ~tags ~options ~post)]
                   :else nil)

        rm (cond (some? rm-fn) rm-fn
                 (some? rm) `(wrap-proc ~src-kw ~tags ~options ~rm)
                 :else '(constantly false))]

    (s/valid? (s/coll-of keyword? :kind set?) tags)
    (s/valid? (s/coll-of keyword? :kind set?) options)

    `(do (swap! srcs assoc (keyword '~src-key)
                {:src ~src
                 :options ~options
                 :tags ~tags
                 :proc (proc/new {:rm ~rm
                                  :pre ~pre
                                  :post ~post})})
         (keyword '~src-key))))

(defmacro fetch-reddit
  "Define a Reddit source with a generated source key and the source tag :reddit."
  {:llar.config/kind :construct
   :llar.config/order 20
   :llar.config/keys [":min-score score below will be filtered out"
                      ":dynamic? enable dynamic score cut-off feature. keep top 5% rated entries"
                      ":tags see fetch"
                      ":options see fetch"]
   :llar.config/example "(fetch-reddit (src/reddit \"clojure\" :top :week)\n  :min-score 50\n  :tags #{:programming})"}
  [src & body]
  (let [{:keys [options tags min-score dynamic?]
         :or {options #{} tags #{} min-score 0 dynamic? true}} (apply hash-map body)]
    (s/valid? (s/coll-of keyword? :kind set?) tags)
    (s/valid? (s/coll-of keyword? :kind set?) options)

    `(do (let [src-key# (keyword (str "reddit-" (string/lower-case (:subreddit ~src))))]
           (swap! srcs assoc src-key#
                  {:src ~src
                   :options ~options
                   :tags (conj ~tags :reddit)
                   :proc (make-reddit-proc src-key# ~src {:min-score ~min-score
                                                          :dynamic? ~dynamic?})})
           (keyword src-key#)))))

;; autoread feature: remove unread tag after a certain time

(defonce autoread (atom {}))

(defn get-autoread-scheds [] @autoread)

(defmacro wrap-predicate [& body]
  (when-not (nil? body)
    (let [source-key (gensym "source-key")
          source (gensym "source")]
      `(fn [[~source-key ~source]]
         (let [~@(source-predicate-let-bindings source-key source)]
           (do ~@body))))))

(defmacro defsched-remove-unread-tag
  "Automatically remove :unread from old items fetched from matching sources."
  {:llar.config/kind :construct
   :llar.config/form "(autoread SCHED-NAME PERIOD PREDICATE)"
   :llar.config/order 40
   :llar.config/keys ["PERIOD is a java-time period such as (time/weeks 4)"
                      "PREDICATE can use $KEY, $SRC, and $TAGS"]
   :llar.config/example "(autoread reddit-ages-fast (time/weeks 4)\n  (some #{:reddit} $TAGS))"}
  [sched-key period pred]
  `(swap! autoread assoc (keyword '~sched-key)
          {:period ~period
           :pred (wrap-predicate ~pred)}))

(fetch bookmark nil :tags #{:bookmark})

(def ^{:llar.config/kind :special-form
       :llar.config/form "(highlight words|authors VALUE...)"
       :llar.config/order 50
       :doc "Add :highlight to items matching configured words or authors."
       :llar.config/keys ["words are matched against extracted item terms"
                          "authors are matched case-insensitively"]
       :llar.config/example "(highlight words \"llar\" \"rss\")\n(highlight authors \"Douglas Engelbart\")"}
  highlight nil)

(def ^{:llar.config/kind :special-form
       :llar.config/form "(sort-default TAG-KEY SORT-ORDER)"
       :llar.config/order 70
       :doc "Set the default sort order for source or item tag views."
       :llar.config/keys ["SORT-ORDER is one of :newest, :ranked, or :oldest"]
       :llar.config/example "(sort-default :blog :ranked)"}
  sort-default nil)

(def config-engine-registry (atom []))

(s/def :llar.config.entry/symbol symbol?)
(s/def :llar.config.entry/var qualified-symbol?)
(s/def :llar.config.entry/doc-var qualified-symbol?)
(s/def :llar.config.entry/handler-fn qualified-symbol?)
(s/def :llar.config.entry/namespace symbol?)
(s/def :llar.config.entry/doc string?)
(s/def :llar.config.entry/docs-kind keyword?)
(s/def :llar.config/kind #{:construct :special-form :helper})
(s/def :llar.config/private? boolean?)
(s/def :llar.config/entry
  (s/and
   (s/keys :req-un [:llar.config.entry/symbol]
           :opt-un [:llar.config.entry/var
                    :llar.config.entry/doc-var
                    :llar.config.entry/handler-fn
                    :llar.config.entry/namespace
                    :llar.config.entry/doc
                    :llar.config.entry/docs-kind]
           :opt [:llar.config/kind
                 :llar.config/private?])
   #(or (:var %) (:doc-var %) (:namespace %))
   #(not (and (:var %) (:namespace %)))))

(defn- normalize-config-engine-entry [entry]
  (-> entry
      (cond-> (:kind entry)
        (assoc :llar.config/kind (:kind entry)))
      (cond-> (:private? entry)
        (assoc :llar.config/private? true))
      (dissoc :kind :private?)))

(defn- assert-valid-config-engine-entry! [entry]
  (when-not (s/valid? :llar.config/entry entry)
    (throw (ex-info "Invalid config engine entry"
                    {:entry entry
                     :explain (s/explain-data :llar.config/entry entry)})))
  entry)

(defn- assert-resolved-var! [entry k]
  (when-let [sym (get entry k)]
    (when-not (requiring-resolve sym)
      (throw (ex-info "Config engine entry target does not resolve"
                      {:entry entry
                       :key k
                       :symbol sym}))))
  entry)

(defn- assert-resolved-handler! [entry]
  (when-let [sym (:handler-fn entry)]
    (let [v (requiring-resolve sym)]
      (when-not (and v (ifn? @v))
        (throw (ex-info "Config engine handler does not resolve to a function"
                        {:entry entry
                         :handler-fn sym})))))
  entry)

(defn- assert-resolved-config-engine-entry! [entry]
  (-> entry
      (assert-resolved-var! :var)
      (assert-resolved-var! :doc-var)
      (assert-resolved-handler!)))

(defn- upsert-config-engine-entry [entries entry]
  (let [entry-symbol (:symbol entry)]
    (if (some #(= entry-symbol (:symbol %)) entries)
      (mapv #(if (= entry-symbol (:symbol %)) entry %) entries)
      (conj (vec entries) entry))))

(defn register-config-engine-entry! [entry]
  (swap! config-engine-registry upsert-config-engine-entry
         (assert-valid-config-engine-entry! (normalize-config-engine-entry entry)))
  nil)

(defmacro defconfig-symbol [symbol & {:keys [var doc-var handler-fn namespace] :as opts}]
  `(register-config-engine-entry!
    ~(cond-> (-> opts
                 (dissoc :var :doc-var :handler-fn :namespace)
                 (assoc :symbol (list 'quote symbol)))
       var (assoc :var (list 'quote var))
       doc-var (assoc :doc-var (list 'quote doc-var))
       handler-fn (assoc :handler-fn (list 'quote handler-fn))
       namespace (assoc :namespace (list 'quote namespace)))))

(defn config-engine-environment []
  (mapv assert-resolved-config-engine-entry! @config-engine-registry))

(defconfig-symbol fetch
  :var llar.config/fetch
  :handler-fn llar.config/handle-fetch-config-form!)

(defconfig-symbol fetch-reddit
  :var llar.config/fetch-reddit
  :handler-fn llar.config/handle-fetch-config-form!)

(defconfig-symbol sched-fetch
  :var llar.update/defsched-feed-by-filter
  :handler-fn llar.config/handle-scheduler-config-form!)

(defconfig-symbol autoread
  :var llar.config/defsched-remove-unread-tag
  :handler-fn llar.config/handle-eval-config-form!)

(defconfig-symbol highlight
  :doc-var llar.config/highlight
  :handler-fn llar.config/handle-highlight-config-form!)

(defconfig-symbol podcast-retention
  :var llar.rc/podcast-retention
  :handler-fn llar.config/handle-eval-config-form!)

(defconfig-symbol digest
  :var llar.rc/digest
  :handler-fn llar.config/handle-eval-config-form!)

(defconfig-symbol podcast-download
  :var llar.rc/podcast-download
  :handler-fn llar.config/handle-eval-config-form!)

(defconfig-symbol sort-default
  :doc-var llar.config/sort-default
  :handler-fn llar.config/handle-sort-default-config-form!)

(defconfig-symbol rc
  :var llar.rc/rc
  :handler-fn llar.config/handle-eval-config-form!)

(defconfig-symbol reader-favorite
  :var llar.rc/reader-favorite
  :handler-fn llar.config/handle-eval-config-form!)

(defconfig-symbol reader-default-list-view
  :var llar.rc/reader-default-list-view
  :handler-fn llar.config/handle-eval-config-form!)

(defconfig-symbol reader-ranking
  :var llar.rc/reader-ranking
  :handler-fn llar.config/handle-eval-config-form!)

(defconfig-symbol reader-url-handler
  :var llar.rc/reader-url-handler
  :handler-fn llar.config/handle-eval-config-form!)

(defconfig-symbol srcs
  :var llar.config/srcs
  :private? true)

(defconfig-symbol $credentials
  :var llar.appconfig/credentials
  :kind :helper
  :doc "Read an entry from credentials.edn.")

(defconfig-symbol $html2text
  :var llar.commands/html2text
  :kind :helper
  :doc "Convert HTML to plain text.")

(defconfig-symbol $ellipsify
  :var llar.human/truncate-ellipsis
  :kind :helper
  :doc "Truncate text with an ellipsis.")

(defconfig-symbol $parse-ts
  :var llar.fetchutils/parse-timestamp
  :kind :helper
  :doc "Parse a timestamp into a zoned date time.")

(defconfig-symbol $category-rm
  :var llar.fetchutils/make-category-filter-deny
  :kind :helper
  :doc "Build a filter that removes items by feed category.")

(defconfig-symbol $extract
  :var llar.fetchutils/readability-contents
  :kind :helper
  :doc "Run article extraction on HTML content.")

(defconfig-symbol $add-tag
  :var llar.fetchutils/add-tag
  :kind :helper
  :doc "Return a processor that adds an item tag.")

(defconfig-symbol $add-tag-filter
  :var llar.fetchutils/add-tag-filter
  :kind :helper
  :doc "Return a processor that adds a tag when a predicate matches.")

(defconfig-symbol $exchange
  :var llar.fetchutils/exchange
  :kind :helper
  :doc "Swap two item paths.")

(defconfig-symbol $html-to-hickory
  :var llar.fetchutils/html-to-hickory
  :kind :helper
  :doc "Parse HTML into Hickory.")

(defconfig-symbol $hickory-sanitize-blobify
  :var llar.fetchutils/hickory-sanitize-blobify
  :kind :helper
  :doc "Sanitize and blobify Hickory content.")

(defconfig-symbol $make-item-hash
  :var llar.fetch/make-item-hash
  :kind :helper
  :doc "Create a stable LLAR item hash.")

(defconfig-symbol $fetch
  :var llar.http/fetch
  :kind :helper
  :doc "Run LLAR HTTP fetch.")

(defconfig-symbol $parse-url
  :var llar.http/absolutify-url
  :kind :helper
  :doc "Parse or absolutify URLs.")

(defconfig-symbol $hickory-to-html
  :var hickory.render/hickory-to-html
  :kind :helper
  :doc "Render Hickory as HTML.")

(defconfig-symbol uri
  :var org.bovinegenius.exploding-fish/uri
  :private? true)

(defconfig-symbol $uri-path
  :var org.bovinegenius.exploding-fish/path
  :kind :helper
  :doc "Read the path from a URI.")

(defconfig-symbol $http-cookie-store
  :var clj-http.cookies/cookie-store
  :kind :helper
  :doc "Create a clj-http cookie store.")

(defconfig-symbol $http-get
  :var clj-http.client/get
  :kind :helper
  :doc "Call clj-http.client/get.")

(defconfig-symbol $http-post
  :var clj-http.client/post
  :kind :helper
  :doc "Call clj-http.client/post.")

(defconfig-symbol src
  :namespace llar.src
  :docs-kind :source)

(defconfig-symbol string
  :namespace clojure.string)

(defconfig-symbol log
  :namespace clojure.tools.logging)

(defconfig-symbol S
  :namespace hickory.select)

(defconfig-symbol time
  :namespace java-time.api)

(defn- var-namespace [sym]
  (symbol (namespace sym)))

(defn- var-name [sym]
  (symbol (name sym)))

(defn- config-var-require-specs []
  (->> (config-engine-environment)
       (filter :var)
       (group-by (comp var-namespace :var))
       (map (fn [[ns-name entries]]
              (let [refers (->> entries (map (comp var-name :var)) distinct vec)
                    renames (->> entries
                                 (keep (fn [{:keys [symbol var]}]
                                         (let [v-name (var-name var)]
                                           (when (not= symbol v-name)
                                             [v-name symbol]))))
                                 (into {}))]
                (cond-> [ns-name :refer refers]
                  (seq renames) (into [:rename renames])))))))

(defn- config-namespace-require-specs []
  (for [{:keys [symbol namespace]} (filter :namespace (config-engine-environment))]
    [namespace :as symbol]))

(defn- require-config-engine! []
  (apply require
         (concat (config-var-require-specs)
                 (config-namespace-require-specs))))

(defn- config-entry [sym]
  (some (fn [{:keys [symbol] :as entry}]
          (when (= sym symbol) entry))
        (config-engine-environment)))

(defn- read-config [path]
  (with-open [r (io/reader path)]
    (let [reader (java.io.PushbackReader. r)]
      (loop [forms []]
        (let [form (try (read reader nil :eof)
                        (catch Exception e
                          (log/errorf "Error reading config file %s: %s" path (ex-message e))
                          :eof))]
          (if (= form :eof)
            forms
            (recur (conj forms form))))))))

(defn handle-unknown-config-form! [form]
  (log/warnf "unknown config definition \"%s\". Skipping." form))

(defn handle-highlight-config-form! [form]
  (try
    (if (#{'words 'authors} (second form))
      (do
        (log/debugf "loading highlight config \"%s\"" form)
        (swap! proc/highlight-matches assoc (keyword (second form))
               (into #{} (map string/lower-case (drop 2 form)))))
      (handle-unknown-config-form! form))
    (catch Exception e
      (log/error e "failed to load highlight def" (second form)))))

(defn handle-fetch-config-form! [form]
  (try+
   (log/debugf "loading fetch \"%s\"" (second form))
   (eval form)
   #_{:clj-kondo/ignore [:unresolved-symbol]}
   (catch [:clojure.spec.alpha/failure :assertion-failed] x
     (log/errorf "failed to load fetch def %s. spec %s failed. value:%s problems:%s form:%s" (second form)
                 (:clojure.spec.alpha/spec x) (:clojure.spec.alpha/value x) (:clojure.spec.alpha/problems x)
                 form))
   #_{:clj-kondo/ignore [:unresolved-symbol]}
   (catch Object _
     (log/errorf (:throwable &throw-context) "failed to load fetch def \"%s\"" (second form)))))

(defn handle-scheduler-config-form! [form]
  (try
    (log/debugf "loading scheduler \"%s\": %s" (second form) form)
    (let [sched (eval form)]
      (log/debug "creating scheduler: " sched)
      (swap! fetch-scheds assoc (keyword (second form)) sched)
      (mount/start (vals @fetch-scheds)))
    (catch Exception e
      (log/error e "failed to load scheduler def" (second form)))))

(defn handle-eval-config-form! [form]
  (try
    (log/debugf "loading config definition \"%s\": %s" (first form) form)
    (eval form)
    (catch Exception e
      (log/error e "failed to load config definition" (first form)))))

(defn handle-sort-default-config-form! [form]
  (try
    (let [tag-key (keyword (second form))
          order (keyword (nth form 2))]
      (when-not (#{:newest :ranked :oldest} order)
        (log/warnf "sort-default: unknown sort order '%s' for tag '%s'" order tag-key))
      (log/debugf "loading sort-default: %s -> %s" tag-key order)
      (swap! sort-order-defaults assoc tag-key order))
    (catch Exception e
      (log/error e "failed to load sort-default def" (second form)))))

(defn- config-form-handler [form]
  (if-let [handler-sym (:handler-fn (when (list? form) (config-entry (first form))))]
    (deref (requiring-resolve handler-sym))
    handle-unknown-config-form!))

(defn- eval-config-form [form]
  (binding [*ns* (create-ns 'llar.config.eval)]
    ;; exclude list based on clojail blacklist-symbols
    (refer-clojure :exclude [eval read alter-var-root intern
                             load-string load-reader ns-resolve resolve find-var
                             *read-eval* ns-publics ns-unmap ns-map ns-interns the-ns
                             push-thread-bindings pop-thread-bindings future-call agent send
                             send-off pmap pcalls in-ns
                             with-redefs-fn])
    (require-config-engine!)
    ((config-form-handler form) form)))

(defn- get-config-files [extension]
  (filter #(string/ends-with? (.getName %) extension)
          (-> (:runtime-config-dir appconfig) io/file file-seq)))

(defn- load-config [files]
  (doseq [file files
          :let [forms (read-config file)]]
    (log/infof "evaluating config file %s: %d defs" file (count forms))
    (doseq [form forms]
      (eval-config-form form))))

(defn- guess-good-source-key [title url]
  (let [host (string/replace (human/host-identifier url) #"[\W\.]" "-")
        title (string/replace (string/lower-case title) #"[\W\.]" "-")]
    (str
     host
     "-"
     (subs title 0 (min 10 (count title))))))

(defn- opml-feed-to-fetch [feed]
  (let [{:keys [title url]} feed
        source-key (guess-good-source-key title url)]
    (format ";; %s\n(fetch %s (src/feed \"%s\") :tags #{:from-opml})\n"
            title source-key url)))

(defn- load-convert-delete-opml [file]
  (try+
   (let [out-file (io/file (str file ".llar.example"))
         feeds (converter/read-opml-feeds (io/input-stream (io/file file)))
         fetch-defs (concat
                     (map opml-feed-to-fetch feeds)
                     ["(sched-fetch from-opml :now-and-hourly (some #{:from-opml} $TAGS))"])]
     (log/infof "loaded %d feeds from OPML file %s" (count feeds) file)
     (spit out-file (string/join "\n" fetch-defs))
     (log/infof "OPML converted to %s. have fun!" out-file)
     (io/delete-file file))
   (catch [:type :llar.converter/opml-parser-error] e
     (log/errorf "failed to parse opml file %s: %s" file e))
   (catch Object e
     (log/errorf "failed to convert opml file %s: %s" file e))))

(defn- handle-config-dir-change [change]
  (when (#{:create :modify} (:type change))
    (log/info "runtime configuration directory changed. reloading config" change)
    (when (string/ends-with? (:path change) ".llar")
      (load-config [(.toFile (:path change))]))
    (when (string/ends-with? (:path change) ".opml")
      (load-convert-delete-opml (.toFile (:path change))))))

(defn load-all []
  (load-config (get-config-files ".llar"))
  (map load-convert-delete-opml (get-config-files ".opml")))

(defn start-watcher []
  (beholder/watch handle-config-dir-change (get appconfig :runtime-config-dir)))

(defstate change-watcher
  :start (start-watcher)
  :stop (beholder/stop change-watcher))
