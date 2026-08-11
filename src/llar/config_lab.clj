(ns llar.config-lab
  "Ephemeral, side-effect-isolated execution for the dashboard Config Lab."
  (:require
   [clj-http.cookies :as http-cookies]
   [clojure.edn :as edn]
   [clojure.string :as string]
   [clojure.walk :as walk]
   [hickory.render :as hick-r]
   [hickory.select :as S]
   [java-time.api :as time]
   [llar.appconfig :as appconfig]
   [llar.blobstore :as blobstore]
   [llar.commands :as commands]
   [llar.fetch :as fetch]
   [llar.fetch.demo]
   [llar.fetch.feed :as feed]
   [llar.fetch.github]
   [llar.fetch.hackernews]
   [llar.fetch.http]
   [llar.fetch.readability]
   [llar.fetchutils :as fetchutils]
   [llar.human :as human]
   [llar.http :as llar-http]
   [llar.src :as src]
   [mount.core :refer [defstate]]
   [org.bovinegenius.exploding-fish :as uri]
   [sci.core :as sci])
  (:import
   [java.net Inet6Address InetAddress URI]
   [java.nio.file Files]
   [java.nio.file.attribute FileAttribute]
   [java.util UUID]
   [java.util.concurrent ExecutionException Executors Semaphore TimeUnit]
   [org.apache.commons.io FileUtils]
   [org.bovinegenius.exploding_fish Uri]))

(def ^:private default-settings
  {:max-concurrent-runs 2
   :run-timeout-ms 20000
   :session-ttl-minutes 30})

(defonce ^:private sessions (atom {}))
(defonce ^:private login-sessions (atom {}))
(defonce ^:private run-limiter (atom nil))

(declare cleanup-expired!)

(defn settings []
  (merge default-settings (or (appconfig/config-lab) {})))

(defn enabled? []
  (true? (:enabled? (settings))))

(defn- now-ms [] (System/currentTimeMillis))

(defn- expires-at []
  (+ (now-ms) (* 60000 (:session-ttl-minutes (settings)))))

(defn- limiter []
  (or @run-limiter
      (let [candidate (Semaphore. (:max-concurrent-runs (settings)) true)]
        (swap! run-limiter #(or % candidate)))))

(defn- run-bounded [f]
  (let [^Semaphore semaphore (limiter)]
    (when-not (.tryAcquire semaphore 100 TimeUnit/MILLISECONDS)
      (throw (ex-info "Config Lab is busy; retry shortly"
                      {:type ::busy})))
    (let [task (future
                 (try
                   (f)
                   (finally
                     (.release semaphore))))
          timeout (:run-timeout-ms (settings))
          timeout-value (Object.)
          result (try
                   (deref task timeout timeout-value)
                   (catch ExecutionException exception
                     (throw (.getCause exception))))]
      (if (identical? timeout-value result)
        (do
          (future-cancel task)
          (throw (ex-info "Config Lab run exceeded its time limit"
                          {:type ::timeout :timeout-ms timeout})))
        result))))

(defn- private-v6? [^InetAddress address]
  (and (instance? Inet6Address address)
       (= 0xfc (bit-and 0xfe (bit-and 0xff (aget (.getAddress address) 0))))))

(defn validate-public-url!
  "Reject URLs capable of reaching local services. Bound into llar.http so the
  same check is repeated for every redirect."
  [raw-url]
  (let [^URI url (try
                   (URI. (str raw-url))
                   (catch Throwable _
                     (throw (ex-info "Config Lab URL is malformed"
                                     {:type ::unsafe-url :url (str raw-url)
                                      :llar.http/preserve? true}))))
        scheme (some-> (.getScheme url) string/lower-case)
        host (.getHost url)]
    (when-not (contains? #{"http" "https"} scheme)
      (throw (ex-info "Config Lab only fetches HTTP(S) URLs"
                      {:type ::unsafe-url :url (str raw-url)
                       :llar.http/preserve? true})))
    (when (or (nil? host) (.getUserInfo url)
              (= "localhost" (string/lower-case host))
              (string/ends-with? (string/lower-case host) ".localhost")
              (string/ends-with? (string/lower-case host) ".local"))
      (throw (ex-info "Config Lab URL has a forbidden host"
                      {:type ::unsafe-url :url (str raw-url)
                       :llar.http/preserve? true})))
    (let [addresses (vec (InetAddress/getAllByName host))]
      (doseq [^InetAddress address addresses]
        (when (or (.isAnyLocalAddress address)
                  (.isLoopbackAddress address)
                  (.isLinkLocalAddress address)
                  (.isSiteLocalAddress address)
                  (.isMulticastAddress address)
                  (private-v6? address))
          (throw (ex-info "Config Lab will not connect to a private address"
                          {:type ::unsafe-url
                           :url (str raw-url)
                           :llar.http/preserve? true
                           :address (.getHostAddress address)}))))
      addresses)))

(def ^:private selector-vars
  {'and S/and
   'or S/or
   'not S/not
   'attr S/attr
   'child S/child
   'class S/class
   'descendant S/descendant
   'el-not S/el-not
   'find-in-text S/find-in-text
   'follow S/follow
   'follow-adjacent S/follow-adjacent
   'has-child S/has-child
   'has-descendant S/has-descendant
   'id S/id
   'node-type S/node-type
   'nth-child S/nth-child
   'nth-last-child S/nth-last-child
   'nth-last-of-type S/nth-last-of-type
   'nth-of-type S/nth-of-type
   'precede S/precede
   'precede-adjacent S/precede-adjacent
   'select S/select
   'tag S/tag})

(defn- unavailable-helper [helper reason]
  (fn [& _]
    (throw (ex-info (str helper " is unavailable in Config Lab: " reason)
                    {:type ::unavailable-helper :helper helper}))))

(defn- require-isolated-run! [helper]
  (when-not llar-http/*url-guard*
    (throw (ex-info (str helper " can only run during a Config Lab fetch or process stage")
                    {:type ::helper-outside-isolation :helper helper}))))

(defn- isolated-helper [helper f]
  (fn [& args]
    (require-isolated-run! helper)
    (apply f args)))

(defn- readability-helper [& args]
  (let [processor (apply fetchutils/readability-contents args)]
    (fn [item]
      (require-isolated-run! '$extract)
      (processor item))))

;; Keep this list explicit: adding a helper to the normal config engine should
;; make the compatibility test fail until its Config Lab security policy has
;; been reviewed.
(def ^:private helper-vars
  {'$credentials (unavailable-helper '$credentials "credentials are never exposed")
   '$html2text commands/html2text
   '$ellipsify human/truncate-ellipsis
   '$parse-ts fetchutils/parse-timestamp
   '$category-rm fetchutils/make-category-filter-deny
   '$extract readability-helper
   '$add-tag fetchutils/add-tag
   '$add-tag-filter fetchutils/add-tag-filter
   '$exchange fetchutils/exchange
   '$html-to-hickory fetchutils/html-to-hickory
   '$hickory-sanitize-blobify (isolated-helper '$hickory-sanitize-blobify
                                               fetchutils/hickory-sanitize-blobify)
   '$make-item-hash fetch/make-item-hash
   '$fetch (isolated-helper '$fetch llar-http/fetch)
   '$parse-url llar-http/absolutify-url
   '$hickory-to-html hick-r/hickory-to-html
   '$uri-path uri/path
   '$http-cookie-store http-cookies/cookie-store
   '$http-get (unavailable-helper '$http-get "use $fetch so URL safety checks apply")
   '$http-post (unavailable-helper '$http-post "arbitrary HTTP writes are disabled")})

(def ^:private sandbox-options
  {:classes {'java.net.URI URI}
   :namespaces
   {'user helper-vars
    'src {'website src/website
          'custom src/custom
          'demo src/demo
          'feed src/feed
          'github-issues src/github-issues
          'github-repos src/github-repos
          'hn src/hn
          'selector-feed src/selector-feed
          'readability src/readability}
    'S selector-vars
    'string {'blank? string/blank?
             'ends-with? string/ends-with?
             'includes? string/includes?
             'join string/join
             'lower-case string/lower-case
             'replace string/replace
             'split string/split
             'starts-with? string/starts-with?
             'trim string/trim
             'upper-case string/upper-case}
    'time {'format time/format
           'instant time/instant
           'local-date time/local-date
           'local-date-time time/local-date-time
           'zoned-date-time time/zoned-date-time}}
   :deny '[add-tap agent alter-var-root eval find-ns future future-call intern
           load-file load-string ns-resolve pmap pcalls print printf println prn
           pvalues remove-tap requiring-resolve resolve send send-off
           set-agent-send-executor! set-agent-send-off-executor! shutdown-agents
           slurp spit tap>]
   :interrupt-fn #(Thread/interrupted)})

(defn- sandbox-eval [form]
  (run-bounded #(sci/eval-string form sandbox-options)))

(defn compile-source [source-form]
  (when (string/blank? source-form)
    (throw (ex-info "Enter a source constructor form" {:type ::invalid-form})))
  (let [source (sandbox-eval source-form)]
    (when-not (src/source? source)
      (throw (ex-info "Source form did not return an LLAR source"
                      {:type ::invalid-source :value (pr-str source)})))
    (when-not (or (instance? llar.src.GenericWebsite source)
                  (instance? llar.src.Custom source)
                  (instance? llar.src.Demo source)
                  (instance? llar.src.Feed source)
                  (instance? llar.src.GitHubSearch source)
                  (instance? llar.src.HackerNews source)
                  (instance? llar.src.SelectorFeed source)
                  (instance? llar.src.Readability source))
      (throw (ex-info "This source type cannot run safely in Config Lab"
                      {:type ::unsupported-source :source-type (str (type source))})))
    source))

(defn- normalize-fns [value stage]
  (let [values (cond
                 (nil? value) []
                 (fn? value) [value]
                 (sequential? value) value
                 :else [value])]
    (when-not (every? fn? values)
      (throw (ex-info (str (name stage) " must evaluate to a function or functions")
                      {:type ::invalid-processors :stage stage})))
    (vec values)))

(defn compile-processors [{:keys [pre filter post]}]
  {:pre (normalize-fns (when-not (string/blank? pre) (sandbox-eval pre)) :pre)
   :filter (normalize-fns (when-not (string/blank? filter) (sandbox-eval filter)) :filter)
   :post (normalize-fns (when-not (string/blank? post) (sandbox-eval post)) :post)})

(defn- rm-tree! [path]
  (when path
    (FileUtils/deleteDirectory (.toFile ^java.nio.file.Path path))))

(defn- cleanup-expired! []
  (let [now (now-ms)
        expired (for [[id session] @sessions :when (< (:expires-at session) now)]
                  [id session])]
    (doseq [[id session] expired]
      (swap! sessions dissoc id)
      (rm-tree! (:temp-dir session)))
    (swap! login-sessions
           (fn [current]
             (into {} (remove (fn [[_ login]] (< (:expires-at login) now)) current))))))

(defstate ^{:depends-on [appconfig/appconfig]} session-janitor
  :start (when (enabled?)
           (let [executor (Executors/newSingleThreadScheduledExecutor)]
             (.scheduleAtFixedRate executor
                                   ^Runnable (fn []
                                               (try
                                                 (cleanup-expired!)
                                                 (catch Throwable _)))
                                   1 1 TimeUnit/MINUTES)
             executor))
  :stop (when session-janitor
          (.shutdownNow ^java.util.concurrent.ScheduledExecutorService
           session-janitor)))

(defn login! [owner]
  (cleanup-expired!)
  (let [token (str (UUID/randomUUID))]
    (swap! login-sessions assoc token {:owner owner :expires-at (expires-at)})
    token))

(defn login-owner [token]
  (cleanup-expired!)
  (when-let [login (get @login-sessions token)]
    (swap! login-sessions assoc-in [token :expires-at] (expires-at))
    (:owner login)))

(defn logout! [token]
  (swap! login-sessions dissoc token))

(defn create-session! [owner input]
  (cleanup-expired!)
  (when (>= (count @sessions) 50)
    (throw (ex-info "Config Lab has reached its global session limit"
                    {:type ::too-many-sessions})))
  (when (>= (count (filter #(= owner (:owner %)) (vals @sessions))) 10)
    (throw (ex-info "Too many active Config Lab sessions" {:type ::too-many-sessions})))
  (let [id (str (UUID/randomUUID))
        temp-dir (Files/createTempDirectory "llar-config-lab-"
                                            (make-array FileAttribute 0))
        session (merge {:id id
                        :owner owner
                        :created-at (now-ms)
                        :expires-at (expires-at)
                        :temp-dir temp-dir}
                       (select-keys input [:source-form :source-key :tags-form :options-form
                                           :processors]))]
    (swap! sessions assoc id session)
    (select-keys session [:id :source-form :source-key :tags-form :options-form
                          :processors :created-at :expires-at])))

(defn- owned-session! [owner id]
  (cleanup-expired!)
  (let [session (get @sessions id)]
    (when-not (and session (= owner (:owner session)))
      (throw (ex-info "Config Lab session not found" {:type ::not-found :id id})))
    (swap! sessions assoc-in [id :expires-at] (expires-at))
    session))

(defn session-view [owner id]
  (select-keys (owned-session! owner id)
               [:id :source-form :source-key :tags-form :options-form :processors
                :created-at :expires-at :stage]))

(defn delete-session! [owner id]
  (let [session (owned-session! owner id)]
    (swap! sessions dissoc id)
    (rm-tree! (:temp-dir session))
    true))

(defn- metadata-set [label form]
  (try
    (let [value (edn/read-string (or form "#{}"))]
      (when-not (and (set? value) (every? keyword? value))
        (throw (ex-info (str label " must be an EDN set of keywords")
                        {:type ::invalid-metadata})))
      value)
    (catch clojure.lang.ExceptionInfo exception
      (throw exception))
    (catch Throwable _
      (throw (ex-info (str label " is not valid EDN")
                      {:type ::invalid-metadata})))))

(defn compile-session! [owner id input]
  (let [session (merge (owned-session! owner id)
                       (select-keys input [:source-form :source-key :tags-form :options-form
                                           :processors]))
        source (compile-source (:source-form session))
        processors (compile-processors (:processors session))]
    (swap! sessions assoc id (assoc session
                                    :source source
                                    :compiled-processors processors
                                    :stage :compiled
                                    :expires-at (expires-at)))
    {:stage :compiled
     :source-type (.getSimpleName (class source))
     :source (str source)
     :suggested-source-key
     (let [source-url (:url source)
           host (some-> source-url uri/host (string/replace #"^www\." ""))
           path (some-> source-url uri/path)
           slug (some-> (str host "-" path)
                        string/lower-case
                        (string/replace #"[^a-z0-9]+" "-")
                        (string/replace #"(^-|-$)" ""))]
       (if (string/blank? slug) "config-lab-source" (subs slug 0 (min 128 (count slug)))))
     :processor-counts (update-vals processors count)}))

(defn- truncate [value limit]
  (if (and (string? value) (> (count value) limit))
    (str (subs value 0 limit) "\n… truncated …")
    value))

(defn json-safe
  "Convert Config Lab response data for its ordinary JSON-driven UI. Value
  inspector snapshots must be prepared before this conversion so Clojure and
  JVM type information is retained."
  [value]
  (walk/postwalk
   (fn [x]
     (cond
       (instance? Uri x) (str x)
       (instance? java.time.temporal.TemporalAccessor x) (str x)
       (set? x) (vec x)
       (fn? x) (str x)
       (string? x) (truncate x 200000)
       :else x))
   value))

(defn- item-summary [item]
  {:title (get-in item [:summary :title])
   :timestamp (get-in item [:summary :ts])
   :url (get-in item [:entry :url])
   :authors (get-in item [:entry :authors])
   :tags (get-in item [:meta :tags])
   :hash (:hash item)
   :contents (update-vals (or (get-in item [:entry :contents]) {})
                          #(truncate % 50000))
   :raw-preview (truncate (pr-str (:raw item)) 20000)})

(defn- http-response-summary [http]
  (let [raw (:raw http)
        body (:body raw)]
    {:status (:status raw)
     :content-type (or (get-in raw [:headers :content-type])
                       (get-in raw [:headers "content-type"]))
     :body-characters (when (string? body) (count body))}))

(defn- http-diagnostics [http]
  {:response (http-response-summary http)
   :summary (:summary http)
   :trace (:trace http)
   ;; This is the exact sanitized, absolutified and blobified tree used by the
   ;; selectors. Keeping it beside the HTTP trace makes Config Lab paths useful
   ;; when developing selectors and extractors.
   :hickory (:hickory http)})

(defn- with-isolation [session f]
  (let [network-timeout (min 15000 (:run-timeout-ms (settings)))]
    (binding [appconfig/*blob-store-dir-override* (str (:temp-dir session))
              blobstore/*http-get* llar-http/get-with-redirect-guard
              llar-http/*url-guard* validate-public-url!
              llar-http/*request-timeouts-override*
              {:connection-timeout network-timeout
               :connection-request-timeout network-timeout
               :socket-timeout network-timeout}]
      (run-bounded f))))

(defn fetch-session! [owner id]
  (let [session (owned-session! owner id)
        source (or (:source session)
                   (throw (ex-info "Compile the source before fetching"
                                   {:type ::not-compiled})))
        result (with-isolation
                 session
                 #(if (instance? llar.src.SelectorFeed source)
                    (let [index (feed/fetch-selector-index source :trace? true)]
                      {:kind :selector
                       :stored index
                       :public (merge
                                {:stage :fetched-index
                                 :source-type :selector-feed}
                                (http-diagnostics (:http index))
                                {:url-selector {:selector (str (get-in source [:selectors :urls]))
                                                :match-count (count (:matches index))
                                                :matches (mapv (fn [match]
                                                                 (truncate (hick-r/hickory-to-html match)
                                                                           2000))
                                                               (take 100 (:matches index)))
                                                :selected-hickory (vec (take 20 (:matches index)))
                                                :selected-hickory-truncated? (> (count (:matches index))
                                                                                20)
                                                :urls (mapv str (take 100 (:item-urls index)))}})})
                    (let [items (vec (take 100 (fetch/fetch-source source {})))]
                      {:kind :generic
                       :stored items
                       :public {:stage :fetched
                                :source-type (.getSimpleName (class source))
                                :item-count (count items)
                                :items (mapv item-summary (take 20 items))}})))]
    (swap! sessions update id merge {:stage (:stage (:public result))
                                     :fetch-kind (:kind result)
                                     :fetched (:stored result)
                                     :selector-items []
                                     :expires-at (expires-at)})
    (:public result)))

(defn fetch-selector-item! [owner id item-index]
  (let [session (owned-session! owner id)
        source (:source session)
        index (:fetched session)]
    (when-not (= :selector (:fetch-kind session))
      (throw (ex-info "Fetch a SelectorFeed index first" {:type ::wrong-stage})))
    (let [raw-url (nth (:item-urls index) item-index nil)]
      (when-not raw-url
        (throw (ex-info "Selected item index is out of range"
                        {:type ::invalid-item-index :item-index item-index})))
      (let [fetched (with-isolation session
                      #(feed/fetch-selector-item source raw-url :trace? true))
            extracted (with-isolation session
                        #(feed/extract-selector-item source index fetched
                                                     :allow-invalid? true))
            stored {:index item-index :fetched fetched :extracted extracted}
            public (merge
                    {:stage :extracted-item
                     :item-index item-index
                     :url (str (:item-url fetched))}
                    (http-diagnostics (:http fetched))
                    {:valid? (:valid? extracted)
                     :errors (:errors extracted)
                     :fields (:fields extracted)
                     :item (some-> (:item extracted) item-summary)})]
        (swap! sessions update id
               (fn [current]
                 (-> current
                     (assoc :stage :extracted-item :expires-at (expires-at))
                     (update :selector-items conj stored))))
        public))))

(defn- apply-pipeline [processors item]
  (let [pre-item (reduce (fn [current f] (f current)) item (:pre processors))
        removed? (boolean (some #(% pre-item) (:filter processors)))
        result (when-not removed?
                 (reduce (fn [current f] (f current)) pre-item (:post processors)))]
    {:removed removed?
     :before (item-summary item)
     :after (when result (item-summary result))}))

(defn process-session! [owner id processors-input]
  (let [session (owned-session! owner id)
        processors (if processors-input
                     (compile-processors processors-input)
                     (:compiled-processors session))
        items (case (:fetch-kind session)
                :generic (:fetched session)
                :selector (->> (:selector-items session)
                               (keep #(get-in % [:extracted :item]))
                               vec)
                nil)]
    (when-not (seq items)
      (throw (ex-info "Fetch at least one item before processing"
                      {:type ::wrong-stage})))
    (let [results (with-isolation
                    session
                    #(mapv (partial apply-pipeline processors) items))]
      (swap! sessions update id merge {:stage :processed
                                       :compiled-processors processors
                                       :processors processors-input
                                       :processed results
                                       :expires-at (expires-at)})
      {:stage :processed
       :item-count (count results)
       :removed-count (count (filter :removed results))
       :items (vec (take 20 results))})))

(defn export-form
  ([owner id]
   (export-form owner id nil))
  ([owner id input]
   (let [session (owned-session! owner id)
         {:keys [source-form]} session
         processors (or (:processors input) (:processors session))
         source-key (str (or (:source-key input) (:source-key session) "config-lab-source"))
         tags-form (or (:tags-form input) (:tags-form session) "#{}")
         options-form (or (:options-form input) (:options-form session) "#{}")
         _tags (metadata-set "Tags" tags-form)
         _options (metadata-set "Options" options-form)
         _ (when-not (re-matches #"[A-Za-z][A-Za-z0-9._-]{0,127}" source-key)
             (throw (ex-info "Source key must be a plain LLAR symbol name"
                             {:type ::invalid-metadata})))
         _processors (compile-processors processors)
         {:keys [pre filter post]} processors]
     (str "(fetch " source-key "\n  " source-form
          (when-not (or (string/blank? tags-form) (= "#{}" (string/trim tags-form)))
            (str "\n  :tags " tags-form))
          (when-not (or (string/blank? options-form) (= "#{}" (string/trim options-form)))
            (str "\n  :options " options-form))
          (when-not (or (string/blank? pre) (= "[]" (string/trim pre)))
            (str "\n  :pre-fns " pre))
          (when-not (or (string/blank? filter) (= "[]" (string/trim filter)))
            (str "\n  :rm-fn " filter))
          (when-not (or (string/blank? post) (= "[]" (string/trim post)))
            (str "\n  :post-fns " post))
          ")\n"))))

(defn session-blob [owner id hash]
  (when-not (re-matches #"[0-9a-f]{64}" hash)
    (throw (ex-info "Config Lab blob not found" {:type ::not-found})))
  (let [session (owned-session! owner id)]
    (binding [appconfig/*blob-store-dir-override* (str (:temp-dir session))]
      (let [^java.io.File file (blobstore/get-local-filename hash)]
        (when-not (.isFile file)
          (throw (ex-info "Config Lab blob not found" {:type ::not-found})))
        (blobstore/get-blob hash)))))

(defn reset-state-for-tests! []
  (doseq [[_ session] @sessions]
    (rm-tree! (:temp-dir session)))
  (reset! sessions {})
  (reset! login-sessions {})
  (reset! run-limiter nil))
