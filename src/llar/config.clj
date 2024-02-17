(ns llar.config
  (:require
   [llar.appconfig :refer [appconfig]]
   [clojure.spec.alpha :as s]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [mount.core :refer [defstate] :as mount]
   [nextjournal.beholder :as beholder]
   [slingshot.slingshot :refer [try+]]
   [llar.fetchutils :refer [make-reddit-proc]]
   [llar.fetch.custom]
   [llar.fetch.feed]
   [llar.fetch.http]
   [llar.fetch.imap]
   [llar.fetch.mercury]
   [llar.fetch.reddit]
   [llar.fetch.twitter]
   [llar.fetch.hackernews]
   [llar.postproc :as proc]))

;; contains all loaded sources
(defonce srcs (atom {}))

;; contains all schedulers created from configuration
;; see llar.sched for other schedulers
(defonce fetch-scheds (atom {}))

(defn get-sources [] @srcs)

(defn get-source [k] (get @srcs k))

(defn remove-source [k] (keys (swap! srcs dissoc k)))

(defmacro wrap-proc [src-key tags options & body]
  (when-not (nil? body)
    `(fn [item#]
       (let [~'$item item#
             ~'$key ~src-key
             ~'$title (get-in item# [:summary :title] "")
             ~'$authors (get-in item# [:entry :authors] "")
             ~'$tags ~tags
             ~'$raw (get item# :raw {})
             ~'$url (get-in item# [:entry :url] "")
             ~'$html (get-in item# [:entry :contents "text/html"] "")
             ~'$text (get-in item# [:entry :contents "text/plain"] "")
             ~'$score (get-in item# [:entry :score] -1)
             ~'$options ~options
             ~'$entry (:entry item#)]
         (do ~@body)))))

;; TODO validate filter code. get dummy from source; pass through see if item exists and things
;; TODO validate pre / post
(defmacro fetch
  [src-key src & body]
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
    `(fn [[k# source#]]
       (let [~'$KEY k#
             ~'$SRC (:src source#)
             ~'$TAGS (:tags source#)]
         (do ~@body)))))

(defmacro defsched-remove-unread-tag [sched-key period pred]
  `(swap! autoread assoc (keyword '~sched-key)
          {:period ~period
           :pred (wrap-predicate ~pred)}))

(fetch bookmark nil :tags #{:bookmark})

(defn- read-config [path]
  (with-open [r (io/reader path)]
    (let [reader (java.io.PushbackReader. r)]
      (loop [forms []]
        (let [form (try (read reader nil :eof)
                        (catch Exception e
                          (log/error e "Error reading config file" path)
                          :eof))]
          (if (= form :eof)
            forms
            (recur (conj forms form))))))))

(defn- eval-config-form [form]
  (binding [*ns* (create-ns 'llar.config.eval)]
    ;; exclude list based on clojail blacklist-symbols
    (refer-clojure :exclude [eval read alter-var-root intern
                             load-string load-reader ns-resolve resolve find-var
                             *read-eval* ns-publics ns-unmap ns-map ns-interns the-ns
                             push-thread-bindings pop-thread-bindings future-call agent send
                             send-off pmap pcalls in-ns
                             with-redefs-fn])
    (require
     '[llar.appconfig :refer [credentials] :rename {credentials $credentials}]
     '[llar.config :refer [fetch fetch-reddit srcs defsched-remove-unread-tag]
       :rename {defsched-remove-unread-tag autoread}]
     '[llar.converter :refer [html2text] :rename {html2text $html2text}]
     '[llar.human :refer [truncate-ellipsis] :rename {truncate-ellipsis $ellipsify}]
     '[llar.fetchutils :refer [parse-timestamp mercury-contents make-category-filter-deny add-tag add-tag-filter exchange html-to-hickory hickory-sanitize-blobify]
       :rename {parse-timestamp $parse-ts
                make-category-filter-deny $category-rm
                mercury-contents $extract
                add-tag $add-tag
                add-tag-filter $add-tag-filter
                exchange $exchange
                html-to-hickory $html-to-hickory
                hickory-sanitize-blobify $hickory-sanitize-blobify}]
     '[llar.fetch :refer [make-item-hash] :rename {make-item-hash $make-item-hash}]
     '[llar.http :refer [fetch absolutify-url] :rename {fetch $fetch absolutify-url $parse-url}]
     '[llar.src :as src]
     '[llar.update :refer [defsched-feed-by-filter] :rename {defsched-feed-by-filter sched-fetch}]
     '[clojure.string :as string]
     '[clojure.tools.logging :as log]
     '[hickory.select :as S]
     '[hickory.render :refer [hickory-to-html] :rename {hickory-to-html $hickory-to-html}]
     '[org.bovinegenius.exploding-fish :refer [uri path] :rename {path $uri-path}]
     '[java-time.api :as time]
     '[clj-http.cookies :refer [cookie-store]
       :rename {cookie-store $http-cookie-store}]
     '[clj-http.client
       :refer [get post]
       :rename {get $http-get
                post $http-post}])
    (cond
      (and (list? form) (#{'highlight} (first form)) (#{'words 'authors} (second form)))
      (try
        (log/debugf "loading highlight config \"%s\"" form)
        (swap! proc/highlight-matches assoc (keyword (second form))
               (into #{} (map string/lower-case (drop 2 form))))
        (catch Exception e
          (log/error e "failed to load highlight def" (second form))))

      (and (list? form) (#{'fetch 'fetch-reddit} (first form)))
      (try+
       (log/debugf "loading fetch \"%s\"" (second form))
       (eval form)
       #_{:clj-kondo/ignore [:unresolved-symbol]}
       (catch [:clojure.spec.alpha/failure :assertion-failed] x
         (log/errorf "failed to load fetch def %s. spec %s failed. value:%s problems:%s form:%s" (second form)
                     (:clojure.spec.alpha/spec x) (:clojure.spec.alpha/value x) (:clojure.spec.alpha/problems x)
                     form))
       #_{:clj-kondo/ignore [:unresolved-symbol]}
       (catch Object e
         (log/errorf e "failed to load fetch def \"%s\"" (second form))))

      (and (list? form) (#{'sched-fetch} (first form)))
      (try
        (log/debugf "loading scheduler \"%s\": %s" (second form) form)
        (let [sched (eval form)]
          (log/debug "creating scheduler: " sched)
          (swap! fetch-scheds assoc (keyword (second form)) sched)
          (mount/start (vals @fetch-scheds)))
        (catch Exception e
          (log/error e "failed to load scheduler def" (second form))))

      (and (list? form) (#{'autoread} (first form)))
      (try
        (log/debugf "loading autoread scheduler configuration \"%s\"" (second form))
        (eval form)
        (catch Exception e
          (log/error e "failed to load autoread scheduler def" (second form))))
      :else
      (log/warnf "unknown config definition \"%s\". Skipping." form))))

(defn- get-config-files []
  (filter #(string/ends-with? (.getName %) ".llar")
          (-> (:runtime-config-dir appconfig) io/file file-seq)))

(defn- load-config [files]
  (doseq [file files
          :let [forms (read-config file)]]
    (log/infof "evaluating config file %s: %d defs" file (count forms))
    (doseq [form forms]
      (eval-config-form form))))

(defn- handle-config-dir-change [change]
  (log/info "Runtime configuration directory changed. Reloading config" change)
  (load-config (get-config-files)))

(defn load-all []
  (load-config (get-config-files)))

(defn start-watcher []
  (beholder/watch handle-config-dir-change (get appconfig :runtime-config-dir)))

(defstate change-watcher
  :start (start-watcher)
  :stop (beholder/stop change-watcher))
