(ns llar.config
  (:require
   [clojure.spec.alpha :as s]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [llar.appconfig :as appconfig]
   [mount.core :refer [defstate]]
   [nextjournal.beholder :as beholder]
   [llar.fetchutils :refer [make-reddit-proc]]
   [llar.fetch.custom]
   [llar.fetch.feed]
   [llar.fetch.http]
   [llar.fetch.imap]
   [llar.fetch.mercury]
   [llar.fetch.reddit]
   [llar.fetch.twitter]
   [llar.postproc :as proc]))

;; contains all loaded sources
(defonce srcs (atom {}))

(defn get-sources [] @srcs)

(defn get-source [k] (get @srcs k))

(defn remove-source [k] (keys (swap! srcs dissoc k)))

(defmacro wrap-proc [src-key tags options & body]
  (when-not (nil? body)
    `(fn [item#]
       (let [~'$item item#
             ~'$key ~src-key
             ~'$title (get-in item# [:summary :title])
             ~'$authors (get-in item# [:entry :authors])
             ~'$tags ~tags
             ~'$raw (get item# :raw)
             ~'$url (get-in item# [:entry :url])
             ~'$html (get-in item# [:entry :contents "text/html"])
             ~'$text (get-in item# [:entry :contents "text/plain"])
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

(fetch bookmark nil :tags #{:bookmark})

(defn- read-config [path]
  (with-open [r (io/reader path)]
    (let [reader (java.io.PushbackReader. r)]
      (loop [forms []]
        (let [form (try (read reader nil :eof)
                        (catch Exception _
                          (log/error "Error reading config file" path)
                          :eof))]
          (if (= form :eof)
            forms
            (recur (conj forms form))))))))

(defn- eval-config-form [form]
  (binding [*ns* (create-ns (gensym 'llar.config.eval))]
    ;; exclude list based on clojail blacklist-symbols
    (refer-clojure :exclude [eval read alter-var-root intern
                             load-string load-reader ns-resolve resolve find-var
                             *read-eval* ns-publics ns-unmap ns-map ns-interns the-ns
                             push-thread-bindings pop-thread-bindings future-call agent send
                             send-off pmap pcalls in-ns
                             with-redefs-fn])
    (require
     '[llar.appconfig :refer [credentials] :rename {credentials $credentials}]
     '[llar.config :refer [fetch fetch-reddit srcs]]
     '[llar.converter :refer [html2text] :rename {html2text $html2text}]
     '[llar.human :refer [truncate-ellipsis] :rename {truncate-ellipsis $ellipsify}]
     '[llar.fetchutils :refer [parse-date-to-zoned-data-time mercury-contents make-hacker-news-filter make-category-filter-deny add-tag add-tag-filter exchange html-to-hickory hickory-sanitize-blobify]
       :rename {parse-date-to-zoned-data-time $parse-date
                make-hacker-news-filter $hn-filter
                make-category-filter-deny $category-rm
                mercury-contents $extract
                add-tag $add-tag
                add-tag-filter $add-tag-filter
                exchange $exchange
                html-to-hickory $html-to-hickory
                hickory-sanitize-blobify $hickory-sanitize-blobify}]
     '[llar.fetch :refer [make-item-hash] :rename {make-item-hash $make-item-hash}]
     '[llar.http :refer [fetch parse-href] :rename {fetch $fetch parse-href $parse-href}]
     '[llar.src :as src]
     '[clojure.string :as string]
     '[hickory.select :as S]
     '[hickory.render :refer [hickory-to-html] :rename {hickory-to-html $hickory-to-html}]
     '[org.bovinegenius.exploding-fish :refer [uri path] :rename {uri $as-uri path $uri-path}]
     '[java-time.api :as time]
     '[clj-http.cookies :refer [cookie-store]
       :rename {cookie-store $http-cookie-store}]
     '[clj-http.client
       :refer [get post]
       :rename {get $http-get
                post $http-post}])
    (cond
      (and (list? form) (#{'fetch 'fetch-reddit} (first form)))
      (do (log/debugf "loading fetch \"%s\"" (second form))
          (eval form))
      :else
      (log/warnf "unknown fetch config definition \"%s\". Skipping." (second form)))))

(defn- get-config-files []
  (filter #(string/ends-with? (.getName %) ".llar")
          (-> (appconfig/runtime-config-dir) io/file file-seq)))

(defn- load-config [files]
  (doseq [file files
          :let [forms (read-config file)]]
    (log/infof "config file %s: read %d defs" file (count forms))
    (doseq [form forms]
      (eval-config-form form))))

(defn- handle-config-dir-change [change]
  (log/info "Runtime configuration directory changed. Reloading config" change)
  (load-config (get-config-files)))

(defn load-all []
  (load-config (get-config-files)))

(defstate change-watcher
  :start (beholder/watch handle-config-dir-change (appconfig/runtime-config-dir))
  :stop (beholder/stop change-watcher))
