(ns llar.docs.config-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [clojure.tools.reader :as reader]
   [llar.config :as config]
   [llar.docs.config :as uut]
   [llar.sched :as sched]))

(defn- read-all-forms [s]
  (let [r (java.io.PushbackReader. (java.io.StringReader. s))]
    (loop [forms []]
      (let [form (reader/read {:eof ::eof} r)]
        (if (= ::eof form)
          forms
          (recur (conj forms form)))))))

(deftest static-html-renders-config-docs
  (let [html (uut/render-static-html)]
    (is (string/includes? html "<!doctype html>"))
    (is (string/includes? html "LLAR Configuration"))
    (is (string/includes? html "fetch"))
    (is (string/includes? html "sched-fetch"))
    (is (string/includes? html "src/feed"))
    (is (string/includes? html "Runtime Config Settings"))
    (is (string/includes? html "[:reader :favorites]"))
    (is (string/includes? html "[:ui :favorites]"))
    (is (string/includes? html "(rc [:reader :ranking] VALUE)"))
    (is (not (string/includes? html "<script")))
    (doseq [schedule-key (keys (sched/canned-schedule-metadata))]
      (is (string/includes? html (str schedule-key))))))

(deftest docs-writer-creates-html-and-css-assets
  (let [out-dir (doto (io/file (System/getProperty "java.io.tmpdir")
                               (str "llar-docs-test-" (System/nanoTime)))
                  (.mkdirs))
        paths (uut/write-static! out-dir)
        path-names (set (map #(.toString %) paths))]
    (is (.exists (io/file out-dir "config.html")))
    (is (.exists (io/file out-dir "static/bootstrap/css/bootstrap.min.css")))
    (is (.exists (io/file out-dir "static/ibmplex/Web/css/ibm-plex.min.css")))
    (is (.exists (io/file out-dir "static/llar.css")))
    (is (contains? path-names (.toString (.toPath (io/file out-dir "config.html")))))))

(deftest documented-schedules-match-runtime-metadata
  (let [html (uut/render-static-html)]
    (is (= (keys sched/canned-schedules)
           (keys (sched/canned-schedule-metadata))))
    (doseq [[schedule-key {:keys [description]}] (sched/canned-schedule-metadata)]
      (is (string/includes? html (str schedule-key)))
      (is (string/includes? html description)))))

(deftest source-docs-come-from-var-metadata
  (let [hn-doc (some #(when (= 'src/hn (:symbol %)) %)
                     (uut/source-constructors))]
    (is hn-doc)
    (is (= :source (:llar.config/kind (meta (:resolved-var hn-doc)))))
    (is (= [:irq0-hn/tag :irq0-hn/args] (:llar.config/specs (meta (:resolved-var hn-doc)))))
    (is (string/includes? (uut/render-static-html) "src/hn"))))

(deftest runtime-docs-come-from-var-metadata
  (let [construct-names (set (map :symbol (uut/runtime-constructs)))]
    (is (contains? construct-names 'fetch))
    (is (contains? construct-names 'sched-fetch))
    (is (contains? construct-names 'rc))
    (is (contains? construct-names 'reader-favorite))
    (is (contains? construct-names 'reader-default-list-view))
    (is (contains? construct-names 'reader-ranking))
    (is (contains? construct-names 'reader-url-handler))
    (is (contains? construct-names 'podcast-retention))
    (is (contains? construct-names 'sort-default))))

(deftest feature-examples-come-from-var-metadata
  (let [feature-names (set (map :symbol (uut/feature-examples)))]
    (is (contains? feature-names 'zotero-export-links))))

(deftest config-engine-registry-drives-runtime-docs
  (let [entries (config/config-engine-environment)
        sort-default-entry (some #(when (= 'sort-default (:symbol %)) %) entries)
        extract-entry (some #(when (= '$extract (:symbol %)) %) entries)]
    (is (= 'llar.config/sort-default (:doc-var sort-default-entry)))
    (is (nil? (:var sort-default-entry)))
    (is (= 'llar.config/handle-sort-default-config-form! (:handler-fn sort-default-entry)))
    (is (= 'llar.fetchutils/readability-contents (:var extract-entry)))
    (is (= :helper (:llar.config/kind extract-entry)))
    (is (string/includes? (uut/render-static-html) "Source Predicate Bindings"))
    (is (string/includes? (uut/render-static-html) "$KEY"))))

(deftest config-engine-registration-is-idempotent
  (let [before (config/config-engine-environment)]
    (config/register-config-engine-entry! {:symbol 'sort-default
                                           :doc-var 'llar.config/sort-default
                                           :handler-fn 'llar.config/handle-sort-default-config-form!})
    (let [after (config/config-engine-environment)]
      (is (= (count before) (count after)))
      (is (= 1 (count (filter #(= 'sort-default (:symbol %)) after)))))))

(deftest config-engine-registration-validates-entry-shape
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid config engine entry"
       (config/register-config-engine-entry! {:symbol 'bad
                                              :var 'llar.config/fetch
                                              :namespace 'llar.src})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid config engine entry"
       (config/register-config-engine-entry! {:symbol 'bad}))))

(deftest config-engine-environment-validates-handler-resolution
  (with-redefs [config/config-engine-registry (atom [])]
    (config/register-config-engine-entry! {:symbol 'bad
                                           :doc-var 'llar.config/sort-default
                                           :handler-fn 'llar.config/missing-handler!})
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Config engine handler does not resolve to a function"
         (config/config-engine-environment)))))

(deftest examples-parse-as-clojure-forms
  (doseq [{:keys [title code]} (uut/examples)]
    (is (seq (read-all-forms code)) title)))
