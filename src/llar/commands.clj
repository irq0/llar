(ns llar.commands
  (:require
   [clojure.edn :as edn]
   [clojure.java.shell :as shell]
   [clojure.string :as string]
   [clojure.xml :as xml]
   [cheshire.core :as cheshire]
   [hickory.select :as hick-s]
   [java-time.api :as time]
   [org.bovinegenius [exploding-fish :as uri]]
   [slingshot.slingshot :refer [throw+]]
   [puget.printer :as puget]
   [llar.appconfig :as appcfg :refer [appconfig postgresql-config]]
   [llar.contentdetect :as contentdetect])
  (:import
   [java.util.concurrent Semaphore Executors TimeUnit]
   [org.bovinegenius.exploding_fish Uri]))

;; Wrapper for all external commands we run

(defonce +kill-timeout-secs+ 120)
(defonce +semaphore+ (delay (Semaphore. (get-in appconfig [:throttle :command-max-concurrent]))))

(defmacro with-throttle [sem & body]
  `(do
     (.acquire ~sem)
     (try
       ~@body
       (finally
         (.release ~sem)))))

(defn sh+timeout [timeout-secs args & opts]
  (let [cmd (into ["/bin/timeout" (str "--kill-after=" +kill-timeout-secs+ "s")
                   (str timeout-secs "s")] args)
        sh-arg (apply concat cmd (when opts (apply concat opts)))
        {:keys [exit out err] :as ret} (apply shell/sh sh-arg)]
    (cond (= exit 124)
          (throw+ {:type ::timeout
                   :exit exit
                   :err err
                   :cmd cmd
                   :timeout-secs {:term timeout-secs :kill +kill-timeout-secs+}})
          (#{125 126 127 137} exit)
          (throw+ {:type ::timeout-command-failed
                   :exit exit
                   :out out
                   :err err
                   :cmd cmd})
          :default
          ret)))

(defn sanitize [raw-html]
  (let [{:keys [out exit err]}
        (with-throttle @+semaphore+
          (sh+timeout (get-in appconfig [:timeouts :readability])
                      ["node" "tools/dompurify"] {:in raw-html}))]
    (if (zero? exit)
      out
      (throw+ {:type :llar.http/sanitize-error
               :message err
               :ret exit}))))

(defn readability [raw-html url]
  (let [{:keys [out exit err]}
        (with-throttle @+semaphore+
          (sh+timeout (get-in appconfig [:timeouts :readability])
                      ["node" "tools/readability"] {:in (cheshire/generate-string {:url url :html raw-html})}))]
    (if (zero? exit)
      (cheshire/parse-string out true)
      (throw+ {:type :llar.http/sanitize-error
               :message err
               :ret exit}))))

(defn html-to-text-command [tool]
  (get {:pandoc [(appcfg/command :pandoc) "-f" "html" "-t" "plain" "--reference-links"]
        :w3m [(appcfg/command :w3m) "-T" "text/html" "-dump"]
        :for-exceptions [(appcfg/command :pandoc) "-f" "html" "-t" "plain"]
        :lynx [(appcfg/command :lynx) "-dump" "-list_inline" "-width 1024" "-stdin"]
        :html2text [(appcfg/command :html2text) "-style" "pretty" "-utf8"]}
       tool))

(defn html2text
  "Convert html to text"
  [html & {:keys [tool] :or {tool :lynx}}]
  (let [cmdline (concat (html-to-text-command tool) [:in html])
        {:keys [exit out]} (with-throttle @+semaphore+ (sh+timeout (get-in appconfig [:timeouts :html2text])
                                                                  cmdline))]
    (if (zero? exit)
      (if (= :for-exceptions tool)
        (string/replace out #"[\n\t]" " ")
        out)
      "")))
