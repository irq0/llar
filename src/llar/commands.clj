(ns llar.commands
  (:require
   [clojure.edn :as edn]
   [clojure.java.shell :as shell]
   [clojure.string :as string]
   [clojure.xml :as xml]
   [clojure.tools.logging :as log]
   [cheshire.core :as cheshire]
   [hickory.select :as hick-s]
   [java-time.api :as time]
   [org.bovinegenius [exploding-fish :as uri]]
   [slingshot.slingshot :refer [try+ throw+]]
   [puget.printer :as puget]
   [llar.appconfig :as appcfg :refer [appconfig postgresql-config]]
   [nio2.core :as nio2]
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

(defmacro with-temp-dir [dir-sym & body]
  `(let [~dir-sym (nio2/create-tmp-dir-on-default-fs "llar-")]
     (try
       ~@body
       (finally
         (when (nio2/exists? ~dir-sym)
           (doseq [path# (reverse (file-seq (nio2/file ~dir-sym)))]
             (.delete path#)))))))

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

(defmacro with-retry [attempts ex-match & body]
  `(let [result#
         (reduce
          (fn [_# attempt#]
            (try+
             (reduced {:next ::return :val (do ~@body)})
             (catch ~ex-match e# {:next ::throw :val (merge e# {:retries attempt#})})))
          nil
          (range ~attempts))]
     (when (= (:next result#) ::throw)
       (throw+ (:val result#)))
     (:val result#)))

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

(defn download-subtitles [url]
  (with-temp-dir dir
    (with-retry 5 [:type ::av-download-error :ret 1]
      (let [{:keys [exit out err]}
            (with-throttle @+semaphore+
              (sh+timeout (get-in appconfig [:timeouts :av-downloader])
                          [(appcfg/command :av-downloader)
                           "--skip-download"
                           "--write-subs"
                           "--write-auto-subs"
                           "--sub-langs=.*-orig"
                           "--sub-format=ttml"
                           (str "--output=" (nio2/path dir "llar"))
                           (str url)]))]
        (log/debugf "av-downloader subtitles: %s -> %d dir:%s err:%s out:%s" url exit dir err out)
        (if (zero? exit)
          (if-let [filename (second (re-find #"(?m)^.*Destination: (.*llar.*)$" out))]
            {:format :ttml
             :subtitles (slurp filename)}
            (throw+ {:type ::av-download-error
                     :detail "cant find destination in output"
                     :tmp-dir dir
                     :out out
                     :err err
                     :ret exit}))
          (throw+ {:type ::av-download-error
                   :tmp-dir dir
                   :out out
                   :err err
                   :ret exit}))))))
