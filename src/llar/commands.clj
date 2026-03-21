(ns llar.commands
  (:require
   [clojure.java.shell :as shell]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [cheshire.core :as cheshire]
   [slingshot.slingshot :refer [try+ throw+]]
   [llar.appconfig :as appcfg :refer [appconfig]]
   [nio2.core :as nio2])
  (:import
   [java.util.concurrent Semaphore]))

;; Wrapper for all external commands we run

(defonce +kill-timeout-secs+ 120)
(defonce +semaphore+ (delay (Semaphore. (get-in appconfig [:throttle :command-max-concurrent]))))
(defonce +semaphore-av-download+ (delay (Semaphore. (get-in appconfig [:throttle :av-downloader-max-concurrent]))))

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
          :else
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

(defn media-metadata
  "Fetch metadata from yt-dlp without downloading"
  [url]
  (let [{:keys [exit out err]}
        (with-throttle @+semaphore-av-download+
          (sh+timeout (get-in appconfig [:timeouts :av-downloader])
                      [(appcfg/command :av-downloader)
                       "--dump-json"
                       (str url)]))]
    (if (zero? exit)
      (cheshire/parse-string out true)
      (throw+ {:type ::av-metadata-error
               :url url :err err :ret exit}))))

(defn- download-media-file!
  "Download media file via yt-dlp into dir. Returns the downloaded File."
  [url dir]
  (let [format-spec (or (appcfg/podcast :video-format)
                        "bestvideo[height<=1080][ext=mp4]+bestaudio[ext=m4a]/best[height<=1080]")
        extra-args (or (appcfg/podcast :av-downloader-extra-args) [])
        timeout (get-in appconfig [:timeouts :av-downloader-transcode]
                        (get-in appconfig [:timeouts :av-downloader]))
        {:keys [exit out err]}
        (with-throttle @+semaphore-av-download+
          (sh+timeout timeout
                      (-> [(appcfg/command :av-downloader)
                           "--format" format-spec
                           "--merge-output-format" "mp4"
                           "--write-info-json"
                           "--no-clean-info-json"
                           (str "--output=" (nio2/path dir "media.%(ext)s"))]
                          (into extra-args)
                          (conj (str url)))))
        media-file (->> (file-seq (nio2/file dir))
                        (filter #(.isFile %))
                        (filter #(re-find #"\.(mp4|m4a|mp3|webm)$" (.getName %)))
                        first)]
    (cond
      (and (zero? exit) media-file) media-file
      (and (not (zero? exit)) media-file)
      (do (log/warnf "av-downloader exited %d but media file exists, continuing (subtitle/metadata error?): %s"
                     exit err)
          media-file)
      :else
      (throw+ {:type ::av-download-error
               :url url :dir dir :out out :err err :ret exit}))))

(defn- read-info-json
  "Read .info.json sidecar file from dir if it exists."
  [dir]
  (let [info-file (->> (file-seq (nio2/file dir))
                       (filter #(.isFile %))
                       (filter #(string/ends-with? (.getName %) ".info.json"))
                       first)]
    (when info-file
      (try+
       (cheshire/parse-string (slurp info-file) true)
       (catch Object e
         (log/warn "failed to parse info.json sidecar:" (.getName info-file) e)
         nil)))))

(defn download-media
  "Download media via yt-dlp into dir. Returns {:file File :metadata map :mime-type string} or throws.
   Caller must manage dir lifecycle (e.g. with-temp-dir). Metadata extracted from .info.json sidecar."
  [url dir]
  (let [media-file (download-media-file! url dir)
        metadata (read-info-json dir)
        ext (some->> (.getName media-file) (re-find #"\.(\w+)$") second)
        mime-type (case ext
                    "mp4" "video/mp4"
                    "m4a" "audio/mp4"
                    "mp3" "audio/mpeg"
                    "webm" "video/webm"
                    "ogg" "audio/ogg"
                    "video/mp4")]
    {:file media-file
     :metadata {:duration (:duration metadata)
                :title (:title metadata)
                :thumbnail (:thumbnail metadata)
                :uploader (:uploader metadata)
                :width (:width metadata)
                :height (:height metadata)
                :ext (or ext (:ext metadata) "mp4")
                :chapters (:chapters metadata)
                :description (:description metadata)}
     :mime-type mime-type}))

(defn download-subtitles [url]
  (with-temp-dir dir
    (with-retry 2 [:type ::av-download-error :ret 1]
      (let [{:keys [exit out err]}
            (with-throttle @+semaphore-av-download+
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
             :subtitles (slurp filename :encoding "UTF-8")}
            (throw+ {:type ::av-download-error
                     :detail "cannot find destination in output"
                     :tmp-dir dir
                     :out out
                     :err err
                     :ret exit}))
          (throw+ {:type ::av-download-error
                   :tmp-dir dir
                   :out out
                   :err err
                   :ret exit}))))))
