(ns llar.commands
  (:require
   [clojure.java.shell :as shell]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [cheshire.core :as cheshire]
   [slingshot.slingshot :refer [try+ throw+]]
   [llar.appconfig :as appcfg :refer [appconfig]]
   [llar.rc :as rc]
   [llar.throttle :as throttle :refer [with-throttle]]
   [mount.core :refer [defstate]]
   [nio2.core :as nio2]))

;; Wrapper for all external commands we run

(defonce +kill-timeout-secs+ 120)

(def ^:private +command-limit-path+ [:throttle :command-max-concurrent])
(def ^:private +av-download-limit-path+ [:throttle :av-downloader-max-concurrent])

(defstate command-throttle
  :start (-> (throttle/make-throttle :command (rc/rc +command-limit-path+))
             (throttle/follow-runtime-config! +command-limit-path+))
  :stop (throttle/shutdown! command-throttle))

(defstate av-download-throttle
  :start (-> (throttle/make-throttle :av-download (rc/rc +av-download-limit-path+))
             (throttle/follow-runtime-config! +av-download-limit-path+))
  :stop (throttle/shutdown! av-download-throttle))

(defmacro with-temp-dir [dir-sym & body]
  `(let [~dir-sym (nio2/create-tmp-dir-on-default-fs "llar-")]
     (try
       ~@body
       (finally
         (when (nio2/exists? ~dir-sym)
           (doseq [path# (reverse (file-seq (nio2/file ~dir-sym)))]
             (.delete path#)))))))

(defn sh+timeout [timeout-secs args & opts]
  (let [cmd (into ["timeout" (str "--kill-after=" +kill-timeout-secs+ "s")
                   (str timeout-secs "s")] args)
        sh-arg (apply concat cmd (when opts (apply concat opts)))
        _ (log/debugf "external command: %s" (pr-str cmd))
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
        (with-throttle command-throttle
          (sh+timeout (get-in appconfig [:timeouts :readability])
                      ["node" "tools/dompurify"] {:in raw-html}))]
    (if (zero? exit)
      out
      (throw+ {:type :llar.http/sanitize-error
               :message err
               :ret exit}))))

(defn readability [raw-html url]
  (let [{:keys [out exit err]}
        (with-throttle command-throttle
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
        {:keys [exit out]} (with-throttle command-throttle (sh+timeout (get-in appconfig [:timeouts :html2text])
                                                                       cmdline))]
    (if (zero? exit)
      (if (= :for-exceptions tool)
        (string/replace out #"[\n\t]" " ")
        out)
      "")))

(def ^:private +cookies-tmp-file+ "/tmp/llar-yt-cookies.txt")

(defn- av-downloader-cookies-args
  "Return [\"--cookies\" path] if :youtube-cookies is set in credentials.
   Writes cookie content to a fixed temp file for yt-dlp to read."
  []
  (if-let [cookies (appcfg/credentials :youtube-cookies)]
    (do (spit +cookies-tmp-file+ cookies)
        ["--cookies" +cookies-tmp-file+])
    []))

(defn media-metadata
  "Fetch metadata from yt-dlp without downloading"
  [url]
  (let [{:keys [exit out err]}
        (with-throttle av-download-throttle
          (sh+timeout (get-in appconfig [:timeouts :av-downloader])
                      (into [(appcfg/command :av-downloader)
                             "--dump-json"]
                            (conj (av-downloader-cookies-args)
                                  (str url)))))]
    (if (zero? exit)
      (cheshire/parse-string out true)
      (throw+ {:type ::av-metadata-error
               :url url :err err :ret exit}))))

(defn- download-media-file!
  "Download media file via yt-dlp into dir. Returns the downloaded File."
  [url dir]
  (let [format-spec (rc/rc [:podcast :download :video-format])
        extra-args (rc/rc [:podcast :download :extra-args])
        timeout (get-in appconfig [:timeouts :av-downloader-transcode]
                        (get-in appconfig [:timeouts :av-downloader]))
        {:keys [exit out err]}
        (with-throttle av-download-throttle
          (sh+timeout timeout
                      (-> [(appcfg/command :av-downloader)
                           "--format" format-spec
                           "--merge-output-format" "mp4"
                           "--write-info-json"
                           "--no-clean-info-json"
                           (str "--output=" (nio2/path dir "media.%(ext)s"))]
                          (into (av-downloader-cookies-args))
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

(def ^:private retained-media-metadata-fields
  [:id :webpage_url :original_url :extractor :extractor_key
   :duration :title :thumbnail :uploader :uploader_id :channel :channel_id
   :upload_date :timestamp :release_date :release_timestamp :language :tags
   :categories :live_status :availability :width :height :fps :vcodec :acodec
   :format_id :chapters :description])

(defn- retain-media-metadata [metadata ext]
  (-> (select-keys metadata retained-media-metadata-fields)
      (update :original_url #(or % (:webpage_url metadata)))
      (assoc :ext (or ext (:ext metadata) "mp4"))))

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
     :metadata (retain-media-metadata metadata ext)
     :mime-type mime-type}))

(defn download-subtitles [url]
  (with-temp-dir dir
    (with-retry 2 [:type ::av-download-error :ret 1]
      (let [{:keys [exit out err]}
            (with-throttle av-download-throttle
              (sh+timeout (get-in appconfig [:timeouts :av-downloader])
                          (-> [(appcfg/command :av-downloader)
                               "--skip-download"
                               "--write-subs"
                               "--write-auto-subs"
                               "--sub-langs=.*-orig"
                               "--sub-format=ttml"
                               (str "--output=" (nio2/path dir "llar"))]
                              (into (av-downloader-cookies-args))
                              (conj (str url)))))]
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
