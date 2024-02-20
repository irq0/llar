(ns llar.converter
  (:require
   [clojure.edn :as edn]
   [clojure.java.shell :as shell]
   [clojure.string :as string]
   [java-time.api :as time]
   [org.bovinegenius [exploding-fish :as uri]]
   [puget.printer :as puget]
   [llar.appconfig :as appconfig]
   [llar.contentdetect :as contentdetect])
  (:import
   [org.bovinegenius.exploding_fish Uri]))

(defn html-to-text-command [tool]
  (get {:pandoc [(appconfig/command :pandoc) "-f" "html" "-t" "plain" "--reference-links"]
        :w3m [(appconfig/command :w3m) "-T" "text/html" "-dump"]
        :for-exceptions [(appconfig/command :pandoc) "-f" "html" "-t" "plain"]
        :lynx [(appconfig/command :lynx) "-dump" "-list_inline" "-width 1024" "-stdin"]
        :html2text [(appconfig/command :html2text) "-style" "pretty" "-utf8"]}
       tool))

(defn html2text
  "Convert html to text"
  [html & {:keys [tool] :or {tool :lynx}}]
  (let [cmdline (concat (html-to-text-command tool) [:in html])
        {:keys [exit out]}
        (apply shell/sh cmdline)]
    (if (zero? exit)
      (if (= :for-exceptions tool)
        (string/replace out #"[\n\t]" " ")
        out)
      "")))

(defmulti base64-encode class)

(defmethod base64-encode String [data]
  (.encodeToString
   (java.util.Base64/getMimeEncoder)
   (.getBytes data)))

(defmethod base64-encode (Class/forName "[B") [data]
  (.encodeToString
   (java.util.Base64/getMimeEncoder)
   data))

(defmulti base64-decode class)

(defmethod base64-decode nil [_]
  nil)

(defmethod base64-decode String [data]
  (.decode
   (java.util.Base64/getMimeDecoder)
   data))

(defn data-uri [data & {:keys [mime-type]
                        :or {mime-type (contentdetect/detect-mime-type data)}}]
  (if (and (instance? String data) (nil? mime-type))
    (format "data:text/plain;%s" (java.net.URLEncoder/encode data "UTF-8"))
    (format "data:%s;base64,%s"
            mime-type (java.net.URLEncoder/encode (base64-encode data) "ASCII"))))

(defn get-mimetype
  [data & {:keys [mime-type]}]
  (if-not (string/blank? mime-type)
    mime-type
    (contentdetect/detect-mime-type data)))

;; propsfile: blobstore metadata

(def +propsfile-handlers+
  {Uri
   (puget/tagged-handler
    'org.irq0.🖖/url str)})

(defn print-propsfile [props]
  (puget/render-str (puget/canonical-printer +propsfile-handlers+)
                    props))

(defn read-edn-propsfile [s]
  (edn/read-string
   {:readers {'org.irq0.🖖/datetime #(time/zoned-date-time (time/formatter :iso-zoned-date-time) %)
              'org.irq0.🖖/url uri/uri
              'org.irq0.🖖/atom (fn [x] (atom x))}}
   s))

;; feed fetch state

(defrecord ExceptionContext [cause message stack-trace throwable data])

(defn make-exception-context-from-slingshot-throw-context [throw-context]
  (let [data (ex-data (:throwable throw-context))]
    (->ExceptionContext
     (:cause throw-context)
     (or (:message throw-context) (ex-message throw-context))
     (:stack-trace throw-context)
     (:throwable throw-context)
     data)))

(def +state-handlers+
  {java.time.ZonedDateTime
   (puget/tagged-handler
    'org.irq0.🖖/datetime
    #(time/format :iso-zoned-date-time %))

   llar.converter.ExceptionContext
   (puget/tagged-handler
    'org.irq0.🖖/exception
    (fn [val] (select-keys val [:message :cause :data])))

   java.time.Duration
   (puget/tagged-handler
    'org.irq0.🖖/duration
    #(str %))

   Uri
   (puget/tagged-handler
    'org.irq0.🖖/url str)})

(defn print-state [state]
  (puget/render-str (puget/canonical-printer +state-handlers+)
                    state))

(defn read-edn-state [s]
  (edn/read-string
   {:readers {'org.irq0.🖖/datetime #(time/zoned-date-time (time/formatter :iso-zoned-date-time) %)
              'org.irq0.🖖/duration #(time/duration %)
              'org.irq0.🖖/exception (fn [val] (when-not (nil? val)
                                                 (->ExceptionContext
                                                  (:cause val)
                                                  (:message val)
                                                  (make-array java.lang.StackTraceElement 0)
                                                  nil
                                                  (:data val))))
              'org.irq0.🖖/url uri/uri}}
   s))

(defn parse-http-ts [ts]
  (when-not (nil? ts)
    (time/zoned-date-time (time/formatter :rfc-1123-date-time)  ts)))

(defn bytea-hex-to-byte-array [bytea]
  (when bytea
    (byte-array
     (map (fn [[a b]] (Integer/parseInt (str a b) 16))
          (drop 1 (partition 2 bytea))))))
