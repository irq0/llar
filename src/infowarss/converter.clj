(ns infowarss.converter
  (:require
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [org.bovinegenius [exploding-fish :as uri]]
   [pantomime.mime :as pm]
   [java-time :as time]
   [taoensso.timbre :as log]
   [clojure.edn :as edn]
   [puget.printer :as puget]
   [clojure.java.shell :as shell]))

(def +html-to-text-tools+
  {:pandoc ["pandoc" "-f" "html" "-t" "plain" "--reference-links"]
   :w3m ["w3m" "-T" "text/html" "-dump"]
   :lynx ["lynx" "-dump" "-list_inline" "-width 1024" "-stdin"]
   :html2text ["html2text" "-style" "pretty" "-utf8"]})

(defn html2text
  "Convert html to text"
  [html & {:keys [tool] :or {tool :lynx}}]
  (let [cmdline (concat (get +html-to-text-tools+ tool) [:in html])
        {:keys [exit out]}
        (apply shell/sh cmdline)]
    (if (zero? exit)
      out
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

(defmethod base64-decode nil [data]
  nil)

(defmethod base64-decode String [data]
  (.decode
   (java.util.Base64/getMimeDecoder)
   data))

(defn data-uri [data & {:keys [mime-type]
                        :or {mime-type (pm/mime-type-of data)}}]
  (if (and (instance? String data) (nil? mime-type))
    (format "data:text/plain;%s" (java.net.URLEncoder/encode data "UTF-8"))
    (format "data:%s;base64,%s"
            mime-type (java.net.URLEncoder/encode (base64-encode data) "ASCII"))))

(defn get-mimetype
  [data & {:keys [mime-type]}]
  (if-not (string/blank? mime-type)
    mime-type
    (pm/mime-type-of data)))

(defmulti convert-to-html get-mimetype)

(defmethod convert-to-html "application/pdf" [data & _]
  (let [file (java.io.File/createTempFile "content" ".pdf")
        out-file (io/as-file (string/replace (.getAbsolutePath file) #"\.pdf$" ".html"))]
    (io/copy data file)
    (let [{:keys [exit out err]} (shell/sh "pdf2htmlEX"
                                           "--no-drm" "1"
                                           "--printing" "1"
                                           "--zoom" "1.3"
                                           (.getName file)
                                           :dir (.getParent file)
                                           :out-enc :bytes)]
      (when-not (zero? exit)
        (throw+ {:type ::conversion-failed :exit exit :out out :file file :err err}))

      (.delete file)
      (with-open [out (java.io.ByteArrayOutputStream.)]
        (clojure.java.io/copy (clojure.java.io/input-stream out-file) out)
        (.delete out-file)
        (str out)))))

(defmulti thumbnail get-mimetype)

(defmethod thumbnail "application/pdf" [data & _]
  (let [{:keys [exit out err]} (shell/sh "pdftocairo" "-png" "-singlefile"
                                         "-scale-to" "800" "-" "-" :in data :out-enc :bytes)]
    (when-not (zero? exit)
      (throw+ {:type ::thumbnail-creation-failed :exit exit :out out :err err}))
    {"image/png" out}))

;; propsfile: blobstore metadata

(def +propsfile-handlers+
  {org.bovinegenius.exploding_fish.Uri
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

;; reader annotations data store

(defn print-annotations [props]
  (puget/render-str (puget/canonical-printer) props))

(defn read-edn-annotations [s]
  (edn/read-string s))

;; feed fetch state

(def +state-handlers+
  {java.time.ZonedDateTime
   (puget/tagged-handler
    'org.irq0.🖖/datetime
    #(time/format :iso-zoned-date-time %))

   clojure.lang.Atom
   (puget/tagged-handler
    'org.irq0.🖖/atom
    (fn [value] (puget/pprint-str @value)))

   org.bovinegenius.exploding_fish.Uri
   (puget/tagged-handler
    'org.irq0.🖖/url str)})

(defn print-state [state]
  (puget/render-str (puget/canonical-printer +state-handlers+)
                    state))

(defrecord GenericTaggedValue [tag value])

(defn read-edn-state [s]
  (edn/read-string
   {:readers {'org.irq0.🖖/datetime #(time/zoned-date-time (time/formatter :iso-zoned-date-time) %)
              'org.irq0.🖖/url uri/uri
              'org.irq0.🖖/atom (fn [x] (atom x))}}
   s))

(defn parse-http-ts [ts]
  (when-not (nil? ts)
    (time/zoned-date-time (time/formatter :rfc-1123-date-time)  ts)))

(defn bytea-hex-to-byte-array [bytea]
  (when bytea
    (byte-array
     (map (fn [[a b]] (Integer/parseInt (str a b) 16))
          (drop 1 (partition 2 bytea))))))

