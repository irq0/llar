(ns llar.converter
  (:require
   [clojure.edn :as edn]
   [clojure.java.shell :as shell]
   [clojure.string :as string]
   [clojure.xml :as xml]
   [hickory.select :as hick-s]
   [java-time.api :as time]
   [org.bovinegenius [exploding-fish :as uri]]
   [slingshot.slingshot :refer [throw+]]
   [puget.printer :as puget]
   [llar.appconfig :as appconfig]
   [llar.contentdetect :as contentdetect])
  (:import
   [org.bovinegenius.exploding_fish Uri]))

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

;; opml import

(defn- ompl-parser-error-handler []
  (proxy [org.xml.sax.helpers.DefaultHandler] []
    (fatalError [^org.xml.sax.SAXParseException e]
      (throw+ {:type ::ompl-parser-error
               :level :fatal
               :line (.getLineNumber e)
               :column (.getColumnNumber e)
               :message (.getMessage e)}))
    (error [^org.xml.sax.SAXParseException e]
      (throw+ {:type ::ompl-parser-error
               :level :error
               :line (.getLineNumber e)
               :column (.getColumnNumber e)
               :message (.getMessage e)}))))

(defn xml-parser-non-validating [s ch]
  (let [factory (doto
                 (javax.xml.parsers.SAXParserFactory/newInstance)
                  (.setValidating false)
                  (.setFeature
                   "http://apache.org/xml/features/nonvalidating/load-external-dtd" false))
        parser (.newSAXParser factory)
        reader (.getXMLReader parser)
        source (doto (org.xml.sax.InputSource. s) (.setEncoding "UTF-8"))]
    (.setErrorHandler reader (ompl-parser-error-handler))
    (.setContentHandler reader ch)
    (.parse reader source)
    parser))

(defn read-opml-feeds [in]
  (->>
   (xml/parse in xml-parser-non-validating)
   (hick-s/select
    (hick-s/descendant
     (hick-s/attr :type #{"rss" "atom"})))
   (map (fn [x]
          (let [{:keys [title type xmlUrl]} (:attrs x)]
            {:title title
             :type type
             :url (uri/uri xmlUrl)})))))

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

(defn ttml2text [ttml]
  (letfn [(collect-p-tags [node]
            (when (= :p (:tag node))
              (apply str (:content node))))
          (walk [node]
            (let [this (collect-p-tags node)
                  children (mapcat walk (:content node))]
              (if this
                (cons this children)
                children)))]
    (let [root (xml/parse (java.io.StringReader. ttml) xml-parser-non-validating)]
      (string/join " " (walk root)))))
