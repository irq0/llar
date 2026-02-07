(ns llar.contentdetect
  (:import [org.apache.tika Tika]
           [org.apache.tika.mime MimeTypes MediaType]))

(def tika-instance (atom nil))
(def mime-types-instance (atom nil))

(defn get-tika []
  (when-not @tika-instance
    (reset! tika-instance (Tika.)))
  @tika-instance)

(defn get-mime-types []
  (when-not @mime-types-instance
    (reset! mime-types-instance (MimeTypes/getDefaultMimeTypes)))
  @mime-types-instance)

(defn detect-mime-type [obj]
  (.detect (get-tika) obj))

(defn mime-extension [mime-str]
  (->> mime-str
       (.forName (get-mime-types))
       .getExtension))

(defn text-mime-type? [mime]
  (= (-> (MediaType/parse mime) .getType) "text"))
