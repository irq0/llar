(ns u1f596.contentdetect
  (:import [org.apache.tika Tika]
           [org.apache.tika.mime MimeTypes MimeType MediaType]))

(def ^Tika tika (Tika.))
(def ^MimeTypes mime-types (MimeTypes/getDefaultMimeTypes))

(defn detect-mime-type [obj]
  (.detect tika obj))

(defn mime-extension [mime-str]
  (->> mime-str
       (.forName mime-types)
       .getExtension))

(defn text-mime-type? [mime]
  (= (-> (MediaType/parse mime) .getType) "text"))
