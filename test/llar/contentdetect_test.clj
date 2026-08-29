(ns llar.contentdetect-test
  (:require
   [clojure.test :refer [deftest is]]
   [llar.contentdetect :as contentdetect])
  (:import
   [java.io ByteArrayInputStream]
   [java.nio.charset StandardCharsets]))

(deftest detects-content-properties
  (let [pdf-bytes (.getBytes "%PDF-1.4\n%%EOF\n" StandardCharsets/US_ASCII)
        mime-type (with-open [stream (ByteArrayInputStream. pdf-bytes)]
                    (contentdetect/detect-mime-type stream))]
    (is (= "application/pdf" mime-type))
    (is (= ".pdf" (contentdetect/mime-extension mime-type)))
    (is (false? (contentdetect/text-mime-type? mime-type)))
    (is (true? (contentdetect/text-mime-type? "text/html; charset=UTF-8")))))
