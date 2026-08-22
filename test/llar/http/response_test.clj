(ns llar.http.response-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llar.http.response :as response]))

(deftest byte-range-parsing
  (testing "open and bounded ranges"
    (is (= {:start 10 :end 99}
           (response/parse-byte-range "bytes=10-" 100)))
    (is (= {:start 10 :end 20}
           (response/parse-byte-range "bytes=10-20" 100))))
  (testing "an oversized end is clamped"
    (is (= {:start 90 :end 99}
           (response/parse-byte-range "bytes=90-999" 100))))
  (testing "suffix ranges"
    (is (= {:start 90 :end 99}
           (response/parse-byte-range "bytes=-10" 100)))
    (is (= {:start 0 :end 99}
           (response/parse-byte-range "bytes=-200" 100))))
  (testing "malformed, multiple, and out-of-bounds ranges are rejected"
    (is (= ::response/unsatisfiable
           (response/parse-byte-range "bytes=100-" 100)))
    (is (= ::response/unsatisfiable
           (response/parse-byte-range "bytes=0-1,4-5" 100)))
    (is (= ::response/unsatisfiable
           (response/parse-byte-range "bytes=-0" 100))))
  (testing "unsupported range units are ignored"
    (is (nil? (response/parse-byte-range "items=0-10" 100)))))

(deftest ranged-file-and-head-responses
  (let [file (java.io.File/createTempFile "llar-range" ".txt")
        resource {:file file
                  :size 10
                  :mime-type "text/plain"
                  :etag "content-hash"}]
    (try
      (spit file "0123456789")
      (let [partial (response/ranged-file-response
                     resource
                     {:request-method :get
                      :headers {"range" "bytes=3-6"}})]
        (is (= 206 (:status partial)))
        (is (= "3456" (slurp (:body partial))))
        (is (= "bytes 3-6/10" (get-in partial [:headers "Content-Range"])))
        (is (= "\"content-hash\"" (get-in partial [:headers "ETag"]))))
      (let [head (response/ranged-file-response
                  resource
                  {:request-method :head
                   :headers {"Range" "bytes=3-6"}})]
        (is (= 200 (:status head)))
        (is (nil? (:body head)))
        (is (= "10" (get-in head [:headers "Content-Length"]))))
      (let [invalid (response/ranged-file-response
                     resource
                     {:request-method :get
                      :headers {"range" "bytes=20-"}})]
        (is (= 416 (:status invalid)))
        (is (= "bytes */10" (get-in invalid [:headers "Content-Range"]))))
      (finally
        (.delete file)))))

(deftest byte-array-responses-use-the-same-range-contract
  (let [response (response/byte-array-response
                  (.getBytes "abcdef" "UTF-8")
                  {:mime-type "text/plain" :etag "bytes"}
                  {:request-method :get
                   :headers {"range" "bytes=-2"}})]
    (is (= 206 (:status response)))
    (is (= "ef" (slurp (:body response))))
    (is (= "\"bytes\"" (get-in response [:headers "ETag"])))))
