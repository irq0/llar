(ns llar.apis.blob-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llar.apis.blob :as uut]
   [llar.blobstore :as blobstore]
   [ring.middleware.not-modified :refer [wrap-not-modified]]))

(def hash-value (apply str (repeat 64 "a")))

(defn- test-blob []
  {:mime-type "image/jpeg"
   :created (java.time.ZonedDateTime/now)
   :size 5
   :data (java.io.ByteArrayInputStream. (.getBytes "image"))})

(deftest response-has-immutable-http-cache-metadata
  (with-redefs [blobstore/get-blob (constantly (test-blob))]
    (let [response (uut/response hash-value)]
      (is (= 200 (:status response)))
      (is (= (str "W/\"" hash-value "\"") (get-in response [:headers "ETag"])))
      (is (= "5" (get-in response [:headers "Content-Length"])))
      (is (= "public, max-age=31536000, immutable"
             (get-in response [:headers "Cache-Control"])))
      (is (= "nosniff" (get-in response [:headers "X-Content-Type-Options"])))
      (is (string? (get-in response [:headers "Last-Modified"]))))))

(deftest conditional-get-returns-not-modified
  (with-redefs [blobstore/get-blob (constantly (test-blob))]
    (let [handler (wrap-not-modified (fn [_] (uut/response hash-value)))
          etag (str "W/\"" hash-value "\"")
          response (handler {:request-method :get
                             :headers {"if-none-match" etag}})]
      (is (= 304 (:status response)))
      (is (nil? (:body response)))
      (is (nil? (get-in response [:headers "Content-Length"]))))))

(deftest missing-and-invalid-blobs-return-not-found
  (testing "invalid hashes do not reach the blobstore"
    (with-redefs [blobstore/get-blob (fn [_] (throw (ex-info "must not read" {})))]
      (is (= {:status 404} (uut/response "invalid")))))
  (testing "missing files are not server errors"
    (with-redefs [blobstore/get-blob
                  (fn [_] (throw (java.io.FileNotFoundException. "missing")))]
      (is (= {:status 404} (uut/response hash-value)))))
  (testing "unexpected storage errors reach the shared exception middleware"
    (with-redefs [blobstore/get-blob (fn [_] (throw (ex-info "corrupt" {})))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"corrupt"
                            (uut/response hash-value))))))
