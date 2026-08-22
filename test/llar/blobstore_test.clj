(ns llar.blobstore-test
  (:require
   [clojure.test :refer [deftest is]]
   [llar.blobstore :as blobstore]
   [llar.converter :as conv]
   [llar.contentdetect :as contentdetect]
   [slingshot.slingshot :refer [throw+]]))

(deftest metadata-lookup-does-not-open-the-blob
  (let [hash (apply str (repeat 64 "a"))
        file (java.io.File/createTempFile "llar-blobstore-metadata-" ".blob")
        propsfile (java.io.File. (str file ".props"))]
    (try
      (spit file "video")
      (spit propsfile (conv/print-propsfile {:mime-type "video/mp4"}))
      (with-redefs [blobstore/get-local-filename (constantly file)]
        (let [metadata (blobstore/get-blob-metadata hash)]
          (is (= hash (:hash metadata)))
          (is (= "video/mp4" (:mime-type metadata)))
          (is (= 5 (:size metadata)))
          (is (= file (:file metadata)))
          (is (not (contains? metadata :data))))
        (with-open [data (:data (blobstore/get-blob hash))]
          (is (= "video" (slurp data)))))
      (finally
        (.delete propsfile)
        (.delete file)))))

(deftest recreate-propsfile-without-error-content
  (let [dir (java.nio.file.Files/createTempDirectory
             "llar-blobstore-test"
             (make-array java.nio.file.attribute.FileAttribute 0))
        propsfile (.toFile (.resolve dir "blob.props"))
        file (.toFile (.resolve dir "blob"))]
    (try
      (java.nio.file.Files/createFile
       (.toPath file)
       (make-array java.nio.file.attribute.FileAttribute 0))
      (with-redefs-fn
        {#'llar.blobstore/read-propsfile
         (fn [_]
           (throw+ {:type :llar.blobstore/props-read-error}))
         #'contentdetect/detect-mime-type
         (constantly "application/octet-stream")}
        #(is (= #{}
                (:orig-urls
                 (blobstore/try-read-propsfile-or-recreate propsfile file)))))
      (finally
        (java.nio.file.Files/deleteIfExists (.toPath propsfile))
        (java.nio.file.Files/deleteIfExists (.toPath file))
        (java.nio.file.Files/deleteIfExists dir)))))
