(ns llar.blobstore-test
  (:require
   [clojure.test :refer [deftest is]]
   [llar.blobstore :as blobstore]
   [llar.contentdetect :as contentdetect]
   [slingshot.slingshot :refer [throw+]]))

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
