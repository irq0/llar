(ns llar.apis.blob
  (:require
   [clojure.tools.logging :as log]
   [llar.blobstore :as blobstore]
   [llar.http.response :as http-response])
  (:import
   [java.io FileNotFoundException]))

(def ^:private immutable-cache-control
  "public, max-age=31536000, immutable")

(defn response
  ([hash]
   (response hash {:request-method :get :headers {}}))
  ([hash request]
   (cond
     (not (#{:get :head} (:request-method request)))
     {:status 405 :headers {"Allow" "GET, HEAD"}}

     (not (re-matches #"[0-9a-f]{64}" hash))
     {:status 404}

     :else
     (try
       (let [blob (blobstore/get-blob-metadata hash)]
         (assoc-in
          (http-response/ranged-file-response
           {:file (:file blob)
            :size (:size blob)
            :mime-type (:mime-type blob)
            :etag hash
            :last-modified (:created blob)}
           request)
          [:headers "Cache-Control"] immutable-cache-control))
       (catch FileNotFoundException _
         (log/debug "blob not found: " hash)
         {:status 404})))))
