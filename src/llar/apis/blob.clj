(ns llar.apis.blob
  (:require
   [clojure.tools.logging :as log]
   [llar.blobstore :as blobstore]
   [ring.util.time :as ring-time])
  (:import
   [java.io FileNotFoundException]
   [java.util Date]))

(def ^:private immutable-cache-control
  "public, max-age=31536000, immutable")

(defn response [hash]
  (if-not (re-matches #"[0-9a-f]{64}" hash)
    {:status 404}
    (try
      (let [blob (blobstore/get-blob hash)]
        {:status 200
         :headers (cond-> {"Content-Type" (:mime-type blob)
                           "Cache-Control" immutable-cache-control
                           "ETag" (str "W/\"" hash "\"")
                           "Last-Modified" (ring-time/format-date
                                            (Date/from (.toInstant (:created blob))))
                           "X-Content-Type-Options" "nosniff"}
                    (number? (:size blob))
                    (assoc "Content-Length" (str (:size blob))))
         :body (:data blob)})
      (catch FileNotFoundException _
        (log/debug "blob not found: " hash)
        {:status 404}))))
