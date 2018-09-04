(ns infowarss.blobstore
  (:require
   [infowarss.converter :as conv]
   [clojure.java.io :as io]
   [taoensso.timbre :as log]
   [clj-http.client :as http2]
   [clj-time.coerce :as tc]
   [pantomime.mime :as pm]
   [slingshot.slingshot :refer [throw+ try+]]
   [mount.core :refer [defstate]]
   [digest :as digest]))

(def +blob-store+ "/tank/scratch/infowarss-blobs")

;; todo add in memory bloom filter to speed things up

(defstate locks :start (into {} (for [x (range 16)]
                                  [(format "%h" x) (Object.)])))

(defn blob-file [hash]
  (io/as-file
    (format "%s/%s/%s/%s/%s"
      +blob-store+
      (subs hash 0 1)
      (subs hash 1 2)
      (subs hash 2 4)
      hash)))

(defn ensure-dir-hierarchy [filename]
  (-> filename
    .getParent
    io/as-file
    .mkdirs))

(defn add-from-url! [url]
  (try+
  (let [response (http2/get (str url)
                   {:as :stream})

        body (.readAllBytes (:body response))
        hash (digest/sha-256 body)

        response-mime (get-in response [:headers "Content-Type"])
        mime (if (or (nil? response-mime)
                   (= response-mime "application/octet-stream"))
               (pm/mime-type-of body)
               response-mime)

        file (blob-file hash)
        propsfile (io/as-file (str file ".props"))

        lock-key (subs hash 0 1)
        lock-obj (get locks lock-key)]
    (log/infof "BLOBSTORE %s -> %s (%s) LOCK %s FILE %s SIZE %s"
      url hash mime lock-key file (count body))
    (locking lock-obj
      (ensure-dir-hierarchy file)
      (let [exists? (and (.exists propsfile)
                      (.exists file))
            props (if (.exists propsfile)
                    (let [prev-probs (conv/read-edn-string (slurp propsfile))]
                      {:orig-urls (conj (:orig-urls prev-probs) url)
                       :hits (+ 1 (:hits prev-probs))
                       :mime-type mime})
                    {:orig-urls #{url}
                     :hits 1
                     :mime-type mime})]
        (when-not exists?
          (with-open [o (io/output-stream file)]
            (.write o body)))
        (spit propsfile (prn-str props))))
    hash)
  (catch (contains? #{400 401 402 403 404 405 406 410} (get % :status))
      {:keys [headers body status]}
    (log/warn "BLOBSTORE Broken url?" status headers body url)
    (throw+ {:type ::perm-fail}))
  (catch Object _
    (log/warn (:throwable &throw-context) "BLOBSTORE ADD failed" url)
    (throw+ {:type ::undefined-error :url url}))))



(defn get-blob [hash]
  (log/info hash)
  (let [file (blob-file hash)
        propsfile (str file ".props")
        props (conv/read-edn-string (slurp propsfile))]
    (-> props
      (assoc :data (io/input-stream file))
      (assoc :created (tc/from-long (.lastModified file))))))
