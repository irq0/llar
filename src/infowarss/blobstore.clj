(ns infowarss.blobstore
  (:require
   [infowarss.converter :as conv]
   [clojure.java.io :as io]
   [taoensso.timbre :as log]
   [clj-http.client :as http2]
   [clj-time.coerce :as tc]
   [schema.core :as s]
   [infowarss.schema :as schema]
   [pantomime.mime :as pm]
   [slingshot.slingshot :refer [throw+ try+]]
   [mount.core :refer [defstate]]
   [clojurewerkz.urly.core :as urly]
   [digest :as digest])
  (:import [java.net URI URL]))


(def +blob-store+ "/tank/scratch/infowarss-blobs")
(def +blob-store-url-index+ "/tank/scratch/infowarss-blobs/url-index")
 (def +blob-store-dupes+ "/tank/scratch/infowarss-blobs/url-dupe-content-index")

(defstate locks :start (into {} (for [x (range 16)]
                                  [(format "%h" x) (Object.)])))

(defn- blob-file
  "Return filename for blob hash"
  [base-dir hash]
  (io/as-file
    (format "%s/%s/%s/%s/%s"
      base-dir
      (subs hash 0 1)
      (subs hash 1 2)
      (subs hash 2 4)
      hash)))

(defn- ensure-dir-hierarchy
  "mkdir -p"
  [filename]
  (-> filename
    .getParent
    io/as-file
    .mkdirs))

(s/defn create-url-index-entry-for-url :- s/Str
  "Create secondary url index entry: url -> hash"
  [content-hash :- s/Str
   url :- clojurewerkz.urly.UrlLike]
  (let [file (blob-file +blob-store+ content-hash)
        propsfile (io/as-file (str file ".props"))
        url-hash (digest/sha-256 (str url))
        empty-file-attributes (make-array java.nio.file.attribute.FileAttribute 0)
        dupe-file (blob-file +blob-store-dupes+ url-hash)
        link (blob-file +blob-store-url-index+ url-hash)
        link-props (io/as-file (str link ".props"))]
    (ensure-dir-hierarchy link)
    (try+
     (java.nio.file.Files/createSymbolicLink
      (.toPath link-props)
      (.toPath propsfile)
      empty-file-attributes)
     (java.nio.file.Files/createSymbolicLink
      (.toPath link)
      (.toPath file)
      empty-file-attributes)
     (catch java.nio.file.FileAlreadyExistsException e
       (log/debug "Symlink already exists:" link-props link)
       (log/trace "Current pri:" propsfile)
       (log/trace "Link dst: " propsfile file
                  (conv/read-edn-string (slurp link-props)))
       (java.nio.file.Files/delete (.toPath link-props))
       (ensure-dir-hierarchy dupe-file)
       (spit dupe-file url)))
    url-hash))


(defn- create-url-index-entry-from-primary-fn
  "Migration function - create secondary / url index entry for each url
  in the primary index props file"
  [content-file]
  (let [content-hash (.getName content-file)
        propsfile (io/as-file (str content-file ".props"))
        props (conv/read-edn-string (slurp propsfile))
        {:keys [orig-urls]} props
        urls (->>
              orig-urls
              (filter #(contains? #{String java.net.URL} (type %)))
              (map (fn [x] (java.net.URI. x)))
              (into #{}))]
    (when-not (= (count urls) (count orig-urls))
      (log/warn "Found broken urls set:" orig-urls urls))
    (doseq [url urls
            :let [url-hash (digest/sha-256 (str url))
                  dupe-file (blob-file +blob-store-dupes+
                                       url-hash)]
            :when (and
                   (not= (.getPath url) "/")
                   (not (.exists dupe-file)))]
      (create-url-index-entry-for-url content-hash url))))


(defn blob-store-file-seq
  "Return seq of blob store entry files - only data files not
  .props"
  []
  (->> +blob-store+
    io/as-file
    file-seq
    (filter #(.isFile %))
    (filter #(re-find #"^[0-9a-f]{64}$" (.getName %)))
    (map #(.getAbsolutePath %))
    (map io/as-file)))


(defn find-in-url-index [url]
  (let [hash (digest/sha-256 (str url))
        link (blob-file +blob-store-url-index+ hash)
        link-props (io/as-file (str link ".props"))]
    (when (and (.exists link) (.exists link-props))
      (let [link-target (java.nio.file.Files/readSymbolicLink
                          (.toPath link))]
;;        (log/debugf "BLOBSTORE: found \"%s\" in url index" url)
        (str (.getFileName link-target))))))

(defn- download-and-add!
  "Downoad, hash, add to primary index - create secondary index entry"
  [url]
  (try+
    (let [response (http2/get (str url)
                     {:as :stream})

          body (.readAllBytes (:body response))
          content-hash (digest/sha-256 body)

          response-mime (get-in response [:headers "Content-Type"])
          mime (if (or (nil? response-mime)
                     (= response-mime "application/octet-stream"))
                 (pm/mime-type-of body)
                 response-mime)

          file (blob-file +blob-store+ content-hash)
          propsfile (io/as-file (str file ".props"))

          lock-key (subs content-hash 0 1)
          lock-obj (get locks lock-key)]

;;      (log/debugf "BLOBSTORE STORE %s -> %s (%s) LOCK %s FILE %s SIZE %s"
        ;; url content-hash mime lock-key file (count body))
      (when (= (.getPath url) "/")
        (log/warn "BLOBSTORE Broken url?" url)
        (throw+ {:type ::perm-fail :url url}))

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
          (spit propsfile (prn-str props))
          (create-url-index-entry-for-url content-hash url)))
      content-hash)
    (catch http2/client-error?
        {:keys [headers body status]}
      (log/debug "BLOBSTORE Client Error (-> perm-fail):" status url)
      (throw+ {:type ::fetch-fail}))
    (catch http2/server-error?
        {:keys [headers body status]}
      (log/debug "BLOBSTORE Server Error (-> temp-fail):" status url)
      (throw+ {:type ::fetch-fail}))
    (catch java.net.MalformedURLException _
      (log/warn (:throwable &throw-context) "URL Kaputt?" url))
    (catch Object _
      (log/warn (:throwable &throw-context) "BLOBSTORE ADD failed" url)
      (throw+ {:type ::undefined-error :url url}))))


(s/defn add-from-url! :- s/Str
  [url :- clojurewerkz.urly.UrlLike]
  (or (find-in-url-index url)
    (download-and-add! url)))

(defn get-blob [hash]
  (let [file (blob-file +blob-store+ hash)
        size (.length file)
        propsfile (str file ".props")
        props (conv/read-edn-string (slurp propsfile))
        blob (-> props
               (assoc :hash hash)
               (assoc :file file)
               (assoc :size size)
               (assoc :data (io/input-stream file))
               (assoc :created (tc/from-long (.lastModified file))))]
    ;; (log/debug "BLOBSTORE GET: " blob)
    blob))
