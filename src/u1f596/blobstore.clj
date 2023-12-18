(ns u1f596.blobstore
  (:require
   [u1f596.converter :as conv]
   [u1f596.regex :as regex-collection]
   [u1f596.appconfig :as appconfig]
   [u1f596.contentdetect :as contentdetect]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [clj-http.client :as http2]
   [java-time.api :as time]
   [schema.core :as s]
   [slingshot.slingshot :refer [throw+ try+]]
   [mount.core :refer [defstate]]
   [org.bovinegenius [exploding-fish :as uri]]
   [nio2.core :as nio2]
   [digest :as digest])
  (:import [org.bovinegenius.exploding_fish UniformResourceIdentifier]))

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

(defn- read-propsfile [filename]
  (let [content (slurp filename)]
    (try+
     (let [props (conv/read-edn-propsfile content)]
       (when (nil? props)
         (throw+ {:type ::props-read-error
                  :reason "file empty"
                  :props props
                  :filename filename
                  :content content}))
       props)
     (catch Object e
       (throw+ {:type ::props-read-error
                :reason (ex-message e)
                :filename filename
                :content content})))))

(defn try-read-propsfile-or-recreate [propsfile file]
  (try+
   (read-propsfile propsfile)
   (catch [:type ::props-read-error] err
     (log/warn "Propsfile broken. Recreating" err)

     (let [urls (or (into #{} (->> (:content err)
                                   (re-seq regex-collection/url)
                                   (map first)
                                   (map uri/uri)))
                    #{})
           new-props {:hits (count urls)
                      :msg (str "recreated by try-read-propsfile-or-recreate "
                                (time/format :iso-instant (time/zoned-date-time)))
                      :failed-file (:content err)
                      :orig-urls urls
                      :mime-type (contentdetect/detect-mime-type (io/input-stream file))}]
       (spit propsfile (conv/print-propsfile new-props))
       new-props))))

(s/defn create-url-index-entry-for-url :- s/Str
  "Create secondary url index entry: url -> hash"
  [content-hash :- s/Str
   url :- UniformResourceIdentifier]
  (let [file-abs (blob-file (appconfig/blob-store-dir) content-hash)
        url-hash (digest/sha-256 (str url))
        dupe-file (-> (blob-file (appconfig/blob-store-dupes-dir) url-hash) io/as-file .toPath)

        link (-> (blob-file (appconfig/blob-store-url-index-dir) url-hash) .toPath)
        link-props (-> (str link ".props") io/as-file .toPath)

        file (.relativize
              (-> link
                  .getParent)
              (-> file-abs
                  .toPath))
        propsfile (-> (str file ".props") io/as-file .toPath)]

    (nio2/create-dirs (.getParent link))
    (try+
     (nio2/create-sym-link link-props propsfile)
     (nio2/create-sym-link link file)
     (catch java.nio.file.FileAlreadyExistsException e
       (log/debug "Symlink already exists" {:content-hash content-hash
                                            :url-hash url-hash
                                            :url url
                                            :props-link link-props
                                            :file-link link
                                            :propsfile propsfile
                                            :file file
                                            :props (conv/read-edn-propsfile (slurp (nio2/input-stream link-props)))})
       ;; (java.nio.file.Files/delete (.toPath link-props))

       (nio2/create-dirs (.getParent dupe-file))
       (spit (nio2/output-stream dupe-file) url)))
    url-hash))

(defn blob-store-file-seq
  "Return seq of blob store entry files - only data files not
  .props"
  []
  (->> (appconfig/blob-store-dir)
       io/as-file
       file-seq
       (filter #(.isFile %))
       (filter #(re-find #"^[0-9a-f]{64}$" (.getName %)))
       (map #(.getAbsolutePath %))
       (map io/as-file)))

(defn find-in-url-index [url]
  (let [hash (digest/sha-256 (str url))
        link (blob-file (appconfig/blob-store-url-index-dir) hash)
        link-props (io/as-file (str link ".props"))]
    (when (and (.exists link) (.exists link-props))
      (let [link-target (java.nio.file.Files/readSymbolicLink
                         (.toPath link))]
        (str (.getFileName link-target))))))

(defn client-error?
  [{:keys [status]}]
  (when (number? status)
    (<= 400 status 499)))

(defn server-error?
  [{:keys [status]}]
  (when (number? status)
    (<= 500 status 599)))

(defn- download-and-add!
  "Download, hash, add to primary index - create secondary index entry"
  [url]
  (try+
   (let [response (http2/get (str url)
                             {:as :stream
                              :socket-timeout 10000
                              :connection-timeout 5000})

         body (.readAllBytes (:body response))
         content-hash (digest/sha-256 body)

         response-mime (get-in response [:headers "Content-Type"])
         mime (if (or (nil? response-mime)
                      (= response-mime "application/octet-stream"))
                (contentdetect/detect-mime-type body)
                response-mime)

         file (blob-file (appconfig/blob-store-dir) content-hash)
         propsfile (io/as-file (str file ".props"))

         lock-key (subs content-hash 0 1)
         lock-obj (get locks lock-key)]

;;      (log/debugf "BLOBSTORE STORE %s -> %s (%s) LOCK %s FILE %s SIZE %s"
        ;; url content-hash mime lock-key file (count body))
     (when (= (uri/path url) "/")
       (log/warn "BLOBSTORE Broken url?" url)
       (throw+ {:type ::perm-fail :url url}))

     ;; we don't do file system locking here to protect the individual prop files,
     ;; but rather have application level locks that lock subtrees.
     (locking lock-obj
       (ensure-dir-hierarchy file)
       (let [exists? (and (.exists propsfile)
                          (.exists file))
             props (if (.exists propsfile)
                     (let [prev-probs (try-read-propsfile-or-recreate propsfile file)
                           prev-urls (into #{} (:orig-urls prev-probs))]
                       {:orig-urls (conj prev-urls url)
                        :hits (+ 1 (:hits prev-probs))
                        :mime-type mime})
                     {:orig-urls #{url}
                      :hits 1
                      :mime-type mime})]
         (when-not exists?
           (with-open [o (io/output-stream file)]
             (.write o body)))
         (spit propsfile (conv/print-propsfile props))
         (create-url-index-entry-for-url content-hash url)))
     content-hash)
   (catch client-error?
          {:keys [headers body status]}
     (log/debug "BLOBSTORE Client Error (-> perm-fail):" status url)
     (throw+ {:type ::fetch-fail}))
   (catch server-error?
          {:keys [headers body status]}
     (log/debug "BLOBSTORE Server Error (-> temp-fail):" status url)
     (throw+ {:type ::fetch-fail}))
   (catch java.net.MalformedURLException _
     (log/warn (:throwable &throw-context) "URL Kaputt?" url))
   (catch java.net.ConnectException e
     (log/debug "BLOBSTORE Server Error (-> temp-fail):" e url)
     (throw+ {:type ::fetch-fail}))
   (catch Object _
     (log/warn (:throwable &throw-context) "BLOBSTORE ADD failed" url)
     (throw+ {:type ::undefined-error :url url}))))

(defn blob-info [content-hash]
  (let [file (blob-file (appconfig/blob-store-dir) content-hash)
        propsfile (io/as-file (str file ".props"))
        props (when (nio2/exists? (.toPath propsfile))
                (conv/read-edn-propsfile (slurp propsfile)))
        urls (:orig-urls props)
        url-files (map #(blob-file (appconfig/blob-store-dupes-dir) (digest/sha-256 (str %))) urls)]
    (when-not (nio2/exists? (.toPath propsfile))
      (throw+ {:type ::invalid-argument :reason ::blob-does-not-exist
               :file file :propsfile propsfile
               :props props :urls urls
               :url-files url-files :content-hash content-hash}))

    [file propsfile url-files]))

(s/defn add-from-url! :- s/Str
  [url :- UniformResourceIdentifier]
  (or (find-in-url-index url)
      (download-and-add! url)))

(defn get-local-filename [hash]
  (blob-file (appconfig/blob-store-dir) hash))

(defn get-blob [hash]
  (let [file (blob-file (appconfig/blob-store-dir) hash)
        size (.length file)
        propsfile (str file ".props")
        props (try-read-propsfile-or-recreate propsfile file)
        blob (-> props
                 (assoc :hash hash)
                 (assoc :file file)
                 (assoc :size size)
                 (assoc :data (io/input-stream file))
                 (assoc :created (time/zoned-date-time (time/instant (.lastModified file)) "UTC")))]
    ;; (log/debug "BLOBSTORE GET: " blob)
    blob))
