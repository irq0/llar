(ns infowarss.blobstore
  (:require
   [infowarss.converter :as conv]
   [clojure.java.io :as io]
   [taoensso.timbre :as log]
   [clj-http.client :as http2]
   [clj-time.coerce :as tc]
   [schema.core :as s]
   [pantomime.mime :as pm]
   [slingshot.slingshot :refer [throw+ try+]]
   [mount.core :refer [defstate]]
   [nio2.core :as nio2]
   [digest :as digest]))


(def +blob-store+ "/fast/infowarss/blobs")
(def +blob-store-url-index+ "/fast/infowarss/blobs/url-index")
(def +blob-store-dupes+ "/fast/infowarss/url-dupe-content-index")

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
  (let [file-abs (blob-file +blob-store+ content-hash)
        url-hash (digest/sha-256 (str url))
        dupe-file (-> (blob-file +blob-store-dupes+ url-hash) io/as-file .toPath)

        link (-> (blob-file +blob-store-url-index+ url-hash) .toPath)
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
                                             :props (conv/read-edn-string (slurp (nio2/input-stream link-props)))

                                             })
       ;; (java.nio.file.Files/delete (.toPath link-props))
       (nio2/create-dirs (.getParent dupe-file))
       (spit (nio2/output-stream dupe-file) url)))
    url-hash))


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
        (str (.getFileName link-target))))))

(defn- setify-urls [x]
  (cond
    (set? x) x
    (set? (first x)) (first x)
    (coll? x) (set x)
    (nil? x) #{}
    :else x))

(defn client-error?
  [{:keys [status]}]
  (when (number? status)
    (<= 400 status 499)))

(defn server-error?
  [{:keys [status]}]
  (when (number? status)
    (<= 500 status 599)))


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
                      (let [prev-probs (conv/read-edn-string (slurp propsfile))
                            prev-urls (setify-urls (:orig-urls prev-probs))]
                        {:orig-urls (conj prev-urls url)
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
    (catch Object _
      (log/warn (:throwable &throw-context) "BLOBSTORE ADD failed" url)
      (throw+ {:type ::undefined-error :url url}))))


(s/defn add-from-url! :- s/Str
  [url :- clojurewerkz.urly.UrlLike]
  (or (find-in-url-index url)
    (download-and-add! url)))

(defn get-local-filename [hash]
  (blob-file +blob-store+ hash))

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
