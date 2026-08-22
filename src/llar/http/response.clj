(ns llar.http.response
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [ring.util.response :as response]
   [ring.util.time :as ring-time])
  (:import
   [java.io ByteArrayInputStream FileInputStream]
   [java.time Instant OffsetDateTime ZonedDateTime]
   [java.util Date]
   [org.apache.commons.io.input BoundedInputStream]))

(defn quote-etag [etag]
  (when (some? etag)
    (let [etag (str etag)]
      (if (re-matches #"(?i)(?:W/)?\".*\"" etag)
        etag
        (str "\"" (str/replace etag #"[\"\r\n]" "") "\"")))))

(defn- ->date [value]
  (cond
    (instance? Date value) value
    (instance? Instant value) (Date/from ^Instant value)
    (instance? ZonedDateTime value) (Date/from (.toInstant ^ZonedDateTime value))
    (instance? OffsetDateTime value) (Date/from (.toInstant ^OffsetDateTime value))
    :else nil))

(defn format-http-date [value]
  (some-> value ->date ring-time/format-date))

(defn parse-byte-range
  "Parse one HTTP bytes range. Returns {:start :end}, nil when no range was
   requested or the range unit is unsupported, and ::unsatisfiable for a
   malformed, multiple, or empty bytes range."
  [range-header size]
  (cond
    (nil? range-header) nil
    (not (string? range-header)) ::unsatisfiable
    (not (str/starts-with? (str/lower-case range-header) "bytes=")) nil
    (or (neg? size)
        (str/includes? range-header ","))
    ::unsatisfiable

    :else
    (if-let [[_ start-text end-text]
             (re-matches #"(?i)^bytes=(\d*)-(\d*)$" range-header)]
      (try
        (cond
          (zero? size)
          ::unsatisfiable

          (and (str/blank? start-text) (str/blank? end-text))
          ::unsatisfiable

          (str/blank? start-text)
          (let [suffix-length (Long/parseLong end-text)]
            (if (pos? suffix-length)
              {:start (max 0 (- size suffix-length))
               :end (dec size)}
              ::unsatisfiable))

          :else
          (let [start (Long/parseLong start-text)
                requested-end (if (str/blank? end-text)
                                (dec size)
                                (Long/parseLong end-text))]
            (if (or (>= start size) (> start requested-end))
              ::unsatisfiable
              {:start start :end (min requested-end (dec size))})))
        (catch NumberFormatException _
          ::unsatisfiable))
      ::unsatisfiable)))

(defn- common-headers [{:keys [size mime-type etag last-modified]}]
  (let [formatted-date (format-http-date last-modified)]
    (cond-> {"Accept-Ranges" "bytes"
             "X-Content-Type-Options" "nosniff"}
      mime-type (assoc "Content-Type" mime-type)
      etag (assoc "ETag" (quote-etag etag))
      formatted-date (assoc "Last-Modified" formatted-date)
      (number? size) (assoc "Content-Length" (str size)))))

(defn- range-headers [headers size start end]
  (let [length (inc (- end start))]
    (merge headers
           {"Content-Length" (str length)
            "Content-Range" (format "bytes %d-%d/%d" start end size)})))

(defn- ranged-response
  [resource request full-body range-body]
  (let [size (:size resource)
        method (:request-method request)
        parsed-range (parse-byte-range
                      (when (= method :get)
                        (response/get-header request "Range"))
                      size)
        headers (common-headers resource)
        head? (= :head method)]
    (cond
      (= parsed-range ::unsatisfiable)
      {:status 416
       :headers (assoc headers
                       "Content-Range" (str "bytes */" size)
                       "Content-Length" "0")
       :body nil}

      parsed-range
      (let [{:keys [start end]} parsed-range]
        {:status 206
         :headers (range-headers headers size start end)
         :body (when-not head? (range-body start end))})

      :else
      {:status 200
       :headers headers
       :body (when-not head? (full-body))})))

(defn ranged-file-response
  "Serve a file with single-range and HEAD support. Resource keys are :file,
   :size, :mime-type, :etag, and optional :last-modified."
  [{:keys [file] :as resource} request]
  (ranged-response
   resource
   request
   (constantly file)
   (fn [start end]
     (let [input (FileInputStream. (io/as-file file))]
       (.position (.getChannel input) start)
       (-> (BoundedInputStream/builder)
           (.setInputStream input)
           (.setMaxCount (inc (- end start)))
           (.get))))))

(defn byte-array-response
  "Serve byte-array content with the same single-range and HEAD behavior used
   for files."
  [bytes {:keys [mime-type etag last-modified]} request]
  (let [size (alength ^bytes bytes)
        resource {:size size
                  :mime-type mime-type
                  :etag etag
                  :last-modified last-modified}]
    (ranged-response
     resource
     request
     #(ByteArrayInputStream. bytes)
     (fn [start end]
       (ByteArrayInputStream. bytes (int start) (int (inc (- end start))))))))
