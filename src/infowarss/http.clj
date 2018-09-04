(ns infowarss.http
  (:require
   [infowarss.blobstore :as blobstore]
   [slingshot.slingshot :refer [throw+ try+]]
   [clj-http.client :as http]
   [hickory.core :as hick]
   [hickory.select :as hick-s]
   [hickory.render :as hick-r]
   [hickory.zip :as hick-z]
   [taoensso.timbre :as log]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [clj-time.format :as tf]
   [clj-time.core :as time]))

;; Infowarss HTTP Fetch utility

(defn parse-http-ts [ts]
  (when-not (nil? ts)
    (tf/parse (tf/formatter "EEE, dd MMM yyyy HH:mm:ss z") ts)))


(defn extract-http-title
  [parsed-html]
  (some-> (hick-s/select (hick-s/child
                      (hick-s/tag :title))
       parsed-html)
    first
    :content
    first
    string/trim))

(defn extract-http-timestamp
  [resp]
  (let [{:keys [headers]} resp
        parser (partial
                 tf/parse (tf/formatter "EEE, dd MMM yyyy HH:mm:ss z"))]
    (try+
      (or (parser (get headers "Last-Modified"))
        (parser (get headers "Date")))
      (catch Object _
        (time/now)))))

(defn absolutify-url [url base-url]
  (let [base-url  (if (instance? java.net.URL base-url)
                    (str base-url)
                    base-url)
        url  (if (instance? java.net.URL url)
               (str url)
               url)

        abs-url (cond
                  (and (string? url) (string? base-url) (string/starts-with? url "/") (string/ends-with? base-url "/"))
                  (str base-url (subs url 1))
                  (and (string? url) (string? base-url) (string/ends-with? base-url "/") (not (string/starts-with? url "http")))
                  (str base-url url)
                  (and (string? url) (string/starts-with? url "/"))
                  (str base-url url)
                  :default
                  url)]
    ;; (log/info "abs url:" url base-url abs-url)
    abs-url))

(defn parse-img-srcset [str]
  (when (string? str)
    (map #(string/split % #"\s") (string/split str #"\s?,\s?"))))


(defn unparse-img-srcset [parsed]
  (when (coll? parsed)
    (string/join ", " (map #(string/join " " %) parsed))))

(defn edit-img-tag [base-url loc]
  (zip/edit loc update-in [:attrs]
    (fn [attrs]
      (let [{:keys [src srcset]} attrs]
        (-> attrs
          (assoc :src (absolutify-url src base-url))
          (assoc :srcset (some->> (parse-img-srcset srcset)
                           (map (fn [[url descr]] [(absolutify-url url base-url) descr]))
                           unparse-img-srcset)))))))


(defn absolutify-links-in-hick [root base-url]
  (let [zipper (hick-z/hickory-zip root)
        edit-tag (fn [tag loc]
                   (case tag
                     :a (zip/edit loc update-in [:attrs :href] absolutify-url base-url)
                     :img (edit-img-tag base-url loc)
                     loc))]
    (loop [loc zipper]
      (if (zip/end? loc)
        (zip/root loc)
        (let [{:keys [tag type content attrs]} (zip/node loc)]
          (if (= type :element)
            (recur
              (zip/next
                (edit-tag tag loc)))
            (recur (zip/next loc))))))))

(defn- maybe-extract-url
  [s]
  (try+
    (io/as-url s)
    (catch java.net.MalformedURLException _
      nil)))

(defn get-base-url [u]
  (let [url (maybe-extract-url u)]
    (try+
      (java.net.URL. (.getProtocol url) (.getHost url) (.getPort url) "/")
      (catch Object _
        (log/warn "http fetcher: failed to get base url for:" u)
        url))))

(defn blobify-store! [url]
  (try+
    (let [h (blobstore/add-from-url! url)]
      (str "/blob/" h))
    (catch [:type :infowarss.blobstore/perm-fail] _
      (log/warn "Blobstore failed permanently - keeping orig item" url)
      (str url))))


(defn blobify-image [loc]
  (zip/edit loc update-in [:attrs]
    (fn [attrs]
      (let [{:keys [src srcset]} attrs]
        (if (or (string/blank? src) (string/starts-with? src "/blob/"))
          attrs
          (-> attrs
            (assoc :src (blobify-store! src))
            (assoc :orig-src src)
            (assoc :srcset
              (some->> (parse-img-srcset srcset)
                (map (fn [[url descr]]
                       (if (or (string/blank? url) (string/starts-with? url "/blob/"))
                         [url descr]
                         [(blobify-store! url) descr])))

                  unparse-img-srcset))))))))



(defn blobify [root]
  (let [zipper (hick-z/hickory-zip root)
        edit-tag (fn [tag loc]
                   (case tag
                     :img (blobify-image loc)
                     loc))]

    (loop [loc zipper]
      (if (zip/end? loc)
        (zip/root loc)
        (let [{:keys [tag type content attrs]} (zip/node loc)]
          (if (= type :element)
            (recur
              (zip/next (edit-tag tag loc)))
            (recur (zip/next loc))))))))



(defn fetch
  "Generic HTTP fetcher"
  [url]
  (try+
    (let [response (http/get (str url) {:headers {:user-agent "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"}
                                  :decode-cookies false
                                  :cookie-policy :none})
          base-url (get-base-url url)
          parsed-html (-> response
                        :body
                        hick/parse hick/as-hickory
                        (absolutify-links-in-hick base-url)
                        blobify)]

      (log/debugf "Fetched HTTP: %s -> %s bytes body" url (count (get response :body)))
      {:raw response
       :body (hick-r/hickory-to-html parsed-html)
       :summary {:ts (extract-http-timestamp response)
                 :title (extract-http-title parsed-html)}
       :hickory parsed-html})

    (catch (contains? #{400 401 402 403 404 405 406 410} (get % :status))
        {:keys [headers body status]}
      (log/errorf "Client error probably due to broken request (%s): %s %s"
        status headers body)
      (throw+ {:type ::request-error}))

    (catch (contains? #{500 501 502 503 504} (get % :status))
        {:keys [headers body status] :as orig}
      (log/errorf "Server Error (%s): %s %s" status headers body)
      (throw+ {:type ::server-error-retry-later}))

    (catch [:status 408]
        {:keys [headers body status]}
      (log/errorf "Client Error (%s): %s %s" status headers body)
      (throw+ {:type :client-error-retry-later}))

    (catch Object _
      (log/error "Unexpected error: " (:throwable &throw-context) url)
      (throw+ {:type ::unexpected-error :url url}))))
