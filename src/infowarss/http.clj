(ns infowarss.http
  (:require
   [infowarss.blobstore :as blobstore]
   [slingshot.slingshot :refer [throw+ try+]]
   [clj-http.client :as http]
   [schema.core :as s]
   [infowarss.schema :as schema]
   [infowarss.converter :as conv]
   [clojure.set :as clojure-set]
   [hickory.core :as hick]
   [hickory.select :as hick-s]
   [hickory.render :as hick-r]
   [hickory.zip :as hick-z]
   [taoensso.timbre :as log]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [mount.core :refer [defstate]]
   [clojurewerkz.urly.core :as urly]
   [clj-time.core :as time])
  (:import [java.net URI URL]))


;; Infowarss HTTP Fetch utility

(def +http-user-agent+
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36")

(def public-blacklists
  ["https://raw.githubusercontent.com/austinheap/sophos-xg-block-lists/master/adguard.txt"
   "https://raw.githubusercontent.com/austinheap/sophos-xg-block-lists/master/easylist.txt"])

(defn get-blacklist [url]
  (-> url
    http/get
    :body
    (string/split #"\n")
    (into #{})))

(defn fetch-domain-blocklists []
  (->> public-blacklists
    (map get-blacklist)
    (apply clojure-set/union)
    (into #{})))

(defstate domain-blacklist
  :start (atom (fetch-domain-blocklists)))


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
  (let [{:keys [headers]} resp]
    (try+
      (or (conv/parse-http-ts (get headers "Last-Modified"))
        (conv/parse-http-ts (get headers "Date")))
      (catch Object _
        (time/now)))))

(defn- parse-url-that-may-be-just-be-a-filename [raw]
  (try+
    (let [raw (-> raw
                str
                string/trim
                (string/replace-first #"^\"" ""))
          url (urly/url-like (or raw ""))]
      url)

    (catch java.lang.NoSuchMethodError e
      (if-not (string/starts-with? raw "/")
        (urly/url-like (str "/" raw))
        (throw+ {:type ::borken-url :url raw})))))

(def unparsable-urls (atom []))

(s/defn absolutify-url :- (s/maybe schema/URLType)
  [raw-url :- (s/maybe (s/cond-pre s/Str schema/URLType))
   raw-base-url :- (s/cond-pre s/Str schema/URLType)]
  (let [url (parse-url-that-may-be-just-be-a-filename raw-url)
        base-url (urly/url-like raw-base-url)]

    (cond
      (and (nil? url) (string? raw-url) (string/starts-with? raw-url "mailto:"))
      (io/as-url raw-url)

      (nil? raw-url)
      nil

      (nil? url)
      (do
        (swap! unparsable-urls conj raw-url)
        (throw+ {:type ::unparsable-url
                 :url raw-url
                 :base-url raw-base-url}))

      (urly/absolute? url)
      url

      (and (urly/relative? url) (urly/absolute? base-url))
      (urly/url-like (urly/absolutize (str url) base-url))

      (and (urly/relative? url) (urly/relative? base-url))
      (throw+ {:type ::absolutify-impossible
               :url url
               :base-url base-url
               :msg "Both urls relative. Can't absolutify"})

      :default
      url)))

(s/defn get-base-url :- (s/constrained clojurewerkz.urly.UrlLike urly/absolute? "Absolute URL")
  [url :- clojurewerkz.urly.UrlLike]
  (-> url
    urly/without-query-string-and-fragment
    (.mutatePath "")))

(defn parse-img-srcset [str]
  (when (string? str)
    (map #(string/split % #"\s")
      (string/split
        (java.net.URLDecoder/decode str "UTF-8")
        #"(?<=\d+[wx]),\s*"))))


(defn unparse-img-srcset [parsed]
  (when (coll? parsed)
      (->>
        parsed
        (map (fn [[url descr]]
               (str
                 url
                 " "
                 descr)))
        (string/join ", "))))

(defn edit-img-tag [base-url loc]
  (zip/edit loc update-in [:attrs]
    (fn [attrs]
      (let [{:keys [src srcset]} attrs]
        (-> attrs
          (assoc :src (str (absolutify-url src base-url)))
          (assoc :srcset (some->> (parse-img-srcset srcset)
                           (map (fn [[url descr]] [(str (absolutify-url url base-url)) descr]))
                           unparse-img-srcset)))))))


(defn absolutify-links-in-hick [root base-url]
  (let [zipper (hick-z/hickory-zip root)
        edit-tag (fn [tag loc]
                   (try+
                     (case tag
                       :a (zip/edit loc update-in
                            [:attrs :href]
                            #(str (absolutify-url % base-url)))
                       :img (edit-img-tag base-url loc)
                       loc)
                     (catch java.lang.Object e
                       (log/error e "Absolutify error loc:"
                         (select-keys (zip/node loc) [:tag :type :attrs]))
                       loc)))]
    (loop [loc zipper]
      (if (zip/end? loc)
        (zip/root loc)
        (let [{:keys [tag type content attrs]} (zip/node loc)]
          (if (= type :element)
            (recur
              (zip/next
                (edit-tag tag loc)))
            (recur (zip/next loc))))))))


(defn try-blobify-url! [url]
  (if (nil? url)
    url
    (let [url (urly/url-like url)]
      (if (or (= (str url) (str (get-base-url url)))
            (= (urly/path-of url) "/")
            (= (urly/path-of url) "/#"))
        (str url)
        (try+
          (let [content-hash (blobstore/add-from-url! url)
                blobstore-url (str "/blob/" content-hash)]
            blobstore-url)
          (catch Object _
            (str url)))))))

(defn blobify-image [loc]
  (zip/edit loc update-in [:attrs]
    (fn [attrs]
      (let [{:keys [src srcset]} attrs]
        (if (or (string/blank? src) (string/starts-with? src "/blob/"))
          attrs
          (-> attrs
            (assoc :src (try-blobify-url! src))
            (assoc :orig-src src)
            (assoc :orig-srcset srcset)
            (assoc :srcset
              (some->> (parse-img-srcset srcset)
                (map (fn [[url descr]]
                       (if (or (string/blank? url) (string/starts-with? url "/blob/"))
                         [url descr]
                         [(try-blobify-url! url) descr])))

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

(defn sanitize
  [root
   & {:keys [remove-css?]
      :or {remove-css? false}}]
  (let [zipper (hick-z/hickory-zip root)
        url-attribs {:a :href
                     :img :src}
        remove-tags #{:script :noscript}]
    (loop [loc zipper]
      (if (zip/end? loc)
        (zip/root loc)
        (let [{:keys [tag type content attrs]} (zip/node loc)]
          (if (= type :element)
            (recur
             (zip/next
              (cond
                (contains? remove-tags tag)
                (zip/edit loc
                          (fn [node]
                            (-> node
                                (assoc :content [])
                                (assoc :attrs {:note "cleared by infowarss html sanitizer"}))))

                (and (= tag :a) (some-> attrs :href (string/starts-with? "mailto:")))
                loc

                (and remove-css? (= tag :link) (= (:rel attrs) "stylesheet"))
                (zip/edit loc
                          (fn [node]
                            (assoc node :attrs {:orig-href (:href attrs)
                                                :note "cleared by infowarss html sanitizer"})))

                (and remove-css? (string? (:style attrs)))
                (zip/edit loc
                          (fn [node]
                            (-> node
                                (assoc-in [:attrs :style] nil)
                                (assoc-in [:attrs :orig-style]
                                          (:style attrs)))))

                (and (contains? #{:a :img} tag)
                     (try+
                      (let [url (urly/url-like (get attrs (get url-attribs tag)))
                            host (urly/host-of url)
                            in-blacklist (contains? @domain-blacklist host)]
                        in-blacklist)

                      (catch Object _
                        (log/error "SANITIZE: Swallowing exception during sanitize uri: "
                                   (:throwable &throw-context) attrs))))
                (zip/edit loc
                          (fn [node]
                            (-> node
                                (assoc :content [])
                                (assoc :attrs {:display "none"
                                               (keyword (str "orig-" (name (get url-attribs tag))))
                                               (str (get attrs (get url-attribs tag)))
                                               :note "cleared by infowarss html sanitizer"}))))
                :default
                loc)))
            (recur (zip/next loc))))))))

(s/defn fetch :- s/Any
  "Generic HTTP fetcher"
  [url :- schema/URLType]
  (let [url (urly/url-like url)
        base-url (get-base-url url)]
    (try+
      (let [response (http/get (str url)
                       {:headers {:user-agent +http-user-agent+}
                        :decode-cookies false
                        :cookie-policy :none})
            parsed-html (-> response
                          :body
                          hick/parse hick/as-hickory
                          (absolutify-links-in-hick base-url)
                          sanitize
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
        (throw+ {:type ::unexpected-error :url url :base-url base-url})))))
