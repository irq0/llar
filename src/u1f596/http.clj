(ns u1f596.http
  (:require
   [u1f596.blobstore :as blobstore]
   [u1f596.converter :as converter]
   [u1f596.regex :as regex-collection]
   [slingshot.slingshot :refer [throw+ try+]]
   [clj-http.client :as http]
   [schema.core :as s]
   [clojure.set :refer [intersection]]
   [u1f596.schema :as schema]
   [u1f596.converter :as conv]
   [clojure.set :as clojure-set]
   [hickory.core :as hick]
   [hickory.select :as hick-s]
   [hickory.render :as hick-r]
   [hickory.zip :as hick-z]
   [taoensso.timbre :as log]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [mount.core :refer [defstate]]
   [org.bovinegenius [exploding-fish :as uri]]
   [java-time :as time]))


;; 🖖 HTTP Fetch utility


(def +http-user-agent+
  {:bot "Mozilla/5.0 (compatible); Googlebot/2.1; +http://www.google.com/bot.html)"
   :browser "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36"
   :default "Mozilla/5.0 (compatible; u1f596/0.1; +http://irq0.org)"})

(defn resolve-user-agent [kw-or-s]
  (cond (string? kw-or-s) kw-or-s
        (keyword? kw-or-s) (get +http-user-agent+ kw-or-s)
        :else (throw+ {:type ::invalid-user-agent
                       :msg "Use string or keyword"
                       :offending-value kw-or-s
                       :offending-type (type kw-or-s)
                       :keywords (keys +http-user-agent+)})))

(def public-blocklists
  ["https://raw.githubusercontent.com/austinheap/sophos-xg-block-lists/master/adguard.txt"
   "https://raw.githubusercontent.com/austinheap/sophos-xg-block-lists/master/easylist.txt"])

(defn get-blocklist [url]
  (-> url
      http/get
      :body
      (string/split #"\n")
      (into #{})))

(defn fetch-domain-blocklists []
  (let [blocklist (->> public-blocklists
                       (map get-blocklist)
                       (apply clojure-set/union)
                       (into #{}))]
    (log/infof "Blocklist: Loaded %s domains from %s"
               (count blocklist)
               public-blocklists)))

(defstate domain-blocklist
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
       (time/zoned-date-time)))))

(defmacro swallow-exceptions [& body]
  `(try+ ~@body (catch Object e#)))

(def unparsable-urls (atom []))

(defn human-host-identifier [url]
  ;; hack because some database entries got a strange serialization
  (let [url (if (map? url) (uri/map->uri url) url)
        host (uri/host url)]
    (try+
     (let [guava-host (com.google.common.net.InternetDomainName/from host)
           site (.topPrivateDomain guava-host)]
       (.name site))
     (catch Object _
       (str host)))))

(defn sensible-uri-or-nil [uri]
  (when (and (uri/scheme uri) (uri/host uri)) uri))

(defn remove-url-garbage [uri]
  (cond-> uri
    (uri/path uri)
    (uri/path
     (string/replace (uri/path uri) #"[^a-zA-Z0-9\.\-_~!&'\(\)*+,;=:@/]" ""))

    (= (uri/scheme-relative uri) "//")
    (uri/scheme-relative "/")

    (nil? (uri/scheme uri))
    (uri/scheme "https")))

(defn parse-url [s]
  (if-let [url (or (sensible-uri-or-nil (uri/uri s))
                   (sensible-uri-or-nil (uri/uri
                                         (second (re-find regex-collection/url s))))
                   (uri/uri s))]
    (remove-url-garbage url)
    (do
      (swap! unparsable-urls conj s)
      (throw+ {:type ::absolutify-impossible
               :reason ::unparsable-url
               :url s} nil))))

(defn parse-href [s]
  (when-let [url (or (swallow-exceptions (parse-url s))
                     (swallow-exceptions (parse-url (str "/" s))))]
    url))

(defn- append-path-if-not-exist [url]
  (if (nil? (uri/path url))
    (uri/path url "/")
    url))

(s/defn absolutify-url :- (s/maybe schema/AbsolutifiedURL)
  [raw-href :- (s/cond-pre schema/NotEmptyStr schema/URLRelaxed)
   raw-base-url :- (s/maybe (s/cond-pre s/Str schema/URLRelaxed))]
  (let [url (parse-href raw-href)
        scheme (uri/scheme url)]
    (cond
      ;; do not touch data / mailto urls!
      (contains? #{"data" "mailto"} scheme)
      (uri/uri raw-href)

      (uri/absolute? url)
      (append-path-if-not-exist url)

      :else
      (let [base-url (parse-url raw-base-url)]
        (cond
          (or (nil? base-url) (and (string? raw-base-url) (string/blank? raw-base-url)))
          (throw+ {:type ::absolutify-impossible
                   :reason ::base-url-useless
                   :raw-href raw-href :raw-base-url raw-base-url
                   :url url :base-url base-url
                   :msg "relative url requires absolute base-url"})

          (not (uri/absolute? base-url))
          (throw+ {:type ::absolutify-impossible
                   :reason ::base-url-relative
                   :raw-href raw-href :raw-base-url raw-base-url
                   :url url :base-url base-url
                   :msg "base-url must be absolute"})

          (nil? (uri/host url))
          (try+
           (let [resolved (-> (uri/resolve-path base-url url)
                              (uri/query (uri/query url))
                              (uri/fragment (uri/fragment url)))]

             (if (uri/absolute-path? resolved)
               resolved
               (update resolved :path #(str "/" %))))
           (catch Object _
             (throw+ {:type ::absolutify-broken
                      :raw-href raw-href
                      :raw-base-url raw-base-url
                      :url url
                      :scheme scheme
                      :base-url base-url})))

          :else
          (uri/uri (uri/resolve-uri base-url (str url))))))))

(s/defn get-base-url :- (s/constrained schema/URLWithAbsPath uri/absolute? "Absolute URL")
  [url :- schema/URL]
  (uri/path url "/"))

(s/defn get-base-url-with-path :- (s/constrained schema/URLWithAbsPath uri/absolute? "Absolute URL")
  [url :- schema/URL]
  (-> url
      (uri/query nil)
      (uri/fragment nil)
      (append-path-if-not-exist)))

(defn parse-img-srcset [str]
  (when (and (string? str) (not (string/blank? str)))
    (map (fn [src]
           (let [pair (string/split src #"\s")]
             (if (= (count pair) 2)
               pair
               [(first pair) "1x"])))

         (string/split
          (java.net.URLDecoder/decode str "UTF-8")
          #"(?<=\d+[wx]|),\s*"))))

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
              (let [{:keys [src srcset]} attrs
                    parsed-srcset (parse-img-srcset srcset)]

                (cond-> attrs
                  (not (or (nil? src) (string/blank? src)))
                  (assoc :src (str (absolutify-url src base-url)))

                  (not (or (nil? srcset) (string/blank? srcset)))
                  (assoc :srcset
                         (some->> parsed-srcset
                                  (map (fn [[url descr]] [(str (absolutify-url url base-url)) descr]))
                                  unparse-img-srcset)))))))

(defn absolutify-links-in-hick [root base-url]
  (let [zipper (hick-z/hickory-zip root)
        edit-tag (fn [tag _type _content attrs loc]
                   (try
                     (cond
                       (and (= tag :a) (string? (:href attrs)) (not (string/blank? (:href attrs))))
                       (zip/edit loc update-in
                                 [:attrs :href]
                                 #(str (absolutify-url % base-url)))

                       (and (= tag :img) (or (string? (:src attrs))
                                             (string? (:srcset attrs))))
                       (edit-img-tag base-url loc)

                       :else
                       loc)
                     (catch Throwable th
                       (log/debug th "Absolutify error loc:"
                                  (merge {:base-url base-url}
                                         (select-keys (zip/node loc) [:tag :type :attrs])))
                       loc)))]
    (loop [loc zipper]
      (if (zip/end? loc)
        (zip/root loc)
        (let [{:keys [tag type content attrs]} (zip/node loc)]
          (if (= type :element)
            (recur
             (zip/next
              (edit-tag tag type content attrs loc)))
            (recur (zip/next loc))))))))

(defn try-blobify-url! [url]
  (let [url (parse-url url)]
    (if (or (nil? url)
            (= (uri/scheme url) "data")
            (= (uri/path url) "/")
            (= (uri/path url) "/#"))
      (str url)
      (try+
       (let [content-hash (blobstore/add-from-url! url)
             blobstore-url (str "/blob/" content-hash)]
         blobstore-url)
       (catch Object e
         (str url))))))

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
        (let [{:keys [tag type]} (zip/node loc)]
          (if (= type :element)
            (recur
             (zip/next (edit-tag tag loc)))
            (recur (zip/next loc))))))))

;; remove all nested div and span - hellishly expensive, lots of copying!
(defn simplify-expensive [root]
  (let [zipper (hick-z/hickory-zip root)]
    (loop [loc zipper]
      (if (zip/end? loc)
        (zip/root loc)
        (recur
          (let [{:keys [tag type attrs content] :as node} (zip/node loc)]
            (if (and (= type :element) (some? content)
                     (some #(contains? #{:div :span} (:tag %)) content))
              (zip/replace loc
                           ;; elevate any div/span content to this node
                           (let [new-content (mapcat (fn [cont]
                                                       (let [{:keys [tag type attrs content]} cont]
                                                         (if (and (= type :element)
                                                                  (some? content)
                                                                  (contains? #{:div :span} tag))
                                                           content
                                                           [cont])))
                                                     content)]
                             (assoc node :content new-content)))
              (zip/next loc))))))))

(defn simplify [root]
  (let [zipper (hick-z/hickory-zip root)]
    (loop [loc zipper]
      (if (zip/end? loc)
        (zip/root loc)
        (recur
         (zip/next
          (let [{:keys [tag type attrs content] :as node} (zip/node loc)]
            (if (and (= type :element)
                     (contains? #{:div :span} tag))
              (zip/replace loc (assoc node :attrs nil))
              loc))))))))

(defn sanitize
  [root
   & {:keys [remove-css?]
      :or {remove-css? false}}]
  (let [zipper (hick-z/hickory-zip root)
        url-attribs {:a :href
                     :img :src}
        remove-by-attrs {:data-app-hidden "true"  ;; spiegel online
                         :class "lazytrigger"  ;; spiegel online
                         :data-component "AffiliateBox" ;; spiegen online
                         }
        remove-tags #{:header :script :noscript :footer :button}]
    (loop [loc zipper]
      (if (zip/end? loc)
        (zip/root loc)
        (let [{:keys [tag type attrs]} (zip/node loc)]
          (if (= type :element)
            (recur
             (zip/next
              (cond
                (or (contains? remove-tags tag)
                    (some (fn [[k v]]
                            (when-let [remove-if-val (get remove-by-attrs k)]
                              (string/includes? (or v "")
                                                remove-if-val)))
                          attrs))
                (zip/edit loc
                          (fn [node]
                            (-> node
                                (assoc :content [])
                                (assoc :attrs {:note "cleared by u1f596 html sanitizer"}))))

                (and (= tag :a) (some-> attrs :href #(or (string/starts-with? %"mailto:")
                                                         (string/starts-with? %"data:"))))
                loc

                (= tag :font)
                (zip/edit loc
                          (fn [node]
                            (assoc node :attrs nil)))

                (and remove-css? (= tag :link) (= (:rel attrs) "stylesheet"))
                (zip/edit loc
                          (fn [node]
                            (assoc node :attrs {:orig-href (:href attrs)
                                                :note "cleared by u1f596 html sanitizer"})))

                (and remove-css? (string? (:style attrs)))
                (zip/edit loc
                          (fn [node]
                            (-> node
                                (assoc-in [:attrs :style] nil)
                                (assoc-in [:attrs :orig-style]
                                          (:style attrs)))))

                (and (= tag :img) (string/starts-with? (or (:srcset attrs) "") "data:"))
                (zip/edit loc
                          (fn [node]
                            (assoc-in node [:attrs :srcset] nil)))

                (= tag :iframe)
                (zip/edit loc
                          (fn [node]
                            (assoc-in node [:attrs :class] "embed-responsive")))


                (and (contains? #{:a :img} tag)
                     (contains? #{:href :src} attrs)
                     (try+
                      (let [url (parse-url (get attrs (get url-attribs tag)))
                            host (uri/host url)
                            in-blocklist (contains? @domain-blocklist host)]
                        in-blocklist)
                      (catch Object _
                        (log/debug "SANITIZE: Swallowing exception during sanitize uri: "
                                   (:throwable &throw-context) attrs))))
                (zip/edit loc
                          (fn [node]
                            (-> node
                                (assoc :content [])
                                (assoc :attrs {:display "none"
                                               (keyword (str "orig-" (name (get url-attribs tag))))
                                               (str (get attrs (get url-attribs tag)))
                                               :note "cleared by u1f596 html sanitizer"}))))
                :else
                loc)))
            (recur (zip/next loc))))))))

(defn fetch
  "Generic HTTP fetcher"
  [url & {:keys [user-agent sanitize? remove-css? simplify? blobify?]
          :or {user-agent :default
               sanitize? true
               blobify? true
               remove-css? false
               simplify? false}}]
  (let [url (parse-url url)
        base-url (get-base-url-with-path url)]
    (try+
     (let [response (http/get (str url)
                              {:headers {:user-agent (resolve-user-agent user-agent)}
                               :decode-cookies false
                               :cookie-policy :none})
           parsed-html (cond-> (-> response
                                   :body
                                   hick/parse hick/as-hickory
                                   (absolutify-links-in-hick base-url))
                         sanitize? (sanitize :remove-css? remove-css?)
                         simplify? (simplify)
                         blobify? (blobify))]
       (log/debugf "Fetched HTTP: %s -> %s bytes body" url (count (get response :body)))
       {:raw response
        :body (hick-r/hickory-to-html parsed-html)
        :summary {:ts (extract-http-timestamp response)
                  :title (extract-http-title parsed-html)}
        :hickory parsed-html})

     (catch (contains? #{400 401 402 403 404 405 406 410} (get % :status))
            {:keys [headers body status]}
       (log/warnf "Client error probably due to broken request (%s): %s %s"
                   status headers body user-agent)
       (throw+ {:type ::request-error
                :code status
                :url url
                :base-url base-url
                :message (converter/html2text body :tool :html2text)}))

     (catch (contains? #{500 501 502 503 504} (get % :status))
            {:keys [headers body status] :as orig}
       (log/warnf "Server Error (%s): %s %s" status headers body)
       (throw+ {:type ::server-error-retry-later
                :code status
                :url url
                :base-url base-url
                :message (converter/html2text body :tool :html2text)}))

     (catch [:status 408]
            {:keys [headers body status]}
       (log/warnf "Client Error (%s): %s %s" status headers body)
       (throw+ {:type :client-error-retry-later
                :code status
                :url url
                :base-url base-url
                :message (converter/html2text body :tool :html2text)}))

     (catch java.net.UnknownHostException ex
       (log/error ex "Host resolution error" url)
       (throw+ {:type ::server-error-retry-later
                :url url
                :base-url base-url}))

     (catch javax.net.ssl.SSLHandshakeException ex
       (throw+ {:type ::server-error-retry-later
                :url url
                :base-url base-url
                :message (str "SSL Handshake failed: " (ex-message ex))}))

     (catch java.net.ConnectException ex
       (throw+ {:type ::server-error-retry-later
                :url url
                :base-url base-url
                :message (str "Connection refused (gone?): " (ex-message ex))}))

     (catch java.security.cert.CertificateExpiredException ex
       (throw+ {:type ::server-error-retry-later
                :url url
                :base-url base-url
                :message (str "Certificate expired: " (ex-message ex))}))

     (catch javax.net.ssl.SSLPeerUnverifiedException ex
       (throw+ {:type ::server-error-retry-later
                :url url
                :base-url base-url
                :message (str "SSL Peer verification error: " (ex-message ex))}))

     (catch java.net.SocketException ex
       (throw+ {:type ::server-error-retry-later
                :url url
                :base-url base-url
                :message (str "SocketException: " (ex-message ex))}))

     (catch java.io.EOFException ex
       (throw+ {:type ::server-error-retry-later
                :url url
                :base-url base-url
                :message (str "EOF Exception: " (ex-message ex))}))

     (catch Object _
       (log/error "Unexpected error: " (:throwable &throw-context) url)
       (throw+ {:type ::unexpected-error
                :url url
                :base-url base-url})))))