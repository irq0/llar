(ns llar.http
  (:require
   [llar.appconfig :refer [appconfig]]
   [llar.blobstore :as blobstore]
   [llar.converter :as converter]
   [llar.commands :refer [html2text] :as commands]
   [llar.regex :as regex-collection]
   [slingshot.slingshot :refer [throw+ try+]]
   [clj-http.client :as http]
   [clojure.spec.alpha :as s]
   [clojure.set :as clojure-set]
   [hickory.core :as hick]
   [hickory.select :as hick-s]
   [hickory.render :as hick-r]
   [hickory.zip :as hick-z]
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [org.bovinegenius [exploding-fish :as uri]]
   [java-time.api :as time]))

;; 🖖 HTTP Fetch utility

(defonce ^:dynamic *domain-blocklist* (atom #{}))

(def +http-user-agent+
  {:bot "Mozilla/5.0 (compatible); Googlebot/2.1; +http://www.google.com/bot.html)"
   :browser "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36"
   :default (format "Mozilla/5.0 (compatible; llar/%s; +http://llar.dev)" (get-in appconfig [:version :version]))})

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
    (log/infof "blocklist: loaded %s domains from %s"
               (count blocklist)
               public-blocklists)))

(defn update-domain-blocklist! []
  (reset! *domain-blocklist* (fetch-domain-blocklists)))

(defn extract-http-title
  [parsed-html]
  (or
   (some-> (hick-s/select (hick-s/child
                           (hick-s/tag :title))
                          parsed-html)
           first :content first string/trim)
   (some-> (hick-s/select (hick-s/class :page-title) parsed-html)
           first :content first string/trim)
   (some-> (hick-s/select (hick-s/child (hick-s/tag :header) (hick-s/tag :h1)) parsed-html)
           first :content first)))

(defn extract-http-timestamp
  [resp]
  (let [{:keys [headers]} resp]
    (try+
     (or (converter/parse-http-ts (get headers :last-modified))
         (converter/parse-http-ts (get headers :date)))
     (catch Object _
       (time/zoned-date-time)))))

(defmacro swallow-exceptions [& body]
  `(try+ ~@body (catch Object e#)))

(def unparsable-urls (atom []))

(defn sensible-uri-or-nil [uri]
  (when (and (uri/scheme uri) (uri/host uri)) uri))

(defn remove-url-garbage [uri]
  (cond-> uri
    ;; What not to filter:
    ;; (uri/path uri)
    ;; (uri/path
    ;;  (string/replace (uri/path uri) #"[^a-zA-Z0-9\.\-_~!&'\(\)*+,;=:@/]" ""))
    ;; Why? unicode chars are actually allowed in url paths

    (uri/path uri)
    (uri/path
     (string/replace (uri/path uri) #"['\"]+$" ""))

    (= (uri/scheme-relative uri) "//")
    (uri/scheme-relative "/")

    (and (uri/host uri) (nil? (uri/scheme uri)))
    (uri/scheme "https")))

(defn- parse-url [s]
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

(defn- parse-href [s]
  (let [decoded (or (swallow-exceptions (java.net.URLDecoder/decode s)) s)]
    (when-let [url (or (swallow-exceptions (parse-url decoded))
                       (swallow-exceptions (parse-url (str "/" decoded))))]
      url)))

(defn- append-path-if-not-exist [url]
  (if (nil? (uri/path url))
    (uri/path url "/")
    url))

(defn absolutify-url
  ([raw-href]
   (absolutify-url raw-href nil))
  ([raw-href
    raw-base-url]
   {:pre [(s/valid? (s/or :string string? :url :irq0/url) raw-href)
          (s/valid? (s/or :none nil? :string string? :url :irq0/url) raw-base-url)]
    :post [(s/valid? (s/or :absolute :irq0/absolute-url
                           :fragment (s/and :irq0/url
                                            #(string/starts-with? (str %) "#"))) %)]}
   (let [url (parse-href raw-href)
         scheme (uri/scheme url)]
     (cond
       ;; do not touch data / mailto urls!
       (contains? #{"data" "mailto"} scheme)
       (uri/uri raw-href)

       ;; try to leave fragment only uris alone - used for in-page navigation
       ;; (footnotes, headings) in the feed reader's rendered output
       (string/starts-with? raw-href "#")
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
           (uri/uri (uri/resolve-uri base-url (str url)))))))))

(defn get-base-url
  [url]
  {:pre [(s/valid? :irq0/url url)]
   :post [(s/valid? :irq0/absolute-url %)]}
  (-> url
      (uri/query nil)
      (uri/fragment nil)
      (append-path-if-not-exist))
  (uri/path url "/"))

(defn get-base-url-with-path
  [url]
  {:pre [(s/valid? :irq0/url url)]
   :post [(s/valid? :irq0/absolute-url %)]}
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
            (#{"img.shields.io" "badge.fury.io"} (uri/host url))
            (= (uri/path url) "/#"))
      (str url)
      (try+
       (let [content-hash (blobstore/add-from-url! url)
             blobstore-url (str "/blob/" content-hash)]
         blobstore-url)
       (catch Object _
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

(defn simplify [root]
  (let [zipper (hick-z/hickory-zip root)]
    (loop [loc zipper]
      (if (zip/end? loc)
        (zip/root loc)
        (recur
         (zip/next
          (let [{:keys [tag type _attrs _content] :as node} (zip/node loc)]
            (if (and (= type :element)
                     (contains? #{:div :span} tag))
              (zip/replace loc (assoc node :attrs nil))
              loc))))))))

(defn raw-sanitize [raw-html]
  (commands/sanitize raw-html))

(defn raw-readability [raw-html url]
  (commands/readability raw-html url))

(defn sanitize
  [root
   & {:keys [remove-css?]
      :or {remove-css? false}}]
  (let [zipper (hick-z/hickory-zip root)
        url-attribs {:a :href
                     :img :src}
        remove-by-attrs {:data-app-hidden "true"  ;; spiegel online
                         :class "lazytrigger"  ;; spiegel online
                         :data-component "AffiliateBox"} ;; spiegel online

        remove-tags #{:script :noscript :button}]
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
                                (assoc :attrs {:note "cleared by llar html sanitizer"}))))

                (and (= tag :a) (some-> attrs :href (#(or (string/starts-with? % "mailto:")
                                                          (string/starts-with? % "data:")))))
                loc

                (= tag :font)
                (zip/edit loc
                          (fn [node]
                            (assoc node :attrs nil)))

                (and remove-css? (= tag :link) (= (:rel attrs) "stylesheet"))
                (zip/edit loc
                          (fn [node]
                            (assoc node :attrs {:orig-href (:href attrs)
                                                :note "cleared by llar html sanitizer"})))

                (and remove-css? (string? (:style attrs)))
                (zip/edit loc
                          (fn [node]
                            (-> node
                                (assoc-in [:attrs :style] nil))))

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
                            in-blocklist (contains? @*domain-blocklist* host)]
                        in-blocklist)
                      (catch Object _
                        (log/debug "html sanitizer: swallowing exception during sanitize uri: "
                                   (:throwable &throw-context) attrs))))
                (zip/edit loc
                          (fn [node]
                            (-> node
                                (assoc :content [])
                                (assoc :attrs {:display "none"
                                               (keyword (str "orig-" (name (get url-attribs tag))))
                                               (str (get attrs (get url-attribs tag)))
                                               :note "cleared by llar html sanitizer"}))))
                :else
                loc)))
            (recur (zip/next loc))))))))

(defn http-resp-throw [ex context]
  (let [{:keys [body headers status reason-phrase]} ex]
    (cond
      (#{400 401 402 403 404 405 406 410} status)
      (let [message (html2text body :tool :for-exceptions)]
        (log/warnf "client error probably due to broken request (%s): body:%s context:%s"
                   status message context)
        (throw+ (merge {:type ::request-error
                        :code status
                        :message (or message reason-phrase)}
                       context)))
      (#{500 501 502 503 504} status)
      (let [message (html2text body  :tool :for-exceptions)]
        (log/warnf "server Error (%s): %s %s" status headers body)
        (throw+ (merge {:type ::server-error-retry-later
                        :code status
                        :message (or message reason-phrase)})
                context))
      (#{408 429} status)
      (do
        (log/warnf "client Error (overloaded?) (%s): %s" status reason-phrase)
        (throw+ (merge {:type ::client-error-retry-later
                        :code status
                        :reason (:reason-phrase ex)
                        :message (html2text body :tool :html2text)}
                       context))))))

(defmacro with-http-exception-handler [throw-extra & body]
  `(try
     (do ~@body)
     (catch clojure.lang.ExceptionInfo e#
       (cond
         (= (:type (ex-data e#)) :clj-http.client/unexceptional-status)
         (http-resp-throw (ex-data e#) (merge ~throw-extra))
         :else
         (throw+ (merge {:type ::unexpected-error} ~throw-extra))))

     (catch java.net.UnknownHostException e#
       (log/error e# "host resolution error" ~throw-extra)
       (throw+ (merge {:type ::server-error-retry-later} ~throw-extra)))

     (catch javax.net.ssl.SSLHandshakeException e#
       (throw+ (merge {:type ::server-error-retry-later
                       :message (str "SSL Handshake failed: " (ex-message e#))}
                      ~throw-extra)))

     (catch java.net.ConnectException e#
       (throw+ (merge {:type ::server-error-retry-later
                       :message (str "Connection refused (gone?): " (ex-message e#))}
                      ~throw-extra)))

     (catch java.security.cert.CertificateExpiredException e#
       (throw+ (merge {:type ::server-error-retry-later
                       :message (str "Certificate expired: " (ex-message e#))}
                      ~throw-extra)))

     (catch javax.net.ssl.SSLPeerUnverifiedException e#
       (throw+ (merge {:type ::server-error-retry-later
                       :message (str "SSL Peer verification error: " (ex-message e#))}
                      ~throw-extra)))

     (catch java.net.SocketException e#
       (throw+ (merge {:type ::server-error-retry-later
                       :message (str "SocketException: " (ex-message e#))}
                      ~throw-extra)))

     (catch java.io.EOFException e#
       (throw+ (merge {:type ::server-error-retry-later
                       :message (str "EOF Exception: " (ex-message e#))}
                      ~throw-extra)))

     (catch java.lang.Throwable e#
       (log/error e# "unexpected error: " ~throw-extra)
       (throw+ (merge {:type ::unexpected-error} ~throw-extra)))))

(defn fetch
  "Generic HTTP fetcher"
  [url & {:keys [user-agent conditionals sanitize? remove-css? simplify? blobify? absolutify-urls?]
          :or {user-agent :default
               conditionals {}
               sanitize? true
               blobify? true
               remove-css? false
               absolutify-urls? true
               simplify? false}}]
  (let [url (parse-url url)
        headers (cond-> {:user-agent (resolve-user-agent user-agent)}
                  (:etag conditionals) (assoc :if-none-match (:etag conditionals))
                  (:last-modified conditionals) (assoc :if-modified-since (:last-modified conditionals)))
        base-url (get-base-url-with-path url)
        response (with-http-exception-handler {:headers headers :url url}
                   (http/get (str url)
                             {:headers headers
                              :decode-cookies false
                              :cookie-policy :none}))

        html (when (and (#{200 206} (:status response))
                        (some? (:body response)))
               (if sanitize? (raw-sanitize (:body response))
                   (:body response)))
        parsed-html (when (some? html) (cond-> (-> html
                                                   hick/parse hick/as-hickory)
                                         absolutify-urls? (absolutify-links-in-hick base-url)
                                         sanitize? (sanitize :remove-css? remove-css?)
                                         simplify? (simplify)
                                         blobify? (blobify)))]
    (log/debugf "HTTP GET: %s req-headers:%s status:%s body:%sB cond:%s" url headers (:status response) (count (get response :body)) (select-keys (:headers response) [:etag :last-modified]))
    (if (= 304 (:status response))
      {:raw response
       :status :not-modified
       :conditional-tokens conditionals}
      {:raw response
       :status :ok
       :body (when parsed-html (hick-r/hickory-to-html parsed-html))
       :conditional-tokens (select-keys (:headers response) [:etag :last-modified])
       :summary {:ts (extract-http-timestamp response)
                 :title (extract-http-title parsed-html)}
       :hickory parsed-html})))
