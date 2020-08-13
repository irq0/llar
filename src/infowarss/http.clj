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
   [clj-time.core :as time]))


;; Infowarss HTTP Fetch utility


(def +http-user-agent+
  {:bot "Mozilla/5.0 (compatible); Googlebot/2.1; +http://www.google.com/bot.html)"
   :browser "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36"
   :default "Mozilla/5.0 (compatible; 🖖/0.1; +http://irq0.org)"})

(defn resolve-user-agent [kw-or-s]
  (cond (string? kw-or-s) kw-or-s
        (keyword? kw-or-s) (get +http-user-agent+ kw-or-s)
        :else (throw+ {:type ::invalid-user-agent
                       :msg "Use string or keyword"
                       :offending-value kw-or-s
                       :offending-type (type kw-or-s)
                       :keywords (keys +http-user-agent+)})))

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

;; gruber: liberal url regex to match web urls
;; https://gist.github.com/gruber/8891611
(def url-regex #"(?i)\b((?:https?:(?:/{1,3}|[a-z0-9%])|[a-z0-9.\-]+[.](?:com|net|org|edu|gov|mil|aero|asia|biz|cat|coop|info|int|jobs|mobi|museum|name|post|pro|tel|travel|xxx|ac|ad|ae|af|ag|ai|al|am|an|ao|aq|ar|as|at|au|aw|ax|az|ba|bb|bd|be|bf|bg|bh|bi|bj|bm|bn|bo|br|bs|bt|bv|bw|by|bz|ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|co|cr|cs|cu|cv|cx|cy|cz|dd|de|dj|dk|dm|do|dz|ec|ee|eg|eh|er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|ge|gf|gg|gh|gi|gl|gm|gn|gp|gq|gr|gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|hu|id|ie|il|im|in|io|iq|ir|is|it|je|jm|jo|jp|ke|kg|kh|ki|km|kn|kp|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|lu|lv|ly|ma|mc|md|me|mg|mh|mk|ml|mm|mn|mo|mp|mq|mr|ms|mt|mu|mv|mw|mx|my|mz|na|nc|ne|nf|ng|ni|nl|no|np|nr|nu|nz|om|pa|pe|pf|pg|ph|pk|pl|pm|pn|pr|ps|pt|pw|py|qa|re|ro|rs|ru|rw|sa|sb|sc|sd|se|sg|sh|si|sj|Ja|sk|sl|sm|sn|so|sr|ss|st|su|sv|sx|sy|sz|tc|td|tf|tg|th|tj|tk|tl|tm|tn|to|tp|tr|tt|tv|tw|tz|ua|ug|uk|us|uy|uz|va|vc|ve|vg|vi|vn|vu|wf|ws|ye|yt|yu|za|zm|zw)/)(?:[^\s()<>{}\[\]]+|\([^\s()]*?\([^\s()]+\)[^\s()]*?\)|\([^\s]+?\))+(?:\([^\s()]*?\([^\s()]+\)[^\s()]*?\)|\([^\s]+?\)|[^\s`!()\[\]{};:'\".,<>?«»“”‘’])|(?:(?<!@)[a-z0-9]+(?:[.\-][a-z0-9]+)*[.](?:com|net|org|edu|gov|mil|aero|asia|biz|cat|coop|info|int|jobs|mobi|museum|name|post|pro|tel|travel|xxx|ac|ad|ae|af|ag|ai|al|am|an|ao|aq|ar|as|at|au|aw|ax|az|ba|bb|bd|be|bf|bg|bh|bi|bj|bm|bn|bo|br|bs|bt|bv|bw|by|bz|ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|co|cr|cs|cu|cv|cx|cy|cz|dd|de|dj|dk|dm|do|dz|ec|ee|eg|eh|er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|ge|gf|gg|gh|gi|gl|gm|gn|gp|gq|gr|gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|hu|id|ie|il|im|in|io|iq|ir|is|it|je|jm|jo|jp|ke|kg|kh|ki|km|kn|kp|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|lu|lv|ly|ma|mc|md|me|mg|mh|mk|ml|mm|mn|mo|mp|mq|mr|ms|mt|mu|mv|mw|mx|my|mz|na|nc|ne|nf|ng|ni|nl|no|np|nr|nu|nz|om|pa|pe|pf|pg|ph|pk|pl|pm|pn|pr|ps|pt|pw|py|qa|re|ro|rs|ru|rw|sa|sb|sc|sd|se|sg|sh|si|sj|Ja|sk|sl|sm|sn|so|sr|ss|st|su|sv|sx|sy|sz|tc|td|tf|tg|th|tj|tk|tl|tm|tn|to|tp|tr|tt|tv|tw|tz|ua|ug|uk|us|uy|uz|va|vc|ve|vg|vi|vn|vu|wf|ws|ye|yt|yu|za|zm|zw)\b/?(?!@)))")

(defmacro swallow-exceptions [& body]
  `(try+ ~@body (catch Object e#)))

(def unparsable-urls (atom []))

(defn parse-url [s]
  (if-let [url (or (swallow-exceptions (urly/url-like s))
                   (swallow-exceptions (urly/url-like
                                        (second (re-find url-regex s))))
                   (swallow-exceptions (urly/url-like (io/as-url s))))]
    url
    (do
      (swap! unparsable-urls conj s)
      (throw+ {:type ::absolutify-impossible
               :reason ::unparsable-url
               :url s} nil))))

(defn parse-href [s]
  (when-let [url (or (swallow-exceptions (parse-url s))
                     (swallow-exceptions (parse-url (str "/" s))))]
    url))

(s/defn absolutify-url :- (s/maybe schema/URLType)
  [raw-href :- (s/cond-pre schema/NotEmptyStr schema/URLType)
   raw-base-url :- (s/maybe (s/cond-pre s/Str schema/URLType))]
  (let [url (parse-href raw-href)]
    (cond
      (and (nil? url) (string? raw-href) (string/starts-with? raw-href "mailto:"))
      (io/as-url raw-href)

      (urly/absolute? url)
      url

      :else
      (let [base-url (parse-url raw-base-url)]
        (cond
          (nil? base-url)
          (throw+ {:type ::absolutify-impossible
                   :reason ::url-not-absolue-base-url-nil
                   :raw-href raw-href :raw-base-url raw-base-url
                   :url url :base-url base-url
                   :msg "relative url requires absolute base-url"})

          (urly/relative? base-url)
          (throw+ {:type ::absolutify-impossible
                   :reason ::base-url-relative
                   :raw-href raw-href :raw-base-url raw-base-url
                   :url url :base-url base-url
                   :msg "base-url must be absolute"})

          (and (urly/relative? url) (urly/absolute? base-url))
          (urly/url-like (urly/absolutize (str url) base-url)))))))

(s/defn get-base-url :- (s/constrained clojurewerkz.urly.UrlLike urly/absolute? "Absolute URL")
  [url :- clojurewerkz.urly.UrlLike]
  (-> url
      urly/without-query-string-and-fragment
      (.mutatePath "")))

(s/defn get-base-url-with-path :- (s/constrained clojurewerkz.urly.UrlLike urly/absolute? "Absolute URL")
  [url :- clojurewerkz.urly.UrlLike]
  (-> url
      urly/without-query-string-and-fragment))

(defn parse-img-srcset [str]
  (when (and (string? str) (not (string/blank? str)))
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
        edit-tag (fn [tag _type _content attrs loc]
                   (try
                     (cond
                       (and (= tag :a) (string? (:href attrs)))
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
            (= (urly/path-of url) "/")
            (= (urly/path-of url) "/#"))
      (str url)
      (try+
       (let [content-hash (blobstore/add-from-url! url)
             blobstore-url (str "/blob/" content-hash)]
         blobstore-url)
       (catch Object e
         (log/info e "blobify failed" url)
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
        (let [{:keys [tag type attrs]} (zip/node loc)]
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
                      (let [url (parse-url (get attrs (get url-attribs tag)))
                            host (urly/host-of url)
                            in-blacklist (contains? @domain-blacklist host)]
                        in-blacklist)
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
                                               :note "cleared by infowarss html sanitizer"}))))
                :else
                loc)))
            (recur (zip/next loc))))))))

(defn fetch
  "Generic HTTP fetcher"
  [url & {:keys [user-agent]
          :or {user-agent :default}}]
  (let [url (parse-url url)
        base-url (get-base-url-with-path url)]
    (try+
     (let [response (http/get (str url)
                              {:headers {:user-agent (resolve-user-agent user-agent)}
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
                   status headers body user-agent)
       (throw+ {:type ::request-error}))

     (catch (contains? #{500 501 502 503 504} (get % :status))
            {:keys [headers body status] :as orig}
       (log/errorf "Server Error (%s): %s %s" status headers body)
       (throw+ {:type ::server-error-retry-later}))

     (catch [:status 408]
            {:keys [headers body status]}
       (log/errorf "Client Error (%s): %s %s" status headers body)
       (throw+ {:type :client-error-retry-later}))

     (catch java.net.UnknownHostException ex
       (log/error ex "Host resolution error" url)
       (throw+ {:type ::server-error-retry-later}))

     (catch Object _
       (log/error "Unexpected error: " (:throwable &throw-context) url)
       (throw+ {:type ::unexpected-error :url url :base-url base-url})))))
