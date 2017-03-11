(ns infowarss.core
  (:require
   [clj-rome.reader :as rome]
   [ring.util.response :as response]
   [digest]
   [clj-http.client :as http]
   [ring.middleware params keyword-params json stacktrace lint reload basic-authentication]
   [hickory.core :as hick]
   [hickory.select :as hick-s]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [clojure.tools.logging :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojurewerkz.serialism.core :as s]
   [clojure.core.async :as async :refer [>!! <!! >! <! alts!! alts! put! go thread]]
   [compojure.core :refer :all]
   [liberator.core :refer [resource defresource]]
   [compojure.route :as route]
    ))

(comment {:feed-items [items]})





;; Helpers

(defprotocol FetchSource
  "Protocol to work with data sources"
  (fetch-source [src]))

(defrecord Http [url title postproc])
(defrecord Feed [url title postproc])

(defn- fetch-http-generic [url]
  "Generic HTTP fetcher"
  (let [response (http/get url)
        parsed-html (-> response :body hick/parse hick/as-hickory)]
    [{:meta {:source {:app "infowar-core/fetch-http"
                      :address url}
             :fetch-ts (time/now)
             :version 0}
      :http (select-keys response [:headers :status])
      :raw (:body response)
      :hash (digest/sha-256 (:body response))
      :summary {:from url
                :title (-> (hick-s/select (hick-s/child
                                            (hick-s/tag :title))
                             parsed-html)
                         first
                         :content
                         first
                         string/trim)}}]))


(defn- extract-feed-authors [authors]
  (for [{:keys [name email]} authors]
    (str
      (when-not (nil? name)
        name)
      (when-not (nil? email)
        (str " <" email ">")))))

(defn- html2text [html]
  html)

(defn- extract-feed-content [contents]
  (let [by-type (into {}
                  (for [{:keys [type value]} contents]
                    [(keyword type) value]))]
    (condp #(contains? %2 %1) by-type
      :text (assoc by-type :default (:text by-type))
      :html (assoc by-type :default (html2text (:html by-type)))
      (assoc by-type :default (first (vals by-type))))))


(defrecord HttpItem [source meta summary hash http raw])
(defrecord FeedItem [source meta summary hash feed-entry feed])

(extend-protocol FetchSource
  Http
  (fetch-source [src]
    (map map->HttpItem (fetch-http-generic (:url src))))
  Feed
  (fetch-source [src]
    (map map->FeedItem
      (let [http (-> (fetch-http-generic (:url src)) first)
            feed (rome/build-feed (-> http :raw))]
        (for [e (:entries feed)]
          (let [authors (extract-feed-authors (:authors e))
                content (extract-feed-content (:contents e))]
            (-> http
              (dissoc :raw :parsed-html :http)
              (assoc :feed-entry (select-keys e [:description :link
                                                 :published-date :title]))
              (assoc-in [:feed-entry :authors] authors)
              (assoc-in [:feed-entry :contents] content)
              (assoc :feed (select-keys feed [:title :language :link :description
                                              :encoding :published-date :feed-type]))
              (assoc :hash (digest/sha-256 (str (:title e) (:default content))))
              (assoc :summary {:from authors
                               :title (:title e)}))))))))

(defonce ^:dynamic *db* (atom nil))

(defn known-feed-hashes []
  (->> @*db*
    (:feed-items)
    (mapv :hash)
    (set)))

(defprotocol ProcessItem
  "Protocol to work with items (fetched by a source)"
  (process-item [item] "Process new item")
  (duplicate? [item] "Already processed?"))

(extend-protocol ProcessItem
  FeedItem
  (duplicate? [item]
    (contains? (known-feed-hashes) (:hash item)))
  (process-item [item]
    (swap! *db* (fn [old]
                  (assoc old :feed-items
                    (conj (:feed-items old) item))))))


(defn process [mixed-items]
  ;; Each vector may contain multiple item types.
  ;; -> Group them by type and call the store method
  (let [by-type (group-by type mixed-items)]
    (log/infof "Processing %d items with keys: %s"
      (count mixed-items) (keys by-type))
    (doseq [[type items] by-type]
      (log/infof "Processing type %s items" type)
      (doseq [item items]
        (if-not (duplicate? item)
          (do
            (log/infof "Processing item \"%s\""
              (get-in item [:summary :title]))
            (process-item item))
          (log/infof "Skipping item \"%s\": duplicate"
            (get-in item [:summary :title])))))))

(defn processor [chans]
  "Run in background to process incomming data"
  (go
    (while true
      (let [[v ch] (alts! chans)]
        (log/infof "Recived from chan %s" ch)
        (process v)))))


(defonce ^:dynamic *subscriptions*
  (atom [(Feed. "http://irq0.org/news/index.atom" "irq0.org feed" nil)
         (Feed. "http://blog.fefe.de/rss.xml?html" "fefe" nil)]))

(comment
  "Example Feed to use with handle-fetch"
  (Feed. "http://irq0.org/news/index.atom" "irq0.org feed" nil)
  (Feed. "http://blog.fefe.de/rss.xml?html" "fefe" nil))

(defn- postproc [result source]
  (if (:postproc source)
    (apply (:postproc source) result)
    result))

(defn handle-fetch [source dst-chan]
  "Fetch sources and push results to channel"
  (log/info "Fetching: " (:title source))
  (try+
    (let [result (map #(assoc % :source source)
                   (fetch-source source))]
      (>!! dst-chan result))
    (catch [:status 404] {:keys [request-time headers body]}
      (log/error "NOT FOUND 404: " request-time headers body))
    (catch Object _
      (log/error (:throwable &throw-context) "unexpected error"))))


(defonce ^:dynamic *api-version* 3)

(defonce ^:dynamic *api-config*
  {:email "infowarss@irq0.org"
   :pass  "foo"})

(defn fever-api-key []
  (let [{:keys [email pass]} *api-config*]
    (digest/md5 (str email ":" pass))))

(defn fever-auth [key]
  (= (string/lower-case (str key)) (fever-api-key)))

(defn fever-auth-error []
  (-> (response/response "Auth error!")
    (response/status 401)))

(defn wrap-fever-auth [handler]
  "Check fever api key and set :fever-auth in request
   Overwrite response with (fever-auth-error) if unauthorized"
  (fn [request]
    #spy/p request
    (let [api-key ((request :params) :api_key)
          auth? (fever-auth api-key)
          request (assoc-in request [:fever-auth] auth?)
          response (handler request)]
      (if auth?
        #spy/p response
        (fever-auth-error)))))

;; (defresource parameter [txt]
;;   :available-media-types ["text/plain"]
;;   :handle-ok (fn [_] (format "The text is %s" txt)))


;;(defresource feed-item [id])

(defn- hash-to-id [h]
  (Long/parseUnsignedLong (subs h 0 8) 16))


(defn- id-of [str]
  (hash-to-id (digest/sha-256 str)))


(defn subscriptions []
  (into {} (map (fn [sub]
            (let [id (id-of (str (:url sub) (:title sub)))]
              [id {"id" id
                   "created_at" "1970-01-01T23:42:00.000000Z",
                   "feed_id" id,
                   "title" (:title sub),
                   "feed_url" (:url sub),
                   "site_url" (:url sub),}]))
    @*subscriptions*)))

(defn empty-if-nil [x]
  (if (nil? x) "" x))

(defn all-entries []
  (into (sorted-map) (map (fn [e]
                            (let [{:keys [source hash meta summary feed feed-entry]} e
                                  id (hash-to-id hash)
                                  feed-id (id-of (str (:url source) (:title source)))]
                              [id {"id" id,
                                   "feed_id" feed-id,
                                   "title" (get feed :title),
                                   "url" (get feed-entry :link),
                                   "author" (string/join "," (get feed-entry :authors)),
                                   "content" (empty-if-nil (get-in feed-entry [:contents :html])),
                                   "summary" (get-in feed-entry [:description :value]),
                                   "published" "2013-02-03T01:00:19.000000Z",
                                   "created_at" (tc/to-string (get meta :fetch-ts))}]))
                       (:feed-items @*db*))))

(defn unread-entries []
  (let [es (for [e (:feed-items @*db*)] (hash-to-id (:hash e)))]
    ;; todo: filter es
    es))

(defresource list-subscriptions
  :available-media-types ["application/json"]
  :handle-ok (fn [_] (vals (subscriptions))))

(defresource subscription [id]
  :available-media-types ["application/json"]
  :exists? (fn [_]
             (let [e (get (subscriptions) id)]
               (if-not (nil? e)
                 {::entry e})))
  :existed? (fn [_] (nil? (get (subscriptions) id ::sentinel)))
  :handle-ok ::entry)

(defresource feed [id]
  :available-media-types ["application/json"]
  :exists? (fn [_]
             (let [e (get (subscriptions) id)]
               (if-not (nil? e)
                 {::entry (dissoc e "feed_id" "created_at")})))
  :existed? (fn [_] (nil? (get (subscriptions) id ::sentinel)))
  :handle-ok ::entry)

(defresource list-entries [page per-page]
  :available-media-types ["application/json"]
  :handle-ok (fn [_]
               (let [vs (vals (all-entries))
                     to (min (* page per-page) (count vs))
                     from (- to per-page)
                     from (if (neg? from) 0 from)]

                 (log/infof "list entires %d %d"
                 from to )
                 (if (or (nil? from) (nil? to))
                   vs
                   (subvec (vec vs) from to)))))


(defresource list-unread-entries
  :available-media-types ["application/json"]
  :handle-ok (fn [_] (unread-entries)))


(defresource feed-entires [feed-id]
  :available-media-types ["application/json"]
  :exists? (fn [_]
             (let [e (filter #(= (get % "feed_id") feed-id)
                       (vals (all-entries)))]
               (if-not (nil? e)
                 {::entry e})))
  :existed? (fn [_] (nil? (get (all-entries) feed-id ::sentinel)))
  :handle-ok ::entry)

(defresource entry [id]
  :available-media-types ["application/json"]
  :exists? (fn [_]
             (let [e (get (all-entries) id)]
               (if-not (nil? e)
                 {::entry e})))
  :existed? (fn [_] (nil? (get (all-entries) id ::sentinel)))
  :handle-ok ::entry)


(defn not-implemented []
  (->
    (response/response "Not Implemented")
    (response/status 500)))

(defn empty-response []
  (response/response "[]"))


(defroutes feedbin-api
    (context "/v2" []
      (GET "/authentication.json" _ (response/response nil))

      ;; Subscriptions
      ;; https://github.com/feedbin/feedbin-api/blob/master/content/subscriptions.md
      (ANY "/subscriptions.json" [] list-subscriptions)
      (ANY "/subscriptions/:id{[0-9]+}.json" [id] (subscription (Long. id)))

      ;; Feeds
      ;; https://github.com/feedbin/feedbin-api/blob/master/content/feeds.md
      ;; Get Feed
      (ANY "/feeds/1.json" [id] (feed 1773230085))
      (ANY "/feeds/2.json" [id] (feed 2074534389))
      (ANY "/feeds/:id{[0-9]+}.json" [id] (feed (Long. id)))
      (ANY "/feeds/:feed-id{[0-9]+}/entries.json" [feed-id] (feed-entires feed-id))


      ;; Entries
      ;; https://github.com/feedbin/feedbin-api/blob/master/content/entries.md
      (ANY "/entries.json" [page per_page]
        (list-entries (Long. page) (Long. per_page)))
      (ANY "/entries/:id{[0-9]+}.json" [id] (entry id))

      ;; Unread Entries
      ;; https://github.com/feedbin/feedbin-api/blob/master/content/unread-entries.md
      (ANY "/unread_entries.json" [] list-unread-entries)


      ;; Starred Entries
      ;; https://github.com/feedbin/feedbin-api/blob/master/content/starred-entries.md
      (ANY "/starred_entries.json" _ (empty-response))

      ;; Taggings
      ;;https://github.com/feedbin/feedbin-api/blob/master/content/taggings.md
      (ANY "/taggings.json" _ (empty-response))
      (ANY "/taggings/:id{0-9]+}.json" [id] (not-implemented))

      ;; Later...

      ;; Saved Searches
      ;; https://github.com/feedbin/feedbin-api/blob/master/content/saved-searches.md
      (ANY "/saved_searches.json" _ (not-implemented))
      (ANY "/saved_searches/:id{[0-9]+}.json" [id] (not-implemented))

      ;; Recently Read Entries
      (ANY "/recently_read_entries.json" _ (not-implemented))

      ;; Updates Entries
      ;; https://github.com/feedbin/feedbin-api/blob/master/content/updated-entries.md
      (ANY "/updated_entries.json" _ (not-implemented))


      (ANY "/subscriptions.json" _ (not-implemented))
      (ANY "/subscriptions.json" _ (not-implemented))
      (ANY "/subscriptions.json" _ (not-implemented))
        )
  (GET "/dump" request (str request))
  (GET "/" [] "<h1>Hello World</h1>")
  (route/not-found "<h1>Page not found</h1>"))


(defonce ^:dynamic *users* (atom {"foo" {:username "foo"
                                     :password "bar"}}))



(defn api-authenticated? [user pass]
  (let [entry (@*users* user)]
    (and
      (= user (:username entry))
      (= pass (:password entry)))))

(def app
  (->
    ;; fever-api-handler
    ;;  wrap-json-response
    ;;  wrap-fever-auth
    feedbin-api
    (ring.middleware.basic-authentication/wrap-basic-authentication api-authenticated? "API")
    ring.middleware.reload/wrap-reload
    ring.middleware.json/wrap-json-response
    ring.middleware.keyword-params/wrap-keyword-params
    ring.middleware.params/wrap-params
;      ring.middleware.stacktrace/wrap-stacktrace
      ring.middleware.lint/wrap-lint))


(comment
  (defonce jetty (run-jetty #'app {:port 8765 :join? false}))
  (.start jetty)
  (.stop jetty))



(comment

  (reset! *db* nil)
  (def chan (async/chan 100))
  (handle-fetch (Feed. "http://irq0.org/news/index.atom" "irq0.org feed" nil) chan)
  (handle-fetch (Feed. "http://blog.fefe.de/rss.xml?html" "fefe" nil) chan)
  (def irq (<!! chan))
  (def fefe (<!! chan))

  (process irq)
  (process fefe))
