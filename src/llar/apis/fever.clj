(ns llar.apis.fever
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.core :refer [ANY GET routes]]
   [compojure.route :as route]
   [digest]
   [java-time.api :as time]
   [llar.apis.blob :as blob-api]
   [llar.appconfig :as appconfig]
   [llar.config :as config]
   [llar.db.sql :as sql]
   [llar.persistency :as persistency])
  (:import
   [java.nio.charset StandardCharsets]
   [org.jsoup Jsoup]
   [org.jsoup.nodes Entities]
   [org.jsoup.safety Safelist]))

(def api-version 3)
(def page-size 50)
(def flat-group-id 1)
(def queue-feed-id 2147483647)
(def ^:private max-logged-param-chars 160)

(defn- datastore [db]
  (or (:datasource db) db))

(defn- config-with-defaults [fever-config]
  (merge appconfig/fever-defaults fever-config))

(defn expected-api-key [username password]
  (digest/md5 (str username ":" password)))

(defn- credential-password [{:keys [credentials]}]
  (let [credential (appconfig/credentials credentials)]
    (if (map? credential) (:password credential) credential)))

(defn authenticated? [fever-config api-key]
  (let [password (credential-password fever-config)]
    (and (string? password)
         (string? api-key)
         (.equalsIgnoreCase api-key (expected-api-key (:username fever-config) password)))))

(defn- selected-source-keys [fever-config]
  (let [source-tag (:source-tag fever-config)]
    (->> (config/get-sources)
         (keep (fn [[source-key source]]
                 (when (contains? (:tags source) source-tag)
                   (name source-key))))
         set)))

(defn selected-sources [db fever-config]
  (let [keys (selected-source-keys fever-config)]
    (->> (sql/fever-sources (datastore db))
         (filter #(contains? keys (:key %)))
         vec)))

(defn- source-url [{:keys [key data]}]
  (or (get data :url)
      (str "llar://source/" key)))

(defn- epoch-millis [timestamp]
  (if timestamp
    (time/to-millis-from-epoch timestamp)
    0))

(defn- seconds [timestamp]
  (quot (epoch-millis timestamp) 1000))

(defn- feed-response [source]
  {:id (:id source)
   :favicon_id 0
   :title (:name source)
   :url (source-url source)
   :site_url (source-url source)
   :is_spark 0
   :last_updated_on_time (seconds (:updated_ts source))})

(def queue-feed
  {:id queue-feed-id
   :favicon_id 0
   :title "Reading Queue"
   :url "llar://reading-queue"
   :site_url "llar://reading-queue"
   :is_spark 0
   :last_updated_on_time 0})

(defn- fever-feeds [sources]
  (conj (mapv feed-response sources) queue-feed))

(defn- feeds-groups [sources]
  [{:group_id flat-group-id
    :feed_ids (string/join "," (map :id sources))}])

(defn- byte-truncate [s max-bytes]
  (if (<= (alength (.getBytes s StandardCharsets/UTF_8)) max-bytes)
    s
    (loop [low 0 high (count s)]
      (if (>= low high)
        (str (subs s 0 low) "\n<p><em>Content truncated by LLAR.</em></p>")
        (let [mid (quot (inc (+ low high)) 2)
              size (alength (.getBytes (subs s 0 mid) StandardCharsets/UTF_8))]
          (if (<= size max-bytes)
            (recur mid high)
            (recur low (dec mid))))))))

(defn- absolute-blob-url [base-url value]
  (if (and (string? base-url) (string/starts-with? value "/blob/"))
    (str (string/replace base-url #"/+$" "") value)
    value))

(defn rewrite-blob-urls [html base-url]
  (if (string/blank? base-url)
    html
    (let [document (Jsoup/parseBodyFragment html)]
      (doseq [element (.select document "[src], [poster]")
              attribute ["src" "poster"]
              :when (.hasAttr element attribute)]
        (.attr element attribute
               (absolute-blob-url base-url (.attr element attribute))))
      (-> document .body .html))))

(defn sanitize-content
  ([content html? base-url max-bytes]
   (sanitize-content content html? base-url max-bytes nil))
  ([content html? base-url max-bytes fever-base-url]
   (let [html (if html?
                (or content "")
                (str "<p>" (Entities/escape (or content "")) "</p>"))
         html (rewrite-blob-urls html fever-base-url)
         url-protocols (into-array String ["http" "https" "data"])
         safelist (doto (Safelist/relaxed)
                    (.addTags (into-array String ["video" "audio" "source"]))
                    (.addAttributes "a" (into-array String ["target"]))
                    (.addAttributes "video" (into-array String ["src" "poster" "controls" "preload" "width" "height"]))
                    (.addAttributes "audio" (into-array String ["src" "controls" "preload"]))
                    (.addAttributes "source" (into-array String ["src" "type" "media"]))
                    (.addProtocols "img" "src" url-protocols)
                    (.addProtocols "video" "src" url-protocols)
                    (.addProtocols "video" "poster" url-protocols)
                    (.addProtocols "audio" "src" url-protocols)
                    (.addProtocols "source" "src" url-protocols))
         cleaned (Jsoup/clean html (or base-url "") safelist)]
     (-> cleaned
         (byte-truncate max-bytes)
         (Jsoup/parseBodyFragment (or base-url ""))
         .body
         .html))))

(defn- item-response [item fever-config]
  {:id (:id item)
   :feed_id (:feed_id item)
   :title (:title item)
   :author (:author item)
   :html (sanitize-content (:content item)
                           (:content_is_html item)
                           (:url item)
                           (:max-content-bytes fever-config)
                           (:base-url fever-config))
   :url (or (:url item) "")
   :is_saved (if (:is_saved item) 1 0)
   :is_read (if (:is_read item) 1 0)
   :created_on_time (seconds (:ts item))})

(defn- parse-positive-long [value]
  (when (and (string? value) (re-matches #"[0-9]+" value))
    (let [n (parse-long value)]
      (when (pos? n) n))))

(defn- parse-id-list [value]
  (when (string? value)
    (let [ids (mapv parse-positive-long (string/split value #","))]
      (when (and (<= (count ids) page-size) (every? some? ids)) ids))))

(defn- working-set-bounds [fever-config now]
  {:unread-after (time/minus now (time/days (:initial-days fever-config)))
   :read-after (time/minus now (time/days (:recent-read-days fever-config)))})

(defn- query-source-ids [source-ids]
  (if (seq source-ids) source-ids [-1]))

(defn- item-query [source-ids fever-config now params]
  (let [since-id (parse-positive-long (get params "since_id"))
        raw-max-id (get params "max_id")
        max-id (parse-positive-long raw-max-id)
        with-ids (parse-id-list (get params "with_ids"))]
    (when (and (contains? params "with_ids") (nil? with-ids))
      (throw (ex-info "Invalid with_ids" {:type ::bad-request})))
    (cond-> (merge {:source-ids (query-source-ids source-ids)
                    :queue-feed-id queue-feed-id
                    :descending? (and (nil? since-id) (nil? with-ids))
                    :limit page-size}
                   (working-set-bounds fever-config now))
      since-id (assoc :since-id since-id)
      (and raw-max-id (not= raw-max-id "0") max-id) (assoc :max-id max-id)
      with-ids (assoc :with-ids with-ids))))

(defn- state-query [source-ids fever-config now state]
  (merge {:source-ids (query-source-ids source-ids)
          :state-query state
          :queue-state? (= "1" state)}
         (working-set-bounds fever-config now)))

(defn- comma-ids [rows]
  (string/join "," (map :id rows)))

(defn- selected-item [db source-ids item-id]
  (sql/fever-item-selected (datastore db)
                           {:source-ids (query-source-ids source-ids)
                            :item-id item-id}))

(defn- apply-item-action! [db source-ids params]
  (let [item-id (parse-positive-long (get params "id"))
        action (get params "as")
        item (when item-id (selected-item db source-ids item-id))]
    (when-not (:selected item)
      (throw (ex-info "Unknown item" {:type ::bad-request})))
    (case action
      "read" (persistency/item-remove-tags! db item-id [:unread])
      "saved" (persistency/item-set-tags! db item-id [:saved])
      "unsaved" (persistency/item-remove-tags!
                 db item-id
                 (cond-> [:saved :in-progress]
                   (:bookmark item) (conj :unread)))
      (throw (ex-info "Unsupported item action" {:type ::bad-request})))))

(defn- before-timestamp [value]
  (when-let [epoch (parse-positive-long value)]
    (java.time.Instant/ofEpochSecond epoch)))

(defn- apply-bulk-action! [db source-ids params]
  (let [mark (get params "mark")
        id (or (parse-long (or (get params "id") "")) 0)
        before (before-timestamp (get params "before"))]
    (when-not (and (= "read" (get params "as")) before)
      (throw (ex-info "Invalid bulk action" {:type ::bad-request})))
    (case mark
      "feed" (when-not (and (pos? id) (contains? (set source-ids) id))
               (throw (ex-info "Unknown feed" {:type ::bad-request})))
      "group" (when-not (#{0 flat-group-id} id)
                (throw (ex-info "Unknown group" {:type ::bad-request})))
      (throw (ex-info "Unsupported bulk action" {:type ::bad-request})))
    (sql/fever-mark-read (datastore db)
                         (cond-> {:source-ids (query-source-ids source-ids)
                                  :before before}
                           (= mark "feed") (assoc :feed-id id)))))

(defn- base-response [sources]
  {:api_version api-version
   :auth 1
   :last_refreshed_on_time (or (some->> sources
                                        (keep :updated_ts)
                                        seq
                                        (apply max-key epoch-millis)
                                        seconds)
                               0)})

(defn- bounded-param [value]
  (let [value (str value)]
    (if (<= (count value) max-logged-param-chars)
      value
      (str (subs value 0 max-logged-param-chars)
           "... [" (count value) " chars]"))))

(defn- request-summary [request]
  {:method (:request-method request)
   :uri (:uri request)
   :remote-addr (:remote-addr request)
   :user-agent (some-> (get-in request [:headers "user-agent"]) bounded-param)
   :content-type (get-in request [:headers "content-type"])
   :content-length (get-in request [:headers "content-length"])
   :params (into (sorted-map)
                 (map (fn [[key value]]
                        [key (if (= "api_key" key)
                               "<redacted>"
                               (bounded-param value))]))
                 (:params request))})

(defn- response-value-summary [value]
  (cond
    (string? value) {:type :string
                     :chars (count value)
                     :bytes (alength (.getBytes value StandardCharsets/UTF_8))}
    (coll? value) {:type :collection :count (count value)}
    :else value))

(defn- response-summary [response elapsed-ms]
  {:status (:status response)
   :elapsed-ms elapsed-ms
   :body (when (map? (:body response))
           (into (sorted-map)
                 (map (fn [[key value]] [key (response-value-summary value)]))
                 (:body response)))})

(defmulti response-part
  "Build one independently requested part of a Fever response."
  (fn [operation _context] operation))

(defmethod response-part :groups [_ {:keys [sources]}]
  (let [feeds (fever-feeds sources)]
    (log/debug "[fever] groups: returned=1")
    {:groups [{:id flat-group-id :title "LLAR"}]
     :feeds_groups (feeds-groups feeds)}))

(defmethod response-part :feeds [_ {:keys [sources]}]
  (let [feeds (fever-feeds sources)]
    (log/debugf "[fever] feeds: returned=%d" (count feeds))
    {:feeds feeds
     :feeds_groups (feeds-groups feeds)}))

(defmethod response-part :favicons [_ _]
  (log/debug "[fever] favicons: unsupported, returned=0")
  {:favicons []})

(defmethod response-part :items
  [_ {:keys [db fever-config source-ids now params]}]
  (let [items (mapv #(item-response % fever-config)
                    (sql/fever-items (datastore db)
                                     (item-query source-ids fever-config now params)))
        total (:total (sql/fever-total-items
                       (datastore db)
                       (merge {:source-ids (query-source-ids source-ids)}
                              (working-set-bounds fever-config now))))]
    (log/debugf "[fever] items: returned=%d total=%d" (count items) total)
    {:items items :total_items total}))

(defn- state-response-part [{:keys [db fever-config source-ids now]} state key]
  (let [rows (sql/fever-item-state-ids
              (datastore db)
              (state-query source-ids fever-config now state))]
    (log/debugf "[fever] %s: returned=%d" (name key) (count rows))
    {key (comma-ids rows)}))

(defmethod response-part :unread-item-ids [_ context]
  (state-response-part context "0" :unread_item_ids))

(defmethod response-part :saved-item-ids [_ context]
  (state-response-part context "1" :saved_item_ids))

(defn- requested-operations [params]
  (cond-> []
    (contains? params "groups") (conj :groups)
    (contains? params "feeds") (conj :feeds)
    (contains? params "favicons") (conj :favicons)
    (contains? params "items") (conj :items)
    (or (contains? params "unread_item_ids")
        (= "read" (get params "as")))
    (conj :unread-item-ids)
    (or (contains? params "saved_item_ids")
        (#{"read" "saved" "unsaved"} (get params "as")))
    (conj :saved-item-ids)))

(defn- apply-request-action! [db source-ids params]
  (when (contains? params "mark")
    (if (= "item" (get params "mark"))
      (apply-item-action! db source-ids params)
      (apply-bulk-action! db source-ids params))))

(defn fever-response [db fever-config request]
  (let [fever-config (config-with-defaults fever-config)
        params (:params request)]
    (if-not (authenticated? fever-config (get params "api_key"))
      {:status 200 :body {:api_version api-version :auth 0}}
      (let [sources (selected-sources db fever-config)
            source-ids (mapv :id sources)
            context {:db db
                     :fever-config fever-config
                     :params params
                     :sources sources
                     :source-ids source-ids
                     :now (time/zoned-date-time)}]
        (apply-request-action! db source-ids params)
        {:status 200
         :body (reduce (fn [response operation]
                         (merge response (response-part operation context)))
                       (base-response sources)
                       (requested-operations params))}))))

(defn handler [db fever-config]
  (routes
   (GET "/blob/:hash" [hash]
     (log/debugf "[fever] blob %s" hash)
     (blob-api/response hash))
   (ANY "/" request
     (let [request-id (str (random-uuid))
           started-at (System/nanoTime)]
       (log/debugf "[fever] request %s: %s" request-id (request-summary request))
       (try
         (let [response (fever-response db fever-config request)
               elapsed-ms (quot (- (System/nanoTime) started-at) 1000000)]
           (log/debugf "[fever] response %s: %s"
                       request-id (response-summary response elapsed-ms))
           response)
         (catch clojure.lang.ExceptionInfo e
           (if (= ::bad-request (:type (ex-data e)))
             (let [params (:params request)
                   response {:status 400
                             :body {:api_version api-version
                                    :auth 1
                                    :error (ex-message e)}}
                   elapsed-ms (quot (- (System/nanoTime) started-at) 1000000)]
               (log/warnf "[fever] Rejected request %s: %s (mark=%s as=%s)"
                          request-id
                          (ex-message e)
                          (get params "mark")
                          (get params "as"))
               (log/debugf "[fever] response %s: %s"
                           request-id (response-summary response elapsed-ms))
               response)
             (throw e))))))
   (route/not-found {:status 404 :body {:error "Not found"}})))
