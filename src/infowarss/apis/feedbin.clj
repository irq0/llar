(ns infowarss.apis.feedbin
  (:require
   [ring.util.response :as response]
   [digest]
   [ring.middleware params keyword-params json stacktrace lint reload basic-authentication]
   [clj-time.coerce :as tc]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [compojure.core :refer :all]
   [liberator.core :refer [resource defresource]]
   [compojure.route :as route]
   [ring.adapter.jetty :refer [run-jetty]]
   [clojure.tools.nrepl.server :as nrepl]
   [cheshire.generate :refer [add-encoder encode-map]])
  (:import [java.util.Base64.Encoder]))

(def ^:dynamic *subscriptions* (atom nil))
(def ^:dynamic *db* (atom nil))

(defonce ^:dynamic *api-version* 3)

(defonce ^:dynamic *api-config*
  {:email "infowarss@irq0.org"
   :pass  "foo"})

;; (defresource parameter [txt]
;;   :available-media-types ["text/plain"]
;;   :handle-ok (fn [_] (format "The text is %s" txt)))

;;(defresource feed-item [id])

(defn- hash-to-id [h]
  (Long/parseUnsignedLong (subs h 0 8) 16))


(defn- id-of [str]
  (hash-to-id (digest/sha-256 str)))


(defn- subscriptions []
  (into {} (map (fn [sub]
            (let [id (id-of (str (:url sub) (:title sub)))]
              [id {"id" id
                   "created_at" "1970-01-01T23:42:00.000000Z",
                   "feed_id" id,
                   "title" (:title sub),
                   "feed_url" (:url sub),
                   "site_url" (:url sub),}]))
    @*subscriptions*)))

(defn- empty-if-nil [x]
  (if (nil? x) "" x))

(defn- all-entries []
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

(defn- unread-entries []
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


(defn- not-implemented []
  (->
    (response/response "Not Implemented")
    (response/status 500)))

(defn- empty-response []
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
