(ns infowarss.couchdb
  (:require
   [infowarss.core :refer [config]]
   [digest]
   [clj-http.client :as http]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojurewerkz.serialism.core :as s]
   [com.ashafa.clutch :as couch]
   [clojure.java.io :as io]
   [cheshire.core :refer :all])
  (:import [java.util.Base64.Encoder]))

;;;; Couchdb interface

(def ^:dynamic *couch-base-url* (get-in config [:couchdb :base-url]))
(def ^:dynamic *couch-default-db* (get-in config [:couchdb :default-db]))
(def ^:dynamic *couch-db-auth* (get-in config [:couchdb :auth]))

(defn- couch-url
  [& ps]
  (let [base (str *couch-base-url* *couch-default-db*)
        path (string/join "/" (map str ps))]
    (str base "/" path)))

(defn get-res-json [res]
  (slurp (io/resource res)))

(defn init-db!
  "Initialize database with design documents"
  []
  (let [views {:hashes {:map (get-res-json "hashes_map.js")}
               :by-tag {:map (get-res-json "by-tag_map.js")}
               :ids {:map (get-res-json "ids_map.js")}
               :feeds {:map (get-res-json "feeds.js")
                       :reduce "_stats"}
               :by-src {:map (get-res-json "by-src_map.js")}}]

    (http/put (couch-url "_design" "lookup")
      {:content-type :json
       :basic-auth *couch-db-auth*
       :form-params {:language "javascript"
                     :views views}
       :accept :json
       :as :json})))

(defn query
  ([selector]
   (query selector ["_id" "_rev"]))
  ([selector fields]
   (try+
     (let [{:keys [body status]} (http/post (couch-url "_find")
                          {:content-type :json
                           :form-params {:selector selector
                                         :limit 1000
                                         :fields fields}
                           :accept :json
                           :as :json})]
       (if (= status 200)
         (:docs body)
         (throw+ {:type ::couch-error :body body})))
    (catch Object _
      (log/error "Unexpected error: " (:throwable &throw-context))
      (throw+ {:type ::unexpected-error})))))

(defn query-ids
  [selector]
  (mapcat vals (query selector [:_id])))

(defn clear-db!
  "Remove up to 1000 infowars docs"
  []
  (let [resp (http/post (couch-url "_find")
               {:content-type :json
                :form-params {:selector {:hash {"$exists" true}}
                              :limit 1000
                              :fields ["_id" "_rev"]}
                :accept :json
                :as :json})
        f (fn [row]
            {"_id" (:_id row)
             "_rev" (:_rev row)
             "_deleted" true})
        ds (map f (get-in resp [:body :docs]))]
    (http/post (couch-url "_bulk_docs")
      {:form-params {:docs ds}
       :content-type :json
       :accept :json
       :as :json})))

(defn add-document!
  "Add new document. Return document ID"
  [params]
  (try+
    (let [{:keys [body]} (http/post (couch-url)
                           {:form-params params
                            :content-type :json
                            :accept :json
                            :as :json})]
      (if (:ok body)
        body
        (throw+ {:type ::couch-error :body body})))

    (catch (contains? #{400 401 404 409} (get % :status))
        {:keys [headers body status]}
      (log/errorf "Client Error (%s): %s %s" status headers body)
      (throw+ (assoc &throw-context :type ::request-error)))

    (catch Object _
      (log/error "Unexpected error: " (:throwable &throw-context))
      (throw+ (assoc &throw-context :type ::unexpected-error)))))

(defn get-document
  "Return document by id"
  [id]
  (:body
   (http/get (couch-url id)
     {:content-type :json
      :accept :json
      :as :json})))

(defn get-attachment
  "Return attachment for docid and attachment key"
  [docid attk]
  (let [resp (http/get (couch-url docid (name attk)))]
    [(get-in resp [:headers "Content-Type"])
     (get resp :body)]))

(defn get-document-with-attachments
  "Get document by id. Return document with attachments"
  [id]
  (let [doc (get-document id)
        attks (-> doc :_attachments keys)
        atts (->>
               attks
               (map #(get-attachment id %))
               (into {}))]
    (assoc-in doc [:entry :contents] atts)))


(defn delete-document!
  [id rev]
  (:body
   (http/delete (couch-url id)
     {:content-type :json
      :headers {"If-Match" rev}
      :accept :json
      :as :json})))

(defn change-document!
  "Set new version of document"
  [id rev params]
  (when (some empty? [id rev params])
    (throw+ {:type ::params-must-not-be-empty
             :params {:id id :rev rev :params params}}))
  (try+
    (let [{:keys [body]} (http/put (couch-url id)
                           {:form-params params
                            :headers {"If-Match" rev}
                            :content-type :json
                            :accept :json
                            :as :json})]
      (if (:ok body)
        body
        (throw+ {:type ::couch-error :body body})))

    (catch (contains? #{400 401 404 409} (get % :status))
        {:keys [headers body status]}
      (log/errorf "Client Error (%s): %s %s" status headers body)
      (throw+ (assoc &throw-context :type ::request-error)))

    (catch Object _
      (log/error "Unexpected error: " (:throwable &throw-context))
      (throw+ (assoc &throw-context :type ::unexpected-error)))))


(defn lookup-hash
  "Lookup database ID by item hash"
  [hash]
  (http/get (couch-url "_design" "lookup" "_view" "hashes")
    {:content-type :json
     :query-params {:key (str "\"" hash "\"")}
     :accept :json
     :as :json}))

(defn swap-document!
  "Swap document: Get current version, apply function on it and save
  the result as the new version"
  [id f]
  (let [doc (get-document id)]
    (let [id (get doc :_id)
          rev (get doc :_rev)
          new-doc (f doc)]
      (change-document! id rev new-doc))))


(defn doc-ids-with-tag
  "Return all doc ids with tag"
  [tag]
  (map :id (couch/get-view (couch-url) "lookup" "by-tag" {:key tag :reduce false})))

(defn all-doc-ids
  "Return all doc ids in db"
  []
  (map :id (couch/get-view (couch-url) "lookup" "ids")))

(defn get-feeds
  "Get feeds that have items stored in the database"
  []
  (map (fn [{:keys [key value]}]
         {:title (:title key)
          :url (io/as-url (:url key))
          :count (:count value)
          :type (keyword (:type key))
          :source-key (keyword (:source-key key))
          :source-name (:source-name key)
          :first-fetch-ts (tc/from-long (:min value))
          :last-fetch-ts (tc/from-long (:max value))})
    (couch/get-view (couch-url) "lookup" "feeds" {:group true})))
