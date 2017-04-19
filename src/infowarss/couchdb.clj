(ns infowarss.couchdb
  (:require
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

(defonce ^:dynamic *couch-base-url* "http://10.23.1.42:5984/")
(defonce ^:dynamic *couch-default-db* "db")
(defonce ^:dynamic *couch-db-auth* "admin:admin")
(defonce ^:dynamic *db* (atom nil))
(defonce ^:dynamic *last-document* (atom nil))

(defn- couch-url
  [& ps]
  (let [base (str *couch-base-url* *couch-default-db*)
        path (string/join "/" (map str ps))]
    (str base "/" path)))

(defn get-res-json [res]
  (slurp (io/resource res)))

(defn init-db! []
  (let [views {:hashes {:map (get-res-json "hashes_map.js")}
               :by-tag {:map (get-res-json "by-tag_map.js")}
               :ids {:map (get-res-json "ids_map.js")}
               :by-src {:map (get-res-json "by-src_map.js")}}]

    (http/put (couch-url "_design" "lookup")
      {:content-type :json
       :basic-auth *couch-db-auth*
       :form-params {:language "javascript"
                     :views views}
       :accept :json
       :as :json})))

(defn clear-db! []
  (let [resp (http/post (couch-url "_find")
               {:content-type :json
                :form-params {:selector {:hash {"$exists" true}}
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

(defn add-document! [params]
  (reset! *last-document* params)
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


(defn get-document [id]
  (:body
   (http/get (couch-url id)
     {:content-type :json
      :accept :json
      :as :json})))

(defn get-attachment [docid attk]
  (let [resp (http/get (couch-url docid (name attk)))]
    [(get-in resp [:headers "Content-Type"])
     (get resp :body)]))

(defn get-document-with-attachments [id]
  (let [doc (get-document id)
        attks (-> doc :_attachments keys)
        atts (->>
               attks
               (map #(get-attachment id %))
               (into {}))]
    (assoc-in doc [:feed-entry :contents] atts)))


(defn change-document! [id rev params]
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


(defn lookup-hash [hash]
  (http/get (couch-url "_design" "lookup" "_view" "hashes")
    {:content-type :json
     :query-params {:key (str "\"" hash "\"")}
     :accept :json
     :as :json}))


(def attachment-extensions
  {"text/html" "html"
   "text/plain" "txt"})


(defn swap-document! [id f]
  (let [doc (get-document id)]
    (let [id (get doc :_id)
          rev (get doc :_rev)
          new-doc (f doc)]
      (change-document! id rev new-doc))))

(defn doc-ids-with-tag [tag]
  (map :id (couch/get-view (couch-url) "lookup" "by-tag" {:key tag})))

(defn all-doc-ids []
  (map :id (couch/get-view (couch-url) "lookup" "ids")))
