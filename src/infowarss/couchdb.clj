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
   [cheshire.core :refer :all])
  (:import [java.util.Base64.Encoder]))

(defonce ^:dynamic *couch-db* "http://10.23.1.42:5984/db/")
(defonce ^:dynamic *db* (atom nil))
(defonce ^:dynamic *last-document* (atom nil))

(defn clear-db! []
  (let [resp (http/post (str *couch-db* "/_find")
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
    (http/post (str *couch-db* "/_bulk_docs")
      {:form-params {:docs ds}
       :content-type :json
       :accept :json
       :as :json})))

(defn add-document! [params]
  (reset! *last-document* params)
  (try+
    (let [{:keys [body]} (http/post *couch-db*
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
   (http/get (str *couch-db* "/" id)
     {:content-type :json
      :accept :json
      :as :json})))

(defn get-attachment [docid attk]
  (let [resp (http/get (str *couch-db* "/" docid "/" (name attk)))]
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
    (let [{:keys [body]} (http/put (str *couch-db* "/" id)
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
  (http/get (str *couch-db* "/_design/lookup/_view/hashes")
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
  (->>
    (couch/get-view *couch-db* "lookup" "by-tag" {:key tag})
    (map :id)))

(defn all-doc-ids []
  (->>
    (couch/get-view *couch-db* "lookup" "ids")
    (map :id)))
