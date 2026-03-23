(ns llar.export.zotero
  (:require
   [clj-http.client :as http]
   [cheshire.core :as json]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [llar.appconfig :as appconfig])
  (:import
   [org.apache.commons.text StringEscapeUtils]))

(defn zotero-credentials []
  (appconfig/credentials :zotero))

(defn- zotero-api-headers [api-key]
  {"Zotero-API-Key" api-key
   "Zotero-API-Version" "3"})

(defn- extract-url
  "Extract URL from item, handling JSON-encoded strings from entry->'url'."
  [item]
  (let [raw (:url item)]
    (if (and (string? raw) (string/starts-with? raw "\""))
      (json/parse-string raw)
      (str raw))))

(defn format-note-html
  "Build HTML note content from an item and its annotations."
  [item annotations]
  (let [url (extract-url item)
        title (str (:title item))
        escaped-title (StringEscapeUtils/escapeHtml4 title)
        escaped-url (StringEscapeUtils/escapeHtml4 url)
        highlights (filter #(some? (:selector %)) annotations)
        notes (filter #(and (some? (:body %))
                            (nil? (:selector %))) annotations)
        sb (StringBuilder.)]
    (.append sb (str "<h2>" escaped-title "</h2>"))
    (.append sb (str "<p>Source: <a href=\"" escaped-url "\">" escaped-url "</a></p>"))
    (when (seq highlights)
      (.append sb "<h3>Highlights</h3>")
      (doseq [h highlights]
        (let [text (get-in h [:selector :quote :exact])]
          (.append sb (str "<blockquote><p>"
                           (StringEscapeUtils/escapeHtml4 (str text))
                           "</p></blockquote>")))))
    (when (seq notes)
      (.append sb "<h3>Notes</h3>")
      (doseq [n notes]
        (.append sb (str "<p>"
                         (StringEscapeUtils/escapeHtml4 (str (:body n)))
                         "</p>"))))
    (.toString sb)))

(defn- user-api-url [user-id]
  (str "https://api.zotero.org/users/" user-id))

(defn resolve-or-create-collection!
  "Find the Zotero collection named 'llar', or create it. Returns the collection key."
  [base-url headers]
  (let [url (str base-url "/collections")
        resp (http/get url {:as :json :headers headers})
        existing (->> (:body resp)
                      (filter #(= "llar" (get-in % [:data :name])))
                      first)]
    (if existing
      (get-in existing [:data :key])
      (let [create-resp (http/post url {:content-type :json
                                        :as :json
                                        :headers headers
                                        :body (json/generate-string [{:name "llar"}])})]
        (get-in create-resp [:body :successful :0 :key])))))

(defn make-webpage-item
  "Create a Zotero webpage item payload."
  [item collection-key]
  (cond-> {:itemType "webpage"
           :title (str (:title item))
           :url (extract-url item)
           :accessDate (str (java.time.LocalDate/now))
           :extra (str "llar-id: " (:id item))
           :tags [{:tag "llar"}]}
    collection-key (assoc :collections [collection-key])))

(defn make-child-note
  "Create a Zotero child note payload."
  [parent-key note-html]
  {:itemType "note"
   :parentItem parent-key
   :note note-html
   :tags [{:tag "llar-annotations"}]})

(defn export-item!
  "Export an item with its annotations to Zotero.
   Returns {:parent-key ... :note-key ...} on success."
  [item annotations]
  (let [creds (zotero-credentials)]
    (when (nil? creds)
      (throw+ {:type ::credentials-missing}))
    (let [{:keys [api-key user-id]} creds
          base-url (user-api-url user-id)
          items-url (str base-url "/items")
          headers (zotero-api-headers api-key)
          collection-key (resolve-or-create-collection! base-url headers)
          webpage-item (make-webpage-item item collection-key)]
      (try+
       (let [resp (http/post items-url {:content-type :json
                                        :as :json
                                        :headers headers
                                        :body (json/generate-string [webpage-item])})
             parent-key (get-in resp [:body :successful :0 :key])
             note-html (format-note-html item annotations)
             child-note (make-child-note parent-key note-html)
             note-resp (http/post items-url {:content-type :json
                                             :as :json
                                             :headers headers
                                             :body (json/generate-string [child-note])})
             note-key (get-in note-resp [:body :successful :0 :key])]
         (log/infof "Exported item to Zotero: parent=%s note=%s title=%s"
                    parent-key note-key (:title item))
         {:parent-key parent-key :note-key note-key})
       (catch [:status 403] {:keys [body]}
         (log/error "Zotero authentication failed")
         (throw+ {:type ::zotero-auth-error :body body}))
       (catch #(and (map? %) (:status %)) {:keys [status body]}
         (log/errorf "Zotero API error: status=%s" status)
         (throw+ {:type ::zotero-api-error :status status :body body}))))))
