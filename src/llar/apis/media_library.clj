(ns llar.apis.media-library
  (:require
   [clojure.data.xml :as xml]
   [clojure.tools.logging :as log]
   [compojure.core :refer [ANY routes]]
   [llar.appconfig :as appconfig]
   [llar.blobstore :as blobstore]
   [llar.http.response :as http-response]
   [llar.media-library :as library]
   [ring.util.response :as response]
   [slingshot.slingshot :refer [try+]])
  (:import
   [java.time Instant OffsetDateTime ZonedDateTime]
   [java.time.format DateTimeFormatter]))

(def ^:private dav-ns "DAV:")

(defn base-url []
  (appconfig/podcast :base-url))

(defn- ->instant [value]
  (cond
    (instance? Instant value) value
    (instance? ZonedDateTime value) (.toInstant ^ZonedDateTime value)
    (instance? OffsetDateTime value) (.toInstant ^OffsetDateTime value)
    :else nil))

(defn- creation-date [value]
  (some->> (->instant value) (.format DateTimeFormatter/ISO_INSTANT)))

(defn- dav-element [name children]
  (xml/element (xml/qname dav-ns name) {} children))

(defn- dav-response
  [href {:keys [collection? display-name size mime-type etag completed-at]}]
  (xml/element
   (xml/qname dav-ns "response") {}
   [(dav-element "href" href)
    (xml/element
     (xml/qname dav-ns "propstat") {}
     [(dav-element
       "prop"
       [(when display-name (dav-element "displayname" display-name))
        (dav-element "resourcetype"
                     (when collection? [(dav-element "collection" [])]))
        (when (some? size) (dav-element "getcontentlength" (str size)))
        (when mime-type (dav-element "getcontenttype" mime-type))
        (when etag (dav-element "getetag" (http-response/quote-etag etag)))
        (when-let [last-modified (http-response/format-http-date completed-at)]
          (dav-element "getlastmodified" last-modified))
        (when-let [created (creation-date completed-at)]
          (dav-element "creationdate" created))])
      (dav-element "status" "HTTP/1.1 200 OK")])]))

(defn- href [{:keys [path collection?]}]
  (str (base-url)
       library/base-path
       (library/encoded-path path)
       (when (and collection? (seq path)) "/")))

(defn- multistatus [{:keys [self children]} include-children?]
  (let [nodes (cond-> [self] include-children? (into children))]
    (xml/emit-str
     (xml/element (xml/qname dav-ns "multistatus")
                  {(keyword "xmlns" "D") dav-ns}
                  (map #(dav-response (href %) %) nodes)))))

(defn- serve-resource [node request]
  (case (:resource-kind node)
    :blob
    (let [blob (blobstore/get-blob-metadata (:blob-hash node))]
      (http-response/ranged-file-response
       {:file (:file blob)
        :size (:size blob)
        :mime-type (or (:mime-type blob) (:mime-type node))
        :etag (:blob-hash node)
        :last-modified (:completed-at node)}
       request))

    :bytes
    (http-response/byte-array-response
     (:bytes node)
     {:mime-type (:mime-type node)
      :etag (:etag node)
      :last-modified (:completed-at node)}
     request)

    nil))

(defn handler [request path]
  (let [entries (library/entries)
        directory (library/directory (library/segments path) entries)
        node (:self directory)
        method (:request-method request)
        depth (response/get-header request "Depth")]
    (cond
      (and (= method :propfind) directory)
      {:status 207
       :headers {"Content-Type" "application/xml; charset=utf-8"
                 "Cache-Control" "private, no-store"
                 "DAV" "1"}
       :body (multistatus directory (not= depth "0"))}

      (and (#{:get :head} method) (:resource-kind node))
      (try+
       (serve-resource node request)
       (catch Object exception
         (log/warn exception "media library: WebDAV resource failed" path)
         {:status 404 :body "Not Found"}))

      (= method :options)
      {:status 200
       :headers {"Allow" "OPTIONS, GET, HEAD, PROPFIND"
                 "DAV" "1"}}

      :else
      {:status (if directory 405 404)
       :headers {"Allow" "OPTIONS, GET, HEAD, PROPFIND"}
       :body (if directory "Method Not Allowed" "Not Found")})))

(def app
  ;; Authentication is deliberately owned by the reverse proxy. Keep this
  ;; service on a private interface/network when exposing it through Caddy.
  (routes
   (ANY "/library" request (handler request nil))
   (ANY "/library/" request (handler request nil))
   (ANY "/library/*" request
     (handler request (get-in request [:params :*])))))
