(ns infowarss.fetch
  (:require
   [infowarss.schema :as schema]
   [digest]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojure.java.io :as io]
   [schema.core :as s]))

;;;; Fetcher - Fetch a Source (infowars.src) to get *Items
;;;; Items have types depending on their sources (e.g Http -> HttpItem)

;;; Item data structures

(defn item-to-string [item]
  (format "[%s: %s/%s/%s]"
    (.getSimpleName (class item))
    (str (get-in item [:meta :source]))
    (if-not (nil? (get-in item [:summary :ts]))
      (tc/to-string (get-in item [:summary :ts]))
      "?")
    (str (get-in item [:summary :title]))))

;;; Constructors

(s/defn make-item-hash :- schema/Hash
  "Make hash to use in *Item"
  [& args]
  (str "SHA-256:" (-> args string/join digest/sha-256)))

(s/defn make-meta :- schema/Metadata
  "Make meta entry from source and optional initial tags"
  [src :- s/Any]
  {:source src
   :source-name (str src)
   :source-key :unkown  ; get added later by postprocessing
   :app "infowarss"
   :ns (str *ns*)
   :fetch-ts (time/now)
   :tags #{}
   :version 2})

;;; Fetch source protocol

(defprotocol FetchSource
  "Protocol to work with data sources"
  (fetch-source [src]))


(defn fetch
  "Fetch feed. Return seq of new items"
  [feed]
  (let [{:keys [src]} feed]
    (log/debug "Fetching: " (str src))
    (fetch-source src)))
