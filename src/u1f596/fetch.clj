(ns u1f596.fetch
  (:require
   [u1f596.schema :as schema]
   [digest]
   [java-time :as time]
   [taoensso.timbre :as log]
   [clojure.string :as string]
   [schema.core :as s]))

;;;; Fetcher - Fetch a Source (infowars.src) to get *Items
;;;; Items have types depending on their sources (e.g Http -> HttpItem)

;;; Item data structures

(defn item-to-string [item]
  (format "[%s: %s/%s/%s]"
          (.getSimpleName (class item))
          (str (get-in item [:meta :source]))
          (if-not (nil? (get-in item [:summary :ts]))
            (time/format (get-in item [:summary :ts]))
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
   :app "u1f596"
   :ns (str *ns*)
   :fetch-ts (time/zoned-date-time)
   :tags #{}
   :version 2})

(defn tag-items [item _]
  (let [title (get-in item [:summary :title])
        src-key (name (or (get-in item [:meta :source-key]) :unknown))
        title-re #"Four short links:|Weekly Programming Digest|Emacs news"]
    (if (or (re-find title-re title)
            (re-find #"99pi|nasa-image-of-the-day" src-key))
      (update-in item [:meta :tags] conj :daily)
      item)))

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