(ns llar.fetch
  (:require
   [clojure.spec.alpha :as s]
   [digest :as digest]
   [java-time.api :as time]
   [clojure.tools.logging :as log]
   [clojure.string :as string]))

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

(defn make-item-hash
  "Make hash to use in *Item"
  [& args]
  {:pre [(s/assert (s/coll-of string?) args)]
   :post [(s/assert :irq0/item-hash %)]}
  (str "SHA-256:" (-> args string/join digest/sha-256)))

(defn make-meta
  "Make meta entry from source and optional initial tags"
  [src]
  {:source src
   :source-name (str src)
   :source-key :unknown  ; get added later by postprocessing
   :fetch-ts (time/zoned-date-time)
   :tags #{}
   :version 2})

;;; Fetch source protocol

(defprotocol FetchSource
  "Protocol to work with data sources"
  (fetch-source
    [src conditional-tokens]
    "Fetch source src and return seq of items. conditional-tokens is a
    map of tokens to be used in conditional fetches - e.g. etag,
    last-modified etc. sources may ignore this."))

(defn fetch
  "Fetch feed. Return seq of new items"
  [feed & {:keys [conditional-tokens]
           :or {conditional-tokens {}}
           :as args}]
  (let [{:keys [src]} feed]
    (log/debug "fetching: " (str src) args)
    (fetch-source src conditional-tokens)))
