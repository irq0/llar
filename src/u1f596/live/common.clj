(ns u1f596.live.common
  (:require
   [u1f596.schema :as schema]
   [schema.core :as s]
   [java-time :as time]))

(def state-template
  "New sources start with this template"
  {:key nil
   :status :new
   :last-exception nil
   :start-ts nil
   :last-update-ts nil})

(defn item-to-string [item]
  (format "[%s: %s/%s/%s]"
          (.getSimpleName (class item))
          (str (get-in item [:meta :source]))
          (if-not (nil? (get-in item [:summary :ts]))
            (time/format :iso-instant (get-in item [:summary :ts]))
            "?")
          (str (get-in item [:summary :title]))))

(defprotocol LiveSource
  "Protocol to work with live sources"
  (start-collecting! [src state chan])
  (stop-collecting! [src state]))

;;; Utilities

(s/defn make-meta :- schema/Metadata
  "Make meta entry from source and optional initial tags"
  ([src :- s/Any]
   (make-meta src {:key :unknown}))
  ([src :- s/Any
    state :- s/Any]
   {:source src
    :source-name (str src)
    :source-key (:key state)
    :app "u1f596"
    :ns (str *ns*)
    :fetch-ts (time/zoned-date-time)
    :tags #{}
    :version 2}))