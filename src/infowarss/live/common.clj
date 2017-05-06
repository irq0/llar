(ns infowarss.live.common
  (:require
   [infowarss.schema :as schema]
   [clj-time.coerce :as tc]
   [schema.core :as s]
   [clj-time.core :as time]))


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
    (str (get-in item [:meta :src]))
    (if-not (nil? (get-in item [:summary :ts]))
      (tc/to-string (get-in item [:summary :ts]))
      "?")
    (str (get-in item [:summary :title]))))


(defprotocol LiveSource
  "Protocol to work with live sources"
  (start-collecting! [src state chan])
  (stop-collecting! [src state]))

;;; Utilities

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
   :version 1})
