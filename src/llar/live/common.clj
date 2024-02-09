(ns llar.live.common
  (:require
   [java-time.api :as time]))

(def state-template
  "New sources start with this template"
  {:key nil
   :status :new
   :last-exception nil
   :start-ts nil
   :last-update-ts nil})

(defprotocol LiveSource
  "Protocol to work with live sources"
  (start-collecting! [src state chan])
  (stop-collecting! [src state]))

;;; Utilities

(defn make-meta
  "Make meta entry from source and optional initial tags"
  ([src]
   (make-meta src {:key :unknown}))
  ([src
    state]
   {:source src
    :source-name (str src)
    :source-key (:key state)
    :fetch-ts (time/zoned-date-time)
    :tags #{}
    :version 2}))
