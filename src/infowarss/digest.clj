(ns infowarss.digest
  (:require
   [schema.core :as s]
   [hiccup.core :refer [html]]
   [infowarss.db :as db]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]))


(s/defn make-source-digest
  [src-key :- s/Keyword
   timespan :- org.joda.time.ReadablePeriod]
  (let [before (time/minus (time/now) timespan)
        src (get (db/get-sources) src-key)
        items (db/get-items-recent
                {:before before
                 :with-data? false
                 :limit 5
                 :with-source-keys [src-key]
                 :simple-filter :unread})]
    [src (map :entry items)]))

;; idea: cronjob creates digest type items
;; digests have a list of items - datastructure
;; have references perhaps
;; need view for that - reader


(comment
  )
