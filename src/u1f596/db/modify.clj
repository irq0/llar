(ns u1f596.db.modify
  (:require
   [digest]
   [u1f596.db.sql :as sql]
   [u1f596.db.core]
   [u1f596.persistency :refer [ItemTagsPersistency ItemPersistency]]
   [java-time :as time]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojure.java.jdbc :as j]
   [mpg.core :as mpg]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [byte-streams :refer [to-byte-buffer]]
   [cheshire.core :refer :all]
   [mount.core :refer [defstate]]
   [org.bovinegenius [exploding-fish :as uri]]
   [hikari-cp.core :as hikari]
   [hugsql.core :as hugsql]
   [pantomime.media :as pantomime-media]
   [cheshire.generate :as json :refer [encode-str]])
  (:import (u1f596.db.core PostgresqlDataStore)))

(def item-data-table-entries
  "Item entries stored in the item_data table, rather than in the items entry column"
  {:contents :item_data_type/content
   :descriptions :item_data_type/description
   :thumbs :item_data_type/thumbnail})

(defn store-item-without-data!
  "store-item! persists item to the database, without data attachment like
  contents and descriptions. Return {:id .. :hash .. } or nil if the item
  already exists. Setting the optional overwrite? flag to true overwrites the
  database row in case of an item hash collision."
  [db item {:keys [overwrite?]}]
  (let [{:keys [meta feed summary entry]} item
        nlp (get-in item [:entry :nlp])]
    (sql/store-item
     db
     {:source {:name (:source-name meta)
               :key (name (:source-key meta))
               :type (keyword "item_type" (name (:type item)))
               :data (select-keys
                      feed [:feed-type :language :title :url])}
      :hash (:hash item)
      :ts (or (:ts summary) (time/zoned-date-time))
      :title (:title summary)
      :author (string/join "," (:authors entry))
      :type (keyword "item_type" (name (:type item)))
      :tags (into {} (map (fn [x] [(name x) nil]) (:tags meta)))
      :nlp-nwords (or (:nwords nlp) -1)
      :nlp-urls (into [] (or (:urls nlp) []))
      :nlp-names (into [] (or (:names nlp) []))
      :nlp-nouns (into [] (or (:nouns nlp) []))
      :nlp-verbs (into [] (or (:verbs nlp) []))
      :nlp-top (or (:top nlp) {})
      :on-conflict (if overwrite? (sql/conflict-items-overwrite-snip)
                       (sql/conflict-items-ignore-dupe-snip))
      :entry (apply dissoc (concat [entry :nlp] item-data-table-entries))})))

(defn store-item-data!
  "store-item-data! persists item data as rows in the item_data table. See
  item-data-table-entires for a list of this special entries"
  [db item-id item]
  (let [{:keys [entry]} item]
    (doall
     (flatten
      (for [[entry-key data-entry-type] item-data-table-entries
            :let [mime-data-pairs (get entry entry-key)]
            :when (some? mime-data-pairs)]
        (for [[mime-type data] mime-data-pairs
              :when (some? data)
              :let [text? (pantomime-media/text? mime-type)]]
          (sql/store-item-data
           db
           {:item-id item-id
            :mime-type mime-type
            :type data-entry-type
            :text (when text? data)
            :data (when-not text? (to-byte-buffer data))})))))))

(defn store-item-and-data!
  "store-item-and-data! combines store-item! and store-item-data! into one
  transaction. Returns nil if the item already exists in the database or {:item
  {:id .. :hash ..} :data ({:id :mime_type :type :is_binary})"
  [db item args]
  (j/with-db-transaction [tx db]
    (when-let [{:keys [id hash]} (store-item-without-data!
                                  tx item args)]
      {:item {:id id
              :hash hash}
       :data (store-item-data! tx id item)})))

(extend-protocol
    ItemPersistency
  PostgresqlDataStore
  (store-item! [this item args] (store-item-and-data! this item args)))

;; ----------

(extend-protocol
    ItemTagsPersistency
  PostgresqlDataStore

  (item-set-tags! [this item-id tags]
    (->>
     (sql/set-tags this
                   {:tags (vec (map name tags))
                    :vals (vec (repeat (count tags) nil))
                    :where [(sql/tag-cond-by-id {:id item-id})]})
     :tags
     keys
     (map keyword)))

  (item-remove-tags! [this item-id tags]
    (->>
     (sql/remove-tags this
                      {:tags (vec (map name tags))
                       :where [(sql/tag-cond-by-id {:id item-id})]})
     :tags
     keys
     (map keyword)))

  (remove-unread-for-items-of-source-older-then! [this source-keys older-then-ts]
    (let [source-ids (sql/resolve-source-keys-to-ids
                      this
                    {:keys (vec (map name source-keys))}
                    {}
                    {:row-fn :id})]
      (sql/remove-tags this
                       {:tags ["unread"]
                        :where [(sql/tag-cond-by-source-id-in {:ids source-ids})
                                ["AND"]
                                ["exist_inline(tags, 'unread')"]
                                ["AND"]
                                (sql/tag-cond-le-ts {:ts older-then-ts})]}))))