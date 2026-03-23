(ns llar.persistency
  (:require
   [cheshire.generate :refer [add-encoder encode-str]]
   [digest]
   [java-time.api :as time])
  (:import
   (org.bovinegenius.exploding_fish UniformResourceIdentifier)))

(add-encoder java.time.ZonedDateTime
             (fn [dt jg]
               (.writeString jg (time/format :iso-zoned-date-time dt))))

(add-encoder UniformResourceIdentifier encode-str)

(defprotocol CouchItem
  (to-couch [item] "Convert item to database form"))

(defprotocol ItemPersistency
  (store-item! [this item args]))

(defprotocol ItemTagsPersistency
  (item-set-tags! [this item-id tags])
  (item-remove-tags! [this item-id tags])
  (remove-unread-for-items-of-source-older-then! [this source-keys older-then-ts]))

(defprotocol StatsQueries
  (get-table-row-counts [this])
  (get-type-stats [this])
  (get-tag-stats [this])
  (get-tags [this])
  (get-word-count-groups [this]))

(defprotocol ItemQueries
  (get-items-recent [this args]
    "get-items-recent returns :limit recent items, optionally
    offsetted with :before {:ts :id} to implement pagination.

    Supports the following filters:
    :with-tag $keyword  -  only $keyword tagged items
    :with-type :item-type/$type  -  only items with type $type
    :simple-filter see #'simple-filter-to-sql  -  only items matching simple filter
    :with-source-keys [$source-key ..]  -  only items from source in vec

    Sorting options:
    :sort-order  -  :newest (default), :oldest, or :ranked
    :offset      -  integer offset for non-cursor pagination
    :highlight-boost  -  hours subtracted for highlighted items (default 48.0)
    :rarity-cap  -  max hours for rarity boost (default 168.0)
    :ranked      -  when truthy, joins source_stats for ranked sorting

    Return item without data when :with-data? or :with-preview-data? is not specified")
  (get-item-by-id [this item-id])
  (get-items-by-tag [this tag]))

(defprotocol SourceQueries
  (get-sources-item-tags-counts [this item-tag simple-filter config-sources])
  (sources-merge-in-tags-counts [this sources])
  (get-sources [this config-sources]))

(defprotocol AnnotationPersistency
  (get-annotations [this item-id])
  (create-annotation! [this item-id selector body])
  (delete-annotation! [this annotation-id]))

(defprotocol RankingQueries
  (get-source-stats [this args])
  (get-ranked-vs-time-preview [this args]))

(defprotocol DataStoreSearch
  (update-index! [this])
  (search [this query args] [this query]))

(defprotocol DataStoreLifeCycle
  (stop-data-store [this]))
