(ns llar.item
  (:require
   [clojure.spec.alpha :as s]
   [llar.src :as src]))

(s/def :irq0-fetch-item/source src/source?)
(s/def :irq0-fetch-item/source-name string?)
(s/def :irq0-fetch-item/source-key keyword?)
(s/def :irq0-fetch-item/fetch-ts #(instance? java.time.ZonedDateTime %))
(s/def :irq0-fetch-item/tags  (s/coll-of keyword? :kind set?))
(s/def :irq0-fetch-item/version int?)
(s/def :irq0-fetch-item/language string?)
(s/def :irq0-fetch-item/ts #(instance? java.time.ZonedDateTime %))
(s/def :irq0-fetch-item/title string?)
(s/def :irq0-fetch-item/feed-type string?)
(s/def :irq0-fetch-item/pub-ts :irq0/ts)
(s/def :irq0-fetch-item/updated-ts :irq0/ts)
(s/def :irq0-fetch-item/descriptions map?)

(s/def :irq0/item-metadata (s/keys :req-un [:irq0-fetch-item/source
                                            :irq0-fetch-item/source-name
                                            :irq0-fetch-item/source-key
                                            :irq0-fetch-item/fetch-ts
                                            :irq0-fetch-item/tags
                                            :irq0-fetch-item/version]))

(s/def :irq0/item-summary (s/keys :req-un [:irq0-fetch-item/ts
                                           :irq0-fetch-item/title]))

(def item-hash-regex #"SHA-256:[0-9a-f]{64}$")
(s/def :irq0/item-hash (s/and string? #(re-matches item-hash-regex %)))

(s/def :irq0/feed (s/keys :req-un [:irq0-fetch-item/title
                                   :irq0-fetch-item/feed-type
                                   :irq0/url]
                          :opt-un [:irq0-fetch-item/language
                                   :irq0-fetch-item/pub-ts
                                   :irq0-fetch-item/updated-ts
                                   :irq0-fetch-item/descriptions]))
