(ns infowarss.schema
  (:require [schema.core :as s :refer [defschema]]
            [clj-time.format :as tf]
            [clojure.java.io :as io]
   ))

(defschema Hash
  "Hash value of the item"
  (s/constrained s/Str (partial re-matches #"SHA-256\:[0-9a-f]{64}")))

(defschema PosInt
  (s/constrained s/Int (partial <= 0)))

(defschema PosFloat
  (s/constrained s/Num pos?))

(defschema BoolInt
  (s/constrained s/Num #{0 1}))

(defschema UnixTimestamp
  (s/constrained s/Num (partial <= 0)))

(defschema TwitterTimestamp
  (s/constrained s/Str
    (partial tf/parse (tf/formatter "EEE MMM dd HH:mm:ss Z yyyy"))))

(defschema FeverIntList
  (s/constrained s/Str (partial re-matches #"(\d+(,\d+)*)?")))

(defschema FeverImageData
  (s/constrained s/Str #(.startsWith % "image/gif;base64;")))

(defschema URLStr
  (s/constrained s/Str io/as-url))

(defschema KwSet
  (s/pred set?))

(defschema StrStrMap
  {s/Str s/Str})

(defschema HttpSource
  {:url URLStr})

(defschema Metadata
  "Metadata about an item"
  {:source s/Any
   :source-name s/Str
   :app s/Str
   :ns s/Str
   :fetch-ts org.joda.time.DateTime
   :tags KwSet
   :version PosInt})

(defschema Summary
  "Summary data about an item"
  {:ts org.joda.time.DateTime
   :title s/Str})


(defschema HttpResponse
  "Http Response"
  {:headers StrStrMap
   :status PosInt
   :body s/Str
   :repeatable? s/Bool
   :protocol-version {:name s/Str
                      :major PosInt
                      :minor PosInt}
   :streaming? s/Bool
   :chunked? s/Bool
   :reason-phrase s/Str
   :length s/Int
   (s/optional-key :cookies) s/Any
   :request-time s/Int
   :trace-redirects (s/maybe [s/Str])
   :orig-content-encoding (s/maybe s/Str)})

(defschema FeedEntry
  {:url (s/maybe java.net.URL)
   :updated-ts (s/maybe org.joda.time.DateTime)
   :pub-ts (s/maybe org.joda.time.DateTime)
   :title s/Str
   :authors [s/Str]
   :contents {(s/required-key "text/plain") (s/maybe s/Str)
              (s/optional-key "text/html") s/Str}
   :description {(s/required-key "text/plain") (s/maybe s/Str)}})

(defschema Feed
  {:title s/Str
   :language (s/maybe s/Str)
   :url (s/maybe java.net.URL)
   :description {(s/required-key "text/plain") (s/maybe s/Str)}
   :encoding (s/maybe s/Str)
   :pub-ts (s/maybe org.joda.time.DateTime)
   :feed-type s/Str})

(defschema TweetEntityIndices
  [(s/one PosInt "s") (s/one PosInt "e")])

(defschema TweetEntityHashtag
  {:indices TweetEntityIndices
   :text s/Str})

(defschema TweetEntityMedia
  {:display_url URLStr
   :expanded_url URLStr
   :id PosInt
   :id_str s/Str
   :indices TweetEntityIndices
   :media_url URLStr
   :media_url_https URLStr
   :sizes s/Any
   (s/optional-key :source_user_id) PosInt
   (s/optional-key :source_user_id_str) s/Str
   (s/optional-key :source_status_id) PosInt
   (s/optional-key :source_status_id_str) s/Str
   :type s/Str
   :url URLStr})

(defschema TweetEntityUrls
  {:display_url URLStr
   :expanded_url URLStr
   :indices TweetEntityIndices
   :url URLStr})

(defschema TweetEntityUserMentions
  {:id PosInt
   :name s/Str
   :screen_name s/Str
   :id_str s/Str
   :indices TweetEntityIndices})

(defschema TweetEntity
  {(s/optional-key :hashtags) [TweetEntityHashtag]
   (s/optional-key :media) [TweetEntityMedia]
   (s/optional-key :urls) [TweetEntityUrls]
   (s/optional-key :symbols) [s/Any]
   (s/optional-key :user_mentions) [TweetEntityUserMentions]})

(defschema TweetUser
  {:contributors_enabled s/Bool
   :created_at s/Str
   :default_profile  s/Bool
   :default_profile_image s/Bool
   :description s/Str
   :entities [TweetEntity]
   :favourites_count PosInt
   (s/optional-key :follow_request_sent) (s/maybe s/Bool)
   (s/optional-key :following) (s/maybe s/Bool)
   :followers_count PosInt
   :friends_count PosInt
   :geo_enabled s/Bool
   :id PosInt
   :id_str s/Str
   :is_translator s/Bool
   :lang (s/maybe s/Str)
   :listed_count PosInt
   :location s/Str
   :name s/Str
   :notifications (s/maybe s/Bool)
   (s/optional-key :profile_background_color) s/Str
   (s/optional-key :profile_background_image_url) URLStr
   (s/optional-key :profile_background_image_url_https) URLStr
   (s/optional-key :profile_background_tile) s/Any
   (s/optional-key :profile_banner_url) URLStr
   (s/optional-key :profile_image_url) URLStr
   (s/optional-key :profile_image_url_https) URLStr
   (s/optional-key :profile_link_color) s/Str
   (s/optional-key :profile_sidebar_border_color) s/Str
   (s/optional-key :profile_sidebar_fill_color) s/Str
   (s/optional-key :profile_text_color) s/Str
   (s/optional-key :profile_use_background_image) s/Bool
   :protected s/Bool
   :screen_name s/Str
   (s/optional-key :status) s/Any
   :statuses_count PosInt
   :time_zone (s/maybe s/Str)
   :url (s/maybe URLStr)
   :utc_offset (s/maybe s/Int)
   :verified s/Bool
   (s/optional-key :withheld_in_countries) s/Str
   (s/optional-key :withheld_scope) s/Str
   :is_translation_enabled s/Bool
   :has_extended_profile s/Bool
   :translator_type s/Any

   })


(defschema Tweet
  "Twitter tweet schema based on https://dev.twitter.com/overview/api/tweets"
  {:contributors s/Any
   :coordinates s/Any
   :created_at TwitterTimestamp
   (s/optional-key :current_user_retweet) s/Any
   :entities [TweetEntity]
   (s/optional-key :extended_entities) s/Any
   (s/optional-key :metadata) s/Any
   :favorite_count PosInt
   (s/optional-key :favorited) s/Bool
   (s/optional-key :filter_level) (s/enum "none" "low" "medium")
   (s/optional-key :is_quote_status) s/Bool
   :geo s/Any
   :id PosInt
   :id_str s/Str
   :in_reply_to_screen_name (s/maybe s/Str)
   :in_reply_to_status_id (s/maybe PosInt)
   :in_reply_to_status_id_str (s/maybe s/Str)
   :in_reply_to_user_id (s/maybe PosInt)
   :in_reply_to_user_id_str (s/maybe s/Str)
   :lang (s/maybe s/Str)
   :place s/Any
   (s/optional-key :possibly_sensitive) (s/maybe s/Bool)
   (s/optional-key :quoted_status_id) (s/maybe PosInt)
   (s/optional-key :quoted_status_id_str) (s/maybe s/Str)
   (s/optional-key :quoted_status)  s/Any
   (s/optional-key :scopes) s/Any
   :retweet_count PosInt
   :retweeted (s/maybe s/Bool)
   (s/optional-key :retweeted_status) s/Any
   :source s/Str
   :text s/Str
   :truncated s/Bool
   :user TweetUser
   (s/optional-key :withheld_copyright) s/Bool
   (s/optional-key :withheld_in_countries) [s/Str]
   (s/optional-key :withheld_scope) s/Str})


(defschema TweetEntry
  {:url (s/maybe java.net.URL)
   :pub-ts (s/maybe org.joda.time.DateTime)
   :score {:favs PosInt
           :retweets PosInt}
   :language s/Str
   :id PosInt
   :type (s/enum :retweet :reply :tweet)
   :entities {:hashtags [s/Str]
              :user_mentions [s/Str]
              :photos [java.net.URL]}
   :authors [s/Str]
   :contents {(s/required-key "text/plain") (s/maybe s/Str)
              (s/optional-key "text/html") s/Str}})

(defschema HackerNewsEntry
  {:score PosInt
   :author s/Str
   :id PosInt
   :pub-ts (s/maybe org.joda.time.DateTime)
   :title s/Str
   :type s/Str
   :url (s/maybe java.net.URL) ;; something
   :hn-url (s/maybe java.net.URL) ;; https://news.ycombinator.com/item?id=
   :contents {(s/required-key "text/plain") (s/maybe s/Str)
              (s/optional-key "text/html") s/Str}})

(defschema FeverAPIRoot
  {:api_version PosInt
   :auth BoolInt})

(defschema FeverGroup
  "Fever API: group object"
  {:id PosInt
   :title s/Str})

(defschema FeverFeedsGroup
  "Fever API: feeds_group object"
  {:group_id PosInt
   :feed_ids FeverIntList})

(defschema FeverFeedsGroups
  "Fever API: feeds_groups object"
  [FeverFeedsGroup])

(defschema FeverGroups
  "Fever API: groups object"
  {(s/optional-key :last_refreshed_on_time) UnixTimestamp
   :groups [FeverGroup]
   :feeds_groups [FeverFeedsGroup]})

(defschema FeverAPIGroups
  "Fever API: groups response"
  (merge FeverAPIRoot FeverGroups))

(defschema FeverFeed
  "Fever API: feed object"
  {:id PosInt
   :favicon_id PosInt
   :title s/Str
   :url URLStr
   :site_url URLStr
   :is_spark BoolInt
   :last_updated_on_time UnixTimestamp})

(defschema FeverFeeds
  "Fever API: feeds object"
  {(s/optional-key :last_refreshed_on_time) UnixTimestamp
   :feeds [FeverFeed]
   :feeds_groups [FeverFeedsGroup]})

(defschema FeverAPIFeeds
  "Fever API: feeds request"
  (merge FeverAPIRoot FeverFeeds))

(defschema FeverFavicon
  "Fever API: favicon object"
  {:id PosInt
   :data FeverImageData})

(defschema FeverFavicons
  "Fever API: favicons object"
  {:favicons [FeverFavicon]})

(defschema FeverAPIFavicons
  "Fever API: favicons request"
  (merge FeverAPIRoot FeverFavicons))

(defschema FeverItem
  "Fever API: item object"
  {:id PosInt
   :feed_id PosInt
   :title s/Str
   :author s/Str
   :html s/Str
   :url URLStr
   :is_saved BoolInt
   :is_read BoolInt
   :created_on_time UnixTimestamp})

(defschema FeverItems
  "Fever API: items object"
  {(s/optional-key :last_refreshed_on_time) UnixTimestamp
   :items [FeverItem]
   :total_items (s/constrained s/Num (partial < 0))})

(defschema FeverAPIItems
  "Fever API: items request"
  (merge FeverAPIRoot FeverItems))

(defschema FeverLink
  "Fever API: link object"
  {:id PosInt
   :feed_id PosInt
   :item_id PosInt
   :temperature PosFloat
   :is_item BoolInt
   :is_local BoolInt
   :is_saved BoolInt
   :title s/Str
   :url URLStr
   :item_ids FeverIntList})

(defschema FeverLinks
  "Fever API: links object"
  {:links [FeverLink]})

(defschema FeverAPILinks
  "Fever API: links request"
  (merge FeverAPIRoot FeverLinks))

(defschema FeverUnreadItemIds
  "Fever API: unread_item_ids object"
  {:unread_item_ids FeverIntList})

(defschema FeverAPIUnreadItemIds
  "Fever API: unread_item_ids request"
  (merge FeverAPIRoot FeverUnreadItemIds))

(defschema FeverSavedItemIds
  "Fever API: saved_item_ids object"
  {:saved_item_ids FeverIntList})

(defschema FeverAPISavedItemIds
  "Fever API: saved_item_ids request"
  (merge FeverAPIRoot FeverSavedItemIds))
