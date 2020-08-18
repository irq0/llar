(ns infowarss.schema
  (:require [schema.core :as s :refer [defschema]]
            [clojure.test :refer [function?]]
            [clojure.string :as string]
            [java-time :as time]
            [clojure.java.io :as io]))

;;;; Schemas - Both for internal and external data

;;; Base "Types"


(defschema Func
  (s/pred function?))

(defschema FuncList
  [Func])

(defschema NotEmptyStr
  (s/constrained s/Str (complement string/blank?)))

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
                 (partial time/zoned-date-time (time/formatter "EEE MMM dd HH:mm:ss Z yyyy"))))

(defschema URLStr
  (s/constrained s/Str io/as-url))

(defschema URLStrOrBlank
  (s/constrained s/Str #(or (string/blank? %) (io/as-url %))))

(defschema KwSet
  (s/pred set?))

(defschema StrStrMap
  {s/Str s/Str})

(defschema StrAnyMap
  {s/Str s/Any})

(defschema HttpSource
  {:url URLStr})

(defschema URLType
  (s/cond-pre java.net.URL java.net.URI org.bovinegenius.exploding_fish.UniformResourceIdentifier))

;;; *Items

(defschema Metadata
  "Metadata about an item"
  {:source s/Any
   :source-name NotEmptyStr
   :source-key s/Keyword
   :app s/Str
   :ns s/Str
   :fetch-ts java.time.ZonedDateTime
   :tags KwSet
   (s/optional-key :view-hints) {:html s/Keyword}
   :version PosInt})

(defschema Summary
  "Summary data about an item"
  {:ts java.time.ZonedDateTime
   :title s/Str})

;;; clj-http responses

(defschema HttpResponse
  "Http Response"
  {:headers StrAnyMap
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
   (s/optional-key :links) s/Any
   :request-time s/Int
   :trace-redirects (s/maybe [s/Str])
   :orig-content-encoding (s/maybe s/Str)})

;;; Feed Items

(defschema FeedEntry
  {:url (s/maybe URL)
   :updated-ts (s/maybe java.time.ZonedDateTime)
   :pub-ts (s/maybe java.time.ZonedDateTime)
   :title s/Str
   :authors [s/Str]
   :contents {(s/required-key "text/plain") (s/maybe s/Str)
              (s/optional-key "text/html") s/Str}
   :descriptions {(s/required-key "text/plain") (s/maybe s/Str)}})

(defschema Feed
  {:title s/Str
   :language (s/maybe s/Str)
   :url (s/maybe URLType)
   :descriptions {(s/required-key "text/plain") (s/maybe s/Str)}
   :encoding (s/maybe s/Str)
   :pub-ts (s/maybe java.time.ZonedDateTime)
   :updated-ts (s/maybe java.time.ZonedDateTime)
   :feed-type s/Str})

;;; Mercury Parser
(defschema MercuryEntry
  {:url (s/maybe URLType)
   :lead-image-url (s/maybe URLType)
   :next-page-url (s/maybe URLType)
   :pub-ts (s/maybe org.joda.time.DateTime)
   :title s/Str
   :authors [s/Str]
   :contents {(s/required-key "text/plain") (s/maybe s/Str)
              (s/optional-key "text/html") s/Str}
   :descriptions {(s/required-key "text/plain") (s/maybe s/Str)}})

;;; Reddit

(defschema RedditEntry
  {:url (s/maybe URLType)
   :thumbnail (s/maybe URLType)
   :pub-ts (s/maybe org.joda.time.DateTime)
   :title s/Str
   :authors [s/Str]
   :id NotEmptyStr
   :score s/Int
   :contents {(s/required-key "text/plain") (s/maybe s/Str)
              (s/optional-key "text/html") s/Str}
   :descriptions {(s/required-key "text/plain") (s/maybe s/Str)}})

(defschema DocumentEntry
  {:url (s/maybe URLType)
   :title s/Str
   :authors [s/Str]
   :pub-ts (s/maybe java.time.ZonedDateTime)
   :npages s/Int
   :orig-mime-type NotEmptyStr
   :orig-size s/Int
   :thumbs {(s/optional-key "image/png") s/Str}
   :contents {(s/required-key "text/plain") (s/maybe s/Str)
              (s/optional-key "application/pdf") s/Str
              (s/optional-key "text/html") s/Str}
   :descriptions {(s/required-key "text/plain") (s/maybe s/Str)}})

(defschema MailEntry
  {:title s/Str
   :id s/Str
   :authors [s/Str]
   :received-ts (s/maybe java.time.ZonedDateTime)
   :sent-ts (s/maybe java.time.ZonedDateTime)
   :contents {(s/required-key "text/plain") (s/maybe s/Str)
              (s/optional-key "text/html") s/Str}
   :descriptions {(s/required-key "text/plain") (s/maybe s/Str)}})


;;; Twitter API (incoming)


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
   :translator_type s/Any})

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

;;; Tweet Items

(defschema TweetEntry
  {:url (s/maybe URLType)
   :pub-ts (s/maybe java.time.ZonedDateTime)
   :score {:favs PosInt
           :retweets PosInt}
   :language s/Str
   :id PosInt
   :type (s/enum :retweet :reply :tweet)
   :entities {:hashtags [s/Str]
              :user_mentions [s/Str]
              :photos [URLType]}
   :authors [s/Str]
   :contents {(s/required-key "text/plain") (s/maybe s/Str)
              (s/optional-key "text/html") s/Str}})

;;; Hacker News Items

(defschema HackerNewsEntry
  {:score PosInt
   :author s/Str
   :id PosInt
   :pub-ts (s/maybe java.time.ZonedDateTime)
   :title s/Str
   :type s/Keyword
   :url (s/maybe URLType) ;; something
   :hn-url (s/maybe URLType) ;; https://news.ycombinator.com/item?id=
   :contents {(s/required-key "text/plain") (s/maybe s/Str)
              (s/optional-key "text/html") s/Str}})

