(ns llar.fetch.bookmark
  (:require
   [org.bovinegenius [exploding-fish :as uri]]
   [clojure.string :as string]
   [llar.fetch :as fetch :refer [make-item-hash]]
   [llar.fetch.readability :as readability]
   [llar.item]
   [llar.human :as human]
   [llar.postproc :as proc]
   [llar.src :as src]))

;;; Bookmarks

(defn bookmark-identity [url]
  (let [url (uri/uri url)
        site (human/host-identifier url)]
    {:url (str url)
     :hash (make-item-hash (str url))
     :source-key (if (some? site)
                   (keyword (str "bookmark-" site))
                   :bookmark)
     :source-name (if (some? site)
                    (format "[Bookmark: %s]" (str site))
                    "[Bookmark]")}))

(defn apply-capture-metadata
  "Give a fetched or placeholder item the stable identity and user state of a capture."
  [item url captured-ts]
  (let [{:keys [hash source-key source-name]} (bookmark-identity url)]
    (cond-> (-> item
                (assoc :hash hash)
                (assoc-in [:meta :source-key] source-key)
                (assoc-in [:meta :source-name] source-name)
                (update-in [:meta :tags] (fnil into #{}) #{:saved :unread}))
      captured-ts (assoc-in [:entry :captured-ts] (str captured-ts)))))

(defn make-raw-bookmark [url title captured-ts]
  (let [source (src/readability url)
        title (if (string/blank? title) (str url) title)]
    (apply-capture-metadata
     (readability/make-readability-item
      (fetch/make-meta source)
      {:ts captured-ts :title title}
      (make-item-hash (str url))
      {:url (str url) :title title :authors []})
     url
     captured-ts)))

(defn make-readability-bookmark-feed
  ([url]
   (make-readability-bookmark-feed url nil))
  ([url captured-ts]
   (let [src (src/readability url)]
     {:src src
      :tags #{:bookmark}
      :proc (proc/new
             {:post [(fn [item]
                       (apply-capture-metadata item url captured-ts))]})})))
