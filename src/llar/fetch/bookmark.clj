(ns llar.fetch.bookmark
  (:require
   [org.bovinegenius [exploding-fish :as uri]]
   [llar.fetch :refer [make-item-hash]]
   [llar.item]
   [llar.human :as human]
   [llar.postproc :as proc]
   [llar.src :as src]))

;;; Bookmarks

(defn make-readability-bookmark-feed [url]
  (let [src (src/readability url)]
    {:src src
     :tags #{:bookmark}
     :proc (proc/new
            {:post [(fn [item]
                      (let [url (some-> item :entry :url uri/uri)
                            site (human/host-identifier url)]
                        (-> item
                            (assoc-in [:meta :source-key]
                                      (if (some? site)
                                        (keyword (str "bookmark-" site))
                                        :bookmark))
                            (assoc :hash (make-item-hash (str url)))
                            (assoc-in [:meta :source-name]
                                      (if (some? site)
                                        (format "[Bookmark: %s]" (str site))
                                        "[Bookmark]")))))]})}))
