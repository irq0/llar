(ns infowarss.config
  (:require
   [infowarss.fetch :refer [make-item-hash] :as fetch]
   [infowarss.fetch.feed]
   [infowarss.fetch.http]
   [infowarss.fetch.document]
   [infowarss.fetch.imap]
   [infowarss.fetch.mercury]
   [infowarss.fetch.reddit]
   [infowarss.fetch.twitter]
   [infowarss.live.hackernews]
   [infowarss.src :as src]
   [taoensso.timbre :as log]
   [infowarss.http :as http]
   [infowarss.converter :as converter]
   [java-time :as time]
   [clojure.set :refer [intersection]]
   [hiccup.core :refer [html]]
   [hickory.select :as S]
   [slingshot.slingshot :refer [try+]]
   [clj-http.client :as http-client]
   [clj-http.cookies :as http-cookies]
   [hickory.core :as hick]
   [hickory.render :refer [hickory-to-html]]
   [clojure.contrib.humanize :as human]
   [infowarss.postproc :as proc]
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [org.bovinegenius [exploding-fish :as uri]]))

(def creds (edn/read-string (slurp (io/resource "credentials.edn"))))

;; second minute hour day-of-week day-of-month month year
(def cron-hourly "23 42 * * * * *")
(def cron-daily "23 42 5,8,12,17,23 * * * *")
(def cron-twitter "23 42 12,18,23 * * * *")

(defn- parse-date-to-zoned-data-time [fmt s]
  (time/zoned-date-time
   (time/local-date (time/formatter fmt) s) 0 (time/zone-id "UTC")))

(defn- parse-date-time-to-zoned-data-time [fmt s]
  (time/zoned-date-time
   (time/local-date-time (time/formatter fmt) s) (time/zone-id "UTC")))


;;; Bookmarks

(defn bookmark-html [i]
  (html
   [:h1 (get-in i [:summary :title])]
   [:div {:class "summary"}
    [:ul
     [:li [:span {:class "key"} "URL: "]
      [:a {:href (get-in i [:entry :url])} (get-in i [:entry :url])]]
     [:li [:span {:class "key"} "Added: "] (time/format (time/zoned-date-time))]
     [:li [:span {:class "key"} "Published: "] (time/format (get-in i [:summary :ts]))]
     [:li [:span {:class "key"} "Next Page URL: "]
      [:a {:href (get-in i [:entry :next-page-url])} (get-in i [:entry :next-pageurl])]]]]
   [:div {:class "description"}
    [:h2 "Summary"]
    [:p (get-in i [:entry :descriptions "text/plain"])]]
   [:div {:class "nlp"}
    [:h2 "Names / Places"]
    [:p (map (fn [name] [:span [:a {:href (str "https://www.startpage.com/do/search?query=" name)} (str " " name " ")] "&nbsp;"]) (get-in i [:entry :nlp :names]))]]
   [:h1 "Content"]))

(def +mercury-site-blacklist+
  #"www\.washingtonpost\.com|semiaccurate\.com|gitlab\.com|youtube|vimeo|reddit|redd\.it|open\.spotify\.com|news\.ycombinator\.com|www\.amazon\.com")

(defn replace-contents-with-mercury [item keep-orig?]
  (let [url (get-in item [:entry :url])
        src (src/mercury (str url))
        mercu (proc/process-feedless-item src (first (fetch/fetch-source src)))
        html (if keep-orig?
               (str "<div class=\"orig-content\">" (get-in item [:entry :contents "text/html"]) "</div>"
                    "<div class=\"mercury\">" (get-in mercu [:entry :contents "text/html"]) "</div>")
               (get-in mercu [:entry :contents "text/html"]))
        text (if keep-orig?
               (str (get-in item [:entry :contents "text/plain"])
                    "\n"
                    (get-in mercu [:entry :contents "text/plain"]))
               (get-in mercu [:entry :contents "text/plain"]))]
    (cond-> (-> item
        (assoc-in [:entry :nlp] (get-in mercu [:entry :nlp]))
        (assoc-in [:entry :descriptions "text/plain"] (get-in mercu [:entry :descriptions "text/plain"]))
        (assoc-in [:entry :contents "text/plain"] text)
        (assoc-in [:entry :lead-image-url] (get-in mercu [:entry :lead-image-url]))
        (assoc-in [:entry :contents "text/html"] html))
      (empty? (get-in item [:entry :authors]))
      (assoc-in [:entry :authors] (get-in mercu [:entry :authors])))


      ))

;; todo add pdf support!!

(defn mercury-contents
  [& {:keys [keep-orig?]
      :or {keep-orig? false}}]
  (fn [item]
    (let [site (some-> item :entry :url uri/host)
          path (or (some-> item :entry :url uri/path) "")]
      (cond
        ;; images
        (or (re-find #"i\.imgur\.com|i\.redd\.it|twimg\.com" site)
            (re-find #"\.(jpg|jpeg|gif|png|pdf)$" path))
        (update-in item [:entry :contents "text/html"]
                   str "<img src=\"" (get-in item [:entry :url]) "\"/>")

        ;; blacklisted sites
        (re-find +mercury-site-blacklist+ site)
        item

        ;; rest: replace with mercury
        :else
        (try+
         (replace-contents-with-mercury item keep-orig?)
         (catch [:type :infowarss.fetch.mercury/not-parsable] _
           (log/error (str item) "Mercury Error. Not replacing content with mercury")
           item))))))

(defn make-readability-bookmark-feed [url]
  (let [src (src/mercury url)]
    {:src src
     :tags #{:bookmark}
     :proc (proc/make
            :post [(fn [item]
                     (let [summary (bookmark-html item)
                           html (get-in item [:entry :contents "text/html"])
                           url (some-> item :entry :url uri/uri)
                           site (http/human-host-identifier url)]
                       (-> item
                           (assoc-in [:entry :contents "text/html"]
                                     (str summary "\n\n\n" html))
                           (assoc-in [:meta :source-key]
                                     (if (some? site)
                                       (keyword (str "bookmark-" (str site)))
                                       :bookmark))
                           (assoc :hash (make-item-hash url))
                           (assoc-in [:meta :source-name]
                                     (if (some? site)
                                       (format "[Bookmark: %s]" (str site))
                                       "[Bookmark]")))))])}))

(defn make-raw-bookmark-feed [url]
  (let [src (src/website url)]
    {:src src
     :tags #{:bookmark}
     :proc (proc/make
            :post [(fn [item]
                     (let [summary (bookmark-html item)
                           html (get-in item [:entry :contents "text/html"])
                           url (some-> item :entry :url uri/uri)
                           site (http/human-host-identifier url)]
                       (-> item
                           (assoc-in [:entry :contents "text/html"]
                                     (str summary "\n\n\n" html))
                           (assoc-in [:meta :source-key]
                                     (if (some? site)
                                       (keyword (str "bookmark-" (str site)))
                                       :bookmark))
                           (assoc :hash (make-item-hash url))
                           (assoc-in [:meta :source-name]
                                     (if (some? site)
                                       (format "[Bookmark: %s]" (str site))
                                       "[Bookmark]")))))])}))

(defn make-doc-feed [url]
  (let [src (src/doc url)]
    {:src src
     :tags #{:document}
     :proc (proc/make
            :post [(fn [item] (assoc-in item [:meta :source-name] "[Document]"))])}))

(defn make-hacker-news-filter [min-score min-score-match]
  (fn [item]
    (let [site (some-> item :entry :url uri/host)
          score (get-in item [:entry :score])
          title (some-> (get-in item [:summary :title]) string/lower-case)
          type (get-in item [:entry :type])]
      (not
       (or
        (and (= :story type)
             (>= score min-score))
        (and (= :story type)
             (re-find #"clojure|lisp|book|alan kay|futurism|rob pike|germany|file system|quobyte|storage" title)
             (>= score min-score-match))
        (and
         (some? site)
         (re-find #"nautil\.us|theatlantic|medium|youtube|theguardian|washingtonpost|99percentinvisible|theverge|phys\.org|bbc\.com"
                  site)
         (>= score min-score-match)))))))

(defn make-category-filter-deny [blacklist]
  (fn [item]
    (let [categories (set (get-in item [:entry :categories]))]
      (>= (count (intersection categories (set blacklist))) 1))))

(defn make-reddit-proc [min-score]
  (proc/make
   :filter (fn [item]
             (let [score (get-in item [:entry :score])]
               (< score min-score)))
   :post [(fn [item]
            (let [site (some-> item :entry :url uri/host)
                  path (some-> item :entry :url uri/path)]
              (cond
                (or (re-find #"i\.imgur\.com|i\.redd\.it|twimg\.com" site)
                    (re-find #"\.(jpg|jpeg|gif|png)$" path))
                (update-in item [:entry :contents "text/html"]
                           str "<img src=\"" (get-in item [:entry :url]) "\"/>")
                (re-find #"youtube|vimeo|reddit|redd\.it|open\.spotify\.com" site) item
                :else ((mercury-contents :keep-orig? true) item))))]))

(def ^:dynamic *srcs*
  {:twit-c3pb {:src (src/twitter-search "c3pb" (:twitter-api creds))
               :proc (proc/make
                      :filter (fn [item]
                                (->> item
                                     :entry
                                     :type
                                     #{:retweet})))
               :tags #{}
               :cron cron-twitter}
   ;; :twit-augsburg-pics {:src (src/twitter-search "augsburg filter:images"
   ;;                             (:twitter-api creds))
   ;;                      :proc (proc/make
   ;;                              :filter (fn [item]
   ;;                                        (let [type (get-in item [:entry :type])
   ;;                                              text (get-in item [:entry :contents "text/plain"])]
   ;;                                          (or (#{:retweet} type)
   ;;                                            (re-find #"pussy|porn|camsex|webcam" text)))))
   ;;                      :tags #{}
   ;;                      :cron cron-hourly}

   :twitter-timeline {:src (src/twitter-timeline (:twitter-api creds))
                      :tags #{}
                      :cron cron-twitter}

   :twit-berlin-pics {:src (src/twitter-search "#berlin filter:images"
                                               (:twitter-api creds))
                      :options #{:mark-read-on-view}
                      :proc (proc/make
                             :filter (fn [item]
                                       (let [type (get-in item [:entry :type])
                                             text (get-in item [:entry :contents "text/plain"])]
                                         (or
                                          (#{:retweet} type)
                                          (= (count (get-in item [:entry :entities :photos])) 0)
                                          (re-find #"pussy|porn|camsex|webcam" text))))
                             :proc [(fn [item]
                                      (assoc-in item [:entry :contents "text/html"]
                                                (format "<img src=\"%s\"/>"
                                                        (first (get-in item [:entry :entities :photos])))))])
                      :tags #{:pics}
                      :cron cron-hourly}



   :fefe {:src (src/selector-feed "https://blog.fefe.de"
                                  {:urls (S/descendant
                                          (S/find-in-text #"\[l\]"))
                                   :ts (S/tag :h3)
                                   :title (S/tag :li)
                                   :content (S/tag :li)}
                                  {:content  #(->> % first :content (drop 1))
                                   :author "fefe"
                                   :urls (fn [l] (map (fn [x] (->> x :attrs :href uri/uri)) l))
                                   :title (fn [l]
                                            (human/truncate
                                             (->> l
                                                  first
                                                  :content
                                                  (drop 1)
                                                  first
                                                  hickory-to-html
                                                  (converter/html2text))
                                             70))

                                   :ts #(->> %
                                             first
                                             :content
                                             first
                                             (parse-date-to-zoned-data-time "EEE MMM d yyyy"))})
          :tags #{:blog}
          :cron cron-hourly}

   :paulgraham {:src (src/selector-feed "http://www.paulgraham.com/articles.html"
                                  {:urls (S/descendant
                                          (S/tag :td)
                                          (S/tag :font)
                                          (S/tag :a))
                                   :ts (S/descendant
                                        (S/tag :td)
                                        (S/tag :font))
                                   :title (S/tag :title)
                                   :content (S/descendant
                                        (S/tag :td)
                                        (S/tag :font))}
                                  {:content  #(->> % first :content (drop 1))
                                   :author "Paul Graham"
                                   :urls (fn [l] (take 10 (map (fn [x] (->> x :attrs :href uri/uri)) l)))
                                   :ts #(->> %
                                             first
                                             hickory-to-html
                                             converter/html2text
                                             (re-find #"(January|February|March|April|May|June|July|August|September|October|November|December)\s+\d{4}")
                                             first
                                             ((fn [maybe-ts] (if (nil? maybe-ts) "January 0001" maybe-ts)))
                                             (str "1 ")
                                             (parse-date-to-zoned-data-time "d MMMM yyyy"))})
                :tags #{:blog}
                :cron cron-hourly}


   ;; :fefe {:src (src/feed "http://blog.fefe.de/rss.xml?html")
   ;;        :proc (proc/make
   ;;                :post [(proc/exchange
   ;;                         [:entry :descriptions]
   ;;                         [:entry :contents])]
   ;;                :filter (fn [item]
   ;;                          (->> item
   ;;                            :summary
   ;;                            :title
   ;;                            (re-find #"Zu Sarin"))))
   ;;        :tags #{:blog}
   ;;        :cron cron-hourly}

   :upwork-personal {:src (src/feed
                           "https://www.upwork.com/ab/feed/topics/atom?securityToken=c037416c760678f3b3aa058a7d31f4a0dc32a269dd2f8f947256d915b19c8219029b5846f9f18209e6890ca6b72be221653cf275086926945f522d934a200eec&userUid=823911365103362048&orgUid=823911365107556353")
                     :tags #{:jobs}
                     :cron cron-hourly
                     :proc (proc/make
                            :post [(fn [item] (update-in item [:meta :tags] disj :unread))]
                            :filter (fn [item]
                                      (let [wants (get-in item [:entry :nlp :nouns])
                                            haves #{"python" "clojure" "ceph" "c++" "c" "logstash" "kibana" "java" "linux" "quobyte" "rust" "kubernetes"}
                                            dontwants #{"php" "node.js" "javascript" "ecmascript" "wordpress" "scrapy" "django"}]
                                        (not
                                         (and (< (count (intersection wants dontwants)) 2)
                                              (>= (count (intersection wants haves)) 1))))))}

   :irq0 {:src (src/feed "http://irq0.org/rss.xml")
          :tags #{:blog}
          :cron cron-daily}

   ;; seems discontinued
   :oreilly-ideas {;;:src (src/feed "https://www.oreilly.com/ideas/feed.atom")
                   :proc (proc/make
                          :post [(mercury-contents :keep-orig? true)])
                   :tags #{:tech :magazine}}

   :oreilly-radar {:src (src/feed "https://www.oreilly.com/radar/feed/index.xml")
                   :cron cron-daily
                   :options #{:mark-read-on-view}
                   :proc (proc/make
                          :filter (fn [item] (string/includes? (get-in item [:summary :title]) "Four short links"))
                          :post [(mercury-contents :keep-orig? true)])
                   :tags #{:tech :magazine}}

   :oreilly-fourshortlinks {:src (src/feed "https://www.oreilly.com/radar/topics/four-short-links/feed/index.xml")
                            :cron cron-daily
                            :proc (proc/make
                                   :post [(proc/add-tag :highlight)
                                          (mercury-contents :keep-orig? false)])
                            :tags #{:tech}}

   :danluu {:src (src/feed "https://danluu.com/atom.xml")
            :tags #{:tech :blog}
            :cron cron-daily}

   :ridiculousfish {:src (src/feed "http://ridiculousfish.com/blog/atom.xml")
                    :tags #{:tech :blog}
                    :cron cron-daily}

   :rachelbythebay {:src (src/feed "https://rachelbythebay.com/w/atom.xml")
                    :tags #{:tech :blog}
                    :cron cron-daily}
   :chneukirchen {:src (src/feed "http://chneukirchen.org/trivium/index.atom")
                  :tags #{:digest}
                  :cron cron-daily}
   :codinghorror {:src (src/feed "http://feeds.feedburner.com/codinghorror")
                  :tags #{:tech :blog}
                  :cron cron-daily}
   :joel-on-software {:src (src/feed "https://www.joelonsoftware.com/feed/")
                      :tags #{:tech :blog}
                      :cron cron-daily}
   :summit-route {:src (src/feed "http://summitroute.com/blog/feed.xml")
                  :tags #{:tech :blog}
                  :proc (proc/make
                         :post [(proc/exchange
                                 [:entry :descriptions]
                                 [:entry :contents])])
                  :cron cron-daily}

   :joe-duffy {:src (src/feed "http://joeduffyblog.com/feed.xml" :deep? true)
               :tags #{:tech :blog}
               :cron cron-daily}

   :aphyr {:src (src/feed "https://aphyr.com/posts.atom")
           :tags #{:tech :blog}
           :cron cron-daily}

   :n99pi {:src (src/feed "https://feeds.feedburner.com/99pi")
           :tags #{:design}
           :proc (proc/make
                  :post [(proc/add-tag :highlight)])
           :cron cron-daily}

   :kottke {:src (src/feed "http://feeds.kottke.org/main")
            :tags #{:magazine}
            :options #{:mark-read-on-view}
            :cron cron-daily}

   :stackoverflow-engineering {:src (src/feed "https://stackoverflow.blog/engineering/feed/")
                               :tags #{:tech :sci}
                               :cron cron-daily}

   :soundcloud-backstage {:src (src/feed "https://developers.soundcloud.com/blog.rss")
                          :proc (proc/make
                                 :post [(proc/exchange
                                         [:entry :descriptions]
                                         [:entry :contents])])
                          :tags #{:tech}
                          :cron cron-daily}

   :weekly-programming-digest {:src (src/feed "http://feeds.feedburner.com/digest-programming")
                               :tags #{:tech :digest}
                               :proc (proc/make
                                      :post [(proc/add-tag :highlight)
                                             (proc/exchange
                                              [:entry :descriptions]
                                              [:entry :contents])])
                               :cron cron-daily}

   :uusenix-conferences {:src (src/feed "https://www.usenix.org/upcoming-conferences-feed")
                         :tags #{:events}
                         :cron cron-daily}

   :acm-queue {:src (src/feed "https://queue.acm.org/rss/feeds/queuecontent.xml"
                              :user-agent :browser
                              :force-update? true)
               :proc (proc/make
                      :post [(mercury-contents :keep-orig? true)])
               :tags #{:tech :magazine :sci}
               :cron cron-daily}

   :reddit-diy {:src (src/reddit "DIY" :hot)
                :cron cron-daily
                :tags #{:reddit}
                :proc (make-reddit-proc 2000)}

   :reddit-datahoarder {:src (src/reddit "DataHoarder" :hot)
                        :cron cron-daily
                        :tags #{:reddit :storage}
                        :proc (make-reddit-proc 10)}

   :reddit-fascinating {:src (src/reddit "fascinating" :hot)
                        :cron cron-daily
                        :tags #{:reddit}
                        :proc (make-reddit-proc 100)}

   :reddit-clojure {:src (src/reddit "Clojure" :hot)
                    :options #{:mark-read-on-view}
                    :cron cron-daily
                    :tags #{:reddit :tech}
                    :proc (make-reddit-proc 50)}

   :reddit-listentothis {:src (src/reddit "listentothis" :hot)
                         :cron cron-daily
                         :tags #{:reddit :music}
                         :proc (make-reddit-proc 150)}

   :reddit-europe {:src (src/reddit "europe" :top)
                   :cron cron-daily
                   :options #{:mark-read-on-view}
                   :tags #{:reddit :news}
                   :proc (make-reddit-proc 2000)}

   :reddit-educationalgifs {:src (src/reddit "educationalgifs" :hot)
                            :cron cron-daily
                            :tags #{:reddit}
                            :proc (make-reddit-proc 3000)}

   :reddit-Foodforthought {:src (src/reddit "Foodforthought" :hot)
                           :cron cron-daily
                           :tags #{:reddit}
                           :proc (make-reddit-proc 400)}

   :reddit-ifyoulikeblank {:src (src/reddit "ifyoulikeblank" :top)
                           :cron cron-daily
                           :tags #{:reddit}
                           :proc (make-reddit-proc 30)}

   :reddit-dataisbeautiful {:src (src/reddit "dataisbeautiful" :top)
                            :cron cron-daily
                            :tags #{:reddit :sci}
                            :proc (make-reddit-proc 5000)}

   :reddit-postrock {:src (src/reddit "postrock" :hot)
                     :cron cron-daily
                     :tags #{:reddit :music}
                     :proc (make-reddit-proc 15)}

   :reddit-albumaday {:src (src/reddit "albumaday" :hot)
                      :cron cron-daily
                      :tags #{:reddit :music}
                      :proc (make-reddit-proc 10)}

   :reddit-albumoftheday {:src (src/reddit "Albumoftheday" :top)
                          :cron cron-daily
                          :tags #{:reddit :music}
                          :proc (make-reddit-proc 5)}

   :reddit-listentoconcerts {:src (src/reddit "listentoconcerts" :top)
                             :cron cron-daily
                             :tags #{:reddit :music}
                             :proc (make-reddit-proc 5)}
   :reddit-indie {:src (src/reddit "indie" :hot)
                  :cron cron-daily
                  :tags #{:reddit :music}
                  :proc (make-reddit-proc 15)}

   :reddit-jwd {:src (src/reddit "jwd" :hot)
                :options #{:mark-read-on-view}
                :cron cron-daily
                :tags #{:reddit :berlin}
                :proc (make-reddit-proc 5)}

   :reddit-berlin {:src (src/reddit "berlin" :hot)
                   :options #{:mark-read-on-view}
                   :cron cron-daily
                   :tags #{:reddit :berlin}
                   :proc (make-reddit-proc 10)}

   :reddit-berlinshopping {:src (src/reddit "berlinshopping" :top)
                           :cron cron-daily
                           :tags #{:reddit :berlin}
                           :proc (make-reddit-proc 1)}

   :reddit-games {:src (src/reddit "games" :hot)
                  :options #{:mark-read-on-view}
                  :cron cron-daily
                  :tags #{:gaming :reddit}
                  :proc (make-reddit-proc 2000)}

   :reddit-pcgaming {:src (src/reddit "games" :hot)
                     :options #{:mark-read-on-view}
                     :cron cron-daily
                     :tags #{:gaming :reddit}
                     :proc (make-reddit-proc 200)}

   :reddit-storage {:src (src/reddit "storage" :hot)
                    :options #{:mark-read-on-view}
                    :cron cron-daily
                    :tags #{:storage :reddit}
                    :proc (make-reddit-proc 10)}

   :reddit-malelivingspace {:src (src/reddit "malelivingspace" :hot)
                                   :options #{:mark-read-on-view}
                    :cron cron-daily
                    :tags #{:recreation :reddit}
                    :proc (make-reddit-proc 100)}

   :gamasutra-pc {:src (src/feed "http://feeds.feedburner.com/GamasutraConsolePCNews")
                  :options #{:mark-read-on-view}
                  :proc (proc/make
                         :post [(mercury-contents :keep-orig? true)])
                  :cron cron-daily
                  :tags #{:gaming}}

   :muspy {:src (src/feed "https://muspy.com/feed?id=1ub5u1nk72w26hnpeiyfuvto9owxfd")
           :cron cron-daily
           :tags #{:music}}

   :mydealz-hot {:src (src/feed "https://www.mydealz.de/rss")
                 :options #{:mark-read-on-view}
                 :cron cron-daily
                 :tags #{:shopping}
                 :proc (proc/make
                        :filter (make-category-filter-deny ["Handys &amp; Tablets" "Home &amp; Living" "Fashion &amp; Accessoires"])
                        :post [(fn [item]
                                 (let [raw (:raw item)
                                       merchant (->> raw
                                                     :foreign-markup
                                                     (some #(when (= (.getName %) "merchant") %)))]
                                   (-> item
                                       (assoc-in [:entry :merchant] (some-> merchant (.getAttribute "name") .getValue))
                                       (assoc-in [:entry :price] (some-> merchant (.getAttribute "price") .getValue)))))])}

   :humblebundle {:src (src/feed "http://blog.humblebundle.com/rss")
                  :options #{:mark-read-on-view}
                  :proc (proc/make
                         :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                  :cron cron-daily
                  :tags #{:shopping}}

   ;; TODO   :metacritic {:src (src/feed "http://www.metacritic.com/rss/music"

   :vice {:src (src/feed "https://www.vice.com/en_us/rss" :deep? true :force-update? true)
          :options #{:mark-read-on-view}
          :tags #{:magazine}
          :cron cron-daily}

   :hn-top {:src (src/hn "topstories" :throttle-secs (* 23 60))
            :tags #{:hackernews}
            :options #{:mark-read-on-view}
            :proc (proc/make
                   :post [(mercury-contents :keep-orig? true)]
                   :filter (make-hacker-news-filter 350 150))}

   :hn-best {:src (src/hn "beststories" :throttle-secs (* 23 60))
             :tags #{:hackernews}
             :options #{:mark-read-on-view}
             :proc (proc/make
                    :post [(mercury-contents :keep-orig? true)]
                    :filter (make-hacker-news-filter 350 150))}

   :hn-ask {:src (src/hn "askstories" :throttle-secs (* 5 60  60))
            :tags #{:hackernews}
            :options #{:mark-read-on-view}
            :proc (proc/make
                   :post [(mercury-contents :keep-orig? true)]
                   :filter (make-hacker-news-filter 200 100))}

   :hn-show {:src (src/hn "showstories" :throttle-secs (* 5 60  60))
             :tags #{:hackernews}
             :options #{:mark-read-on-view}
             :proc (proc/make
                    :post [(mercury-contents :keep-orig? true)]
                    :filter (make-hacker-news-filter 200 100))}

   :xkcd {:src (src/feed "https://xkcd.com/rss.xml")
          :proc (proc/make
                 :post [(fn [item]
                          (let [hick (-> (hick/parse (get-in item [:entry :descriptions "text/html"])) hickory.core/as-hickory)
                                alt-text (-> (S/select
                                              (S/descendant
                                               (S/tag :img)) hick)
                                             first :attrs :alt)]
                            (-> item
                                (assoc-in [:entry :contents "text/html"]
                                          (str
                                           (get-in item [:entry :descriptions "text/html"])
                                           "<p>" alt-text "</p>")))))])
          :cron cron-daily
          :tags #{:comics}}

   :daily-wtf {:src (src/feed "http://syndication.thedailywtf.com/TheDailyWtf")
               :tags #{:recreation}
               :proc (proc/make
                      :filter (fn [item]
                                (let [title (get-in item [:summary :title])]
                                  (re-find #"^(Sponsor Post|CodeSOD|Error'd|Representative Line):" title)))
                      :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
               :cron cron-daily}

   :pixelenvy {:src (src/feed "https://feedpress.me/pxlnv")
               :cron cron-daily
               :options #{:mark-read-on-view :main-list-use-description}
               :tags #{:magazine}
               :proc (proc/make
                      :filter (fn [item]
                                (let [names (get-in item [:entry :nlp :names])
                                      dontwant #{"iOS" "App Store" "Apple" "Apple Music" "Apple Store" "MacOS" "OSX"}]
                                  (>= (count (intersection names dontwant)) 2))))}

   :atlantic-best-of {:src (src/feed "https://www.theatlantic.com/feed/best-of/")
                      :options #{:main-list-use-description}
                      :tags #{:magazine}
                      :cron cron-daily}

   :fail {:src
          (src/feed "http://irq0.org/404")}

   :newsletter-mailbox {:src (src/imap "imap://mail.cpu0.net/NEWSLETTER" (:imap creds))
                        :proc (proc/make
                               :post [(proc/add-tag-filter :highlight
                                                           #(= (:source-key %)
                                                               :newsletter-hacker_newsletter))
                                      (fn [item]
                                        (let [sane-html (some-> (get-in item [:entry :contents "text/html"])
                                                                hick/parse
                                                                hick/as-hickory
                                                                http/sanitize
                                                                http/blobify
                                                                hickory-to-html)
                                              headers (get-in item [:raw :headers])
                                              find-header (fn [k] (some-> (filter
                                                                           (fn [x]
                                                                             (= (some-> x first key string/lower-case) k))
                                                                           headers)
                                                                          first first val))
                                              mail-sender (or
                                                           (some-> item :raw :from first :name)
                                                           (find-header "sender")
                                                           (find-header "from")
                                                           (find-header "list-id")
                                                           "unknown")
                                              src-key (str "newsletter-"
                                                           (-> mail-sender
                                                               (string/replace #"[^\w]" "_")
                                                               string/lower-case))
                                              src-name (str "[NEWSLETTER: " mail-sender)]
                                          (-> item
                                              (assoc-in [:entry :contents "text/html"] sane-html)
                                              (assoc-in [:meta :source-key] src-key)
                                              (assoc-in [:meta :source-name] src-name))))])
                        :cron cron-daily}

   :pocoo-lucumr {:src (src/feed "http://lucumr.pocoo.org/feed.atom")
                  :tags #{:tech}
                  :cron cron-daily}

   :programmingisterrible {:src (src/feed "http://programmingisterrible.com/rss")
                           :tags #{:tech :blog}
                           :proc (proc/make
                                  :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                           :cron cron-daily}

   :preshing {:src (src/feed "http://preshing.com/feed")
              :tags #{:tech :blog}
              :cron cron-daily}

   ;; :isoblog {:src (src/feed "http://blog.koehntopp.info/index.php/feed/")
   ;;           :tags #{:blog :tech}
   ;;           :cron cron-daily}

   :elibendersky {:src (src/feed "http://eli.thegreenplace.net/feeds/all.atom.xml")
                  :tags #{:tech :blog}
                  :proc (proc/make
                         :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                  :cron cron-daily}

   ;; :abstrusegoose {:src (src/feed "http://abstrusegoose.com/atomfeed.xml")
   ;;                 :proc (proc/make
   ;;                        :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
   ;;                 :tags #{:comics}
   ;;                 :cron cron-daily}

   :cyanidehappiness {:src (src/feed "https://feeds.feedburner.com/Explosm")
                      :proc (proc/make
                             :post [(fn [item]
                                      (let [h (:hickory (http/fetch (get-in item [:entry :url]) :user-agent :browser))

                                            comic-elem (S/select (S/id "main-comic") h)
                                            comic-link (-> comic-elem
                                                           first
                                                           :attrs
                                                           :src)
                                            author-elem (S/select (S/id "comic-author") h)
                                            author (-> author-elem
                                                       first :content
                                                       (nth 2)
                                                       (subs 4))]
                                        (-> item
                                            (assoc-in [:entry :authors]
                                                      [author])
                                            (assoc-in [:entry :contents "text/html"]
                                                      (format "<img src=\"%s\"/>" comic-link)))))])
                      :tags #{:comics}
                      :cron cron-daily}

   ;; :joyoftech {:src (src/feed "http://www.joyoftech.com/joyoftech/jotblog/atom.xml" :force-update? false)
   ;;             :proc (proc/make
   ;;                    :post [(fn [item]
   ;;                             (let [html (http/fetch (get-in item [:entry :url]))
   ;;                                   comic-img (-> (S/select
   ;;                                                  (S/descendant
   ;;                                                   (S/tag :div)
   ;;                                                   (S/nth-child 3)
   ;;                                                   (S/tag :a)
   ;;                                                   (S/tag :img))
   ;;                                                  (:hickory html))
   ;;                                                 second :attrs)
   ;;                                   comic-src (-> comic-img :src (subs 3))
   ;;                                   comic-alt (-> comic-img :alt)
   ;;                                   comic-url (str "https://www.geekculture.com/joyoftech/" comic-src)]
   ;;                               (-> item
   ;;                                   (assoc-in [:entry :contents "text/html"]
   ;;                                             (format "<img src=\"%s\"/><p>%s</p>" comic-url comic-alt)))))])
   ;;             :tags #{:comics}
   ;;             :cron cron-daily}

   :theoatmeal {:src (src/feed "https://feeds.feedburner.com/oatmealfeed"
                               :force-update? false)
                :tags #{:comics}
                :proc (proc/make
                       :post [(fn [item]
                                (let [html (http/fetch (get-in item [:entry :url]))
                                      content (-> (S/select
                                                   (S/descendant
                                                    (S/id :comic))
                                                   (:hickory html))
                                                  first hickory.render/hickory-to-html)]
                                  (if (string? content)
                                    (-> item
                                        (assoc-in [:entry :contents "text/html"]
                                                  content))
                                    item)))])
                :cron cron-daily}

   :bookmark {:src nil
              :tags #{:bookmark}}
   :document {:src nil
              :tags #{:document}}

   :tumblr-worstofchefkoch {:src (src/feed "https://worstofchefkoch.tumblr.com/rss"
                                           :user-agent :bot)
                            :tags #{:recreation}
                            :cron cron-daily}

   :tumblr-powerlinesinanime {:src (src/feed "https://powerlinesinanime.tumblr.com/rss"
                                             :user-agent :bot)
                              :tags #{:recreation}
                              :cron cron-daily}

   :tumblr-awkwardstockphotos {:src (src/feed "http://awkwardstockphotos.com/rss"
                                              :user-agent :bot)
                               :tags #{:recreation}
                               :proc (proc/make
                                      :post [(proc/exchange [:entry :descriptions]
                                                            [:entry :contents])])
                               :cron cron-daily}

   :tumblr-weirdtumblrs {:src (src/feed "http://weirdtumblrs.tumblr.com/rss"
                                        :user-agent :bot)
                         :tags #{:recreation}
                         :proc (proc/make
                                :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                         :cron cron-daily}

   :tumblr-mcmensionhell {:src (src/feed "http://mcmansionhell.com/rss"
                                         :user-agent :bot)
                          :tags #{:recreation}
                          :proc (proc/make
                                 :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                          :cron cron-daily}

   :tumblr-runningahackerspace {:src (src/feed "https://runningahackerspace.tumblr.com/rss"
                                               :user-agent :bot)
                                :tags #{:recreation}
                                :proc (proc/make
                                       :post [(proc/exchange [:entry :descriptions]
                                                             [:entry :contents])])
                                :cron cron-daily}

   :orkpiraten {:src (src/feed "https://www.orkpiraten.de/blog/feed")
                :tags #{:recreation}}

   :berlintypography {:src (src/feed "https://berlintypography.wordpress.com/feed/")
                      :tags #{:design}
                      :cron cron-daily}

   :iconicphotos {:src (src/feed "https://iconicphotos.wordpress.com/feed/")
                  :tags #{:recreation :design}
                  :cron cron-daily}

   :googleprojectzero {:src (src/feed "https://googleprojectzero.blogspot.com/feeds/posts/default")
                       :tags #{:tech}
                       :cron cron-daily}

   :allthingsdistributed {:src (src/feed "http://www.allthingsdistributed.com/atom.xml" :force-update? false)
                          :tags #{:tech :blog}
                          :cron cron-daily}

   ;; :rub-onwebsecurity {:src (src/feed "https://web-in-security.blogspot.com/feeds/posts/default")
   ;;                     :tags #{:security :blog}
   ;;                     :cron cron-daily}

   :github-paperswelove {:src (src/feed "https://github.com/papers-we-love/papers-we-love/commits/master.atom")
                         :tags #{:sci}
                         :cron cron-daily}

   :usenix-multimedia {:src (src/feed "https://www.usenix.org/multimedia/rss.xml")
                       :proc (proc/make
                              :post [(fn [item]
                                       (let [url (get-in item [:entry :url])
                                             title (get-in item [:entry :title])]
                                         (if-let
                                          [conference-name
                                            (second
                                             (re-find
                                              #"www\.usenix\.org/conference/(\w+)/"
                                              (str url)))]
                                           (-> item
                                               (assoc-in [:entry :conference] conference-name)
                                               (assoc-in [:summary :title]
                                                         (format "[%s] %s" conference-name title)))
                                           item)))
                                     (mercury-contents)])
                       :tags #{:sci}
                       :cron cron-daily}

   :economist-scitech {:src (src/feed "https://www.economist.com/science-and-technology/rss.xml")
                       :options #{:mark-read-on-view}
                       :tags #{:magazine}
                       :proc (proc/make
                              :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                       :cron cron-daily}

   :theverge {:src (src/feed "https://www.theverge.com/rss/full.xml")
              :options #{:mark-read-on-view}
              :tags #{:magazine}
              :cron cron-daily}

   :vox {:src (src/feed "https://www.vox.com/rss/index.xml")
         :options #{:mark-read-on-view}
         :tags #{:magazine}
         :cron cron-daily}

   :ccc {:src (src/feed "http://www.ccc.de/de/rss/updates.rdf")
         :tags #{:politics}
         :proc (proc/make
                :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
         :cron cron-daily}

   :flarp {:src (src/feed "http://www.flarp.de/feed/")
           :proc (proc/make
                  :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
           :tags #{:berlin}
           :cron cron-daily}

   :inside-hpc {:src (src/feed "http://feeds.feedburner.com/insidehpc")
                :options #{:mark-read-on-view}
                :tags #{:tech}
                :proc (proc/make
                       :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                :cron cron-daily}

   :github-trending-c++ {:src (src/feed "https://mshibanami.github.io/GitHubTrendingRSS/monthly/c++.xml")
                         :tags #{:trends :tech}
                         :options #{:mark-read-on-view}
                         :proc (proc/make
                                :post [(proc/exchange [:entry :descriptions] [:entry :contents])
                                       (fn [item] (assoc-in item [:summary :ts]
                                                            (get-in item [:feed :pub-ts])))])
                         :cron cron-daily}

   :github-trending-java {:src (src/feed "https://mshibanami.github.io/GitHubTrendingRSS/monthly/java.xml")
                          :tags #{:trends :tech}
                          :options #{:mark-read-on-view}
                          :proc (proc/make
                                 :post [(proc/exchange [:entry :descriptions] [:entry :contents])
                                        (fn [item] (assoc-in item [:summary :ts]
                                                             (get-in item [:feed :pub-ts])))])
                          :cron cron-daily}

   :github-trending-rust {:src (src/feed "https://mshibanami.github.io/GitHubTrendingRSS/monthly/rust.xml")
                          :tags #{:trends :tech}
                          :options #{:mark-read-on-view}
                          :proc (proc/make
                                 :post [(proc/exchange [:entry :descriptions] [:entry :contents])
                                        (fn [item] (assoc-in item [:summary :ts]
                                                             (get-in item [:feed :pub-ts])))])
                          :cron cron-daily}

   :github-trending-clojure {:src (src/feed "https://mshibanami.github.io/GitHubTrendingRSS/monthly/clojure.xml")
                             :tags #{:trends :tech}
                             :options #{:mark-read-on-view}
                             :proc (proc/make
                                    :post [(proc/exchange [:entry :descriptions] [:entry :contents])
                                           (fn [item] (assoc-in item [:summary :ts]
                                                                (get-in item [:feed :pub-ts])))])
                             :cron cron-daily}

   :github-trending-python {:src (src/feed "https://mshibanami.github.io/GitHubTrendingRSS/monthly/python.xml")
                            :tags #{:trends :tech}
                            :options #{:mark-read-on-view}
                            :proc (proc/make
                                   :post [(proc/exchange [:entry :descriptions] [:entry :contents])
                                          (fn [item] (assoc-in item [:summary :ts]
                                                               (get-in item [:feed :pub-ts])))])
                            :cron cron-daily}

   :snia-storage {:src (src/feed "http://sniablog.org/feed/atom/")
                  :tags #{:storage}
                  :proc (proc/make
                         :post [(mercury-contents :keep-orig? true)])
                  :cron cron-daily}

   :katemats {:src (src/feed "https://katemats.com/blog?format=rss")
              :tags #{:blog}
              :cron cron-daily}

   :sachachua-emacs {:src (src/feed "http://sachachua.com/blog/category/emacs/feed")
                     :tags #{:emacs :blog}
                     :cron cron-daily}

   :pragmaticemacs {:src (src/feed "http://pragmaticemacs.com/feed/")
                    :tags #{:emacs :blog}
                    :cron cron-daily}

   :mattmight {:src (src/feed "http://matt.might.net/articles/feed.rss" :force-update? false)
               :tags #{:blog}
               :proc (proc/make
                      :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
               :cron cron-daily}

   :infoq-articles {:src (src/feed "https://www.infoq.com/feed/articles" :force-update? false)
                    :cron cron-daily
                    :proc (proc/make
                           :post [(fn [item]
                                    (if-let [title-without (second
                                                            (re-find #"Article: (.+)"
                                                                     (get-in item [:summary :title])))]
                                      (assoc-in item [:summary :title] title-without)
                                      item))
                                  (mercury-contents :keep-orig? true)])
                    :tags #{:tech}}

   ;; keep deep? true to get timestamp before replacing content with mercury
   :gatesnotes {:src (src/feed "https://www.gatesnotes.com/rss" :force-update? true :deep? true)
                :tags #{:blog}
                :proc (proc/make
                       :post [(mercury-contents :keep-orig? false)
                              (fn [item]
                                (let [h (-> (get-in item [:entry :contents "text/html"])
                                            hick/parse
                                            hick/as-hickory)
                                      foo (log/spy h)
                                      date-elem (S/select (S/class "article_top_dateline") h)
                                      date-str (-> date-elem first :content first)
                                      date (parse-date-to-zoned-data-time (time/formatter "MMMM dd, yyyy ")
                                                                          date-str)]
                                  (assoc-in item [:summary :ts] date)))
                              ])
                :cron cron-daily}

   :startup50 {:src (src/feed "http://startup50.com/feed/" :force-update? false)
               :tags #{:corporate}
               :proc (proc/make
                      :post [(mercury-contents :keep-orig? true)])
               :cron cron-daily}

   :clearskydata {:src (src/feed "https://www.clearskydata.com/blog/rss.xml")
                  :tags #{:corporate}
                  :cron cron-daily}

   :cloudendure {:src (src/feed "https://www.cloudendure.com/feed/")
                 :tags #{:corporate}}

   :elastifile {:src (src/feed "https://blog.elastifile.com/rss.xml")
                :tags #{:corporate}
                :cron cron-daily}

   :excelero {:src (src/feed "https://www.excelero.com/feed/")
              :tags #{:corporate}
              :cron cron-daily}

   :hedviginc {:src (src/feed "https://www.hedviginc.com/blog/rss.xml" :deep? true :force-update true)
               :tags #{:corporate}
               :cron cron-daily}

   :igneous {:src (src/feed "https://inside.igneous.io/rss.xml")
             :tags #{:corporate}
             :cron cron-daily}

   :iofabric {:src (src/feed "https://www.iofabric.com/feed/")
              :tags #{:corporate}
              :cron cron-daily}

   :quobyte {:src (src/feed "https://www.quobyte.com/blog/feed/")
             :tags #{:corporate}
             :cron cron-daily}

   :reduxio {:src (src/feed "https://beyondtheblocks.reduxio.com/feed/")
             :tags #{:corporate}
             :cron cron-daily}

   :smartiops {:src (src/feed "http://www.smartiops.com/feed/")
               :tags #{:corporate}
               :cron cron-daily}

   :snowflake {:src (src/feed "https://www.snowflake.net/feed/")
               :tags #{:corporate}
               :cron cron-daily}

   :softnas {:src (src/feed "https://www.softnas.com/wp/feed/")
             :tags #{:corporate}}

   :storpool {:src (src/feed "https://storpool.com/feed")
              :tags #{:corporate}
              :cron cron-daily}

   :kinvolk {:src (src/feed "https://kinvolk.io/blog/index.xml")
             :tags #{:corporate}
             :cron cron-daily}

   :gruenderszene-de {:src (src/feed "https://www.gruenderszene.de/feed")
                      :options #{:mark-read-on-view}
                      :proc (proc/make
                             :post [(mercury-contents :keep-orig? true)])
                      :tags #{:magazine :tech}
                      :cron cron-daily}

   :themorningpaper {:src (src/feed "https://blog.acolyer.org/feed/")
                     :proc (proc/make
                            :post [(proc/add-tag :highlight)])
                     :tags #{:sci :tech}
                     :cron cron-daily}

   :theregister-storage {:src (src/feed "https://www.theregister.co.uk/data_centre/storage/headlines.atom" :force-update? false)
                         :proc (proc/make
                                :post [(fn [item]
                                         (let [url (get-in item [:entry :url])
                                               fixed-url (string/replace url #"go\.theregister\.com/feed/" "")]
                                           (-> item
                                               (assoc-in [:entry :url] fixed-url))))
                                       (mercury-contents :keep-orig? false)])
                         :options #{:mark-read-on-view}
                         :tags #{:storage :magazine}
                         :cron cron-daily}

   :blocksandfiles {:src (src/wp-json "https://blocksandfiles.com/wp-json/")
                    :options #{:mark-read-on-view}
                    :tags #{:storage :magazine}
                    :cron cron-daily}

   :infostor {:src (src/feed "http://www.infostor.com/index/rssfaq/rss_article_and_wp.noncurrentissue.articles.infostor.html?block" :force-update? false)
              :proc (proc/make
                     :post [(mercury-contents :keep-orig? true)])
              :tags #{:storage :magazine}
              :cron cron-daily}

   ;; wordpress
   :scality {:src (src/selector-feed "https://www.scality.com/blog/"
                                     {:urls (S/descendant
                                             (S/class "read--more--button")
                                             (S/tag :a))
                                      :description (S/and
                                                    (S/tag :meta)
                                                    (S/attr :name #(= % "description")))
                                      :author (S/class "author")
                                      :ts (S/and
                                           (S/tag :meta)
                                           (S/attr :property #(= % "article:published_time")))
                                      :content (S/tag :article)}
                                     {:description #(-> % first :attrs :content)
                                      :author #(-> % first :content)
                                      :ts (fn [item]
                                            (time/zoned-date-time
                                             (time/formatter :iso-zoned-date-time)
                                             (-> item first :attrs :content)))})
             :tags #{:corporate}
             :cron cron-daily}

   ;; 2020-07-26 appears abandoned
   :qumulo-eng {:src (src/selector-feed "https://qumulo.com/blog/tag/engineering/"
                                        {:urls (S/descendant
                                                (S/class "listitem-media__body")
                                                (S/tag :a))
                                         :description (S/and
                                                       (S/tag :meta)
                                                       (S/attr :name #(= % "description")))
                                         :title (S/descendant (S/class "band") (S/tag :h1))
                                         :author (S/descendant (S/class "band") (S/tag :h5))
                                         :ts (S/descendant (S/class "band") (S/tag :h5))}
                                        {:title #(-> % first :content first)
                                         :description #(-> % first :attrs :content)
                                         :author #(some->> % first :content first
                                                           (re-seq #"\s+(.+) —") (map second))
                                         :ts #(some->> % first :content first
                                                       (re-find #"Posted (\w+ \d+, \d{4})")
                                                       second
                                                       (time/zoned-date-time (time/formatter "MMMM dd, yyyy")))})
                :tags #{:corporate}}

   :qumulo {:src (src/wp-json "https://qumulo.com/wp-json/")
            :tags #{:corporate}
            :cron cron-daily}

   :joyent {:src (src/feed "https://www.joyent.com/blog/feed")
            :proc (proc/make
                   :post [(mercury-contents :keep-orig? true)])
            :tags #{:corporate}}


   ;; no rss feed, Anonymous access to the WordPress Rest API has been restricted by Shield


   :wekaio {:src (src/wp-json "https://www.weka.io/wp-json/")
            :tags #{:corporate}}

   :quantum {:src (src/wp-json "https://blog.quantum.com/wp-json/")
             :tags #{:corporate}
             :cron cron-daily}

   :objectmatrix {:src (src/wp-json "http://object-matrix.com/wp-json/")
                  :tags #{:corporate}
                  :cron cron-daily}

   :collabora {:src (src/feed "https://www.collabora.com/newsroom-rss-feed.rss")
               :tags #{:corporate}
               :proc (proc/make
                      :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
               :cron cron-daily}

   :systemswelove {:src (src/website "https://systemswe.love/")
                   :tags #{:conference}
                   :cron cron-daily}

   :signalvnoise {:src (src/feed "https://m.signalvnoise.com/feed")
                  :tags #{:tech :blog}
                  :cron cron-daily}

   :seriouseats-foodlab {:src (src/feed "https://feeds.feedburner.com/SeriousEats-thefoodlab"
                                        :force-update? false)
                         :proc (proc/make
                                :post [(mercury-contents)])
                         :cron cron-daily
                         :tags #{:food}}
   :seriouseats-recipes {:src (src/feed "https://feeds.feedburner.com/seriouseats/recipes"
                                        :force-update? false)
                         :proc (proc/make
                                :post [(mercury-contents)])
                         :cron cron-daily
                         :tags #{:food}}

   :ccc-cpu {:src (src/feed "https://cpu.ccc.de/feed/")
             :tags #{:hackerspace}
             :cron cron-daily}

   :cbase {:src (src/feed "https://logbuch.c-base.org/feed")
           :tags #{:hackerspace :berlin}
           :cron cron-daily}

   :xhain {:src (src/feed "https://x-hain.de/de/index.xml")
           :tags #{:hackerspace :berlin}
           :cron cron-daily}

   :otherlandbooks {:src (src/feed "https://www.otherland-berlin.de/share/otherland_magazin.xml" :deep? true)
                    :tags #{:berlin}
                    :proc (proc/make
                           :post [(fn [item]
                                    (let [hickory (->
                                                   (hick/parse (get-in item [:entry :contents "text/html"]))
                                                   hick/as-hickory)
                                          content (-> (S/select (S/descendant (S/id "news-details")) hickory)
                                                      first)
                                          html (hickory-to-html content)]
                                      (-> item
                                          (assoc-in [:entry :contents]
                                                    {"text/html" html
                                                     "text/plain" (converter/html2text html)}))))])
                    :cron cron-daily}

   :manu-el {:src (src/feed "https://manuel-uberti.github.io/feed" :deep? true)
             :tags #{:tech :blog}
             :cron cron-daily}

   :uswitch-labs {:src (src/feed "https://medium.com/feed/uswitch-labs")
                  :tags #{:tech}
                  :cron cron-daily}

   :wirres {:src (src/feed "http://wirres.net/article/rss/full/0/10/")
            :tags #{:blog}
            :cron cron-daily}

   :nerdcore {:src (src/wp-json "https://nerdcore.de/wp-json/")
              :options #{:mark-read-on-view}
              :tags #{:magazine :recreation}}

   :meetups-my {:src (src/feed "https://www.meetup.com/events/rss/139002912/50f0499c4b59a743ecbf7d1e950eb8078ca2cf5b/going")
                :tags #{:berlin}
                :proc (proc/make
                       :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                :cron cron-daily}

   :berlin-backyard-fleamarkets {:src (src/website "http://polly.sternenlaub.de/fleamarkets/list")
                                 :tags #{:berlin}
                                 :cron cron-daily}

   :netflix-tech {:src (src/feed "https://medium.com/feed/@NetflixTechBlog")
                  :tags #{:tech}
                  :cron cron-daily}

   :nasa-image-of-the-day {:src (src/feed "https://www.nasa.gov/rss/dyn/image_of_the_day.rss")
                           :options #{:mark-read-on-view}
                           :proc (proc/make
                                  :post [(fn [item]
                                           (let [orig-img-url (some-> item :raw :enclosures first :url)
                                                 img-url (http/try-blobify-url! orig-img-url)
                                                 title (get-in item [:summary :title])
                                                 descr (get-in item [:entry :descriptions "text/plain"])
                                                 content {"text/plain" (string/join "\n"
                                                                                    [title orig-img-url descr])
                                                          "text/html" (hiccup.core/html
                                                                       [:div
                                                                        [:h1 title]
                                                                        [:img {:src img-url
                                                                               :orig-src orig-img-url}]
                                                                        [:p descr]])}]
                                             (-> item
                                                 (assoc-in [:entry :contents] content)
                                                 (assoc-in [:entry :lead-image-url] img-url))))])
                           :tags #{:pics}
                           :cron cron-daily}

   :wired {:src (src/feed "https://www.wired.com/feed")
           :options #{:mark-read-on-view}
           :proc (proc/make
                  :post [(mercury-contents)])
           :tags #{:magazine}
           :cron cron-daily}

   :erdgeist {:src (src/feed "https://erdgeist.org/blog/rss.xml")
              :tags #{:blog}
              :proc (proc/make
                     :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
              :cron cron-daily}

   :martin-kleppmann {:src (src/feed "https://feeds.feedburner.com/martinkl")
              :tags #{:blog}
              :cron cron-daily}

   :frankrieger {:src (src/feed "http://frank.geekheim.de/?feed=rss2")
                 :tags #{:blog}
                 :cron cron-daily}

   :corydoctorow {:src (src/feed "https://craphound.com/feed/")
                  :tags #{:politics}
                  :cron cron-daily}

   :juliaevans {:src (src/feed "https://jvns.ca/atom.xml")
                :tags #{:blog}
                :cron cron-daily}

   :willcrichton {:src (src/selector-feed "http://willcrichton.net/notes/"
                                          {:urls (S/class "note-link")
                                           :ts (S/class "date")
                                           :content (S/class "note")}
                                          :author (constantly ["Will Crichton"])
                                          {:content #(-> % first :attrs :content)
                                          :author (constantly ["Will Crichton"])
                                           :ts #(->> % first :content first (re-find #"\w+\s\d{1,2},\s\d{4}")
                                                     (parse-date-to-zoned-data-time
                                                      (time/formatter "MMMM d, yyyy")))})
                  :tags #{:blog}
                  :cron cron-daily}

   :slatestarcodex {:src (src/wp-json "http://slatestarcodex.com/wp-json/")
                    :tags #{:blog}
                    :cron cron-daily}

   :joschabach {:src (src/feed "http://bach.ai/feed.xml")
                :tags #{:blog}
                :cron cron-daily}

   :baldurbjarnason {:src (src/feed "https://www.baldurbjarnason.com/index.xml" :deep? true)
                     :tags #{:blog}
                     :cron cron-daily}

   :golem {:src (src/feed "https://rss.golem.de/rss_sub_media.php?token=7t10zqba")
           :options #{:mark-read-on-view}
           :proc (proc/make
                  :pre [(fn [item]
                          (let [raw-html (get-in item [:entry :descriptions "text/plain"])
                                html (some-> raw-html
                                             hick/parse
                                             hick/as-hickory
                                             (http/sanitize :remove-css? true)
                                             http/blobify
                                             hickory-to-html)]
                            (-> item
                                (assoc-in [:entry :descriptions]
                                          {"text/plain" ""})
                                (assoc-in [:entry :contents]
                                          {"text/html" html
                                           "text/plain" (converter/html2text html)}))))])
           :tags #{:tech :itnews}
           :cron cron-daily}

   ;; :lwn-weekly {:src (src/website+paywall
   ;;                     "https://lwn.net/current/bigpage"
   ;;                     (fn [] (let [cs (clj-http.cookies/cookie-store)]
   ;;                             (http-client/post "https://lwn.net/Login/"
   ;;                               {:form-params (:lwn creds)
   ;;                                :cookie-store cs})
   ;;                             cs)))
   ;;              :proc (proc/make
   ;;                      :pre [(fn [item]
   ;;                              (let [content (-> (S/select
   ;;                                                  (S/descendant
   ;;                                                    (S/class "ArticleText"))
   ;;                                                  (:hickory item))
   ;;                                              first)
   ;;                                    html (hickory-to-html content)
   ;;                                    html-no-comment-count
   ;;                                    (-> html
   ;;                                      (string/replace #">Comments .+posted.<" ">Comments<")
   ;;                                      (string/replace #">comments: .+<" ""))]
   ;;                                (assoc-in item [:entry :contents "text/html"] html-no-comment-count)))])
   ;;              :tags #{:tech :itnews}
   ;;              :cron cron-daily}
   :lwn-weekly {:src (src/website+paywall "https://lwn.net/current"
                                          (fn [] (let [cs (http-cookies/cookie-store)]
                                                   (http-client/post "https://lwn.net/Login/"
                                                                     {:form-params (:lwn creds)
                                                                      :cookie-store cs})
                                                   cs))
                                          :user-agent :browser)
                :proc (proc/make
                       :pre [(fn [item]
                               (let [new-items
                                     (->> (S/select (S/descendant
                                                     (S/class "SummarySection"))
                                                    (:hickory item))
                                          first :content
                                          (reduce (fn [r item]
                                                    (let [hl-hick (S/select (S/and (S/tag :h3) (S/class "SummaryHL")) item)
                                                          meta-hick (S/select (S/class "FeatureByline") item)
                                                          title (-> hl-hick first :content first :content first)
                                                          author (-> meta-hick first :content second :content first)
                                                          index (dec (count r))]
                                                      (log/info title author index)
                                                      (cond
                                                        (string? title)
                                                        (conj r {:title title
                                                                 :hick []
                                                                 :author nil})

                                                        (string? author)
                                                        (assoc-in r [index :author] author)

                                                        (>= index 0)
                                                        (update-in r [index :hick] conj item)

                                                        :else
                                                        r)))
                                                  []))]
                                 ;; Note: html already absolutified / blobified / sanitized by fetcher!
                                 (for [{:keys [title hick author]} new-items
                                       :let [html (-> {:type :element
                                                       :attrs nil
                                                       :tag :div
                                                       :content hick}
                                                      hickory-to-html)]]
                                   (-> item
                                       (assoc :hash (make-item-hash title))
                                       (assoc-in [:summary :title] title)
                                       (assoc-in [:entry :title] title)
                                       (assoc :hickory hick)
                                       (assoc-in [:entry :authors] [author])
                                       (assoc-in [:entry :descriptions]
                                                 {"text/plain" ""})
                                       (assoc-in [:entry :contents]
                                                 {"text/html" html
                                                  "text/plain" (converter/html2text html)})))))])
                :tags #{:tech :itnews}
                :cron cron-daily}

   :thenewstack {:src (src/feed "https://thenewstack.io/feed/")
                 :options #{:mark-read-on-view}
                 :tags #{:tech :itnews}
                 :cron cron-daily}

   :manybutfinite {:src (src/feed "https://manybutfinite.com/feed.xml")
                   :tags #{:zzz-abandoned-blogs}}

   :mechanical-sympathy {:src (src/feed "https://mechanical-sympathy.blogspot.com/feeds/posts/default")
                         :tags #{:tech}
                         :cron cron-daily}

   :igoro {:src (src/feed "http://feeds.feedburner.com/igoro")
           :tags #{:tech}
           :cron cron-daily}

   :randomascii {:src (src/feed "https://randomascii.wordpress.com/feed/")
                 :tags #{:tech}
                 :cron cron-daily}

   :sdn-clinic {:src (src/wp-json "https://blog.sdn.clinic/wp-json/")
                :tags #{:tech}
                :cron cron-daily}

   :scotthyoung {:src (src/feed "https://www.scotthyoung.com/blog/feed/")
                 :tags #{:zzz-abandoned-blogs}}

   :kallasch& {:src (src/feed "http://fetchrss.com/rss/5cc575018a93f856418b45675cc575408a93f807428b4567.xml")
               :tags #{:berlin}
               :cron cron-daily}

   :petersburg-artspace {:src (src/feed "http://fetchrss.com/rss/5cc575018a93f856418b45675cc574d18a93f8303d8b4567.xml")
                         :tags #{:berlin}
                         :cron cron-daily}

   :scrively {:src (src/wp-json "http://scrively.org/wp-json/")
              :tags #{:blog}
              :cron cron-daily}
   :longform {:src (src/feed "https://longform.org/feed.rss")
              :proc (proc/make
                     :pre [(mercury-contents)
                           (fn [item] (let [redirect-page (http/fetch (get-in item [:entry :url]))
                                            real-article-link (some-> (S/select
                                                                       (S/descendant
                                                                        (S/class "post__link"))
                                                                       (:hickory redirect-page))
                                                                      first :attrs :href)]
                                        (assoc-in item [:entry :url] (uri/uri real-article-link))))])
              :tags #{:magazine}
              :options #{:main-list-use-description}
              :cron cron-daily}

   :rogerebert {:src (src/feed "https://www.rogerebert.com/feed")
                :options #{:mark-read-on-view}
                :tags #{:movies}
                :cron cron-daily}

   :movieweb-reviews {:src (src/feed "https://movieweb.com/rss/movie-reviews/")
                      :options #{:mark-read-on-view}
                      :proc (proc/make
                             :post [(mercury-contents)])
                      :tags #{:movies}
                      :cron cron-daily}

   :screenrant {:src (src/feed "https://screenrant.com/feed/")
                :options #{:mark-read-on-view}
                :tags #{:movies :gaming}
                :cron cron-daily}

   :comingsoon {:src (src/wp-json "https://www.comingsoon.net/wp-json/")
                :options #{:mark-read-on-view}
                :tags #{:movies :gaming}
                :cron cron-daily}

   :filmfestivals {:src (src/feed "https://www.filmfestivals.com/taxonomy/term/31%2B32%2B590757/0/feed")
                   :options #{:mark-read-on-view}
                   :tags #{:movies}
                   :cron cron-daily}

   :pedestrianobservations {:src (src/feed "https://pedestrianobservations.com/feed/")
                            :tags #{:zzz-abandoned-blogs}}

   :drewdevault {:src (src/feed "https://drewdevault.com/feed.xml")
                 :tags #{:blog}
                 :proc (proc/make
                        :post [(proc/exchange
                                [:entry :descriptions]
                                [:entry :contents])])
                 :cron cron-daily}

   :dereksivers {:src (src/feed "https://sivers.org/en.atom")
                 :tags #{:blog}
                 :cron cron-daily}

   :waitbutwhy {:src (src/wp-json "https://waitbutwhy.com/wp-json/" :user-agent :bot)
                :tags #{:blog}
                :cron cron-daily}

   :ribbonfarm {:src (src/wp-json "https://www.ribbonfarm.com/wp-json/")
                :tags #{:zzz-abandoned-blogs}}

   :unixe {:src (src/wp-json "https://www.unixe.de/wp-json/")
           :cron cron-daily
           :tags #{:blog}}

   :syonyk {:src (src/feed "https://syonyk.blogspot.com/feeds/posts/default")
            :tags #{:blog}
            :cron cron-daily}

   :absorptions {:src (src/feed "http://www.windytan.com/feeds/posts/default")
                 :tags #{:blog}
                 :cron cron-daily}

   :guzey {:src (src/selector-feed
                 "https://guzey.com/archive/"
                 {:urls (S/descendant
                         (S/class :article)
                         (S/and
                          (S/tag :a)
                          (S/attr :href #(string/starts-with? % "https://guzey.com/"))))
                  :title (S/class :article-title)
                  :ts (S/descendant (S/class :post-date)
                                    (S/tag :time))
                  :author (constantly ["Alexey Guzey"])
                  :content (S/tag :article)}
                 {:author (constantly ["Alexey Guzey"])
                  :ts (fn [hick]
                        (let [raw (->> hick first :content first)]
                          (when (string? raw)
                            (parse-date-to-zoned-data-time (time/formatter :iso-date) raw))))})
           :proc (proc/make
                  :post [(fn [item] (-> item
                                       (assoc :hash (make-item-hash
                                                     (some-> item :entry :url uri/uri)))))])
           :tags #{:blog}
           :cron cron-daily}

   :granolashotgun {:src (src/feed "https://www.granolashotgun.com/granolashotguncom?format=rss")
                    :tags #{:design}
                    :cron cron-daily}

   :ga-quobyte {:src (src/feed "https://www.google.com/alerts/feeds/12214230541754356951/11912435152804193698")
                :options #{:mark-read-on-view}
                :proc (proc/make
                       :post [(proc/add-tag :highlight)])
                :tags #{:google-alert :tech}
                :cron cron-daily}

   :ga-marcellauhoff {:src (src/feed "https://www.google.com/alerts/feeds/12214230541754356951/17432466600270792644")
                      :options #{:mark-read-on-view}
                      :proc (proc/make
                             :post [(proc/add-tag :highlight)])
                      :tags #{:google-alert}
                      :cron cron-daily}

   :ga-ml-irq0-org {:src (src/feed "https://www.google.com/alerts/feeds/12214230541754356951/6287790772305614620")
                    :options #{:mark-read-on-view}
                    :proc (proc/make
                           :post [(proc/add-tag :highlight)])
                    :tags #{:google-alert}
                    :cron cron-daily}

   :ga-job-search {:src (src/feed "https://www.google.com/alerts/feeds/12214230541754356951/18101484729339824556")
                   :options #{:mark-read-on-view}
                   :proc (proc/make
                          :post [(proc/add-tag :highlight)])
                   :tags #{:google-alert}
                   :cron cron-daily}

   :gutmet {:src (src/feed "https://gutmet.org/blog/feed.rss")

            ;; (src/selector-feed
            ;;       "https://gutmet.org/blog/timeline.html"
            ;;       {:urls (S/descendant (S/class "content") (S/tag "a"))
            ;;        :ts (S/class "time")
            ;;        :author (constantly ["Alexander Weinhold"])
            ;;        :content (S/descendant (S/class "content"))}
            ;;       {:content #(-> % first :attrs :content)
            ;;        :author (constantly ["Alexander Weinhold"])
            ;;        :ts #(->> %
            ;;                  first :content first
            ;;                  string/trim
            ;;                  (parse-date-time-to-zoned-data-time "yyyy-MM-dd HH:mm"))})
            :tags #{:blog}
            :cron cron-daily}

   :n-gate {:src (src/feed "http://n-gate.com/index.atom")
            :tags #{:blog}
            :cron cron-daily}

   :youtube-rickbeato {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCJquYOG5EL82sKTfH9aMA9Q")
                       :options #{:mark-read-on-view}
                       :tags #{:music :youtube-channel}
                       :cron cron-daily}

   :youtube-japanology {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCvDzbjrw5B5RW10HyJbxX9g")
                        :options #{:mark-read-on-view}
                        :tags #{:recreation :youtube-channel}
                        :cron cron-daily}


   :lastweektonight {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UC3XTzVzaHQEd30rQbuvCtTQ")
                     :options #{:mark-read-on-view}
                     :tags #{:youtube-channel}
                     :proc (proc/make
                            :post [(proc/add-tag :download)])
                     :cron cron-daily}

   :lgr {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCLx053rWZxCiYWsBETgdKrQ")
         :options #{:mark-read-on-view}
         :tags #{:youtube-channel :recreation :retro}
         :cron cron-daily}

   :daseule {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCD5XwjZDCDEXhVPW_jc0fbg")
             :options #{:mark-read-on-view}
             :tags #{:youtube-channel :ernährung}
             :cron cron-daily}

   :jonathanpie {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCO79NsDE5FpMowUH1YcBFcA")
                 :options #{:mark-read-on-view}
                 :tags #{:youtube-channel :recreation}
                 :cron cron-daily}

   :ave {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UChWv6Pn_zP0rI6lgGt3MyfA")
         :options #{:mark-read-on-view}
         :tags #{:youtube-channel :recreation}
         :cron cron-daily}

   :terra-x-lesch {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UC5E9-r42JlymhLPnDv2wHuA")
                   :options #{:mark-read-on-view}
                   :tags #{:youtube-channel :sci}
                   :cron cron-daily}

   :cppcon {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCMlGfpWw-RUdWX_JbLCukXg")
            :options #{:mark-read-on-view}
            :tags #{:youtube-channel :conference}
            :cron cron-daily}

   :mit-distributed-systems {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UC_7WrbZTCODu1o_kfUMq88g")
                             :options #{:mark-read-on-view}
                             :proc (proc/make
                                    :post [(proc/add-tag :download)])
                             :tags #{:youtube-channel :course}
                             :cron cron-daily}

   :clojure-tv {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCaLlzGqiPE2QRj6sSOawJRg")
                :options #{:mark-read-on-view}
                :tags #{:youtube-channel :tech :clojure}
                :cron cron-daily}

   :youtube-usenix {:src (src/feed
                          "https://www.youtube.com/feeds/videos.xml?channel_id=UC4-GrpQBx6WCGwmwozP744Q")
                    :options #{:mark-read-on-view}
                    :tags #{:youtube-channel :tech :sci}
                    :cron cron-daily}

   :mailab {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCyHDQ5C6z1NDmJ4g6SerW8g")
            :options #{:mark-read-on-view}
            :tags #{:youtube-channel :sci}
            :cron cron-daily}

   :youtube-acm {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCPyA0XmU6aS4JCwVoIBTmIQ")
                 :options #{:mark-read-on-view}
                 :tags #{:youtube-channel :sci}
                 :cron cron-daily}

   :natswhatireckon {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCEFW1E8QzP-hKxjO2Rj68wg")
                     :options #{:mark-read-on-view}
                     :tags #{:youtube-channel :recreation :cooking}
                     :cron cron-daily}

   :oxfordunion {:src (src/feed
                       "https://www.youtube.com/feeds/videos.xml?channel_id=UCY7dD6waquGnKTZSumPMTlQ")
                 :options #{:mark-read-on-view}
                 :tags #{:youtube-channel :debates}
                 :cron cron-daily}

   :emacsrocks {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCkRmQ_G_NbdbCQMpALg6UPg")
                :options #{:mark-read-on-view}
                :tags #{:youtube-channel :tech :emacs}
                :cron cron-daily}

   :theartoftheproblem {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCotwjyJnb-4KW7bmsOoLfkg")
                        :options #{:mark-read-on-view}
                        :tags #{:youtube-channel :tech}
                        :cron cron-daily}

   :kurzgesagt {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCsXVk37bltHxD1rDPwtNM8Q")
                :options #{:mark-read-on-view}
                :tags #{:youtube-channel :sci}
                :cron cron-daily}

   :crashcourse {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCX6b17PVsYBQ0ip5gyeme-Q")
                 :options #{:mark-read-on-view}
                 :tags #{:youtube-channel :sci}
                 :cron cron-daily}

   :ted {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCAuUUnT6oDeKwE6v1NGQxug")
         :options #{:mark-read-on-view}
         :tags #{:youtube-channel :sci}
         :cron cron-daily}

   :computer-history-museum {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCHDr4RtxwA1KqKGwxgdK4Vg")
                             :options #{:mark-read-on-view}
                             :tags #{:youtube-channel :sci}
                             :cron cron-daily}

   :computerphile {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA")
                   :options #{:mark-read-on-view}
                   :tags #{:youtube-channel :sci}
                   :cron cron-daily}

   :two-minute-papers {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCbfYPyITQ-7l4upoX8nvctg")
                       :options #{:mark-read-on-view}
                       :tags #{:youtube-channel :sci}
                       :cron cron-daily}

   :doktor-whatson {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCesjlAoEgN_Sz_cKTvKEmmw")
                    :options #{:mark-read-on-view}
                    :tags #{:youtube-channel}
                    :cron cron-daily}

   :ultralativ {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCb-cM927p9tWkqmpOrBabOQ")
                :options #{:mark-read-on-view}
                :tags #{:youtube-channel}
                :cron cron-daily}

   :lexfridman {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCSHZKyawb77ixDdsGog4iWA")
                :options #{:mark-read-on-view}
                :tags #{:youtube-channel}
                :cron cron-daily}

   :realworldtech {:src (src/wp-json "https://www.realworldtech.com/wp-json/")
                   :options #{:mark-read-on-view}
                   :tags #{:tech}
                   :cron cron-daily}

   :pavelmayer {:src (src/feed "https://pavelmayer.de/feed/")
                :tags #{:blog}
                :cron cron-daily}

   :dmeister {:src (src/feed "https://dmeister.github.io/blog/atom.xml")
              :tags #{:blog}
              :cron cron-daily}

   :internet-protocol-journal {:src (src/website "https://ipj.dreamhosters.com/internet-protocol-journal/issues/current-issue/")
                               :tags #{:tech :sci}
                               :proc (proc/make
                                      :post [(proc/add-tag :highlight)
                                             (fn [item]
                                               (let [article (S/select
                                                              (S/tag :article) (:hickory item))
                                                     html (-> {:type :element
                                                               :attrs nil
                                                               :tag :article
                                                               :content article}
                                                              infowarss.http/sanitize hickory-to-html)]
                                                 (assoc-in item [:entry :contents]
                                                           {"text/html" html
                                                            "text/plain" (converter/html2text html)})))])
                               :cron cron-daily}
   :isotopp {:src (src/feed "https://blog.koehntopp.info/feed.xml")
             :tags #{:blog}
             :cron cron-daily}

   :netzpolitik {:src (src/wp-json "https://netzpolitik.org/wp-json/")
                 :options #{:mark-read-on-view}
                 :tags #{:politics :magazine}
                 :cron cron-daily}
   :volksverpetzer {:src (src/wp-json "https://www.volksverpetzer.de/wp-json/")
                    :options #{:mark-read-on-view}
                    :tags #{:politics}
                    :cron cron-daily}

   :correctiv {:src (src/wp-json "https://correctiv.org/wp-json/")
               :options #{:mark-read-on-view}
               :tags #{:politics}
               :cron cron-daily}

   :malleablesystems {:src (src/feed "https://malleable.systems/blog/index.xml")
                      :tags #{:tech :emacs}
                      :cron cron-daily}

   :tagesschau {:src (src/feed "https://www.tagesschau.de/xml/rss2")
                :cron cron-hourly
                :options #{:mark-read-on-view :main-list-use-description}
                :proc (proc/make
                       :filter (fn [item]
                                 (let [title (get-in item [:summary :title])
                                       url (get-in item [:entry :url])]
                                   (or
                                    (re-find #"Liveblog: \+\+" title)
                                    (re-find #"/(sport|fussball)/" (uri/path url)))))
                       :post [(mercury-contents)])
                :tags #{:news}}

   :spiegel-online {:src (src/feed "https://www.spiegel.de/schlagzeilen/index.rss")
                    :cron cron-hourly
                    :proc (proc/make
                           :filter (fn [item]
                                     (or ((make-category-filter-deny ["Sport"]) item)
                                         (re-find #"(?smi)Weiterlesen mit.*Ihre Vorteile mit SPIEGEL.*Sie haben bereits ein Digital-Abonnement"
                                                  (get-in item [:entry :contents "text/plain"]))
                                         (empty? (get-in item [:entry :authors]))))
                           :pre
                           [(fn [item]
                              (let [h (:hickory (http/fetch (get-in item [:entry :url])
                                                            :remove-css? true
                                                            :simplify? true
                                                            :user-agent :browser))
                                    main (S/select (S/tag :main) h)
                                    author-tags (S/select (S/and
                                                           (S/tag :meta)
                                                           (S/attr :name #(= % "author"))) h)
                                    author-tag (or (-> author-tags first :attrs :content) "")

                                    authors (->> (string/split author-tag #",")
                                                 (map string/trim)
                                                 (remove #(= % "DER SPIEGEL")))

                                    html (-> {:type :element
                                              :attrs nil
                                              :tag :main
                                              :content main}
                                             infowarss.http/sanitize hickory-to-html)]
                                (-> item
                                    (assoc-in [:entry :authors] authors)
                                    (assoc-in [:entry :contents]
                                              {"text/html" html
                                               "text/plain" (converter/html2text html)}))))]
                           )
                    :options #{:mark-read-on-view :main-list-use-description}
                    :tags #{:news}}
   :taz-online {:src (src/feed "https://taz.de/!s=&ExportStatus=Intern&SuchRahmen=Online;atom/")
                :cron cron-hourly
                :proc (proc/make
                       :post [(mercury-contents)
                              (proc/exchange [:entry :descriptions] [:entry :contents])])
                :options #{:mark-read-on-view :main-list-use-description}
                :tags #{:news}}

   :tagesspiegel {:src (src/feed "https://www.tagesspiegel.de/contentexport/feed/home")
                  :cron cron-hourly
                  :proc (proc/make
                         :filter (make-category-filter-deny ["Sport"])
                         :post [(mercury-contents)])
                  :options #{:mark-read-on-view :main-list-use-description}
                  :tags #{:news}}

   :nytimes-mostshared {:src (src/feed
                              "https://rss.nytimes.com/services/xml/rss/nyt/MostShared.xml")
                        :cron cron-hourly
                        :proc (proc/make
                         :post [(mercury-contents)])
                        :options #{:mark-read-on-view :main-list-use-description}
                        :tags #{:news}}

   :nytimes-mostemailed {:src (src/feed
                              "https://rss.nytimes.com/services/xml/rss/nyt/MostEmailed.xml")
                         :cron cron-hourly
                         :proc (proc/make
                                :post [(mercury-contents)])
                         :options #{:mark-read-on-view :main-list-use-description}
                         :tags #{:news}}

   :nytimes-mostviewed {:src (src/feed
                              "https://rss.nytimes.com/services/xml/rss/nyt/MostViewed.xml")
                         :cron cron-hourly
                         :proc (proc/make
                                :post [(mercury-contents)])
                         :options #{:mark-read-on-view :main-list-use-description}
                         :tags #{:news}}

   :nytimes-top {:src (src/feed
                       "https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml")
                         :cron cron-hourly
                 :proc (proc/make
                        :filter (make-category-filter-deny ["Sport" "Fraternities and Sororities" "Horse Racing" "Contests and Prizes"])
                        :post [(mercury-contents)])
                 :options #{:mark-read-on-view :main-list-use-description}
                 :tags #{:news}}
  })

;;;; todo


(comment
  "https://www.questionablecontent.net/QCRSS.xml"
  "http://www.radiolab.org/"
  "http://ask.metafilter.com/")

(comment (src/selector-feed "https://guzey.com/archive/"
                            {:urls (S/descendant
                                    (S/class :article)
                                    (S/and
                                     (S/tag :a)
                                     (S/attr :href #(string/starts-with? % "../"))))
                             :ts (S/descendant (S/class :post-date)
                                               (S/tag :i)
                                               S/first-child)
                             :author (constantly "Alexey Guzey")
                             :content (S/tag :article)}
                            {:author (constantly "Alexey Guzey")
                             :ts #(->> %
                                       first
                                       :content
                                       first
                                       (time/zoned-date-time (time/formatter :iso-date)))}))

(comment
  (src/selector-feed "https://guzey.com/archive/"
                     {:urls (S/descendant
                             (S/class :article)
                             (S/and
                              (S/tag :a)
                              (S/attr :href #(string/starts-with? % "../"))))
                      :title (S/class :article-title)
                      :ts (S/descendant (S/class :post-date)
                                        (S/tag :time))
                      :content (S/tag :article)}
                     {:author (constantly "Alexey Guzey")
                      :urls (fn [elems]
                              (map (fn [elem]
                                     (let [article-path (-> elem :attrs :href)]
                                       (str "https://guzey.com/"
                                            (subs article-path 3))))
                                   elems))
                      :ts #(->> %
                                first
                                :content
                                first
                                (time/zoned-date-time (time/formatter :iso-date)))}))
