(ns infowarss.core
  (:require
   [taoensso.timbre :as log]
   [infowarss.src :as src]
   [infowarss.converter :as converter]
   [infowarss.fetch.http]
   [clj-time.periodic :refer [periodic-seq]]
   [clj-time.core :as time]
   [hiccup.core :refer [html]]
   [clj-time.coerce :as tc]
   [infowarss.postproc :as proc]
   [clojure.string :as string]
   [hara.io.scheduler :as sched]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [mount.core :refer [defstate]]
   [hara.time.joda]))

;;;; Core configuration and data structures

;;; config
(def config (edn/read-string (slurp (io/resource "config.edn"))))
(def creds (edn/read-string (slurp (io/resource "credentials.edn"))))


;;; Sources

(def cron-hourly "23 42 * * * * *")
(def cron-daily "0 42 23 * * * *")

;;; Bookmarks

(defn bookmark-html [i]
  (html
    [:h1 (get-in i [:summary :title])]
    [:div {:class "summary"}
     [:ul
      [:li [:span {:class "key"} "URL: "]
       [:a {:href (get-in i [:entry :url])} (get-in i [:entry :url])]]
      [:li [:span {:class "key"} "Added: "] (tc/to-string (time/now))]
      [:li [:span {:class "key"} "Published: "] (tc/to-string (get-in i [:summary :ts]))]
      [:li [:span {:class "key"} "Next Page URL: "]
       [:a {:href (get-in i [:entry :next-page-url])} (get-in i [:entry :next-pageurl])]]]]
    [:div {:class "description"}
     [:h2 "Summary"]
     [:p (get-in i [:entry :descriptions "text/plain"])]]
    [:div {:class "nlp"}
     [:h2 "Names / Places"]
     [:p (map (fn [name] [:span [:a {:href (str "https://www.startpage.com/do/search?query=" name)} (str " " name " ")] "&nbsp;" ]) (get-in i [:entry :nlp :names]))]]
    [:h1 "Content"]))


(defn make-bookmark-feed [url]
  (let [src (src/mercury url (get-in creds [:mercury :api-key]))]
    {:src src
     :tags #{:bookmark}
     :proc (proc/make
             :post [(fn [item]
                     (let [summary (bookmark-html item)
                           html (get-in item [:entry :contents "text/html"])]
                       (-> item
                         (assoc-in [:entry :contents "text/html"]
                           (str summary "\n\n\n" html))
                         (assoc-in [:meta :source-name]
                           "[Bookmark]"))))])}))

(defn make-doc-feed [url]
  (let [src (src/doc url)]
    {:src src
     :tags #{:document}
     :proc (proc/make
             :post [(fn [item] (assoc-in item [:meta :source-name] "[Document]"))])}))


(defn make-hacker-news-filter [min-score min-score-match]
  (fn [item]
    (let [site (some-> item :entry :url .getHost)
          score (get-in item [:entry :score])
          title (some-> (get-in item [:summary :title]) string/lower-case)
          type (get-in item [:entry :type])]
      (not
        (or
          (and (= :story type)
            (>= score min-score))
          (and (= :story type)
            (re-find #"clojure|lisp|book|alan kay" title)
            (>= score min-score-match))
          (and
            (some? site)
            (re-find #"theatlantic|medium|youtube|nytimes|theguardian|washingtonpost|99percentinvisible|theverge|phys.org|bbc.com"
              site)
            (>= score min-score-match)))))))

(defn make-category-filter [blacklist]
  (fn [item]
    (let [has (some-> item :entry :categories set)]
      (>= (count (clojure.set/intersection has (set blacklist))) 1))))


(defn make-reddit-proc [min-score]
  (proc/make
    :filter (fn [item]
              (let [site (some-> item :entry :url .getHost)
                    score (get-in item [:entry :score])
                    title (get-in item [:summary :title])]
                (< score min-score)))
    :post [(fn [item]
             (let [site (some-> item :entry :url .getHost)]
               (cond
                 (re-find #"youtube|vimeo|reddit|redd\.it|open\.spotify\.com" site) item
                 (re-find #"i\.imgur\.com|i\.redd\.it" site) (update-in item [:entry :contents "text/html"]
                                           str "<img src=\"" (get-in item [:entry :url]) "\"/>")
                 :else ((proc/mercury-contents (:mercury creds) :keep-orig true) item))))]))


(def ^:dynamic *srcs*
  {:twit-c3pb {:src (src/twitter-search "c3pb" (:twitter-api creds))
               :proc (proc/make
                       :filter (fn [item]
                                 (->> item
                                   :entry
                                   :type
                                   #{:retweet})))
               :tags #{}
               :cron cron-hourly}
   :twit-augsburg-pics {:src (src/twitter-search "augsburg filter:images"
                               (:twitter-api creds))
                        :proc (proc/make
                                :filter (fn [item]
                                          (let [type (get-in item [:entry :type])
                                                text (get-in item [:entry :contents "text/plain"])]
                                            (or (#{:retweet} type)
                                              (re-find #"pussy|porn|camsex|webcam" text)))))
                        :tags #{}
                        :cron cron-hourly}

   :fefe {:src (src/feed "http://blog.fefe.de/rss.xml?html")
          :proc (proc/make
                  :post [(proc/exchange
                           [:entry :descriptions]
                           [:entry :contents])]
                  :filter (fn [item]
                            (->> item
                              :summary
                              :title
                              (re-find #"Zu Sarin"))))
          :tags #{:tech}
          :cron cron-hourly}

   :upwork-personal {:src (src/feed
"https://www.upwork.com/ab/feed/topics/atom?securityToken=c037416c760678f3b3aa058a7d31f4a0dc32a269dd2f8f947256d915b19c8219029b5846f9f18209e6890ca6b72be221653cf275086926945f522d934a200eec&userUid=823911365103362048&orgUid=823911365107556353")
                     :tags #{:jobs}
                     :cron cron-hourly
                     :proc (proc/make
                             :filter (fn [item]
                                       (let [wants (get-in item [:entry :nlp :nouns])
                                             haves #{"python" "clojure" "ceph" "c++" "c" "logstash" "kibana" "java" "linux"}
                                             dontwants #{"php" "node.js" "javascript" "ecmascript" "wordpress" "scrapy" "django"}]
                                         (not
                                           (and (< (count (clojure.set/intersection wants dontwants)) 2)
                                             (>= (count (clojure.set/intersection wants haves)) 1))))))}

   :irq0 {:src (src/feed "http://irq0.org/news/index.atom")
          :proc (proc/make
                  :post [(proc/add-tag :personal)])
          :tags #{:personal}
          :cron cron-daily}
   :oreilly-ideas {:src (src/feed "https://www.oreilly.com/ideas/feed.atom")
                   :cron cron-daily}

   :danluu {:src (src/feed "https://danluu.com/atom.xml" :deep? true)
            :cron cron-daily}
   :ridiculousfish {:src (src/feed "http://ridiculousfish.com/blog/atom.xml")
                    :cron cron-daily}
   :rachelbythebay {:src (src/feed "https://rachelbythebay.com/w/atom.xml")
                    :cron cron-daily}
   :chneukirchen {:src (src/feed "http://chneukirchen.org/trivium/index.atom")
                  :cron cron-daily}
   :codinghorror {:src (src/feed "http://feeds.feedburner.com/codinghorror")
                  :cron cron-daily}
   :joel-on-software {:src (src/feed "https://www.joelonsoftware.com/feed/")
                      :cron cron-daily}
   :summit-route {:src (src/feed "http://summitroute.com/blog/feed.xml")
                  :proc (proc/make
                          :post [(proc/exchange
                                   [:entry :descriptions]
                                   [:entry :contents])])
                  :cron cron-daily}
   :joe-duffy {:src (src/feed "http://joeduffyblog.com/feed.xml" :deep? true)
               :cron cron-daily}

   :aphyr {:src (src/feed "https://aphyr.com/posts.atom")
           :cron cron-daily}

   :99pi {:src (src/feed "https://feeds.feedburner.com/99pi")
          :cron cron-daily}

   :kottke {:src (src/feed "http://feeds.kottke.org/main")
            :cron cron-daily}

   :stackoverflow-engineering {:src (src/feed "https://stackoverflow.blog/engineering/feed/")
                               :cron cron-daily}

   :spotify-insights {:src (src/feed "https://insights.spotify.com/us/feed/")
                      :cron cron-daily}

   :weekly-programming-digest {:src (src/feed "http://feeds.feedburner.com/digest-programming")
                               :proc (proc/make
                                       :post [(proc/exchange
                                                [:entry :descriptions]
                                                [:entry :contents])])
                               :cron cron-daily}

   :usenix-conferences {:src (src/feed "https://www.usenix.org/upcoming-conferences-feed")
                        :tags #{:events}
                        :cron cron-daily}

   :acm-queue {:src (src/feed "https://queue.acm.org/rss/feeds/queuecontent.xml" :deep? true)
               :cron cron-daily}

   :reddit-diy {:src (src/reddit "DIY" :hot)
                :cron cron-daily
                :tags #{:reddit}
                :proc (make-reddit-proc 500)}

   :reddit-techsupportgore {:src (src/reddit "techsupportgore" :hot)
                            :cron cron-daily
                            :tags #{:reddit}
                            :proc (make-reddit-proc 500)}

   :reddit-techsupportmacgyver {:src (src/reddit "techsupportmacgyver" :hot)
                                :cron cron-daily
                                :tags #{:reddit}
                                :proc (make-reddit-proc 300)}

   :reddit-somethingimade {:src (src/reddit "somethingimade" :hot)
                           :cron cron-daily
                           :tags #{:reddit}
                           :proc (make-reddit-proc 300)}

   :reddit-Justrolledintotheshop {:src (src/reddit "Justrolledintotheshop" :hot)
                                  :cron cron-daily
                                  :tags #{:reddit}
                                  :proc (make-reddit-proc 400)}

   :reddit-fixit {:src (src/reddit "fixit" :hot)
                  :cron cron-daily
                  :tags #{:reddit}
                  :proc (make-reddit-proc 300)}

   :reddit-lifeprotips {:src (src/reddit "LifeProTips" :top)
                        :cron cron-daily
                        :tags #{:reddit}
                        :proc (make-reddit-proc 100)}


   :reddit-depthhub {:src (src/reddit "DepthHub" :hot)
                     :cron cron-daily
                     :tags #{:reddit}
                     :proc (make-reddit-proc 500)}

   :reddit-fascinating {:src (src/reddit "fascinating" :hot)
                        :cron cron-daily
                        :tags #{:reddit}
                        :proc (make-reddit-proc 100)}

   :reddit-learnuselesstalents {:src (src/reddit "LearnUselessTalents" :hot)
                                :cron cron-daily
                                :tags #{:reddit}
                                :proc (make-reddit-proc 1000)}

   :reddit-clojure {:src (src/reddit "Clojure" :hot)
                    :cron cron-daily
                    :tags #{:reddit}
                    :proc (make-reddit-proc 40)}

   :reddit-listentothis {:src (src/reddit "listentothis" :hot)
                         :cron cron-daily
                         :tags #{:reddit}
                         :proc (make-reddit-proc 150)}

   :reddit-popular {:src (src/reddit "popular" :hot)
                    :cron cron-daily
                    :tags #{:reddit}
                    :proc (make-reddit-proc 30000)}

   :reddit-europe {:src (src/reddit "europe" :top)
                    :cron cron-daily
                    :tags #{:reddit}
                   :proc (make-reddit-proc 300)}

   :reddit-photoshopbattles {:src (src/reddit "photoshopbattles" :hot)
                             :cron cron-daily
                             :tags #{:reddit}
                             :proc (make-reddit-proc 100)}

   :reddit-educationalgifs {:src (src/reddit "educationalgifs" :hot)
                            :cron cron-daily
                            :tags #{:reddit}
                            :proc (make-reddit-proc 1000)}

   :reddit-iwanttolearn {:src (src/reddit "IWantToLearn" :hot)
                         :cron cron-daily
                         :tags #{:reddit}
                         :proc (make-reddit-proc 100)}

   :reddit-getdisciplined {:src (src/reddit "getdisciplined" :hot)
                           :cron cron-daily
                           :tags #{:reddit}
                           :proc (make-reddit-proc 100)}
   :reddit-Foodforthought {:src (src/reddit "Foodforthought" :hot)
                           :cron cron-daily
                           :tags #{:reddit}
                           :proc (make-reddit-proc 200)}

   :reddit-ifyoulikeblank {:src (src/reddit "ifyoulikeblank" :top)
                           :cron cron-daily
                           :tags #{:reddit}
                           :proc (make-reddit-proc 5)}

   :reddit-dataisbeautiful {:src (src/reddit "dataisbeautiful" :top)
                            :cron cron-daily
                            :tags #{:reddit}
                            :proc (make-reddit-proc 20)}

   :reddit-postrock {:src (src/reddit "postrock" :hot)
                     :cron cron-daily
                     :tags #{:reddit :music}
                     :proc (make-reddit-proc 6)}

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
                  :proc (make-reddit-proc 5)}

   :reddit-rock {:src (src/reddit "rock" :hot)
                 :cron cron-daily
                 :tags #{:reddit :music}
                 :proc (make-reddit-proc 5)}

   :muspy {:src (src/feed "https://muspy.com/feed?id=1ub5u1nk72w26hnpeiyfuvto9owxfd")
           :cron cron-daily
           :tags #{:music}}

   :mydealz-hot {:src (src/feed "https://www.mydealz.de/rss")
                 :cron cron-daily
                 :tags #{:shopping}
                 :proc (proc/make
                         :filter (make-category-filter ["Handys &amp; Tablets" "Home &amp; Living" "Fashion &amp; Accessoires"])
                         :post [(fn [item]
                                  (let [raw (:raw item)
                                        merchant (->> raw
                                                   :foreign-markup
                                                   (some #(when (= (.getName %) "merchant") %)))]
                                    (-> item
                                      (assoc-in [:entry :merchant] (some-> merchant (.getAttribute "name") .getValue))
                                      (assoc-in [:entry :price] (some-> merchant (.getAttribute "price") .getValue)))))])}

   :humblebundle {:src (src/feed "http://blog.humblebundle.com/rss")
                  :proc (proc/make
                          :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                  :cron cron-daily
                  :tags #{:shopping}}

;; TODO   :metacritic {:src (src/feed "http://www.metacritic.com/rss/music"

   :vice {:src (src/feed "https://www.vice.com/en_us/rss")
          :cron cron-daily}

   :hn-top {:src (src/hn "topstories" :throttle-secs (* 23 60))
            :proc (proc/make
                    :post [(proc/mercury-contents (:mercury creds) :keep-orig true)]
                    :filter (make-hacker-news-filter 300 150))}

   :hn-best {:src (src/hn "beststories" :throttle-secs (* 23 60))
             :proc (proc/make
                     :post [(proc/mercury-contents (:mercury creds) :keep-orig true)]
                     :filter (make-hacker-news-filter 300 150))}

   :xkcd {:src (src/feed "https://xkcd.com/rss.xml")
          :proc (proc/make
                  :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
          :cron cron-daily
          :tags #{:comics}}

   :daily-wtf {:src (src/feed "http://syndication.thedailywtf.com/TheDailyWtf")
               :proc (proc/make
                       :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
               :cron cron-daily}

   :pixelenvy {:src (src/feed "https://feedpress.me/pxlnv")
               :cron cron-daily
               :proc (proc/make
                       :filter (fn [item]
                                 (let [names (get-in item [:entry :nlp :names])
                                       dontwant #{"App Store" "Apple" "Apple Music" "Apple Store" "MacOS" "OSX"}]
                                   (log/spy (clojure.set/intersection names dontwant))
                                   (>= (count (clojure.set/intersection names dontwant)) 2))))}
   :rumpus {:src (src/feed "http://therumpus.net/feed/")
            :cron cron-daily}

   :atlantic-best-of {:src (src/feed "https://www.theatlantic.com/feed/best-of/")
                      :cron cron-daily}

   :fail {:src
          (src/feed "http://irq0.org/404")}

   :newsletter-mailbox {:src (src/imap "imap://mail.cpu0.net/NEWSLETTER" (:imap creds))
                        :cron cron-daily}

   :pocoo-lucumr {:src (src/feed "http://lucumr.pocoo.org/feed.atom")
                  :cron cron-daily}


   :yt-kirstenlepore {:src (src/feed "https://www.youtube.com/user/kirstenlepore")
                      :cron cron-daily}

   :programmingisterrible {:src (src/feed "http://programmingisterrible.com/rss")
                           :proc (proc/make
                                   :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                           :cron cron-daily}

   :preshing {:src (src/feed "http://preshing.com/feed")
              :cron cron-daily}

   :elibendersky {:src (src/feed "http://eli.thegreenplace.net/feeds/all.atom.xml")
                         :proc (proc/make
                                 :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                  :cron cron-daily}

   :abstrusegoose {:src (src/feed "http://abstrusegoose.com/atomfeed.xml")
                   :proc (proc/make
                           :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                   :tags #{:comics}
                   :cron cron-daily}

   :cyanidehappiness {:src (src/feed "https://feeds.feedburner.com/Explosm")
                      :proc (proc/make
                              :post [(fn [item]
                                       (let [html (infowarss.fetch.http/fetch-http-generic
                                                    (src/http (str (get-in item [:entry :url]))))
                                             comic-link (-> (hickory.select/select
                                                             (hickory.select/descendant
                                                               (hickory.select/id "comic-container")
                                                               (hickory.select/tag :img)) (:hickory html))
                                                          first :attrs :src)]
                                         (-> item
                                           (assoc-in [:entry :contents "text/html"] (format "<img src=\"http:%s\"/>" comic-link)))))])
                      :tags #{:comics}
                      :cron cron-daily}

   :joyoftech {:src (src/feed "http://www.joyoftech.com/joyoftech/jotblog/atom.xml")
               :tags #{:comics}
               :cron cron-daily}

   :theoatmeal {:src (src/feed "https://feeds.feedburner.com/oatmealfeed")
               :tags #{:comics}
               :cron cron-daily}

   :bookmark {:src nil
              :tags #{:bookmark}}
   :document {:src nil
              :tags #{:document}}
   })


;;;; todo
(comment
  "https://www.questionablecontent.net/QCRSS.xml"
  "http://www.radiolab.org/"
  "http://ask.metafilter.com/")
