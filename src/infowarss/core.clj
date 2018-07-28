(ns infowarss.core
  (:require
   [taoensso.timbre :as log]
   [infowarss.src :as src]
   [infowarss.converter :as converter]
   [infowarss.fetch.http]
   [clj-time.periodic :refer [periodic-seq]]
   [clj-time.core :as time]
   [clj-time.format :as tf]
   [hiccup.core :refer [html]]
   [hickory.select :as S]
   [slingshot.slingshot :refer [throw+ try+]]
   [hickory.core :as hick]
   [hickory.render :refer [hickory-to-html]]
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

;; second minute hour day-of-week day-of-month month year
(def cron-hourly "23 42 * * * * *")
(def cron-daily "23 42 5,8,12,17,23 * * * *")
(def cron-twitter "23 42 12,18,23 * * * *")

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


(defn human-host-identifier [str-url]
  (let [url (io/as-url str-url)
        host (.getHost url)]
    (try+
      (let [domain (com.google.common.net.InternetDomainName/from host)
            site (.topPrivateDomain host)]
        (.name site))
      (catch Object _
        (str host)))))


(defn make-bookmark-feed [url]
  (let [src (src/mercury url (get-in creds [:mercury :api-key]))]
    {:src src
     :tags #{:bookmark}
     :proc (proc/make
             :post [(fn [item]
                     (let [summary (bookmark-html item)
                           html (get-in item [:entry :contents "text/html"])
                           url (some-> item :entry :url)
                           site (human-host-identifier url)]
                       (-> item
                         (assoc-in [:entry :contents "text/html"]
                           (str summary "\n\n\n" html))
                         (assoc-in [:meta :source-key]
                           (if (some? site)
                             (keyword (str "bookmark-" (str site)))
                             :bookmark))
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
    (let [site (some-> item :entry :url .getHost)
          score (get-in item [:entry :score])
          title (some-> (get-in item [:summary :title]) string/lower-case)
          type (get-in item [:entry :type])]
      (not
        (or
          (and (= :story type)
            (>= score min-score))
          (and (= :story type)
            (re-find #"clojure|lisp|book|alan kay|futurism" title)
            (>= score min-score-match))
          (and
            (some? site)
            (re-find #"nautil\.us|theatlantic|medium|youtube|nytimes|theguardian|washingtonpost|99percentinvisible|theverge|phys\.org|bbc\.com"
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
             (let [site (some-> item :entry :url .getHost)
                   path (some-> item :entry :url .getPath)]
               (cond
                 (or (re-find #"i\.imgur\.com|i\.redd\.it|twimg\.com" site)
                   (re-find #"\.(jpg|jpeg|gif|png)$" path))
                 (update-in item [:entry :contents "text/html"]
                   str "<img src=\"" (get-in item [:entry :url]) "\"/>")
                 (re-find #"youtube|vimeo|reddit|redd\.it|open\.spotify\.com" site) item
                 :else ((proc/mercury-contents (:mercury creds) :keep-orig? true) item))))]))


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
                      :cron cron-twitter}

   :twit-berlin-pics {:src (src/twitter-search "#berlin filter:images"
                               (:twitter-api creds))
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
          :tags #{:blog}
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
                   :cron cron-daily
                   :tags #{:tech :magazine}}

   :oreilly-fourshortlinks {:src (src/feed "http://feeds.feedburner.com/FourShortLinks")
                            :cron cron-daily
                            :tags #{:tech :magazine}}

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
                    :tags #{:tech :blog}
                  :cron cron-daily}
   :codinghorror {:src (src/feed "http://feeds.feedburner.com/codinghorror")
                  :tags #{:tech :blog}
                  :cron cron-daily}
   :joel-on-software {:src (src/feed "https://www.joelonsoftware.com/feed/")
                      :tags #{:tech :blog}
                      :cron cron-daily}
   :summit-route {:src (src/feed "http://summitroute.com/blog/feed.xml")
                  :tags #{:tech :security :blog}
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

   :99pi {:src (src/feed "https://feeds.feedburner.com/99pi")
          :tags #{:design}
          :cron cron-daily}

   :kottke {:src (src/feed "http://feeds.kottke.org/main")
            :tags #{:magazine}
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

   :spotify-insights {:src (src/feed "https://insights.spotify.com/us/feed/")
                      :tags #{:tech :sci}
                      :cron cron-daily}

   :weekly-programming-digest {:src (src/feed "http://feeds.feedburner.com/digest-programming")
                               :tags #{:tech :digest}
                               :proc (proc/make
                                       :post [(proc/exchange
                                                [:entry :descriptions]
                                                [:entry :contents])])
                               :cron cron-daily}

   :usenix-conferences {:src (src/feed "https://www.usenix.org/upcoming-conferences-feed")
                        :tags #{:events}
                        :cron cron-daily}

   :acm-queue {:src (src/feed "https://queue.acm.org/rss/feeds/queuecontent.xml" :deep? true :force-update? true)
               :tags #{:tech :magazine}
               :cron cron-daily}

   :reddit-diy {:src (src/reddit "DIY" :hot)
                :cron cron-daily
                :tags #{:reddit}
                :proc (make-reddit-proc 500)}

   :reddit-depthhub {:src (src/reddit "DepthHub" :hot)
                     :cron cron-daily
                     :tags #{:reddit}
                     :proc (make-reddit-proc 500)}

   :reddit-fascinating {:src (src/reddit "fascinating" :hot)
                        :cron cron-daily
                        :tags #{:reddit}
                        :proc (make-reddit-proc 100)}

   :reddit-clojure {:src (src/reddit "Clojure" :hot)
                    :cron cron-daily
                    :tags #{:reddit :clojure :tech}
                    :proc (make-reddit-proc 40)}

   :reddit-listentothis {:src (src/reddit "listentothis" :hot)
                         :cron cron-daily
                         :tags #{:reddit :music}
                         :proc (make-reddit-proc 150)}

   :reddit-popular {:src (src/reddit "popular" :hot)
                    :cron cron-daily
                    :tags #{:reddit}
                    :proc (make-reddit-proc 50000)}

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
                            :tags #{:reddit :sci}
                            :proc (make-reddit-proc 80)}

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

   :reddit-hfy {:src (src/reddit "HFY" :top)
                :cron cron-daily
                :tags #{:reddit}
                :proc (make-reddit-proc 100)}

   :reddit-jwd {:src (src/reddit "jwd" :hot)
                :cron cron-daily
                :tags #{:reddit :berlin}
                :proc (make-reddit-proc 5)}


   :reddit-berlin {:src (src/reddit "berlin" :hot)
                   :cron cron-daily
                   :tags #{:reddit :berlin}
                   :proc (make-reddit-proc 10)}

   :reddit-berlinshopping {:src (src/reddit "berlinshopping" :top)
                           :cron cron-daily
                           :tags #{:reddit :berlin}
                           :proc (make-reddit-proc 1)}

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

   :vice {:src (src/feed "https://www.vice.com/en_us/rss" :deep? true :force-update? true)
          :tags #{:magazine}
          :cron cron-daily}

   :hn-top {:src (src/hn "topstories" :throttle-secs (* 23 60))
          :tags #{:tech}
            :proc (proc/make
                    :post [(proc/mercury-contents (:mercury creds) :keep-orig? true)]
                    :filter (make-hacker-news-filter 350 150))}

   :hn-best {:src (src/hn "beststories" :throttle-secs (* 23 60))
          :tags #{:tech}
             :proc (proc/make
                     :post [(proc/mercury-contents (:mercury creds) :keep-orig? true)]
                     :filter (make-hacker-news-filter 350 150))}

   :xkcd {:src (src/feed "https://xkcd.com/rss.xml")
          :proc (proc/make
                  :post [(fn [item]
                           (let [hick (-> (hick/parse (get-in item [:entry :descriptions "text/html"])) hickory.core/as-hickory)
                                 alt-text (-> (S/select
                                               (S/descendant
                                                 (S/tag :img)) hick)
                                            first :attrs :alt)]
                             (log/spy alt-text)
                             (-> item
                               (assoc-in [:entry :contents "text/html"]
                                 (str
                                   (get-in item [:entry :descriptions "text/html"])
                                   "<p>" alt-text "</p>")))))])
          :cron cron-daily
          :tags #{:comics}}

   :daily-wtf {:src (src/feed "http://syndication.thedailywtf.com/TheDailyWtf")
               :tags #{:fun}
               :proc (proc/make
                       :filter (fn [item]
                                 (let [title (get-in item [:summary :title])]
                                   (re-find #"^(Sponsor Post|CodeSOD|Error'd|Representative Line):" title)))
                       :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
               :cron cron-daily}

   :pixelenvy {:src (src/feed "https://feedpress.me/pxlnv")
               :cron cron-daily
               :tags #{:magazine}
               :proc (proc/make
                       :filter (fn [item]
                                 (let [names (get-in item [:entry :nlp :names])
                                       dontwant #{"App Store" "Apple" "Apple Music" "Apple Store" "MacOS" "OSX"}]
                                   (log/spy (clojure.set/intersection names dontwant))
                                   (>= (count (clojure.set/intersection names dontwant)) 2))))}
   :rumpus {:src (src/feed "http://therumpus.net/feed/")
            :tags #{:magazine}
            :cron cron-daily}

   :atlantic-best-of {:src (src/feed "https://www.theatlantic.com/feed/best-of/")
                      :tags #{:magazine}
                      :cron cron-daily}

   :fail {:src
          (src/feed "http://irq0.org/404")}

   :newsletter-mailbox {:src (src/imap "imap://mail.cpu0.net/NEWSLETTER" (:imap creds))
                        :cron cron-daily}

   :pocoo-lucumr {:src (src/feed "http://lucumr.pocoo.org/feed.atom")
                  :tags #{:tech}
                  :cron cron-daily}

   :yt-kirstenlepore {:src (src/feed "https://www.youtube.com/user/kirstenlepore")
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
                                             comic-link (-> (S/select
                                                             (S/descendant
                                                               (S/id "comic-container")
                                                               (S/tag :img)) (:hickory html))

                                                          first :attrs :src (string/replace #"^http://explosm.net///" "http://"))]
                                         (-> item
                                           (assoc-in [:entry :contents "text/html"] (format "<img src=\"%s\"/>" comic-link)))))])
                      :tags #{:comics}
                      :cron cron-daily}

   :joyoftech {:src (src/feed "http://www.joyoftech.com/joyoftech/jotblog/atom.xml" :force-update? false)
               :proc (proc/make
                       :post [(fn [item]
                                (let [html (infowarss.fetch.http/fetch-http-generic
                                             (src/http (str (get-in item [:entry :url]))))
                                      comic-img (-> (S/select
                                                      (S/descendant
                                                        (S/tag :div)
                                                        (S/nth-child 3)
                                                        (S/tag :a)
                                                        (S/tag :img))
                                                      (:hickory html))
                                                  second :attrs)
                                      comic-src (-> comic-img :src (subs 3))
                                      comic-alt (-> comic-img :alt)
                                      comic-url (str "https://www.geekculture.com/joyoftech/" comic-src)]
                                  (log/info comic-url)
                                  (-> item
                                    (assoc-in [:entry :contents "text/html"]
                                      (format "<img src=\"%s\"/><p>%s</p>" comic-url comic-alt)))))])
                       :tags #{:comics}
                       :cron cron-daily}

   :theoatmeal {:src (src/feed "https://feeds.feedburner.com/oatmealfeed"
                       :force-update? false)
                :tags #{:comics}
                :proc (proc/make
                        :post [(fn [item]
                                 (let [html (infowarss.fetch.http/fetch-http-generic
                                              (src/http (str (get-in item [:entry :url]))))
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

   :tumblr-worstofchefkoch {:src (src/feed "https://worstofchefkoch.tumblr.com/rss")
                            :tags #{:fun}
                            :cron cron-daily}

   :tumblr-powerlinesinanime {:src (src/feed "https://powerlinesinanime.tumblr.com/rss")
                              :tags #{:fun}
                              :cron cron-daily}

   :tumblr-awkwardstockphotos {:src (src/feed "http://awkwardstockphotos.com/rss")
                               :tags #{:fun}
                               :proc (proc/make
                                       :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                               :cron cron-daily}
   :tumblr-weirdtumblrs {:src (src/feed "http://weirdtumblrs.tumblr.com/rss")
                         :tags #{:fun}
                         :proc (proc/make
                                 :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                         :cron cron-daily}

   :tumblr-mcmensionhell {:src (src/feed "http://mcmansionhell.com/rss")
                          :tags #{:fun}
                          :proc (proc/make
                                  :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                          :cron cron-daily}

   :tumblr-runningahackerspace {:src (src/feed "https://runningahackerspace.tumblr.com/rss")
                                :tags #{:fun}
                          :proc (proc/make
                                  :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                          :cron cron-daily}

   :orkpiraten {:src (src/feed "https://www.orkpiraten.de/blog/feed")
                :tags #{:fun :blog}
                :cron cron-daily}

   :berlintypography {:src (src/feed "https://berlintypography.wordpress.com/feed/")
                      :tags #{:design}
                      :cron cron-daily}

   :atomicannhilation {:src (src/feed "https://atomic-annhilation.blogspot.com/feeds/posts/default")
                       :cron cron-daily
                       :tags #{:comics}}

   :iconicphotos {:src (src/feed "https://iconicphotos.wordpress.com/feed/")
                  :tags #{:fun :design}
                  :cron cron-daily}

   :googleprojectzero {:src (src/feed "https://googleprojectzero.blogspot.com/feeds/posts/default")
                       :tags #{:security}
                       :cron cron-daily}

   :allthingsdistributed {:src (src/feed "http://www.allthingsdistributed.com/atom.xml" :force-update? false)
                          :tags #{:tech :blog}
                          :cron cron-daily}

   :rub-onwebsecurity {:src (src/feed "https://web-in-security.blogspot.com/feeds/posts/default")
                       :tags #{:security :blog}
                       :cron cron-daily}

   :github-paperswelove {:src (src/feed "https://github.com/papers-we-love/papers-we-love/commits/master.atom")
                         :tags #{:sci}
                         :cron cron-daily}

   :usenix-multimedia {:src (src/feed "https://www.usenix.org/multimedia/rss.xml")
                       :proc (proc/make
                               :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                       :tags #{:sci}
                       :cron cron-daily}

   :economist-scitech {:src (src/feed "http://www.economist.com/sections/science-technology/rss.xml")
                       :tags #{:magazine}
                       :proc (proc/make
                               :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                       :cron cron-daily}

   :theverge {:src (src/feed "https://www.theverge.com/rss/full.xml")
              :tags #{:magazine}
              :cron cron-daily}

   :vox {:src (src/feed "https://www.vox.com/rss/index.xml")
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
                :tags #{:hpc}
                  :proc (proc/make
                          :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                :cron cron-daily}

   :github-trends {:src (src/feed "http://github-trends.ryotarai.info/rss/github_trends_all_weekly.rss")
                   :tags #{:trends}
                  :proc (proc/make
                          :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                   :cron cron-daily}


   :snia-storage {:src (src/feed "http://sniablog.org/feed/atom/")
                  :tags #{:storage}
                  :proc (proc/make
                          :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                  :cron cron-daily}

   :ibm-dev-storage {:src (src/feed "https://developer.ibm.com/storage/feed/")
                     :tags #{:storage}
                     :cron cron-daily}

   :katemats {:src (src/feed "http://katemats.com/feed/")
              :tags #{:tech-meta :blog}
              :cron cron-daily}

   :sachachua-emacs {:src (src/feed "http://sachachua.com/blog/category/emacs/feed")
                     :tags #{:emacs :blog}
                     :cron cron-daily}


   :pragmaticemacs {:src (src/feed "http://pragmaticemacs.com/feed/")
                     :tags #{:emacs :blog}
                     :cron cron-daily}

   :mattmight {:src (src/feed "http://matt.might.net/articles/feed.rss" :force-update? false)
               :tags #{:tech-meta :blog}
               :proc (proc/make
                       :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
               :cron cron-daily}

   :infoq-articles {:src (src/feed "https://www.infoq.com/feed/articles" :force-update? false)
                    :proc (proc/make
                            :post [(proc/mercury-contents (:mercury creds) :keep-orig? true)])
                    :cron cron-daily
                    :tags #{:tech :magazine}}

   :gatesnotes {:src (src/feed "https://www.gatesnotes.com/rss" :deep? true :force-update false)
                :tags #{:tech-meta :blog}
                :proc (proc/make
                       :post [(fn [item]
                                (let [hickory (->
                                             (hick/parse (get-in item [:entry :contents "text/html"]))
                                             hick/as-hickory)
                                      content (-> (S/select (S/descendant (S/id "mainContent")) hickory)
                                                first)
                                      html (hickory-to-html content)]
                                  (-> item
                                    (assoc-in [:entry :contents]
                                      {"text/html" html
                                       "text/plain" (converter/html2text html)}))))])

                :cron cron-daily}

   :startup50 {:src (src/feed "http://startup50.com/feed/" :force-update? false)
               :tags #{:startups}
               :proc (proc/make
                       :post [(proc/mercury-contents (:mercury creds) :keep-orig? true)])
               :cron cron-daily}

   :clearskydata {:src (src/feed "https://www.clearskydata.com/blog/rss.xml")
                  :tags #{:startups}
                  :cron cron-daily}

   :cloudendure {:src (src/feed "https://www.cloudendure.com/feed/")
                 :tags #{:startups}
                 :cron cron-daily}

   :elastifile {:src (src/feed "https://blog.elastifile.com/rss.xml")
                :tags #{:startups}
                :cron cron-daily}

   :excelero {:src (src/feed "https://www.excelero.com/feed/")
              :tags #{:startups}
              :cron cron-daily}

   :hedviginc {:src (src/feed "https://www.hedviginc.com/blog/rss.xml" :deep? true :force-update true)
               :tags #{:startups}
               :cron cron-daily}

   :igneous {:src (src/feed "https://inside.igneous.io/rss.xml")
             :tags #{:startups}
             :cron cron-daily}

   :iofabric {:src (src/feed "https://www.iofabric.com/feed/")
              :tags #{:startups}
              :cron cron-daily}

   :quobyte {:src (src/feed "https://www.quobyte.com/blog/feed/")
             :tags #{:startups}
             :cron cron-daily}

   :reduxio {:src (src/feed "https://beyondtheblocks.reduxio.com/rss.xml")
             :tags #{:startups}
             :cron cron-daily}

   :smartiops {:src (src/feed "http://www.smartiops.com/feed/")
               :tags #{:startups}
               :cron cron-daily}

   :snowflake {:src (src/feed "https://www.snowflake.net/feed/")
               :tags #{:startups}
               :cron cron-daily}

   :softnas {:src (src/feed "https://www.softnas.com/wp/feed/")
             :tags #{:startups}
             :cron cron-daily}

   :storpool {:src (src/feed "https://storpool.com/feed")
              :tags #{:startups}
              :cron cron-daily}

   :wasabi {:src (src/feed "https://wasabi.com/blog/feed/")
            :tags #{:startups}
            :cron cron-daily}

   :theregister-storage {:src (src/feed "https://www.theregister.co.uk/data_centre/storage/headlines.atom" :force-update? false)
                         :proc (proc/make
                                 :post [(fn [item]
                                  (let [url (get-in item [:entry :url])
                                        fixed-url (string/replace url #"go\.theregister\.com/feed/" "")]
                                    (log/info url)
                                    (-> item
                                      (assoc-in [:entry :url] fixed-url))))
                                (proc/mercury-contents (:mercury creds) :keep-orig? false)])
                         :tags #{:storage :magazine}
                         :cron cron-daily}

   :infostor {:src (src/feed "http://www.infostor.com/index/rssfaq/rss_article_and_wp.noncurrentissue.articles.infostor.html?block" :force-update? false)
              :proc (proc/make
                      :post [(proc/mercury-contents (:mercury creds) :keep-orig? true)])
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
                     :ts #(-> % first :attrs :content tc/from-string)})
             :tags #{:startups}
             :cron cron-daily}

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
                              (tf/parse (tf/formatter "MMMM dd, yyyy")))})
             :tags #{:startups}
                :cron cron-daily}



   :wekaio {:src (src/wp-json "https://www.weka.io/wp-json")
            :tags #{:startups}
            :cron cron-daily}


   :signalvnoise {:src (src/feed "https://m.signalvnoise.com/feed")
                  :tags #{:tech :blog}
                  :cron cron-daily}

   :seriouseats-foodlab {:src (src/feed "https://feeds.feedburner.com/SeriousEats-thefoodlab"
                                :force-update? false)
                         :proc (proc/make
                                 :post [(proc/mercury-contents (:mercury creds) :keep-orig? true)])
                         :cron cron-daily
                         :tags #{:food}}
   :seriouseats-recipes {:src (src/feed "https://feeds.feedburner.com/seriouseats/recipes"
                                :force-update? false)
                         :proc (proc/make
                                 :post [(proc/mercury-contents (:mercury creds) :keep-orig? true)])
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

   :manu-el {:src (src/feed "https://manuel-uberti.github.io/feed" :deep? true)
             :tags #{:tech :blog}
             :cron cron-daily}


   :send-more-paramedics {:src (src/feed "http://blog.fogus.me/feed/")
                  :tags #{:tech :blog}
                  :proc (proc/make
                          :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                  :cron cron-daily}

   :uswitch-labs {:src (src/feed "https://labs.uswitch.com/rss/")
             :tags #{:tech}
                  :cron cron-daily}


   :wirres {:src (src/feed "http://wirres.net/article/rss/full/6/15/")
            :tags #{:blog}
            :cron cron-daily}

   :nerdcore {:src (src/wp-json "https://nerdcore.de/wp-json/")
              :tags #{:magazine :fun}
              :cron cron-daily}

   :meetups-my {:src (src/feed "https://www.meetup.com/events/rss/139002912/50f0499c4b59a743ecbf7d1e950eb8078ca2cf5b/going")
                :tags #{:berlin}
                  :proc (proc/make
                          :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
                :cron cron-daily}

   :krisk {:src (src/g+activity "+KristianKohntopp" (:google creds))
           :tags #{:blog}
           :cron cron-daily}

   :berlin-backyard-fleamarkets {:src (src/website "http://polly.sternenlaub.de/fleamarkets/list")
                                 :tags #{:berlin}
                                 :cron cron-daily}

   ;; hubspot
   ;; :elastifile {:src (src/selector-feed "https://www.datera.io/blog/"
   ;;                     {:urls (S/descendant
   ;;                              (S/class "post-header")
   ;;                              (S/tag :h2)
   ;;                              (S/tag :a))
   ;;                      :description (S/and
   ;;                                     (S/tag :meta)
   ;;                                     (S/attr :name #(= % "description")))
   ;;                      :author (S/class "hs-blog-author")
   ;;                      :ts (S/class "hd-blog-data")
   ;;                      :content (S/class "blog-section")}
   ;;                     {:description #(-> % first :attrs :content)
   ;;                      :author #(-> % first :content first)
   ;;                      :ts #(some-> % first :content first
   ;;                            (partial tf/parse (tf/formatter "MMMM dd, yyyy")))})
   ;;              :tags #{:startups}
   ;;              :cron cron-daily}
   })

;;;; todo
(comment
  "https://www.questionablecontent.net/QCRSS.xml"
  "http://www.radiolab.org/"
  "http://ask.metafilter.com/")
