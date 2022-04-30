(ns u1f596.config
  (:require
   [u1f596.fetch :refer [make-item-hash] :as fetch]
   [u1f596.fetch.feed]
   [u1f596.fetch.http]
   [u1f596.fetch.imap]
   [u1f596.fetch.mercury]
   [u1f596.fetch.reddit]
   [u1f596.fetch.twitter]
   [u1f596.fetch.custom]
   [u1f596.live.hackernews]
   [u1f596.src :as src]
   [u1f596.fetchutils :refer [make-reddit-proc make-category-filter-deny make-hacker-news-filter mercury-contents parse-date-to-zoned-data-time parse-date-time-to-zoned-data-time]]
   [taoensso.timbre :as log]
   [u1f596.http :as http]
   [u1f596.converter :as converter]
   [u1f596.notifier :as notifier]
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
   [u1f596.postproc :as proc]
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [org.bovinegenius [exploding-fish :as uri]]))

(def creds (edn/read-string (slurp (io/resource "credentials.edn"))))

(def ^:dynamic *srcs*
  {:twit-c3pb {:src (src/twitter-search "c3pb" (:twitter-api creds))
               :proc (proc/make
                      :filter (fn [item]
                                (->> item
                                     :entry
                                     :type
                                     #{:retweet})))
               :tags #{}}

   :twitter-timeline {:src (src/twitter-timeline (:twitter-api creds))
                      :tags #{}}

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
                      :tags #{:pics}}

   :impfcenter-berlin {:last-state (atom {})
                       :src (src/custom :impfcenter (fn [] (let [data (:body (http-client/get "https://www.joerss.dev/api/ampel/" {:as :json}))
                                                                 color-num-to-kw {0 :red 1 :amber 2 :green}]
                                                             (for [{:keys [ciz color updateDateTime]} data]
                                                               {:summary {:title (str "Impfcenter " ciz)
                                                                          :ts (time/zoned-date-time (time/formatter :iso-date-time) updateDateTime)}
                                                                :entry [ciz (get color-num-to-kw color)]}))))
                       :proc (proc/make
                              :filter (constantly true)
                              :pre [(fn [item]
                                      (let [[name new-state] (:entry item)
                                            last-state-atom (get-in *srcs* [(get-in item [:meta :source-key])
                                                                            :last-state])
                                            last-state (get @last-state-atom name)]
                                        (log/info name last-state "->" new-state)
                                        (when (and (#{:red :amber} last-state) (= :green new-state))
                                          (notifier/notify :vac (str "Go! " name ": " last-state " -> " new-state)))
                                        (when (and (= :green last-state) (#{:red :amber} new-state))
                                          (notifier/notify :vac (str "Don't go "  name ": " last-state " -> " new-state)))
                                        (swap! last-state-atom assoc name new-state)
;;                                        (notifier/notify :vac (str name ": " last-state " -> " new-state))
                                        item))])}


   :usenix-login {:src
                  (u1f596.src/selector-feed "https://www.usenix.org/publications/loginonline"
                                            {:urls (S/and (S/tag :a)
                                                          (S/has-descendant (S/find-in-text #"Read article now")))
                                             :ts (S/class "date-display-single")
                                             :title (S/id "page-title")
                                             :author (S/descendant (S/class "field-pseudo-field--author-list") (S/tag :a))
                                             :content (S/id "block-system-main")}
                                            {:urls (fn [l] (map (fn [x] (log/spy (->> x :attrs :href http/parse-href))) l))
                                             :ts #(->> %
                                                       first
                                                       :content
                                                       first
                                                       (parse-date-to-zoned-data-time "MMMM d, yyyy"))
                                             :author (fn [l] (map #(->> % :attrs :title) l))
                                             :title (fn [l] (->> l first :content first))
                                             :content (fn [l] (log/spy l) (->> l first :content))
                                             })
                  :tags #{:deep-tech :sci}}


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
          :tags #{:blog}}

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
                :tags #{:blog}}

   :xahteiwi {:src (src/feed "https://xahteiwi.eu/feeds/all.atom.xml")
              :tags #{:blog}}

   :irq0 {:src (src/feed "http://irq0.org/rss.xml")
          :tags #{:blog}}

   ;; seems discontinued
   :oreilly-ideas {;;:src (src/feed "https://www.oreilly.com/ideas/feed.atom")
                   :proc (proc/make
                          :post [(mercury-contents :keep-orig? true)])
                   :tags #{:tech :magazine}}

   :oreilly-radar {:src (src/feed "https://www.oreilly.com/radar/feed/index.xml")
                   :options #{:mark-read-on-view}
                   :proc (proc/make
                          :filter (fn [item] (string/includes? (get-in item [:summary :title]) "Four short links"))
                          :post [(mercury-contents :keep-orig? true)])
                   :tags #{:tech :magazine}}

   :oreilly-fourshortlinks {:src (src/feed "https://www.oreilly.com/radar/topics/four-short-links/feed/index.xml")
                            :proc (proc/make
                                   :post [(proc/add-tag :highlight)
                                          (mercury-contents :keep-orig? false)])
                            :tags #{:tech}}

   :danluu {:src (src/feed "https://danluu.com/atom.xml")
            :tags #{:blog}}

   :benkuhn {:src (src/feed "https://www.benkuhn.net/index.xml")
             :tags #{:blog}}

   :ridiculousfish {:src (src/feed "http://ridiculousfish.com/blog/atom.xml")
                    :tags #{:tech :blog}}
   :rachelbythebay {:src (src/feed "https://rachelbythebay.com/w/atom.xml")
                    :tags #{:tech :blog}}
   :codinghorror {:src (src/feed "http://feeds.feedburner.com/codinghorror")
                  :tags #{:tech :blog}}
   :joel-on-software {:src (src/feed "https://www.joelonsoftware.com/feed/")
                      :tags #{:tech :blog}}
   :summit-route {:src (src/feed "http://summitroute.com/blog/feed.xml")
                  :tags #{:tech :blog}
                  :proc (proc/make
                         :post [(proc/exchange
                                 [:entry :descriptions]
                                 [:entry :contents])])}

   :joe-duffy {:src (src/feed "http://joeduffyblog.com/feed.xml" :deep? true)
               :tags #{:tech :blog}}

   :aphyr {:src (src/feed "https://aphyr.com/posts.atom")
           :tags #{:tech :blog}}

   :n99pi {:src (src/feed "https://feeds.feedburner.com/99pi")
           :tags #{:design}
           :proc (proc/make
                  :post [(proc/add-tag :highlight)])}

   :kottke {:src (src/feed "http://feeds.kottke.org/main")
            :tags #{:magazine}
            :options #{:mark-read-on-view}}

   :stackoverflow-engineering {:src (src/feed "https://stackoverflow.blog/engineering/feed/")
                               :tags #{:tech :sci}}

   :soundcloud-backstage {:src (src/feed "https://developers.soundcloud.com/blog.rss")
                          :proc (proc/make
                                 :post [(proc/exchange
                                         [:entry :descriptions]
                                         [:entry :contents])])
                          :tags #{:tech}}

   :weekly-programming-digest {:src (src/feed "http://feeds.feedburner.com/digest-programming")
                               :tags #{:tech :digest}
                               :proc (proc/make
                                      :post [(proc/add-tag :highlight)
                                             (proc/exchange
                                              [:entry :descriptions]
                                              [:entry :contents])])}

   :uusenix-conferences {:src (src/feed "https://www.usenix.org/upcoming-conferences-feed")
                         :tags #{:events}}

   :acm-queue {:src (src/feed "https://queue.acm.org/rss/feeds/queuecontent.xml"
                              :user-agent :browser
                              :force-update? true)
               :proc (proc/make
                      :post [(mercury-contents :keep-orig? true)])
               :tags #{:tech :magazine :sci}}

   :reddit-diy {:src (src/reddit "DIY" :top)
                :tags #{:reddit}
                :proc (make-reddit-proc 2000)}

   :reddit-datahoarder {:src (src/reddit "DataHoarder" :top)
                        :tags #{:reddit :storage}
                        :proc (make-reddit-proc 10)}

   :reddit-fascinating {:src (src/reddit "fascinating" :top)
                        :tags #{:reddit}
                        :proc (make-reddit-proc 100)}

   :reddit-clojure {:src (src/reddit "Clojure" :top)
                    :options #{:mark-read-on-view}
                    :tags #{:reddit :tech}
                    :proc (make-reddit-proc 50)}

   :reddit-listentothis {:src (src/reddit "listentothis" :top)
                         :tags #{:reddit :music}
                         :proc (make-reddit-proc 150)}

   :reddit-europe {:src (src/reddit "europe" :top)
                   :options #{:mark-read-on-view}
                   :tags #{:reddit :news}
                   :proc (make-reddit-proc 2000)}

   :reddit-educationalgifs {:src (src/reddit "educationalgifs" :top)
                            :tags #{:reddit}
                            :proc (make-reddit-proc 3000)}

   :reddit-Foodforthought {:src (src/reddit "Foodforthought" :top)
                           :tags #{:reddit}
                           :proc (make-reddit-proc 400)}

   :reddit-ifyoulikeblank {:src (src/reddit "ifyoulikeblank" :top)
                           :tags #{:reddit}
                           :proc (make-reddit-proc 30)}

   :reddit-dataisbeautiful {:src (src/reddit "dataisbeautiful" :top)
                            :tags #{:reddit :sci}
                            :proc (make-reddit-proc 5000)}

   :reddit-postrock {:src (src/reddit "postrock" :top)
                     :tags #{:reddit :music}
                     :proc (make-reddit-proc 15)}

   :reddit-albumaday {:src (src/reddit "albumaday" :top)
                      :tags #{:reddit :music}
                      :proc (make-reddit-proc 10)}

   :reddit-albumoftheday {:src (src/reddit "Albumoftheday" :top)
                          :tags #{:reddit :music}
                          :proc (make-reddit-proc 5)}

   :reddit-listentoconcerts {:src (src/reddit "listentoconcerts" :top)
                             :tags #{:reddit :music}
                             :proc (make-reddit-proc 5)}
   :reddit-indie {:src (src/reddit "indie" :top)
                  :tags #{:reddit :music}
                  :proc (make-reddit-proc 15)}

   :reddit-jwd {:src (src/reddit "jwd" :top)
                :options #{:mark-read-on-view}
                :tags #{:reddit :berlin}
                :proc (make-reddit-proc 5)}

   :reddit-berlin {:src (src/reddit "berlin" :top)
                   :options #{:mark-read-on-view}
                   :tags #{:reddit :berlin}
                   :proc (make-reddit-proc 10)}

   :reddit-berlinshopping {:src (src/reddit "berlinshopping" :top)
                           :tags #{:reddit :berlin}
                           :proc (make-reddit-proc 1)}

   :reddit-games {:src (src/reddit "games" :top)
                  :options #{:mark-read-on-view}
                  :tags #{:gaming :reddit}
                  :proc (make-reddit-proc 2000)}

   :reddit-pcgaming {:src (src/reddit "games" :top)
                     :options #{:mark-read-on-view}
                     :tags #{:gaming :reddit}
                     :proc (make-reddit-proc 1000)}

   :reddit-storage {:src (src/reddit "storage" :top)
                    :options #{:mark-read-on-view}
                    :tags #{:storage :reddit}
                    :proc (make-reddit-proc 10)}

   :reddit-malelivingspace {:src (src/reddit "malelivingspace" :top)
                                   :options #{:mark-read-on-view}
                    :tags #{:recreation :reddit}
                    :proc (make-reddit-proc 100)}

   :reddit-experienceddevs {:src (src/reddit "experienceddevs" :top)
                            :options #{:mark-read-on-view}
                            :tags #{:tech :reddit}
                            :proc (make-reddit-proc 75)}

   :gamasutra-pc {:src (src/feed "http://feeds.feedburner.com/GamasutraConsolePCNews")
                  :options #{:mark-read-on-view}
                  :proc (proc/make
                         :post [(mercury-contents :keep-orig? true)])
                  :tags #{:gaming}}

   :muspy {:src (src/feed "https://muspy.com/feed?id=1ub5u1nk72w26hnpeiyfuvto9owxfd")
           :tags #{:music}}

   :mydealz-hot {:src (src/feed "https://www.mydealz.de/rss")
                 :options #{:mark-read-on-view}
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
                  :tags #{:shopping}}

   :vice {:src (src/feed "https://www.vice.com/en_us/rss" :deep? true :force-update? true)
          :options #{:mark-read-on-view}
          :tags #{:magazine}}

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
          :tags #{:AAA :comics}}

   :daily-wtf {:src (src/feed "http://syndication.thedailywtf.com/TheDailyWtf")
               :tags #{:recreation}
               :proc (proc/make
                      :filter (fn [item]
                                (let [title (get-in item [:summary :title])]
                                  (re-find #"^(Sponsor Post|CodeSOD|Error'd|Representative Line):" title)))
                      :post [(proc/exchange [:entry :descriptions] [:entry :contents])])}

   :pixelenvy {:src (src/feed "https://feedpress.me/pxlnv")
               :options #{:mark-read-on-view :main-list-use-description}
               :tags #{:magazine}
               :proc (proc/make
                      :filter (fn [item]
                                (let [names (get-in item [:entry :nlp :names])
                                      dontwant #{"iOS" "App Store" "Apple" "Apple Music" "Apple Store" "MacOS" "OSX"}]
                                  (>= (count (intersection names dontwant)) 2))))}

   :atlantic-best-of {:src (src/feed "https://www.theatlantic.com/feed/best-of/")
                      :options #{:main-list-use-description}
                      :tags #{:magazine}}

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
                                              (assoc-in [:meta :source-name] src-name))))])}

   :pocoo-lucumr {:src (src/feed "http://lucumr.pocoo.org/feed.atom")
                  :tags #{:tech}}

   :programmingisterrible {:src (src/feed "http://programmingisterrible.com/rss")
                           :tags #{:tech :blog}
                           :proc (proc/make
                                  :post [(proc/exchange [:entry :descriptions] [:entry :contents])])}

   :preshing {:src (src/feed "http://preshing.com/feed")
              :tags #{:tech :blog}}

   :elibendersky {:src (src/feed "http://eli.thegreenplace.net/feeds/all.atom.xml")
                  :tags #{:tech :blog}
                  :proc (proc/make
                         :post [(proc/exchange [:entry :descriptions] [:entry :contents])])}

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
                      :tags #{:AAA}}

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
                                    item)))])}

   :bookmark {:src nil
              :tags #{:bookmark}}
   :tumblr-worstofchefkoch {:src (src/feed "https://worstofchefkoch.tumblr.com/rss"
                                           :user-agent :bot)
                            :tags #{:recreation}}

   :tumblr-awkwardstockphotos {:src (src/feed "http://awkwardstockphotos.com/rss"
                                              :user-agent :bot)
                               :tags #{:recreation}
                               :proc (proc/make
                                      :post [(proc/exchange [:entry :descriptions]
                                                            [:entry :contents])])}

   :tumblr-weirdtumblrs {:src (src/feed "http://weirdtumblrs.tumblr.com/rss"
                                        :user-agent :bot)
                         :tags #{:recreation}
                         :proc (proc/make
                                :post [(proc/exchange [:entry :descriptions] [:entry :contents])])}

   :tumblr-mcmensionhell {:src (src/feed "http://mcmansionhell.com/rss"
                                         :user-agent :bot)
                          :tags #{:recreation}
                          :proc (proc/make
                                 :post [(proc/exchange [:entry :descriptions] [:entry :contents])])}

   :tumblr-runningahackerspace {:src (src/feed "https://runningahackerspace.tumblr.com/rss"
                                               :user-agent :bot)
                                :tags #{:recreation}
                                :proc (proc/make
                                       :post [(proc/exchange [:entry :descriptions]
                                                             [:entry :contents])])}

   :orkpiraten {:src (src/feed "https://www.orkpiraten.de/blog/feed")
                :tags #{:recreation}}

   :berlintypography {:src (src/feed "https://berlintypography.wordpress.com/feed/")
                      :tags #{:design}}

   :iconicphotos {:src (src/feed "https://iconicphotos.wordpress.com/feed/")
                  :tags #{:recreation :design :AAA}}

   :googleprojectzero {:src (src/feed "https://googleprojectzero.blogspot.com/feeds/posts/default")
                       :tags #{:tech}}

   :allthingsdistributed {:src (src/feed "http://www.allthingsdistributed.com/atom.xml" :force-update? false)
                          :tags #{:tech :blog}}

   :github-paperswelove {:src (src/feed "https://github.com/papers-we-love/papers-we-love/commits/master.atom")
                         :tags #{:sci}}

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
                       :tags #{:sci}}

   :economist-scitech {:src (src/feed "https://www.economist.com/science-and-technology/rss.xml")
                       :options #{:mark-read-on-view}
                       :tags #{:magazine}
                       :proc (proc/make
                              :post [(proc/exchange [:entry :descriptions] [:entry :contents])])}

   :theverge {:src (src/feed "https://www.theverge.com/rss/full.xml")
              :options #{:mark-read-on-view}
              :tags #{:magazine}}

   :vox {:src (src/feed "https://www.vox.com/rss/index.xml")
         :options #{:mark-read-on-view}
         :tags #{:magazine}}

   :ccc {:src (src/feed "http://www.ccc.de/de/rss/updates.rdf")
         :tags #{:politics :AAA}
         :proc (proc/make
                :post [(proc/exchange [:entry :descriptions] [:entry :contents])])}

   :flarp {:src (src/feed "http://www.flarp.de/feed/")
           :proc (proc/make
                  :post [(proc/exchange [:entry :descriptions] [:entry :contents])])
           :tags #{:berlin}}

   :inside-hpc {:src (src/feed "http://feeds.feedburner.com/insidehpc")
                :options #{:mark-read-on-view}
                :tags #{:tech}
                :proc (proc/make
                       :post [(proc/exchange [:entry :descriptions] [:entry :contents])])}

   :github-trending-c++ {:src (src/feed "https://mshibanami.github.io/GitHubTrendingRSS/monthly/c++.xml")
                         :tags #{:trends :tech}
                         :options #{:mark-read-on-view}
                         :proc (proc/make
                                :post [(proc/exchange [:entry :descriptions] [:entry :contents])
                                       (fn [item] (assoc-in item [:summary :ts]
                                                            (get-in item [:feed :pub-ts])))])}

   :github-trending-java {:src (src/feed "https://mshibanami.github.io/GitHubTrendingRSS/monthly/java.xml")
                          :tags #{:trends :tech}
                          :options #{:mark-read-on-view}
                          :proc (proc/make
                                 :post [(proc/exchange [:entry :descriptions] [:entry :contents])
                                        (fn [item] (assoc-in item [:summary :ts]
                                                             (get-in item [:feed :pub-ts])))])}

   :github-trending-rust {:src (src/feed "https://mshibanami.github.io/GitHubTrendingRSS/monthly/rust.xml")
                          :tags #{:trends :tech}
                          :options #{:mark-read-on-view}
                          :proc (proc/make
                                 :post [(proc/exchange [:entry :descriptions] [:entry :contents])
                                        (fn [item] (assoc-in item [:summary :ts]
                                                             (get-in item [:feed :pub-ts])))])}

   :github-trending-clojure {:src (src/feed "https://mshibanami.github.io/GitHubTrendingRSS/monthly/clojure.xml")
                             :tags #{:trends :tech}
                             :options #{:mark-read-on-view}
                             :proc (proc/make
                                    :post [(proc/exchange [:entry :descriptions] [:entry :contents])
                                           (fn [item] (assoc-in item [:summary :ts]
                                                                (get-in item [:feed :pub-ts])))])}

   :github-trending-python {:src (src/feed "https://mshibanami.github.io/GitHubTrendingRSS/monthly/python.xml")
                            :tags #{:trends :tech}
                            :options #{:mark-read-on-view}
                            :proc (proc/make
                                   :post [(proc/exchange [:entry :descriptions] [:entry :contents])
                                          (fn [item] (assoc-in item [:summary :ts]
                                                               (get-in item [:feed :pub-ts])))])}

   :snia-storage {:src (src/feed "http://sniablog.org/feed/atom/")
                  :tags #{:storage :tech}
                  :proc (proc/make
                         :post [(mercury-contents :keep-orig? true)])}

   :katemats {:src (src/feed "https://katemats.com/blog?format=rss")
              :tags #{:blog}}

   :sachachua-emacs {:src (src/feed "http://sachachua.com/blog/category/emacs/feed")
                     :tags #{:emacs :blog}}

   :pragmaticemacs {:src (src/feed "http://pragmaticemacs.com/feed/")
                    :tags #{:emacs :blog}}

   :mattmight {:src (src/feed "http://matt.might.net/articles/feed.rss" :force-update? false)
               :tags #{:blog}
               :proc (proc/make
                      :post [(proc/exchange [:entry :descriptions] [:entry :contents])])}

   :infoq-articles {:src (src/feed "https://www.infoq.com/feed/articles" :force-update? false)
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
                              ])}

   :clearskydata {:src (src/feed "https://www.clearskydata.com/blog/rss.xml")
                  :tags #{:corporate}}

   :cloudendure {:src (src/feed "https://www.cloudendure.com/feed/")
                 :tags #{:corporate}}

   :elastifile {:src (src/feed "https://blog.elastifile.com/rss.xml")
                :tags #{:corporate}}

   :excelero {:src (src/feed "https://www.excelero.com/feed/")
              :tags #{:corporate}}

   :hedviginc {:src (src/feed "https://www.hedviginc.com/blog/rss.xml" :deep? true :force-update true)
               :tags #{:corporate}}

   :igneous {:src (src/feed "https://inside.igneous.io/rss.xml")
             :tags #{:corporate}}

   :iofabric {:src (src/feed "https://www.iofabric.com/feed/")
              :tags #{:corporate}}

   :quobyte {:src (src/feed "https://www.quobyte.com/blog/feed/")
             :tags #{:corporate}}

   :reduxio {:src (src/feed "https://beyondtheblocks.reduxio.com/feed/")
             :tags #{:corporate}}

   :smartiops {:src (src/feed "http://www.smartiops.com/feed/")
               :tags #{:corporate}}

   :snowflake {:src (src/feed "https://www.snowflake.net/feed/")
               :tags #{:corporate}}

   :softnas {:src (src/feed "https://www.softnas.com/wp/feed/")
             :tags #{:corporate}}

   :storpool {:src (src/feed "https://storpool.com/feed")
              :tags #{:corporate}}

   :kinvolk {:src (src/feed "https://kinvolk.io/blog/index.xml")
             :tags #{:corporate}}

   :gruenderszene-de {:src (src/feed "https://www.gruenderszene.de/feed")
                      :options #{:mark-read-on-view}
                      :proc (proc/make
                             :post [(mercury-contents :keep-orig? true)])
                      :tags #{:magazine :tech}}

   :themorningpaper {:src (src/feed "https://blog.acolyer.org/feed/")
                     :proc (proc/make
                            :post [(proc/add-tag :highlight)])
                     :tags #{:sci :tech}}

   :theregister-storage {:src (src/feed "https://www.theregister.co.uk/data_centre/storage/headlines.atom" :force-update? false)
                         :proc (proc/make
                                :post [(fn [item]
                                         (let [url (get-in item [:entry :url])
                                               fixed-url (string/replace url #"go\.theregister\.com/feed/" "")]
                                           (-> item
                                               (assoc-in [:entry :url] fixed-url))))
                                       (mercury-contents :keep-orig? false)])
                         :options #{:mark-read-on-view}
                         :tags #{:storage :magazine}}

   :blocksandfiles {:src (src/wp-json "https://blocksandfiles.com/wp-json/")
                    :options #{:mark-read-on-view}
                    :tags #{:storage :magazine}}

   :infostor {
              ;; :src (src/feed "http://www.infostor.com/index/rssfaq/rss_article_and_wp.noncurrentissue.articles.infostor.html?block" :force-update? false)
              :proc (proc/make
                     :post [(mercury-contents :keep-orig? true)])
              :tags #{:storage :magazine}}

   :qumulo {:src (src/wp-json "https://qumulo.com/wp-json/")
            :tags #{:corporate}}

   :joyent {:src (src/feed "https://www.joyent.com/blog/feed")
            :proc (proc/make
                   :post [(mercury-contents :keep-orig? true)])
            :tags #{:corporate}}

   :wekaio {:src (src/wp-json "https://www.weka.io/wp-json/")
            :tags #{:corporate}}

   :quantum {:src (src/wp-json "https://blog.quantum.com/wp-json/")
             :tags #{:corporate}}

   :objectmatrix {:src (src/wp-json "http://object-matrix.com/wp-json/")
                  :tags #{:corporate}}

   :collabora {:src (src/feed "https://www.collabora.com/newsroom-rss-feed.rss")
               :tags #{:corporate}
               :proc (proc/make
                      :post [(proc/exchange [:entry :descriptions] [:entry :contents])])}

   :systemswelove {:src (src/website "https://systemswe.love/")
                   :tags #{:conference}}

   :signalvnoise {:src (src/feed "https://m.signalvnoise.com/feed")
                  :tags #{:tech :blog}}

   :seriouseats-foodlab {:src (src/feed "https://feeds.feedburner.com/SeriousEats-thefoodlab"
                                        :force-update? false)
                         :proc (proc/make
                                :post [(mercury-contents)])
                         :tags #{:food :AAA}}
   :seriouseats-recipes {:src (src/feed "https://feeds.feedburner.com/seriouseats/recipes"
                                        :force-update? false)
                         :proc (proc/make
                                :post [(mercury-contents)])
                         :tags #{:food :AAA}}

   :ccc-cpu {:src (src/feed "https://cpu.ccc.de/feed/")
             :tags #{:hackerspace}}

   :cbase {:src (src/feed "https://logbuch.c-base.org/feed")
           :tags #{:hackerspace :berlin}}

   :xhain {:src (src/feed "https://x-hain.de/de/index.xml")
           :tags #{:hackerspace :berlin}}

   :uswitch-labs {:src (src/feed "https://medium.com/feed/uswitch-labs")
                  :tags #{:tech}}

   :wirres {:src (src/feed "http://wirres.net/article/rss/full/0/10/")
            :tags #{:blog}}

   :meetups-my {:src (src/feed "https://www.meetup.com/events/rss/139002912/50f0499c4b59a743ecbf7d1e950eb8078ca2cf5b/going")
                :tags #{:berlin :AAA}
                :proc (proc/make
                       :post [(proc/exchange [:entry :descriptions] [:entry :contents])])}

   :berlin-backyard-fleamarkets {:src (src/website "http://polly.sternenlaub.de/fleamarkets/list")
                                 :tags #{:berlin}}

   :netflix-tech {:src (src/feed "https://medium.com/feed/@NetflixTechBlog")
                  :tags #{:tech}}

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
                           :tags #{:pics}}

   :wired {:src (src/feed "https://www.wired.com/feed")
           :options #{:mark-read-on-view}
           :proc (proc/make
                  :post [(mercury-contents)])
           :tags #{:magazine}}

   :erdgeist {:src (src/feed "https://erdgeist.org/blog/rss.xml")
              :tags #{:blog}
              :proc (proc/make
                     :post [(proc/exchange [:entry :descriptions] [:entry :contents])])}

   :martin-kleppmann {:src (src/feed "https://feeds.feedburner.com/martinkl")
              :tags #{:blog}}

   :frankrieger {:src (src/feed "http://frank.geekheim.de/?feed=rss2")
                 :tags #{:blog}}

   :corydoctorow {:src (src/feed "https://craphound.com/feed/")
                  :tags #{:politics}}

   :juliaevans {:src (src/feed "https://jvns.ca/atom.xml")
                :tags #{:blog}}

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
                  :tags #{:blog}}

   :slatestarcodex {:src (src/wp-json "http://slatestarcodex.com/wp-json/")
                    :tags #{:blog}}

   :joschabach {:src (src/feed "http://bach.ai/feed.xml")
                :tags #{:blog}}

   :baldurbjarnason {:src (src/feed "https://www.baldurbjarnason.com/index.xml" :deep? true)
                     :tags #{:blog}}

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
           :tags #{:tech}}

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
                                                      ;; (log/info title author index)
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
                :tags #{:tech :deep-tech}}

   :thenewstack {:src (src/feed "https://thenewstack.io/feed/")
                 :options #{:mark-read-on-view}
                 :tags #{:tech}}

   :mechanical-sympathy {:src (src/feed "https://mechanical-sympathy.blogspot.com/feeds/posts/default")
                         :tags #{:tech}}

   :igoro {:src (src/feed "http://feeds.feedburner.com/igoro")
           :tags #{:tech}}

   :randomascii {:src (src/feed "https://randomascii.wordpress.com/feed/")
                 :tags #{:tech}}

   :sdn-clinic {:src (src/wp-json "https://blog.sdn.clinic/wp-json/")
                :tags #{:tech}}

   :scotthyoung {:src (src/feed "https://www.scotthyoung.com/blog/feed/")
                 :tags #{:zzz-abandoned-blogs}}

   :scrively {:src (src/wp-json "http://scrively.org/wp-json/")
              :tags #{:blog}}
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
              :options #{:main-list-use-description}}

   :rogerebert {:src (src/feed "https://www.rogerebert.com/feed")
                :options #{:mark-read-on-view}
                :tags #{:movies}}

   :movieweb-reviews {:src (src/feed "https://movieweb.com/rss/movie-reviews/")
                      :options #{:mark-read-on-view}
                      :proc (proc/make
                             :post [(mercury-contents)])
                      :tags #{:movies}}

   :screenrant {:src (src/feed "https://screenrant.com/feed/")
                :options #{:mark-read-on-view}
                :tags #{:movies :gaming}}

   :comingsoon {:src (src/wp-json "https://www.comingsoon.net/wp-json/")
                :options #{:mark-read-on-view}
                :tags #{:movies :gaming}}

   :filmfestivals {:src (src/feed "https://www.filmfestivals.com/taxonomy/term/31%2B32%2B590757/0/feed")
                   :options #{:mark-read-on-view}
                   :tags #{:movies}}

   :pedestrianobservations {:src (src/feed "https://pedestrianobservations.com/feed/")
                            :tags #{:zzz-abandoned-blogs}}

   :drewdevault {:src (src/feed "https://drewdevault.com/feed.xml")
                 :tags #{:blog}
                 :proc (proc/make
                        :post [(proc/exchange
                                [:entry :descriptions]
                                [:entry :contents])])}

   :dereksivers {:src (src/feed "https://sivers.org/en.atom")
                 :tags #{:blog}}

   :waitbutwhy {:src (src/wp-json "https://waitbutwhy.com/wp-json/" :user-agent :bot)
                :tags #{:blog}}

   :ribbonfarm {:src (src/wp-json "https://www.ribbonfarm.com/wp-json/")
                :tags #{:zzz-abandoned-blogs}}

   :unixe {:src (src/feed "https://www.unixe.de/feed/")
           :tags #{:blog}}

   :syonyk {:src (src/feed "https://syonyk.blogspot.com/feeds/posts/default")
            :tags #{:blog}}

   :absorptions {:src (src/feed "http://www.windytan.com/feeds/posts/default")
                 :tags #{:blog}}

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
           :tags #{:blog}}

   :granolashotgun {:src (src/feed "https://www.granolashotgun.com/granolashotguncom?format=rss")
                    :tags #{:design}}

   :ga-quobyte {:src (src/feed "https://www.google.com/alerts/feeds/12214230541754356951/11912435152804193698")
                :options #{:mark-read-on-view}
                :proc (proc/make
                       :post [(proc/add-tag :highlight)])
                :tags #{:google-alert :tech}}

   :ga-marcellauhoff {:src (src/feed "https://www.google.com/alerts/feeds/12214230541754356951/17432466600270792644")
                      :options #{:mark-read-on-view}
                      :proc (proc/make
                             :post [(proc/add-tag :highlight)])
                      :tags #{:google-alert}}

   :ga-ml-irq0-org {:src (src/feed "https://www.google.com/alerts/feeds/12214230541754356951/6287790772305614620")
                    :options #{:mark-read-on-view}
                    :proc (proc/make
                           :post [(proc/add-tag :highlight)])
                    :tags #{:google-alert}}

   :ga-job-search {:src (src/feed "https://www.google.com/alerts/feeds/12214230541754356951/18101484729339824556")
                   :options #{:mark-read-on-view}
                   :proc (proc/make
                          :post [(proc/add-tag :highlight)])
                   :tags #{:google-alert}}

   :gutmet {:src (src/feed "https://gutmet.org/blog/feed.rss")
            :tags #{:blog}}

   :n-gate {:src (src/feed "http://n-gate.com/index.atom")
            :tags #{:blog}}

   :youtube-rickbeato {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCJquYOG5EL82sKTfH9aMA9Q")
                       :options #{:mark-read-on-view}
                       :tags #{:music :youtube-channel}}

   :youtube-japanology {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCvDzbjrw5B5RW10HyJbxX9g")
                        :options #{:mark-read-on-view}
                        :tags #{:recreation :youtube-channel}}
   :lastweektonight {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UC3XTzVzaHQEd30rQbuvCtTQ")
                     :options #{:mark-read-on-view}
                     :tags #{:youtube-channel}
                     :proc (proc/make
                            :post [(proc/add-tag :download)])}

   :lgr {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCLx053rWZxCiYWsBETgdKrQ")
         :options #{:mark-read-on-view}
         :tags #{:youtube-channel :recreation :retro}}

   :cppcon {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCMlGfpWw-RUdWX_JbLCukXg")
            :options #{:mark-read-on-view}
            :tags #{:youtube-channel :conference}}

   :mit-distributed-systems {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UC_7WrbZTCODu1o_kfUMq88g")
                             :options #{:mark-read-on-view}
                             :proc (proc/make
                                    :post [(proc/add-tag :download)])
                             :tags #{:youtube-channel :course}}

   :clojure-tv {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCaLlzGqiPE2QRj6sSOawJRg")
                :options #{:mark-read-on-view}
                :tags #{:youtube-channel :tech :clojure}}

   :youtube-usenix {:src (src/feed
                          "https://www.youtube.com/feeds/videos.xml?channel_id=UC4-GrpQBx6WCGwmwozP744Q")
                    :options #{:mark-read-on-view}
                    :tags #{:youtube-channel :tech :sci}}

   :mailab {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCyHDQ5C6z1NDmJ4g6SerW8g")
            :options #{:mark-read-on-view}
            :tags #{:youtube-channel :sci}}

   :youtube-acm {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCPyA0XmU6aS4JCwVoIBTmIQ")
                 :options #{:mark-read-on-view}
                 :tags #{:youtube-channel :sci}}

   :emacsrocks {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCkRmQ_G_NbdbCQMpALg6UPg")
                :options #{:mark-read-on-view}
                :tags #{:youtube-channel :tech :emacs}}

   :theartoftheproblem {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCotwjyJnb-4KW7bmsOoLfkg")
                        :options #{:mark-read-on-view}
                        :tags #{:youtube-channel :tech}}

   :kurzgesagt {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCsXVk37bltHxD1rDPwtNM8Q")
                :options #{:mark-read-on-view}
                :tags #{:youtube-channel :sci}}

   :crashcourse {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCX6b17PVsYBQ0ip5gyeme-Q")
                 :options #{:mark-read-on-view}
                 :tags #{:youtube-channel :sci}}

   :computer-history-museum {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCHDr4RtxwA1KqKGwxgdK4Vg")
                             :options #{:mark-read-on-view}
                             :tags #{:youtube-channel :sci}}

   :two-minute-papers {:src (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCbfYPyITQ-7l4upoX8nvctg")
                       :options #{:mark-read-on-view}
                       :tags #{:youtube-channel :sci}}

   :realworldtech {:src (src/wp-json "https://www.realworldtech.com/wp-json/")
                   :options #{:mark-read-on-view}
                   :tags #{:tech}}

   :pavelmayer {:src (src/feed "https://pavelmayer.de/feed/")
                :tags #{:blog}}

   :dmeister {:src (src/feed "https://dmeister.github.io/blog/atom.xml")
              :tags #{:blog}}

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
                                                              u1f596.http/sanitize hickory-to-html)]
                                                 (assoc-in item [:entry :contents]
                                                           {"text/html" html
                                                            "text/plain" (converter/html2text html)})))])}
   :isotopp {:src (src/feed "https://blog.koehntopp.info/feed.xml")
             :tags #{:blog}}

   :netzpolitik {:src (src/wp-json "https://netzpolitik.org/wp-json/")
                 :options #{:mark-read-on-view}
                 :tags #{:politics :magazine}}
   :volksverpetzer {:src (src/wp-json "https://www.volksverpetzer.de/wp-json/")
                    :options #{:mark-read-on-view}
                    :tags #{:politics}}

   :correctiv {:src (src/wp-json "https://correctiv.org/wp-json/")
               :options #{:mark-read-on-view}
               :tags #{:politics}}

   :malleablesystems {:src (src/feed "https://malleable.systems/blog/index.xml")
                      :tags #{:tech :emacs}}

   :tagesschau {:src (src/feed "https://www.tagesschau.de/xml/rss2")
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
                                             u1f596.http/sanitize hickory-to-html)]
                                (-> item
                                    (assoc-in [:entry :authors] authors)
                                    (assoc-in [:entry :contents]
                                              {"text/html" html
                                               "text/plain" (converter/html2text html)}))))]
                           )
                    :options #{:mark-read-on-view :main-list-use-description}
                    :tags #{:news}}
   :taz-online {:src (src/feed "https://taz.de/!s=&ExportStatus=Intern&SuchRahmen=Online;atom/")
                :proc (proc/make
                       :post [(mercury-contents)
                              (proc/exchange [:entry :descriptions] [:entry :contents])])
                :options #{:mark-read-on-view :main-list-use-description}
                :tags #{:news}}

   :tagesspiegel {:src (src/feed "https://www.tagesspiegel.de/contentexport/feed/home")
                  :proc (proc/make
                         :filter (make-category-filter-deny ["Sport"])
                         :post [(mercury-contents)])
                  :options #{:mark-read-on-view :main-list-use-description}
                  :tags #{:news}}

   :nytimes-mostshared {:src (src/feed
                              "https://rss.nytimes.com/services/xml/rss/nyt/MostShared.xml")
                        :proc (proc/make
                         :post [(mercury-contents)])
                        :options #{:mark-read-on-view :main-list-use-description}
                        :tags #{:news}}

   :nytimes-mostemailed {:src (src/feed
                              "https://rss.nytimes.com/services/xml/rss/nyt/MostEmailed.xml")
                         :proc (proc/make
                                :post [(mercury-contents)])
                         :options #{:mark-read-on-view :main-list-use-description}
                         :tags #{:news}}

   :nytimes-mostviewed {:src (src/feed
                              "https://rss.nytimes.com/services/xml/rss/nyt/MostViewed.xml")
                         :proc (proc/make
                                :post [(mercury-contents)])
                         :options #{:mark-read-on-view :main-list-use-description}
                         :tags #{:news}}

   :nytimes-top {:src (src/feed
                       "https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml")
                 :proc (proc/make
                        :filter (make-category-filter-deny ["Sport" "Fraternities and Sororities" "Horse Racing" "Contests and Prizes"])
                        :post [(mercury-contents)])
                 :options #{:mark-read-on-view :main-list-use-description}
                 :tags #{:news}}
  })
