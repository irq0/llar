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
   [schema.core :as s]
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

(defonce ^:private srcs (atom {}))

(defn get-sources [] @srcs)

(defn get-source [k] (get @srcs k))

(defn remove-source [k] (keys (swap! srcs dissoc k)))

(defmacro wrap-proc [src-key tags options & body]
  (when-not (nil? body)
    `(fn [item#]
       (let [~'$item item#
             ~'$key ~src-key
             ~'$title (get-in item# [:summary :title])
             ~'$authors (get-in item# [:entry :authors])
             ~'$tags ~tags
             ~'$raw (get item# :raw)
             ~'$url (get-in item# [:entry :url])
             ~'$html (get-in item# [:entry :contents "text/html"])
             ~'$text (get-in item# [:entry :contents "text/plain"])
             ~'$options ~options
             ~'$entry (:entry item#)]
         (do ~@body)))))

;; TODO validate filter code. get dummy from source; pass through see if item exists and things
;; TODO validate pre / post
(defmacro fetch
  [src-key src & body]
  (let [{:keys [options tags post pre rm post-fns pre-fns rm-fn]
         :or {options #{} tags #{}}
         :as params}
        (apply hash-map body)

        src-kw (keyword src-key)

        pre (cond (some? pre-fns) pre-fns
                  (some? pre) [`(wrap-proc ~src-kw ~tags ~options ~pre)]
                  :default nil)

        post (cond (some? post-fns) post-fns
                  (some? post) [`(wrap-proc ~src-kw ~tags ~options ~post)]
                  :default nil)

        rm (cond (some? rm-fn) rm-fn
                  (some? rm) `(wrap-proc ~src-kw ~tags ~options ~rm)
                  :default '(constantly false))]


    (s/validate #{s/Keyword} tags)
    (s/validate #{s/Keyword} options)

    `(do (swap! srcs assoc (keyword '~src-key)
                {:src ~src
                 :options ~options
                 :tags ~tags
                 :proc (proc/new {:rm ~rm
                                  :pre ~pre
                                  :post ~post})})
         (keyword '~src-key))))


(defmacro fetch-reddit
  [src & body]
  (let [{:keys [options tags min-score dynamic?]
         :or {options #{} tags #{} min-score 0 dynamic? true}
         :as params} (apply hash-map body)]
    (s/validate #{s/Keyword} tags)
    (s/validate #{s/Keyword} options)

    `(do (let [src-key# (keyword (str "reddit-" (string/lower-case (:subreddit ~src))))]
           (swap! srcs assoc src-key#
                  {:src ~src
                   :options ~options
                   :tags (conj ~tags :reddit)
                   :proc (make-reddit-proc src-key# ~src {:min-score ~min-score
                                                          :dynamic? ~dynamic?})})
          (keyword src-key#)))))

(fetch twit-c3pb (src/twitter-search "c3pb" (:twitter-api creds))
       :rm (->> $item
                :entry
                :type
                #{:retweet})
       :tags #{:paderborn})

(fetch twit-berlin-pics (src/twitter-search "#berlin filter:images" (:twitter-api creds))
       :options #{:mark-read-on-view}
       :rm (or (#{:retreet} (:type $entry))
               (= (count (get-in $entry [:entities :photos])) 0)
               (re-find #"pussy|porn|camsex|webcam" $text))
       :post (assoc-in $item [:entry :contents "text/html"]
                       (format "<img src=\"%s\"/>"
                               (first (get-in $entry [:entities :photos]))))
       :tags #{:pics})

(fetch twitter-timeline (src/twitter-timeline (:twitter-api creds)))



(fetch usenix-login
       (src/selector-feed "https://www.usenix.org/publications/loginonline"
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
                           :content (fn [l] (log/spy l) (->> l first :content))})

       :tags #{:deep-tech :sci})

(fetch fefe
       (src/selector-feed "https://blog.fefe.de"
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
       :tags #{:blog})

(fetch paulgraham (src/selector-feed "http://www.paulgraham.com/articles.html"
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
       :tags #{:blog})

(fetch xahteiwi (src/feed "https://xahteiwi.eu/feeds/all.atom.xml")
       :tags #{:blog})

(fetch irq0 (src/feed "http://irq0.org/rss.xml")
       :tags #{:blog})

(fetch oreilly-radar (src/feed "https://www.oreilly.com/radar/feed/index.xml")
       :options #{:mark-read-on-view}
       :rm (string/includes? $title "Four short links")
       :post-fns [(mercury-contents :keep-orig? true)]
       :tags #{:tech :magazine})

(fetch oreilly-fourshortlinks (src/feed "https://www.oreilly.com/radar/topics/four-short-links/feed/index.xml")
       :post-fns  [(proc/add-tag :highlight) (mercury-contents :keep-orig? false)]
       :tags #{:tech})

(fetch danluu (src/feed "https://danluu.com/atom.xml")
       :tags #{:blog})

(fetch benkuhn (src/feed "https://www.benkuhn.net/index.xml")
       :tags #{:blog})

(fetch ridiculousfish (src/feed "http://ridiculousfish.com/blog/atom.xml")
       :tags #{:tech :blog})

(fetch rachelbythebay (src/feed "https://rachelbythebay.com/w/atom.xml")
       :tags #{:tech :blog})

(fetch codinghorror (src/feed "http://feeds.feedburner.com/codinghorror")
       :tags #{:tech :blog})

(fetch joel-on-software (src/feed "https://www.joelonsoftware.com/feed/")
       :tags #{:tech :blog})

(fetch summit-route (src/feed "http://summitroute.com/blog/feed.xml")
       :tags #{:tech :blog}
       :post-fns [(proc/exchange [:entry :descriptions] [:entry :contents])])

(fetch joe-duffy (src/feed "http://joeduffyblog.com/feed.xml" :deep? true)
       :tags #{:tech :blog})

(fetch aphyr (src/feed "https://aphyr.com/posts.atom")
       :tags #{:tech :blog})

(fetch n99pi (src/feed "https://feeds.feedburner.com/99pi")
       :tags #{:design}
       :post-fns [(proc/add-tag :highlight)])

(fetch stackoverflow-engineering (src/feed "https://stackoverflow.blog/engineering/feed/")
       :tags #{:tech})

(fetch soundcloud-backstage (src/feed "https://developers.soundcloud.com/blog.rss")
       :post-fns [(proc/exchange [:entry :descriptions] [:entry :contents])]
       :tags #{:tech})

(fetch weekly-programming-digest (src/feed "http://feeds.feedburner.com/digest-programming")
       :tags #{:tech :digest}
       :post-fns [(proc/add-tag :highlight)
                  (proc/exchange [:entry :descriptions] [:entry :contents])])

(fetch acm-queue (src/feed "https://queue.acm.org/rss/feeds/queuecontent.xml"
                           :user-agent :browser
                                 :force-update? true)
       :post-fns [(mercury-contents :keep-orig? true)]
       :tags #{:tech :magazine :sci})


(fetch-reddit (src/reddit "DIY" :top) :min-score 2000)
(fetch-reddit (src/reddit "DataHoarder" :top) :min-score 10
              :options #{:mark-read-on-view}
              :tags #{:tech})
(fetch-reddit (src/reddit "fascinating" :top) :min-score 100)

(fetch-reddit (src/reddit "Clojure" :top) :min-score 50
              :options #{:mark-read-on-view}
              :tags #{:reddit :tech})

(fetch-reddit (src/reddit "listentothis" :top) :min-score 150
              :tags #{:reddit :music})


(fetch-reddit (src/reddit "europe" :top) :min-score 2500
              :options #{:mark-read-on-view})

(fetch-reddit (src/reddit "educationalgifs" :top) :min-score 4000)
(fetch-reddit (src/reddit "Foodforthought" :top) :min-score 400)

(fetch-reddit (src/reddit "ifyoulikeblank" :top) :min-score 50
              :tags #{:music})

(fetch-reddit (src/reddit "dataisbeautiful" :top) :min-score 5000
              :tags #{:sci})

(fetch-reddit (src/reddit "postrock" :top) :min-score 15
              :tags #{:music})
(fetch-reddit (src/reddit "albumaday" :top) :min-score 10
              :tags #{:music})

(fetch-reddit (src/reddit "Albumoftheday" :top) :min-score 5
                          :tags #{:music})
(fetch-reddit (src/reddit "listentoconcerts" :top) :min-score 5
              :tags #{:music})

(fetch-reddit (src/reddit "indie" :top) :min-score 15
              :tags #{:music})

(fetch-reddit (src/reddit "jwd" :top) :min-score 5
              :options #{:mark-read-on-view}
              :tags #{:berlin})

(fetch-reddit (src/reddit "berlin" :top) :min-score 5
              :options #{:mark-read-on-view}
              :tags #{:berlin})

(fetch-reddit (src/reddit "berlinshopping" :top) :min-score 1
              :tags #{:berlin})

(fetch-reddit (src/reddit "games" :top) :min-score 2000
              :options #{:mark-read-on-view}
              :tags #{:gaming})

(fetch-reddit (src/reddit "storage" :top) :min-score 10
              :options #{:mark-read-on-view}
              :tags #{:storage})

(fetch-reddit (src/reddit "malelivingspace" :top) :min-score 150
              :options #{:mark-read-on-view}
              :tags #{:recreation})

(fetch-reddit (src/reddit "experienceddevs" :top) :min-score 70
              :options #{:mark-read-on-view}
              :tags #{:tech})


(fetch gamasutra-pc (src/feed "http://feeds.feedburner.com/GamasutraConsolePCNews")
       :options #{:mark-read-on-view}
       :post-fns [(mercury-contents :keep-orig? true)]
       :tags #{:gaming})

(fetch muspy (src/feed "https://muspy.com/feed?id=1ub5u1nk72w26hnpeiyfuvto9owxfd")
       :tags #{:music})

(fetch humblebundle (src/feed "http://blog.humblebundle.com/rss")
       :options #{:mark-read-on-view}
       :post-fns [(proc/exchange [:entry :descriptions] [:entry :contents])]
       :tags #{:shopping})

(fetch vice (src/feed "https://www.vice.com/en_us/rss" :deep? true :force-update? true)
       :options #{:mark-read-on-view}
       :tags #{:magazine})

(fetch hn-top (src/hn "topstories" :throttle-secs (* 23 60))
       :tags #{:hackernews}
       :options #{:mark-read-on-view}
       :post-fns [(mercury-contents :keep-orig? true)]
       :rm-fn (make-hacker-news-filter 350 150))

(fetch hn-best (src/hn "beststories" :throttle-secs (* 23 60))
       :tags #{:hackernews}
       :options #{:mark-read-on-view}
       :post-fns [(mercury-contents :keep-orig? true)]
       :rm-fn (make-hacker-news-filter 350 150))

(fetch hn-ask (src/hn "askstories" :throttle-secs (* 5 60  60))
       :tags #{:hackernews}
       :options #{:mark-read-on-view}
       :post-fns [(mercury-contents :keep-orig? true)]
       :rm-fn (make-hacker-news-filter 200 100))

(fetch hn-show (src/hn "showstories" :throttle-secs (* 5 60  60))
       :tags #{:hackernews}
       :options #{:mark-read-on-view}
       :post-fns [(mercury-contents :keep-orig? true)]
       :rm-fn (make-hacker-news-filter 200 100))

(fetch xkcd (src/feed "https://xkcd.com/rss.xml")
       :tags #{:AAA :comics}
       :post (let [hick (-> (hick/parse (get-in $item [:entry :descriptions "text/html"])) hickory.core/as-hickory)
                   alt-text (-> (S/select
                                 (S/descendant
                                  (S/tag :img)) hick)
                                first :attrs :alt)]
               (-> $item
                   (assoc-in [:entry :contents "text/html"]
                             (str
                              (get-in $item [:entry :descriptions "text/html"])
                              "<p>" alt-text "</p>"))))
          :tags #{:AAA :comics})

(fetch daily-wtf (src/feed "http://syndication.thedailywtf.com/TheDailyWtf")
       :tags #{:recreation}
       :rm (re-find #"^(Sponsor Post|CodeSOD|Error'd|Representative Line):" $title)
       :post-fns [(proc/exchange [:entry :descriptions] [:entry :contents])])

(fetch atlantic-best-of (src/feed "https://www.theatlantic.com/feed/best-of/")
       :options #{:main-list-use-description}
       :tags #{:magazine})

(fetch fail (src/feed "http://irq0.org/404"))
                               ;; :post [(proc/add-tag-filter :highlight
                               ;;                             #(= (:source-key %)
                               ;;                                 :newsletter-hacker_newsletter))

(fetch newsletter-mailbox (src/imap "imap://mail.cpu0.net/NEWSLETTER" (:imap creds))
       ;; TODO move this to fetch/
       :post (let [sane-html (some-> $html
                                     hick/parse
                                     hick/as-hickory
                                     http/sanitize
                                     http/blobify
                                     hickory-to-html)
                   headers (:headers $raw)
                   find-header (fn [k] (some-> (filter
                                                (fn [x]
                                                  (= (some-> x first key string/lower-case) k))
                                                headers)
                                               first first val))
                   mail-sender (or
                                (some-> $raw :from first :name)
                                (find-header "sender")
                                (find-header "from")
                                (find-header "list-id")
                                "unknown")
                   src-key (str "newsletter-"
                                (-> mail-sender
                                    (string/replace #"[^\w]" "_")
                                    string/lower-case))
                   src-name (str "[NEWSLETTER: " mail-sender)]
               (-> $item
                   (assoc-in [:entry :contents "text/html"] sane-html)
                   (assoc-in [:meta :source-key] src-key)
                   (assoc-in [:meta :source-name] src-name))))

(fetch pocoo-lucumr (src/feed "http://lucumr.pocoo.org/feed.atom")
       :tags #{:tech})

(fetch programmingisterrible (src/feed "http://programmingisterrible.com/rss")
       :tags #{:tech :blog}
       :post-fns [(proc/exchange [:entry :descriptions] [:entry :contents])])

(fetch preshing (src/feed "http://preshing.com/feed")
       :tags #{:tech :blog})

(fetch elibendersky (src/feed "http://eli.thegreenplace.net/feeds/all.atom.xml")
       :tags #{:tech :blog}
       :post-fns [(proc/exchange [:entry :descriptions] [:entry :contents])])

(fetch cyanidehappiness (src/feed "https://feeds.feedburner.com/Explosm")
       :post (let [h (:hickory (http/fetch $url :user-agent :browser))
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
               (-> $item
                   (assoc-in [:entry :authors]
                             [author])
                   (assoc-in [:entry :contents "text/html"]
                             (format "<img src=\"%s\"/>" comic-link))))
       :tags #{:AAA})

(fetch bookmark nil :tags #{:bookmark})

(fetch tumblr-worstofchefkoch (src/feed "https://worstofchefkoch.tumblr.com/rss"
                                        :user-agent :bot)
       :tags #{:recreation})

(fetch tumblr-runningahackerspace (src/feed "https://runningahackerspace.tumblr.com/rss"
                                            :user-agent :bot)
       :tags #{:recreation}
       :post-fns [(proc/exchange [:entry :descriptions]
                             [:entry :contents])])

(fetch berlintypography (src/feed "https://berlintypography.wordpress.com/feed/")
       :tags #{:design})

(fetch iconicphotos (src/feed "https://iconicphotos.wordpress.com/feed/")
       :tags #{:pics :recreation :design :AAA})

(fetch allthingsdistributed (src/feed "http://www.allthingsdistributed.com/atom.xml" :force-update? false)
       :tags #{:tech :blog})

(fetch github-paperswelove (src/feed "https://github.com/papers-we-love/papers-we-love/commits/master.atom")
       :tags #{:sci})

(fetch economist-scitech (src/feed "https://www.economist.com/science-and-technology/rss.xml")
       :options #{:mark-read-on-view}
       :tags #{:magazine}
       :post-fns [(proc/exchange [:entry :descriptions] [:entry :contents])])

(fetch theverge (src/feed "https://www.theverge.com/rss/full.xml")
       :options #{:mark-read-on-view}
       :tags #{:magazine})

(fetch vox (src/feed "https://www.vox.com/rss/index.xml")
       :options #{:mark-read-on-view}
       :tags #{:magazine})

(fetch ccc (src/feed "http://www.ccc.de/de/rss/updates.rdf")
       :tags #{:politics :AAA}
       :post-fns [(proc/exchange [:entry :descriptions] [:entry :contents])])

(fetch flarp (src/feed "https://www.flarp.de/index.xml")
       :post-fns [(proc/exchange [:entry :descriptions] [:entry :contents])]
       :tags #{:berlin})

(fetch inside-hpc (src/feed "http://feeds.feedburner.com/insidehpc")
       :options #{:mark-read-on-view}
       :tags #{:tech}
       :post-fns [(proc/exchange [:entry :descriptions] [:entry :contents])])

(fetch github-trending-c++ (src/feed "https://mshibanami.github.io/GitHubTrendingRSS/monthly/c++.xml")
       :tags #{:trends :tech}
       :options #{:mark-read-on-view}
       :post-fns [(proc/exchange [:entry :descriptions] [:entry :contents])
                  (fn [item] (assoc-in item [:summary :ts] (get-in item [:feed :pub-ts])))])

(fetch github-trending-java (src/feed "https://mshibanami.github.io/GitHubTrendingRSS/monthly/java.xml")
       :tags #{:trends :tech}
       :options #{:mark-read-on-view}
       :post-fns [(proc/exchange [:entry :descriptions] [:entry :contents])
                  (fn [item] (assoc-in item [:summary :ts] (get-in item [:feed :pub-ts])))])

(fetch github-trending-rust (src/feed "https://mshibanami.github.io/GitHubTrendingRSS/monthly/rust.xml")
       :tags #{:trends :tech}
       :options #{:mark-read-on-view}
       :post-fns [(proc/exchange [:entry :descriptions] [:entry :contents])
                  (fn [item] (assoc-in item [:summary :ts] (get-in item [:feed :pub-ts])))])

(fetch github-trending-clojure (src/feed "https://mshibanami.github.io/GitHubTrendingRSS/monthly/clojure.xml")
       :tags #{:trends :tech}
       :options #{:mark-read-on-view}
       :post-fns [(proc/exchange [:entry :descriptions] [:entry :contents])
                  (fn [item] (assoc-in item [:summary :ts] (get-in item [:feed :pub-ts])))])

(fetch github-trending-python (src/feed "https://mshibanami.github.io/GitHubTrendingRSS/monthly/python.xml")
       :tags #{:trends :tech}
       :options #{:mark-read-on-view}
       :post [(proc/exchange [:entry :descriptions] [:entry :contents])
              (fn [item] (assoc-in item [:summary :ts] (get-in item [:feed :pub-ts])))])

(fetch snia-storage (src/feed "http://sniablog.org/feed/atom/")
       :tags #{:storage :tech}
       :post-fns [(mercury-contents :keep-orig? true)])

(fetch katemats (src/feed "https://katemats.com/blog?format=rss")
       :tags #{:blog})

(fetch sachachua-emacs (src/feed "http://sachachua.com/blog/category/emacs/feed")
       :tags #{:emacs :blog})

(fetch pragmaticemacs (src/feed "http://pragmaticemacs.com/feed/")
       :tags #{:emacs :blog})

(fetch infoq-articles (src/feed "https://www.infoq.com/feed/articles" :force-update? false)
       :post  (if-let [title-without (second (re-find #"Article: (.+)" $title))]
                (-> $item
                    (assoc-in [:summary :title] title-without)
                    (apply (mercury-contents :keep-orig? true))))
       :tags #{:tech})

(fetch quobyte (src/feed "https://www.quobyte.com/blog/feed/")
       :tags #{:tech :corporate})

(fetch gruenderszene-de (src/feed "https://www.gruenderszene.de/feed")
       :options #{:mark-read-on-view}
       :post-fns [(mercury-contents :keep-orig? true)]
       :tags #{:magazine :tech})

(fetch themorningpaper (src/feed "https://blog.acolyer.org/feed/")
       :post-fns [(proc/add-tag :highlight)]
       :tags #{:sci :tech :AAA})

(fetch theregister-storage (src/feed "https://www.theregister.co.uk/data_centre/storage/headlines.atom" :force-update? false)
       :post (-> $item
                 (assoc-in [:entry :url] (string/replace $url #"go\.theregister\.com/feed/" ""))
                 (apply (mercury-contents)))
       :options #{:mark-read-on-view}
       :tags #{:storage :magazine})

(fetch blocksandfiles (src/wp-json "https://blocksandfiles.com/wp-json/")
       :options #{:mark-read-on-view}
       :tags #{:storage :magazine})

(fetch seriouseats-foodlab (src/feed "https://feeds.feedburner.com/SeriousEats-thefoodlab")
       :post-fns [(mercury-contents)]
       :tags #{:food :AAA})

(fetch seriouseats-recipes (src/feed "https://feeds.feedburner.com/seriouseats/recipes")
       :post-fns [(mercury-contents)]
       :tags #{:food :AAA})

(fetch cbase (src/feed "https://logbuch.c-base.org/feed")
       :tags #{:hackerspace :berlin})

(fetch netflix-tech (src/feed "https://medium.com/feed/@NetflixTechBlog")
       :tags #{:tech})

(fetch nasa-image-of-the-day (src/feed "https://www.nasa.gov/rss/dyn/image_of_the_day.rss")
       :options #{:mark-read-on-view}
       :post (let [orig-img-url (some-> $raw :enclosures first :url)
                   img-url (http/try-blobify-url! orig-img-url)
                   descr (get-in $entry [:descriptions "text/plain"])
                   content {"text/plain" (string/join "\n"
                                                      [$title orig-img-url descr])
                            "text/html" (hiccup.core/html
                                         [:div
                                          [:h1 $title]
                                          [:img {:src img-url
                                                 :orig-src orig-img-url}]
                                          [:p descr]])}]
                  (-> $item
                                                 (assoc-in [:entry :contents] content)
                                                 (assoc-in [:entry :lead-image-url] img-url)))
       :tags #{:pics})

(fetch wired (src/feed "https://www.wired.com/feed")
       :options #{:mark-read-on-view}
       :post-fns [(mercury-contents)]
       :tags #{:magazine})

(fetch erdgeist (src/feed "https://erdgeist.org/blog/rss.xml")
       :tags #{:blog}
       :post-fns [(proc/exchange [:entry :descriptions] [:entry :contents])])

(fetch martin-kleppmann (src/feed "https://feeds.feedburner.com/martinkl")
       :tags #{:blog})

(fetch frankrieger (src/feed "http://frank.geekheim.de/?feed=rss2")
       :tags #{:blog})

(fetch joschabach (src/feed "http://bach.ai/feed.xml")
       :tags #{:blog})

(fetch golem (src/feed "https://rss.golem.de/rss_sub_media.php?token=7t10zqba")
       :options #{:mark-read-on-view}
       :pre (let [raw-html (get-in $entry [:descriptions "text/plain"])
                  html (some-> raw-html
                               hick/parse
                               hick/as-hickory
                               (http/sanitize :remove-css? true)
                               http/blobify
                               hickory-to-html)]
              (-> $item
                  (assoc-in [:entry :descriptions]
                            {"text/plain" ""})
                  (assoc-in [:entry :contents]
                            {"text/html" html
                             "text/plain" (converter/html2text html)})))
       :tags #{:tech})

(fetch lwn-weekly (src/website+paywall "https://lwn.net/current"
                                          (fn [] (let [cs (http-cookies/cookie-store)]
                                                   (http-client/post "https://lwn.net/Login/"
                                                                     {:form-params (:lwn creds)
                                                                      :cookie-store cs})
                                                   cs))
                                          :user-agent :browser)
       :pre (let [new-items
                  (->> (S/select (S/descendant
                                  (S/class "SummarySection"))
                                 (:hickory $item))
                       first :content
                       (reduce (fn [r $item]
                                 (let [hl-hick (S/select (S/and (S/tag :h3) (S/class "SummaryHL")) $item)
                                       meta-hick (S/select (S/class "FeatureByline") $item)
                                       title (-> hl-hick first :content first :content first)
                                       author (-> meta-hick first :content second :content first)
                                       index (dec (count r))]
                                   (cond
                                     (string? title)
                                     (conj r {:title title
                                              :hick []
                                              :author nil})

                                     (string? author)
                                     (assoc-in r [index :author] author)

                                     (>= index 0)
                                     (update-in r [index :hick] conj $item)

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
                (-> $item
                    (assoc :hash (make-item-hash title))
                    (assoc-in [:summary :title] title)
                    (assoc-in [:entry :title] title)
                    (assoc :hickory hick)
                    (assoc-in [:entry :authors] [author])
                    (assoc-in [:entry :descriptions]
                              {"text/plain" ""})
                    (assoc-in [:entry :contents]
                              {"text/html" html
                               "text/plain" (converter/html2text html)}))))
            :tags #{:tech :deep-tech})

(fetch thenewstack (src/feed "https://thenewstack.io/feed/")
       :options #{:mark-read-on-view}
       :tags #{:tech})

(fetch mechanical-sympathy (src/feed "https://mechanical-sympathy.blogspot.com/feeds/posts/default")
       :tags #{:tech})

(fetch igoro (src/feed "http://feeds.feedburner.com/igoro")
       :tags #{:tech})

(fetch randomascii (src/feed "https://randomascii.wordpress.com/feed/")
       :tags #{:tech})

(fetch sdn-clinic (src/wp-json "https://blog.sdn.clinic/wp-json/")
       :tags #{:tech})

(fetch longform (src/feed "https://longform.org/feed.rss")
       :pre-fns [(mercury-contents)
                 (fn [item] (let [redirect-page (http/fetch (get-in item [:entry :url]))
                                  real-article-link (some-> (S/select
                                                             (S/descendant
                                                              (S/class "post__link"))
                                                             (:hickory redirect-page))
                                                        first :attrs :href)]
                             (assoc-in item [:entry :url] (uri/uri real-article-link))))]
       :tags #{:magazine}
       :options #{:main-list-use-description})

(fetch waitbutwhy (src/wp-json "https://waitbutwhy.com/wp-json/" :user-agent :bot)
       :tags #{:blog})

(fetch absorptions (src/feed "http://www.windytan.com/feeds/posts/default")
       :tags #{:blog})

(fetch guzey (src/selector-feed
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
       :post (assoc $item :hash (make-item-hash (some-> $url uri/uri)))
       :tags #{:blog})

(fetch ga-quobyte (src/feed "https://www.google.com/alerts/feeds/12214230541754356951/11912435152804193698")
       :options #{:mark-read-on-view}
       :post-fns [(proc/add-tag :highlight)]
       :tags #{:google-alert :tech})

(fetch ga-marcellauhoff (src/feed "https://www.google.com/alerts/feeds/12214230541754356951/17432466600270792644")
       :options #{:mark-read-on-view}
       :post-fns [(proc/add-tag :highlight)]
       :tags #{:google-alert :AAA})

(fetch ga-ml-irq0-org (src/feed "https://www.google.com/alerts/feeds/12214230541754356951/6287790772305614620")
       :options #{:mark-read-on-view}
       :post-fns [(proc/add-tag :highlight)]
       :tags #{:google-alert :AAA})

(fetch ga-job-search (src/feed "https://www.google.com/alerts/feeds/12214230541754356951/18101484729339824556")
       :options #{:mark-read-on-view}
       :post-fns [(proc/add-tag :highlight)]
       :tags #{:google-alert :AAA})

(fetch gutmet (src/feed "https://gutmet.org/blog/feed.rss")
       :tags #{:blog})

(fetch n-gate (src/feed "http://n-gate.com/index.atom")
       :tags #{:blog})

(fetch youtube-rickbeato (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCJquYOG5EL82sKTfH9aMA9Q")
       :options #{:mark-read-on-view}
       :tags #{:music :youtube-channel})

(fetch lastweektonight (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UC3XTzVzaHQEd30rQbuvCtTQ")
       :options #{:mark-read-on-view}
       :tags #{:youtube-channel}
       :post-fns [(proc/add-tag :download)])

(fetch cppcon (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCMlGfpWw-RUdWX_JbLCukXg")
       :options #{:mark-read-on-view}
       :tags #{:youtube-channel :conference})

(fetch clojure-tv (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCaLlzGqiPE2QRj6sSOawJRg")
       :options #{:mark-read-on-view}
       :tags #{:youtube-channel :tech :clojure})

(fetch youtube-usenix (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UC4-GrpQBx6WCGwmwozP744Q")
       :options #{:mark-read-on-view}
       :tags #{:youtube-channel :tech :sci})

(fetch mailab (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCyHDQ5C6z1NDmJ4g6SerW8g")
       :options #{:mark-read-on-view}
       :tags #{:youtube-channel :sci})

(fetch youtube-acm (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCPyA0XmU6aS4JCwVoIBTmIQ")
       :options #{:mark-read-on-view}
       :tags #{:youtube-channel :sci})

(fetch emacsrocks (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCkRmQ_G_NbdbCQMpALg6UPg")
       :options #{:mark-read-on-view}
       :tags #{:youtube-channel :tech :emacs})

(fetch theartoftheproblem (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCotwjyJnb-4KW7bmsOoLfkg")
       :options #{:mark-read-on-view}
       :tags #{:youtube-channel :tech})

(fetch kurzgesagt (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCsXVk37bltHxD1rDPwtNM8Q")
       :options #{:mark-read-on-view}
       :tags #{:youtube-channel :sci})

(fetch crashcourse (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCX6b17PVsYBQ0ip5gyeme-Q")
       :options #{:mark-read-on-view}
       :tags #{:youtube-channel :sci})

(fetch computer-history-museum (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCHDr4RtxwA1KqKGwxgdK4Vg")
       :options #{:mark-read-on-view}
       :tags #{:youtube-channel :sci})

(fetch two-minute-papers (src/feed "https://www.youtube.com/feeds/videos.xml?channel_id=UCbfYPyITQ-7l4upoX8nvctg")
       :options #{:mark-read-on-view}
       :tags #{:youtube-channel :sci})

(fetch realworldtech (src/wp-json "https://www.realworldtech.com/wp-json/")
       :options #{:mark-read-on-view}
       :tags #{:tech})

(fetch pavelmayer (src/feed "https://pavelmayer.de/feed/")
       :tags #{:blog})

(fetch dmeister (src/feed "https://dmeister.github.io/blog/atom.xml")
       :tags #{:blog})

(fetch coredumped (src/feed "https://coredumped.dev/index.xml" :deep? true)
       :tags #{:blog})

(fetch internet-protocol-journal (src/website "https://ipj.dreamhosters.com/internet-protocol-journal/issues/current-issue/")
       :tags #{:tech :sci}
       :post-fns [(proc/add-tag :highlight)
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
                                 "text/plain" (converter/html2text html)})))])
(fetch isotopp (src/feed "https://blog.koehntopp.info/feed.xml")
       :tags #{:blog})

(fetch netzpolitik (src/wp-json "https://netzpolitik.org/wp-json/")
       :options #{:mark-read-on-view}
       :tags #{:politics :magazine})

(fetch volksverpetzer (src/wp-json "https://www.volksverpetzer.de/wp-json/")
       :options #{:mark-read-on-view}
       :tags #{:politics})

(fetch correctiv (src/wp-json "https://correctiv.org/wp-json/")
       :options #{:mark-read-on-view}
       :tags #{:politics})

(fetch malleablesystems (src/feed "https://malleable.systems/blog/index.xml")
       :tags #{:tech :emacs})

(fetch tagesschau (src/feed "https://www.tagesschau.de/xml/rss2")
       :options #{:mark-read-on-view :main-list-use-description}
       :rm (or (re-find #"Liveblog: \+\+" $title)
               (re-find #"/(sport|fussball)/" (uri/path $url)))
       :post-fns [(mercury-contents)]
       :tags #{:news})

(fetch spiegel-online (src/feed "https://www.spiegel.de/schlagzeilen/index.rss")
       :rm (or ((make-category-filter-deny ["Sport"]) $item)
               (empty? $authors)
               (re-find #"(?smi)Weiterlesen mit.*Ihre Vorteile mit SPIEGEL.*Sie haben bereits ein Digital-Abonnement" $text))
       :pre (let [h (:hickory (http/fetch $url
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
              (-> $item
                  (assoc-in [:entry :authors] authors)
                  (assoc-in [:entry :contents]
                            {"text/html" html
                             "text/plain" (converter/html2text html)})))
       :options #{:mark-read-on-view :main-list-use-description}
       :tags #{:news})

(fetch taz-online (src/feed "https://taz.de/!s=&ExportStatus=Intern&SuchRahmen=Online;atom/")
       :post-fns [(mercury-contents)
                  (proc/exchange [:entry :descriptions] [:entry :contents])]
       :options #{:mark-read-on-view :main-list-use-description}
       :tags #{:news})

(fetch tagesspiegel (src/feed "https://www.tagesspiegel.de/contentexport/feed/home")
       :rm-fn (make-category-filter-deny ["Sport"])
       :post-fns [(mercury-contents)]
       :options #{:mark-read-on-view :main-list-use-description}
       :tags #{:news})

(fetch nytimes-mostshared (src/feed "https://rss.nytimes.com/services/xml/rss/nyt/MostShared.xml")
       :post-fns [(mercury-contents)]
       :options #{:mark-read-on-view :main-list-use-description}
       :tags #{:news})

(fetch nytimes-mostemailed (src/feed "https://rss.nytimes.com/services/xml/rss/nyt/MostEmailed.xml")
       :post-fns [(mercury-contents)]
       :options #{:mark-read-on-view :main-list-use-description}
       :tags #{:news})

(fetch nytimes-mostviewed (src/feed "https://rss.nytimes.com/services/xml/rss/nyt/MostViewed.xml")
       :post-fns [(mercury-contents)]
       :options #{:mark-read-on-view :main-list-use-description}
       :tags #{:news})

(fetch nytimes-top (src/feed "https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml")
       :rm-fn (make-category-filter-deny ["Sport" "Fraternities and Sororities" "Horse Racing" "Contests and Prizes"])
       :post-fns [(mercury-contents)]
       :options #{:mark-read-on-view :main-list-use-description}
       :tags #{:news})

(comment
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
                                            last-state-atom (get-in (get-sources) [(get-in item [:meta :source-key])
                                                                                   :last-state])
                                            last-state (get @last-state-atom name)]
                                        (log/info name last-state "->" new-state)
                                        (when (and (#{:red :amber} last-state) (= :green new-state))
                                          (notifier/notify :vac (str "Go! " name ": " last-state " -> " new-state)))
                                        (when (and (= :green last-state) (#{:red :amber} new-state))
                                          (notifier/notify :vac (str "Don't go "  name ": " last-state " -> " new-state)))
                                        (swap! last-state-atom assoc name new-state)
;;                                        (notifier/notify :vac (str name ": " last-state " -> " new-state))
                                        item))])})
