(ns llar.fetch-test
  (:require
   [llar.appconfig :as appconfig]
   [llar.apis.reader :as reader]
   [llar.commands :as commands]
   [llar.repl :as repl]
   [llar.store :as store]
   [llar.fetch :as uut]
   [llar.fetch.custom]
   [llar.fetch.feed]
   [llar.fetch.http]
   [llar.fetch.imap]
   [llar.fetch.readability :as readability]
   [llar.converter :as converter]
   [hickory.select :as S]
   [llar.fetch.reddit :as reddit]
   [llar.fetch.twitter]
   [llar.http :as http]
   [mount.core :as mount]
   [hickory.core :as hick]
   [cheshire.core :as cheshire]
   [hickory.render :as hick-r]
   [clj-http.client :as http-client]
   [clj-http.fake :as http-fake]
   [llar.specs]
   [llar.src :as src]
   [java-time.api :as time]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [slingshot.slingshot :refer [throw+]]
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is testing] :as t]))

(s/check-asserts true)

(deftest readability-published-time-test
  (let [parse-published-time @#'readability/parse-published-time]
    (testing "compact numeric UTC offset"
      (is (= "2016-01-07T00:00-05:00"
             (str (parse-published-time "2016-01-07T00:00:00-0500")))))
    (testing "ISO offset forms remain unchanged"
      (is (= "2016-01-07T00:00Z"
             (str (parse-published-time "2016-01-07T00:00:00Z"))))
      (is (= "2016-01-07T00:00:00.123-05:00"
             (str (parse-published-time "2016-01-07T00:00:00.123-05:00"))))
      (is (= "2016-01-07T00:00-05:00[America/New_York]"
             (str (parse-published-time
                   "2016-01-07T00:00:00-0500[America/New_York]")))))
    (testing "zone-less forms emitted by Readability"
      (is (= "2018-04-05T06:00Z"
             (str (parse-published-time "2018-04-05T06:00"))))
      (is (= "2019-04-28T06:01:07Z"
             (str (parse-published-time "2019-04-28 06:01:07"))))
      (is (= "2017-11-03T03:01Z"
             (str (parse-published-time "2017-11-03 03:01:00.000000"))))
      (is (= "2020-09-21T00:00Z"
             (str (parse-published-time "2020-09-21")))))))

(defn fake-fetch-rss [url & _args]
  (when-not (= (str url) "http://example.com/feed.xml")
    (throw+ {:type ::test-error
             :url url}))
  {:raw {:body (slurp (io/resource "test/example_rss.xml"))}
   :body nil
   :summary {:ts (time/zoned-date-time)
             :title "Test Feed Title"}
   :hickory [:html {} [:head {}] [:body {}]]})

(def fake-hicory
  {:type :document
   :content
   [{:type :element
     :attrs nil
     :tag :html
     :content
     [{:type :element
       :attrs nil
       :tag :head
       :content []}
      {:type :element
       :attrs nil
       :tag :body
       :content
       [{:type :element
         :attrs nil
         :tag :h1
         :content
         ["Test Heading"]}]}]}]})

(defn fake-fetch-html [url & _args]
  (cond (string/starts-with? (str url) "https://irq0.org")
        {:raw {:body ""}
         :body ""
         :summary {:ts (time/zoned-date-time)
                   :title "Test Item Title"}
         :hickory fake-hicory}
        (not= (str url) "http://example.com/index.html")
        (throw+ {:type ::test-error
                 :url url})
        :else
        (let [html (slurp (io/resource "test/example_html.html"))
              parsed (-> html hick/parse hick/as-hickory)]
          {:raw {:body html}
           :body (hick-r/hickory-to-html parsed)
           :summary {:ts (time/zoned-date-time)
                     :title "Test Feed Title"}
           :hickory parsed})))

(defn fake-http-get-wp-json [url & _args]
  (cond (= (str url) "http://example.com/wp-json/")
        {:body
         {:name "Test Feed Title"
          :home "http://example.com/wp-json/"
          :routes {(keyword "/wp/v2/posts") {:_links {:self "http://example.com/POSTS"}}}}}
        (string/starts-with? (str url) "http://example.com/POSTS")
        {:body
         (java.io.StringReader.
          (cheshire/generate-string
           [{:title {:rendered "Test Title"}
             :link "http://example.com/POSTS/1"
             :_links {:self [{:href "http://example.com/POSTS/1"}]}
             :date_gmt "2024-01-30T23:35:35+01:00"
             :excerpt {:rendered ""}
             :content {:rendered "<html><head></head><body></body></html>"}}]))}))

(defn fake-http-get-reddit [_url & _args]
  {:body
   {:data {:children [{:data {:url "https://www.example.com"
                              :permalink "/asdf"
                              :thumbnail "https://www.example.com/image"
                              :created_utc 1
                              :title "Foo"
                              :author "Bar"
                              :id "2342"
                              :score 1337
                              :selftext "text text text"}}]}}})

(def basic-tests
  [{:src (src/reddit "does-not-exists-23-42" :top)
    :fake-http-get #'fake-http-get-reddit
    :n-items 1}
   {:src (src/feed "http://example.com/feed.xml")
    :fake-fetch #'fake-fetch-rss
    :n-items 1}
   {:src (src/selector-feed "http://example.com/index.html"
                            {:urls (S/and (S/tag :a)
                                          (S/find-in-text #"Read More…"))} {} {})
    :fake-fetch #'fake-fetch-html
    :n-items 8}
   {:src (src/wp-json "http://example.com/wp-json/")
    :fake-http-get #'fake-http-get-wp-json
    :n-items 1}])

(deftest basics-test
  (mount/start-with {#'appconfig/appconfig {:update-max-retry 5
                                            :credentials-file "/tmp/credentials.edn"
                                            :runtime-config-dir "/dev/null"
                                            :throttle {:command-max-concurrent 10}
                                            :timeouts {:readability 30
                                                       :av-downloader 600
                                                       :html2text 30}
                                            :commands {:pandoc "/bin/true"
                                                       :w3m "/bin/true"
                                                       :lynx "/bin/true"
                                                       :av-downloader "/bin/true"
                                                       :html2text "/bin/true"}
                                            :blob-store-dir "/tmp"}
                     #'reader/frontend-db nil
                     #'store/backend-db nil
                     #'repl/nrepl-server nil})
  (doseq [{:keys [src fake-fetch fake-http-get n-items]} basic-tests]
    (testing (str src)
      ;; the reddit fetcher authenticates via OAuth2; stub the token and user agent so
      ;; no credentials file or network round trip is needed here
      (with-redefs [http/fetch fake-fetch
                    http-client/get fake-http-get
                    reddit/access-token (constantly "test-token")
                    reddit/reddit-user-agent (constantly "java:llar:test (by /u/test)")]
        (let [fetched (uut/fetch-source src {})
              item (first fetched)]
          (is (= n-items (count fetched)))
          (is (every? #(= (get-in % [:meta :source]) src) fetched))
          (log/info "Test item:" item))))))

(deftest titleless-rss-feed-test
  (let [url "https://bsky.app/profile/alexmillerdb.bsky.social/rss"
        post-url "https://bsky.app/profile/alexmillerdb.bsky.social/post/3mqug4xumy723"
        src (src/feed url)
        response {:raw {:body (slurp (io/resource "test/bluesky_rss.xml"))}
                  :summary {:ts (time/zoned-date-time)
                            :title nil}}]
    (with-redefs [http/fetch (fn [& _] response)
                  http/raw-sanitize identity
                  commands/html2text identity]
      (let [item (first (uut/fetch-source src {}))]
        (is (= url (get-in item [:meta :source-name])))
        (is (= "A titleless Bluesky post"
               (get-in item [:summary :title])))
        (is (= post-url (str (get-in item [:entry :url]))))
        (is (= (uut/make-item-hash post-url) (:hash item)))))))

(deftest titled-rss-feed-backward-compatibility-test
  (let [url "http://example.com/feed.xml"
        post-url "https://irq0.org/test/"
        src (src/feed url)
        response {:raw {:body (slurp (io/resource "test/example_rss.xml"))}
                  :summary {:ts (time/zoned-date-time)
                            :title "Test Feed Title"}}]
    (with-redefs [http/fetch (fn [& _] response)
                  http/raw-sanitize identity
                  commands/html2text identity]
      (let [item (first (uut/fetch-source src {}))]
        (is (= "🄸🅁🅀␀" (get-in item [:meta :source-name])))
        (is (= "Test" (get-in item [:summary :title])))
        (is (= post-url (str (get-in item [:entry :url]))))
        ;; This was the hashing scheme before titleless feeds were supported.
        (is (= (uut/make-item-hash "Test" post-url)
               (:hash item)))))))

(deftest rss-feed-preserves-conditional-tokens-on-not-modified
  (let [source (src/feed "https://example.com/feed.xml")
        conditional-tokens {:etag "W/\"ec627-nOlkMvp2fc86N4xolHdhQgksVmY\""}
        requested-conditionals (atom nil)]
    (with-redefs [http/fetch
                  (fn [_url & {:keys [conditionals]}]
                    (reset! requested-conditionals conditionals)
                    {:status :not-modified
                     :conditional-tokens conditionals})]
      (let [fetched (uut/fetch-source source conditional-tokens)]
        (is (empty? fetched))
        (is (= conditional-tokens @requested-conditionals))
        (is (= {:conditional-tokens conditional-tokens}
               (meta fetched)))))))

(deftest http-fetcher
  (let [resource {:etag "W/\"ec627-nOlkMvp2fc86N4xolHdhQgksVmY\"",
                  :last-modified-ts (converter/parse-http-ts "Thu, 22 Feb 2024 18:31:13 GMT")
                  :last-modified "Thu, 22 Feb 2024 18:31:13 GMT"
                  :data "<html><head><title>http-fetch-test</title></head><body><h1>foo</h1></body></html>"}]
    (http-fake/with-global-fake-routes-in-isolation
      {"http://example.com/304"
       (fn [req]
         (let [if-mod-since (converter/parse-http-ts (get-in req [:headers "If-Modified-Since"]))
               if-etag (get-in req [:headers "If-None-Match"])]
           (if (or (and if-mod-since (time/before? if-mod-since (get resource :last-modified-ts)))
                   (and if-etag (= if-etag (get resource :etag))))
             {:status 304}
             {:status 200
              :headers (select-keys resource [:etag :last-modified])
              :body (get resource :data)})))}
      (testing "regular non-conditional fetch"
        (let [fetch (http/fetch "http://example.com/304" :sanitize? false)]
          (is (= :ok (:status fetch)))
          (is (= (select-keys resource [:etag :last-modified])
                 (:conditional-tokens fetch)))
          (is (= (:last-modified-ts resource) (get-in fetch [:summary :ts])))
          (is (= (:data resource) (:body fetch)))
          (is (= "http-fetch-test" (get-in fetch [:summary :title])))))
      (testing "conditional fetch - modified"
        (let [fetch (http/fetch "http://example.com/304" :sanitize? false :conditionals {:etag "foo"})]
          (is (= :ok (:status fetch)))
          (is (= (select-keys resource [:etag :last-modified])
                 (:conditional-tokens fetch)))
          (is (= #{:raw :status :conditional-tokens :summary :hickory :body} (into #{} (keys fetch))))))
      (testing "conditional fetch - not modified"
        (let [fetch (http/fetch "http://example.com/304"  :sanitize? false :conditionals (select-keys resource [:etag :last-modified]))]
          (is (= :not-modified (:status fetch)))
          (is (= (select-keys resource [:etag :last-modified])
                 (:conditional-tokens fetch)))
          (is (= #{:raw :status :conditional-tokens} (into #{} (keys fetch)))))))))
