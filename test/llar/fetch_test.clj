(ns llar.fetch-test
  (:require
   [llar.fetch :as uut]
   [llar.fetch.custom]
   [llar.fetch.http]
   [llar.fetch.imap]
   [llar.fetch.mercury]
   [hickory.select :as S]
   [llar.fetch.reddit]
   [llar.fetch.twitter]
   [llar.http :as http]
   [hickory.core :as hick]
   [cheshire.core :as cheshire]
   [hickory.render :as hick-r]
   [clj-http.client :as http-client]
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
        (= (str url) "http://example.com/POSTS")
        {:body
         (java.io.StringReader.
          (cheshire/generate-string
           [{:title {:rendered "Test Title"}
             :link "http://example.com/POSTS/1"
             :_links {:self "http://example.com/POSTS/1"}
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
  (doseq [{:keys [src fake-fetch fake-http-get n-items]} basic-tests]
    (testing (str src)
      (with-redefs [http/fetch fake-fetch
                    http-client/get fake-http-get]
        (let [fetched (uut/fetch-source src)
              item (first fetched)]
          (is (= n-items (count fetched)))
          (is (every? #(= (get-in % [:meta :source]) src) fetched))
          (log/error item))))))
