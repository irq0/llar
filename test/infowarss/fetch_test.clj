(ns infowarss.fetch-test
  (:require
   [infowarss.schema-test :as schema-test]
   [infowarss.src :as src]
   [infowarss.schema :as schema]
   [infowarss.fetch.twitter :refer [htmlize-tweet-text]]
   [infowarss.http :as infowarss-http]
   [schema-generators.complete :as c]
   [schema-generators.generators :as g]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [schema.core :as s]
   [clj-http.client :as http]
   [clj-time.core :as time]
   [twitter.api.restful :as twitter]
   [clojure.test.check :as tc]
   [clojure.java.io :as io]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test :refer :all]
   [infowarss.fetch :refer :all]))

(def demo-tweet
  {:created_at "Wed Aug 27 13:08:45 +0000 2008"
   :favorite_count 1
   :retweet_count 100
   :lang "de"
   :id 2342
   :id_str "2342"
   :user {:screen_name "irq0"}
   :entities {:hashtags [{:text "HASH1", :indices [22 28]}
                         {:text "HASH2", :indices [29 35]}]
              :symbols [],
              :user_mentions [{:screen_name "USER1",
                               :name "Example 1",
                               :indices [7 13]}
                              {:screen_name "USER2",
                               :name "Example 2",
                               :indices [15 21]}]
              :urls [{:expanded_url "http://example.com"
                      :url "http://example.com"
                      :indices [48 66]}]}
   :text "BLABLA @USER1: @USER2 #HASH1 #HASH2 bla bla bla http://example.com"})


(deftest text-htmlize-tweet-text
  (is (= (htmlize-tweet-text demo-tweet)
        "BLABLA <a class=\"mention-entity\" href=\"https://twitter.com/USER1\">@USER1</a>: <a class=\"mention-entity\" href=\"https://twitter.com/USER2\">@USER2</a> <a class=\"hashtag-entity\" href=\"https://twitter.com/hashtag/HASH1\">#HASH1</a> <a class=\"hashtag-entity\" href=\"https://twitter.com/hashtag/HASH2\">#HASH2</a> bla bla bla <a class=\"url-entity\" href=\"http://example.com\">URL</a>"
        )))

(defn- fake-http-response [body]
  (c/complete {:body body
               :status 200
               :headers {"Last-Modified" "Thu, 20 Apr 2017 12:33:15 GMT"}}
               schema/HttpResponse))

(deftest test-fetch-http-generic
  (let [body (slurp (io/resource "fetch_test.html"))
        src (g/generate schema/HttpSource schema-test/leaf-generators)]
    (with-redefs [http/get (fn [_] (fake-http-response body))]
      (let [resp (infowarss-http/fetch (:url src))]
        (is (= 200 (get-in resp [:raw :status])))
        (is (= body (get-in resp [:raw :body])))))))

(deftest test-fetch-feed
  (let [body (slurp (io/resource "fetch_test.atom"))
        src (src/feed "http://example.com")]
    (with-redefs [http/get (fn [_] (fake-http-response body))]
      (doseq [item (fetch-source src)]
        (let [{:keys [feed entry summary]} item]
          (is (= "Example Feed" (:title feed)))
          (is (= (io/as-url "http://example.org/") (:url feed)))
          (is (= (time/date-time 2003 12 13 18 30 2) (:pub-ts feed)))
          (is (= "atom_1.0" (:feed-type feed)))

          (is (= "Atom-Powered Robots Run Amok" (:title entry)))
          (is (= (io/as-url "http://example.org/2003/12/13/atom03") (:url entry)))
          (is (= "Dinge sachen und zeug" (get-in entry [:contents "text/plain"])))

          (is (= (time/date-time 1993 12 13 18 30 2) (:ts summary)))
          (is (= "Atom-Powered Robots Run Amok" (:title summary))))))))

(deftest test-fetch-html
  (let [body (slurp (io/resource "fetch_test.html"))
        src (src/website "http://example.com")]
    (with-redefs [http/get (fn [_] (fake-http-response body))]
      (doseq [item (fetch-source src)]
        (let [{:keys [summary]} item]
          (is (= (time/date-time 2017 04 20 12 33 15) (:ts summary)))
          (is (= "irq0.org: irq0.org" (:title summary))))))))


(defn- fake-twitter-responses [n]
  (g/sample n schema/Tweet schema-test/leaf-generators))

(deftest test-twitter-fetch-fake-data
  (with-redefs [twitter/search-tweets (fn [& args]
                                        {:body {:statuses (fake-twitter-responses 10)}})]
    (let [resp (fetch-source (src/twitter-search "foo" (:twitter-api infowarss.core/creds)))]
      (is (= 10 (count resp))))))

(deftest test-twitter-fetch-fake-data
  (with-redefs [twitter/search-tweets (fn [& args]
                                        {:body {:statuses [demo-tweet]}})]
    (let [items (fetch-source (src/twitter-search "foo" (:twitter-api infowarss.core/creds)))]
      (is (= 1 (count items)))
      (doseq [item items]
        (let [{:keys [summary entry]} item]
          (is (= (time/date-time 2008 8 27 13 8 45) (:ts summary)))
          (is (= "BLABLA @USER1: @USER2 #HASH1 #HASH2 bla bla bla ht…" (:title summary)))

          (is (= (io/as-url "https://twitter.com/irq0/status/2342") (:url entry)))
          (is (= (time/date-time 2008 8 27 13 8 45) (:pub-ts entry)))
          (is (= 1 (get-in entry [:score :favs])))
          (is (= 100 (get-in entry [:score :retweets])))
          (is (= :de (:language entry)))
          (is (= :tweet (:type entry)))
          (is (= 2 (count (get-in entry [:entities :hashtags]))))
          (is (= 2 (count (get-in entry [:entities :mentions]))))
          (is (= 0 (count (get-in entry [:entities :photos]))))
          (is (= ["irq0"] (:authors entry)))
          (is (= "BLABLA @USER1: @USER2 #HASH1 #HASH2 bla bla bla http://example.com"
                (get-in entry [:contents "text/plain"]))))))))
