(ns llar.postproc-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [java-time.api :as time]
   [llar.appconfig :as appconfig]
   [llar.apis.reader :as reader]
   [llar.fetch :as fetch]
   [llar.fetch.hackernews]
   [llar.item]
   [llar.postproc :as uut]
   [llar.repl :as repl]
   [llar.src :as src]
   [llar.store :as store]
   [mount.core :as mount]))

(def +fake-appconfig+
  {:update-max-retry 5
   :credentials-file "/tmp/credentials.edn"
   :runtime-config-dir "/dev/null"
   :throttle {:command-max-concurrent 10}
   :timeouts {:readability 30 :av-downloader 600 :html2text 30}
   :commands {:pandoc "/bin/true" :w3m "/bin/true" :lynx "/bin/true"
              :av-downloader "/bin/true" :html2text "/bin/true"}
   :blob-store-dir "/tmp"})

(defn with-mount [f]
  (mount/start-with {#'appconfig/appconfig +fake-appconfig+
                     #'reader/frontend-db nil
                     #'store/backend-db nil
                     #'repl/nrepl-server nil})
  (f)
  (mount/stop))

(use-fixtures :once with-mount)

(deftest add-tag-via-process-test
  (testing "all-items-process-first adds :unread tag"
    (let [example-src (src/hn :front_page)
          item {:meta {:source example-src
                       :source-name (str example-src)
                       :source-key :unknown
                       :fetch-ts (time/zoned-date-time)
                       :tags #{}
                       :version 2}
                :summary {:ts (time/zoned-date-time)
                          :title "Test"}
                :hash (fetch/make-item-hash "test")
                :entry {:url nil :contents {}}}
          state {:key :test}
          result (uut/all-items-process-first item example-src state)]
      (is (contains? (get-in result [:meta :tags]) :unread))))

  (testing "all-items-process-first sets source-key from state"
    (let [example-src (src/hn :front_page)
          item {:meta {:source example-src
                       :source-name (str example-src)
                       :source-key :unknown
                       :fetch-ts (time/zoned-date-time)
                       :tags #{}
                       :version 2}
                :summary {:ts (time/zoned-date-time) :title "Test"}
                :hash (fetch/make-item-hash "test")
                :entry {:url nil :contents {}}}
          state {:key :my-feed}
          result (uut/all-items-process-first item example-src state)]
      (is (= :my-feed (get-in result [:meta :source-key]))))))

(deftest highlight-item-test
  (testing "item without matching words is not highlighted"
    (reset! uut/highlight-matches {:words #{} :authors #{}})
    (let [item {:entry {:nlp {:names #{} :nouns #{} :top {:words {}}}
                        :authors []}}]
      (is (false? (uut/highlight-item? item)))))

  (testing "item with matching noun is highlighted"
    (reset! uut/highlight-matches {:words #{"clojure"} :authors #{}})
    (let [item {:entry {:nlp {:names #{} :nouns #{"clojure"} :top {:words {}}}
                        :authors []}}]
      (is (uut/highlight-item? item))))

  (testing "item with matching author is highlighted"
    (reset! uut/highlight-matches {:words #{} :authors #{"john doe"}})
    (let [item {:entry {:nlp {:names #{} :nouns #{} :top {:words {}}}
                        :authors ["John Doe"]}}]
      (is (uut/highlight-item? item)))))

(deftest video-url-predicate-test
  (testing "youtube URL is a video"
    (is (uut/video-url? "https://www.youtube.com/watch?v=abc123"))
    (is (uut/video-url? "https://youtu.be/abc123")))
  (testing "regular URL is not a video"
    (is (not (uut/video-url? "https://example.com/article")))
    (is (not (uut/video-url? nil)))))
