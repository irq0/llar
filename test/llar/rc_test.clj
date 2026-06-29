(ns llar.rc-test
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is use-fixtures]]
   [llar.appconfig :as appconfig]
   [llar.rc :as uut]))

(use-fixtures :each
  (fn [f]
    (uut/reset-rc!)
    (f)
    (uut/reset-rc!)))

(defn- shipped-system-config-defaults []
  (edn/read-string (slurp (io/resource "config.edn"))))

(deftest rc-defaults-validate
  (is (= uut/rc-defaults (uut/verify-rc-defaults!)))
  (let [shipped (shipped-system-config-defaults)]
    (is (= (get-in shipped [:ui :favorites])
           (uut/rc [:reader :favorites])))
    (is (= (get-in shipped [:ui :default-list-view])
           (uut/rc [:reader :default-list-view])))
    (is (= (get-in shipped [:ranking :highlight-boost-hours])
           (uut/rc [:reader :ranking :highlight-boost-hours])))
    (is (= (get-in shipped [:export :url-handler])
           (uut/rc [:reader :export :url-handler])))
    (is (= (get-in shipped [:api :podcast :retention])
           (uut/rc [:podcast :retention])))
    (is (false? (uut/rc [:digest :enabled?])))
    (is (= 200 (uut/rc [:digest :limit])))
    (is (false? (uut/rc [:podcast :enabled?])))
    (is (= (get-in shipped [:api :podcast :video-format])
           (uut/rc [:podcast :download :video-format])))
    (is (= (get-in shipped [:api :podcast :av-downloader-extra-args])
           (uut/rc [:podcast :download :extra-args])))
    (is (= 200 (uut/rc [:podcast :scan :limit])))
    (is (= (get shipped :update-max-retry)
           (uut/rc [:update :max-retry])))))

(deftest rc-entries-describe-supported-paths
  (let [entries (uut/rc-entries)
        favorites (some #(when (= [:reader :favorites] (:path %)) %) entries)]
    (is favorites)
    (is (= [:ui :favorites] (:appconfig-path favorites)))
    (is (= :irq0-appconfig/favorites (:spec favorites)))
    (is (= (uut/rc [:reader :favorites]) (:effective favorites)))
    (is (= "(rc [:reader :favorites] VALUE)" (:example favorites)))))

(deftest rc-uses-appconfig-path-fallback
  (with-redefs [appconfig/appconfig {:ui {:favorites [[:appconfig :source-tag]]
                                          :default-list-view {:appconfig :gallery}}
                                     :ranking {:highlight-boost-hours 12
                                               :rarity-boost-cap-hours 24}
                                     :update-max-retry 9
                                     :export {:url-handler {:name "Handler"
                                                            :template "app://save?url={url}"}}
                                     :api {:digest {:to "digest@example.org"
                                                    :limit 10}
                                           :podcast {:retention {:default-episode-limit 30}
                                                     :video-format "worst"
                                                     :av-downloader-extra-args ["--foo"]}}}]
    (uut/reset-rc!)
    (is (= [[:appconfig :source-tag]] (uut/rc [:reader :favorites])))
    (is (= :gallery (uut/rc [:reader :default-list-view :appconfig])))
    (is (= 12 (uut/rc [:reader :ranking :highlight-boost-hours])))
    (is (= 30 (uut/rc [:podcast :retention :default-episode-limit])))
    (is (= {:enabled? true
            :to "digest@example.org"
            :limit 10}
           (select-keys (uut/rc [:digest]) [:enabled? :to :limit])))
    (is (= "worst" (uut/rc [:podcast :download :video-format])))
    (is (= ["--foo"] (uut/rc [:podcast :download :extra-args])))
    (is (= 9 (uut/rc [:update :max-retry])))
    (is (= {:name "Handler"
            :template "app://save?url={url}"}
           (uut/rc [:reader :export :url-handler])))))

(deftest rc-appconfig-omits-shipped-defaults
  (with-redefs [appconfig/appconfig (shipped-system-config-defaults)]
    (uut/reset-rc!)
    (is (= {} (uut/rc-appconfig)))
    (is (= uut/rc-defaults (uut/rc-baseline)))
    (is (= uut/rc-defaults (uut/rc-effective)))))

(deftest rc-overrides-win-over-appconfig-path
  (with-redefs [appconfig/appconfig {:ui {:favorites [[:appconfig :source-tag]]}
                                     :ranking {:highlight-boost-hours 12
                                               :rarity-boost-cap-hours 24}}]
    (uut/reset-rc!)
    (is (= [[:appconfig :source-tag]]
           (get-in (uut/rc-baseline) [:reader :favorites])))
    (is (= [[:configured :item-tags]]
           (uut/rc [:reader :favorites] [[:configured :item-tags]])))
    (is (= [[:configured :item-tags]] (uut/rc [:reader :favorites])))
    (is (= 96 (uut/rc [:reader :ranking :highlight-boost-hours] 96)))
    (is (= {:highlight-boost-hours 12
            :rarity-boost-cap-hours 24}
           (get-in (uut/rc-baseline) [:reader :ranking])))
    (is (= {:highlight-boost-hours 96
            :rarity-boost-cap-hours 24}
           (uut/rc [:reader :ranking])))))

(deftest reader-front-constructs-write-rc
  (uut/reset-rc!)
  (is (some #{[:blog :source-tag]} (uut/reader-favorite :blog :source-tag)))
  (is (some #{[:blog :item-tags]} (uut/reader-favorite :blog :item-tags)))
  (is (= 1 (count (filter #(= :blog (first %))
                          (uut/rc [:reader :favorites])))))
  (is (= :headlines
         (uut/reader-default-list-view :blog :headlines)))
  (is (= :headlines
         (uut/rc [:reader :default-list-view :blog])))
  (is (= {:highlight-boost-hours 72
          :rarity-boost-cap-hours 12}
         (uut/reader-ranking :highlight-boost-hours 72
                             :rarity-boost-cap-hours 12)))
  (is (= {:name "Handler"
          :template "app://save?url={url}"}
         (uut/reader-url-handler :name "Handler"
                                 :template "app://save?url={url}")))
  (is (nil? (uut/reader-url-handler nil)))
  #_{:clj-kondo/ignore [:unresolved-symbol]}
  (is (= 10 (uut/podcast-retention my-video-feed 10)))
  (is (= 10 (uut/rc [:podcast :retention :sources :my-video-feed])))
  (is (= 12 (eval '(llar.rc/podcast-retention another-feed 12))))
  (is (= 12 (uut/rc [:podcast :retention :sources :another-feed])))
  (is (= 50 (uut/rc [:podcast :retention :default-episode-limit] 50)))
  (is (= {:enabled? true
          :to "digest@example.org"
          :limit 25
          :inline-images? false
          :keep-unread-issues 2}
         (select-keys (uut/digest :to "digest@example.org"
                                  :limit 25
                                  :inline-images? false
                                  :keep-unread-issues 2)
                      [:enabled? :to :limit :inline-images? :keep-unread-issues])))
  (is (true? (uut/rc [:digest :enabled?])))
  (is (= {:video-format "worst"
          :extra-args ["--embed-metadata"]
          :max-attempts 5
          :retry-cooldown-minutes 10}
         (uut/podcast-download :video-format "worst"
                               :extra-args ["--embed-metadata"]
                               :max-attempts 5
                               :retry-cooldown-minutes 10)))
  (is (true? (uut/rc [:podcast :enabled?]))))

(deftest reader-ranking-validates-pairs
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"reader-ranking expects key/value pairs"
       (uut/reader-ranking :highlight-boost-hours))))

(deftest reader-url-handler-validates-shape
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"reader-url-handler expects a map, nil, or key/value pairs"
       (uut/reader-url-handler :name)))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid runtime config value"
       (uut/reader-url-handler {:name "Handler"}))))

(deftest digest-validates-shape
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"digest expects key/value pairs"
       (uut/digest :to)))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"unknown option"
       (uut/digest :to "digest@example.org" :unknown true)))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid runtime config value"
       (uut/digest :limit 10))))

(deftest podcast-download-validates-shape
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"podcast-download expects key/value pairs"
       (uut/podcast-download :video-format)))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"unknown option"
       (uut/podcast-download :unknown true)))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid runtime config value"
       (uut/podcast-download :max-attempts 0))))

(deftest rc-rejects-unknown-paths-and-invalid-values
  (uut/reset-rc!)
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Unknown runtime config path"
       (uut/rc [:postgresql :backend])))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid runtime config value"
       (uut/rc [:reader :default-list-view :blog] :not-a-list-style)))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid runtime config value"
       (uut/rc [:reader :favorites] [[:blog :not-a-view-group]])))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid runtime config value"
       (uut/rc [:podcast :retention :sources :my-feed] 0)))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid runtime config value"
       (uut/rc [:podcast :retention :default-episode-limit] -1))))
