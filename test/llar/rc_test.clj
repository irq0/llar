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
           (uut/rc [:reader :ranking :highlight-boost-hours])))))

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
                                               :rarity-boost-cap-hours 24}}]
    (uut/reset-rc!)
    (is (= [[:appconfig :source-tag]] (uut/rc [:reader :favorites])))
    (is (= :gallery (uut/rc [:reader :default-list-view :appconfig])))
    (is (= 12 (uut/rc [:reader :ranking :highlight-boost-hours])))))

(deftest rc-overrides-win-over-appconfig-path
  (with-redefs [appconfig/appconfig {:ui {:favorites [[:appconfig :source-tag]]}
                                     :ranking {:highlight-boost-hours 12
                                               :rarity-boost-cap-hours 24}}]
    (uut/reset-rc!)
    (is (= [[:configured :item-tags]]
           (uut/rc [:reader :favorites] [[:configured :item-tags]])))
    (is (= [[:configured :item-tags]] (uut/rc [:reader :favorites])))
    (is (= 96 (uut/rc [:reader :ranking :highlight-boost-hours] 96)))
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
                             :rarity-boost-cap-hours 12))))

(deftest reader-ranking-validates-pairs
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"reader-ranking expects key/value pairs"
       (uut/reader-ranking :highlight-boost-hours))))

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
       (uut/rc [:reader :favorites] [[:blog :not-a-view-group]]))))
