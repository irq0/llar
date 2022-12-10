(ns u1f596.converter-test
  (:require
   [u1f596.converter :as uut]
   [clojure.test :refer :all]
   [org.bovinegenius [exploding-fish :as uri]]
   [clojure.set :refer [union intersection]]
   [clojure.string :as string]
   [java-time :as time]
   [slingshot.test :refer :all]
   [mount.core :as mount])
  (:import (org.bovinegenius.exploding_fish Uri)))

(deftest read-edn-propsfile
  (is (= {:orig-urls #{(uri/uri "http://files.explosm.net/thumbs/videos/splash-5f344ac96cfc2.png")}
          :hits 4
          :mime-type "image/png"}
         (uut/read-edn-propsfile
          "{:orig-urls #{#org.irq0.🖖/url \"http://files.explosm.net/thumbs/videos/splash-5f344ac96cfc2.png\"}, :hits 4, :mime-type \"image/png\"}"))))

(deftest read-edn-state
  (testing
   "regular ok source"
    (is (= {:ok
            {:key :ok,
             :last-successful-fetch-ts (time/zoned-date-time 2020 8 16 12 42 23 0 "Z")
             :last-attempt-ts (time/zoned-date-time 2020 8 16 12 42 23 0 "Z")
             :forced-update? nil,
             :status :ok,
             :last-exception nil,
             :retry-count 0}}
           (uut/read-edn-state
            "{:ok {:key :ok, :last-successful-fetch-ts #org.irq0.🖖/datetime \"2020-08-16T12:42:23.0Z\", :last-attempt-ts #org.irq0.🖖/datetime \"2020-08-16T12:42:23.0Z\", :forced-update? nil, :status :ok, :last-exception nil, :retry-count 0}}\n")))))

(deftest read-what-you-print
  (testing "propsfile"
    (let [in {:orig-urls #{(uri/uri "https://media.wired.com/photos/5bfc71a83ee8d605f3dd0edc/master/w_942,c_limit/googlepixel_top.jpg")}
                 :hits 1,
                 :mime-type "image/jpeg"}
          out (-> in
                  uut/print-propsfile
                  uut/read-edn-propsfile)]
      (is (= (-> in :orig-urls first type) (-> out :orig-urls first type)))
      (is (instance? Uri (-> out :orig-urls first)))
      (is (= in out))))
  (testing "state"
    (let [state {:ok
                 {:key :ok,
                  :last-successful-fetch-ts (time/zoned-date-time 2020 8 16 12 42 23 23)
                  :last-attempt-ts (time/zoned-date-time 2020 8 16 12 42 23 23)
                  :forced-update? nil,
                  :status :ok,
                  :last-exception nil,
                  :retry-count 0}}]
      (is (= state
             (-> state
                 uut/print-state
                 uut/read-edn-state))))))
