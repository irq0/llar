(ns llar.config-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [llar.config :as uut]
   [mount.core :as mount]))

(use-fixtures :each
  (fn [f]
    (let [original @uut/fetch-scheds]
      (reset! uut/fetch-scheds {})
      (try
        (f)
        (finally
          (reset! uut/fetch-scheds original))))))

(deftest scheduler-config-starts-only-the-new-state-and-replaces-by-name
  (let [events (atom [])
        generation (atom 0)]
    (with-redefs [clojure.core/eval
                  (fn [form]
                    (keyword (str (name (second form)) "-" (swap! generation inc))))
                  mount/start
                  (fn [sched]
                    (swap! events conj [:start sched]))
                  mount/stop
                  (fn [sched]
                    (swap! events conj [:stop sched]))]
      (uut/handle-scheduler-config-form! '(sched-fetch news :hourly true))
      (uut/handle-scheduler-config-form! '(sched-fetch reddit :hourly true))
      (uut/handle-scheduler-config-form! '(sched-fetch news :hourly false))

      (is (= [[:start :news-1]
              [:start :reddit-2]
              [:stop :news-1]
              [:start :news-3]]
             @events))
      (is (= {:news :news-3
              :reddit :reddit-2}
             @uut/fetch-scheds)))))
