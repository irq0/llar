(ns llar.apis.config-lab-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [java-time.api :as time]
   [llar.apis.config-lab :as api]
   [llar.appconfig :as appconfig]
   [llar.config-lab :as lab]))

(def token "0123456789abcdef0123456789abcdef")

(use-fixtures :each
  (fn [test-fn]
    (lab/reset-state-for-tests!)
    (try
      (test-fn)
      (finally
        (lab/reset-state-for-tests!)))))

(deftest auth-is-scoped-to-config-lab-and-requires-write-header
  (let [seen (atom nil)
        handler (api/wrap-auth #(do (reset! seen %) {:status 200}))]
    (with-redefs [lab/enabled? (constantly true)
                  lab/login-owner #(when (= "valid-session" %) "tester")]
      (is (= 200 (:status (handler {:request-method :post
                                    :uri "/api/config-lab/login"}))))
      (is (= 200 (:status (handler {:request-method :post
                                    :uri "/api/update/unrelated"}))))
      (is (= 401 (:status (handler {:request-method :get
                                    :uri "/api/config-lab/sessions/one"}))))
      (is (= 403 (:status (handler {:request-method :post
                                    :uri "/api/config-lab/sessions"
                                    :cookies {api/cookie-name {:value "valid-session"}}
                                    :headers {}}))))
      (is (= 200 (:status (handler {:request-method :post
                                    :uri "/api/config-lab/sessions"
                                    :cookies {api/cookie-name {:value "valid-session"}}
                                    :headers {"x-llar-config-lab" "1"}}))))
      (is (= "tester" (:config-lab/owner @seen))))))

(deftest login-exchanges-dedicated-token-for-opaque-cookie
  (with-redefs [appconfig/config-lab (fn
                                       ([] {:enabled? true :credentials :config-lab})
                                       ([key] (get {:enabled? true
                                                    :credentials :config-lab}
                                                   key)))
                appconfig/credentials (constantly {:tokens {:tester token}})
                lab/settings (constantly {:enabled? true
                                          :session-ttl-minutes 5
                                          :max-concurrent-runs 1
                                          :run-timeout-ms 1000})]
    (let [response (api/login-response {:params {:token token}
                                        :headers {}
                                        :scheme :http})
          cookie (get-in response [:cookies api/cookie-name])]
      (is (= 200 (:status response)))
      (is (not= token (:value cookie)))
      (is (true? (:http-only cookie)))
      (is (= :strict (:same-site cookie))))
    (is (= 401 (:status (api/login-response {:params {:token "wrong"}}))))))

(deftest enabled-lab-refuses-to-start-without-dedicated-credentials
  (with-redefs [lab/enabled? (constantly true)
                appconfig/config-lab (fn [key]
                                       (when (= :credentials key) :config-lab))
                appconfig/credentials (constantly {})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Config Lab credentials"
                          (api/wrap-auth identity)))))

(deftest export-post-passes-deployment-metadata
  (let [seen (atom nil)]
    (with-redefs [lab/export-form (fn [owner id input]
                                    (reset! seen [owner id input])
                                    "(fetch example ...)\n")]
      (let [response (api/export-response
                      {:config-lab/owner "tester"
                       :params {:source-key "example" :tags-form "#{:news}"}}
                      "session-id")]
        (is (= 200 (:status response)))
        (is (= ["tester" "session-id"
                {:source-key "example" :tags-form "#{:news}"}]
               @seen))))))

(deftest config-lab-responses-carry-a-clojure-fidelity-inspector-snapshot
  (with-redefs [lab/compile-session! (fn [& _]
                                       {:stage :compiled
                                        :tags #{:news :storage}})]
    (let [response (api/compile-response {:config-lab/owner "tester"
                                          :params {}}
                                         "session-id")
          body (:body response)
          tags-entry (some #(when (= ":tags" (get-in % [:key :printed])) %)
                           (get-in body [:_llar-inspector :children]))]
      (is (= 200 (:status response)))
      (is (vector? (:tags body)) "ordinary Config Lab fields remain JSON-safe")
      (is (= "set" (get-in tags-entry [:value :semantic-type])))
      (is (= "clojure.lang.PersistentHashSet"
             (get-in tags-entry [:value :runtime-type]))))))

(deftest blob-response-is-private-and-nosniff
  (let [created (time/zoned-date-time)
        file (java.io.File/createTempFile "config-lab-response" ".png")]
    (try
      (spit file "png")
      (with-redefs [lab/session-blob (fn [owner id hash]
                                       (is (= ["tester" "session-id" "abc"]
                                              [owner id hash]))
                                       {:mime-type "image/png"
                                        :size 3
                                        :created created
                                        :file file})]
        (let [response (api/blob-response {:config-lab/owner "tester"
                                           :request-method :get
                                           :headers {}}
                                          "session-id" "abc")]
          (is (= 200 (:status response)))
          (is (= "image/png" (get-in response [:headers "Content-Type"])))
          (is (= "nosniff" (get-in response [:headers "X-Content-Type-Options"])))
          (is (= "no-store" (get-in response [:headers "Cache-Control"])))
          (is (= "png" (slurp (:body response))))))
      (finally
        (.delete file)))))
