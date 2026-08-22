(ns llar.apis.capture-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [llar.apis.capture :as uut]
   [llar.bookmark-capture :as capture]))

(def credentials
  {:tokens {:iphone "iphone-secret-0123456789-0123456789"
            :macbook "macbook-secret-0123456789-0123456789"}})

(defn- capture-handler []
  (uut/wrap-token-auth (uut/handler :db) credentials))

(defn- request
  ([handler token]
   (request handler token {:url "https://example.com/article"}))
  ([handler token params]
   (handler {:request-method :post
             :uri "/api/v1/captures"
             :headers (cond-> {} token (assoc "authorization" (str "Bearer " token)))
             :params params})))

(deftest api-authenticates-and-records-the-token-name
  (let [seen (atom nil)
        handler (capture-handler)]
    (with-redefs [capture/enqueue!
                  (fn [db url title submitted-by]
                    (reset! seen [db url title submitted-by])
                    {:id 7 :status :pending :inserted true})]
      (is (= 401 (:status (request handler nil))))
      (is (= 401 (:status (request handler "wrong"))))
      (let [response (request handler "iphone-secret-0123456789-0123456789"
                              {:url "https://example.com/article" :title "Title"})]
        (is (= 201 (:status response)))
        (is (= "queued" (get-in response [:body :result])))
        (is (= [:db "https://example.com/article" "Title" "iphone"] @seen))))))

(deftest duplicate-outcomes-give-honest-feedback
  (doseq [[capture-row expected-status expected-result]
          [[{:id 1 :status :pending :inserted false} 200 "already_queued"]
           [{:id 2 :status :processing :inserted false} 200 "already_queued"]
           [{:id 3 :status :complete :inserted false :item-id 42} 200 "already_saved"]
           [{:id 4 :status :failed :inserted false} 409 "needs_attention"]
           [{:id 5 :status :dismissed :inserted false} 409 "needs_attention"]]]
    (with-redefs [capture/enqueue! (fn [& _] capture-row)]
      (let [response (request (capture-handler)
                              "iphone-secret-0123456789-0123456789")]
        (is (= expected-status (:status response)))
        (is (= expected-result (get-in response [:body :result])))))))

(deftest feedback-page-does-not-require-authentication
  (let [response ((capture-handler)
                  {:request-method :get :uri "/"})]
    (is (= 200 (:status response)))
    (is (string/includes? (:body response) "Save to Llar"))
    (is (string/includes? (:body response) "static/capture.js"))))

(deftest invalid-captures-are-client-errors-and-storage-errors-are-retryable
  (testing "validation"
    (with-redefs [capture/enqueue!
                  (fn [& _]
                    (throw (ex-info "bad URL"
                                    {:type :llar.bookmark-capture/invalid-url})))]
      (is (= 400 (:status (request (capture-handler)
                                   "iphone-secret-0123456789-0123456789"))))))
  (testing "durability failure"
    (with-redefs [capture/enqueue! (fn [& _] (throw (Exception. "database down")))]
      (is (= 503 (:status (request (capture-handler)
                                   "iphone-secret-0123456789-0123456789")))))))
