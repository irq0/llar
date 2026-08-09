(ns llar.fetch.reddit-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [llar.appconfig :as appconfig]
   [llar.apis.reader :as reader]
   [llar.repl :as repl]
   [llar.store :as store]
   [llar.fetch :as fetch]
   [llar.fetch.reddit :as uut]
   [llar.fetchutils :as fetchutils]
   [llar.src :as src]
   [clj-http.client :as http-client]
   [java-time.api :as time]
   [mount.core :as mount])
  (:import
   [java.io File]))

(defn- credentials-file
  "Throwaway credentials.edn so the real credential path is exercised."
  []
  (let [f (File/createTempFile "llar-test-credentials" ".edn")]
    (.deleteOnExit f)
    (spit f (pr-str {:reddit {:app-id "testappid"
                              :secret "testsecret"
                              :username "testuser"}}))
    (.getAbsolutePath f)))

(defn- start-appconfig!
  ([] (start-appconfig! (credentials-file)))
  ([credentials-path]
   (mount/start-with {#'appconfig/appconfig
                      {:credentials-file credentials-path
                       :runtime-config-dir "/dev/null"
                       :version {:version "1.2.3"}
                       :throttle {:command-max-concurrent 10}
                       :timeouts {:html2text 30}
                       ;; the 4xx error path renders the body via html2text
                       :commands {:lynx "true"
                                  :w3m "true"
                                  :pandoc "true"
                                  :html2text "true"}}
                      #'reader/frontend-db nil
                      #'store/backend-db nil
                      #'repl/nrepl-server nil})))

(def ^:private valid-child
  {:url "https://www.example.com/article"
   :permalink "/r/clojure/comments/abc123/an_article/"
   :thumbnail "self"
   :created_utc 1735689600
   :title "An article"
   :author "someone"
   :id "abc123"
   :score 42
   :selftext "the body"})

(defn- listing-response [children]
  {:headers {}
   :body {:data {:children (mapv (fn [c] {:data c}) children)}}})

;; ---------------------------------------------------------------- url building

(deftest listing-path-test
  (testing "subreddit scoped listings"
    (is (= "/r/clojure/top" (uut/listing-path (src/reddit "clojure" :top))))
    (is (= "/r/clojure/new" (uut/listing-path (src/reddit "clojure" :new))))
    (is (= "/r/clojure/rising" (uut/listing-path (src/reddit "clojure" :rising)))))
  (testing ":best is the front page and cannot honor the subreddit"
    (is (= "/best" (uut/listing-path (src/reddit "clojure" :best))))))

(deftest listing-params-test
  (testing "timeframe is sent only where reddit honors it"
    (is (= {:limit 100 :raw_json 1 :t "week"}
           (uut/listing-params (src/reddit "clojure" :top :week))))
    (is (= {:limit 100 :raw_json 1 :t "day"}
           (uut/listing-params (src/reddit "clojure" :controversial :day)))))
  (testing "timeframe is omitted where reddit ignores it"
    (is (= {:limit 100 :raw_json 1}
           (uut/listing-params (src/reddit "clojure" :new :week))))
    (is (= {:limit 100 :raw_json 1}
           (uut/listing-params (src/reddit "clojure" :hot :month))))))

(deftest random-listing-unsupported-test
  (testing ":random never returned a listing, so it is rejected outright"
    (is (not (contains? src/reddit-supported-listings :random)))
    (is (thrown? AssertionError (src/reddit "clojure" :random)))))

;; ------------------------------------------------------------------ user agent

(deftest user-agent-test
  (start-appconfig!)
  (testing "matches the format reddit mandates: platform:app:version (by /u/user)"
    (is (= "java:llar:v1.2.3 (by /u/testuser)" (uut/reddit-user-agent)))))

;; ----------------------------------------------------------------- oauth token

(defn- reset-token-cache! []
  (reset! @#'uut/token-cache nil))

(deftest access-token-test
  (start-appconfig!)
  (testing "token is fetched once and then served from cache"
    (let [posts (atom 0)]
      (with-redefs [http-client/post (fn [_url _opts]
                                       (swap! posts inc)
                                       {:body {:access_token "tok-1" :expires_in 86400}})]
        (reset-token-cache!)
        (is (= "tok-1" (uut/access-token)))
        (is (= "tok-1" (uut/access-token)))
        (is (= 1 @posts) "cached token is reused")
        (is (= "tok-1" (uut/access-token true)))
        (is (= 2 @posts) "forced refresh re-requests"))))

  (testing "credentials are sent as HTTP basic auth with the client_credentials grant"
    (let [captured (atom nil)]
      (with-redefs [http-client/post (fn [url opts]
                                       (reset! captured [url opts])
                                       {:body {:access_token "tok" :expires_in 86400}})]
        (reset-token-cache!)
        (uut/access-token)
        (let [[url opts] @captured]
          (is (= "https://www.reddit.com/api/v1/access_token" url))
          (is (= ["testappid" "testsecret"] (:basic-auth opts)))
          (is (= {:grant_type "client_credentials"} (:form-params opts)))))))

  (testing "an expired cached token is renewed"
    (let [posts (atom 0)]
      (with-redefs [http-client/post (fn [_url _opts]
                                       (swap! posts inc)
                                       {:body {:access_token "tok-fresh" :expires_in 86400}})]
        (reset! @#'uut/token-cache
                {:token "tok-stale"
                 :expires-at (time/minus (time/zoned-date-time) (time/hours 1))})
        (is (= "tok-fresh" (uut/access-token)))
        (is (= 1 @posts)))))

  (testing "a token about to expire is renewed ahead of time"
    (let [posts (atom 0)]
      (with-redefs [http-client/post (fn [_url _opts]
                                       (swap! posts inc)
                                       {:body {:access_token "tok-fresh" :expires_in 86400}})]
        (reset! @#'uut/token-cache
                {:token "tok-almost-stale"
                 :expires-at (time/plus (time/zoned-date-time) (time/minutes 1))})
        (is (= "tok-fresh" (uut/access-token))
            "within the refresh margin, so renewed rather than used")
        (is (= 1 @posts)))))

  (testing "a response without an access_token is an error, not a nil token"
    (with-redefs [http-client/post (fn [_url _opts] {:body {}})]
      (reset-token-cache!)
      (is (thrown? clojure.lang.ExceptionInfo (uut/access-token))))))

(deftest concurrent-access-token-renewal-test
  (start-appconfig!)
  (let [posts (atom 0)
        request-started (promise)
        release-request (promise)]
    (with-redefs [http-client/post
                  (fn [_url _opts]
                    (swap! posts inc)
                    (deliver request-started true)
                    @release-request
                    {:body {:access_token "shared-token" :expires_in 86400}})]
      (reset-token-cache!)
      (let [first-caller (future (uut/access-token))]
        @request-started
        (let [waiting-callers (doall (repeatedly 7 #(future (uut/access-token))))]
          (deliver release-request true)
          (is (= (repeat 8 "shared-token")
                 (mapv deref (cons first-caller waiting-callers))))
          (is (= 1 @posts) "concurrent callers share a single token request"))))))

(deftest missing-credentials-test
  (start-appconfig! "/nonexistent/credentials.edn")
  (testing "absent credentials fail loudly instead of sending an empty bearer token"
    (is (thrown? clojure.lang.ExceptionInfo (uut/reddit-user-agent)))))

;; --------------------------------------------------------------------- request

(deftest reddit-get-sends-bearer-token-test
  (start-appconfig!)
  (let [captured (atom nil)]
    (with-redefs [uut/access-token (constantly "tok")
                  http-client/get (fn [url opts]
                                    (reset! captured [url opts])
                                    (listing-response []))]
      (uut/reddit-get "/r/clojure/top" {:limit 100 :t "week"})
      (let [[url opts] @captured]
        (testing "requests go to the oauth host, without the dead .json suffix"
          (is (= "https://oauth.reddit.com/r/clojure/top" url))
          (is (not (re-find #"\.json" url))))
        (testing "bearer token and user agent are set"
          (is (= "bearer tok" (get-in opts [:headers :authorization])))
          (is (= "java:llar:v1.2.3 (by /u/testuser)" (get-in opts [:headers :user-agent]))))
        (testing "params are passed as query params rather than formatted into the url"
          (is (= {:limit 100 :t "week"} (:query-params opts))))))))

(deftest reddit-get-renews-token-on-401-test
  (start-appconfig!)
  (let [gets (atom 0)
        tokens (atom [])]
    (with-redefs [uut/access-token (fn
                                     ([] "stale-token")
                                     ([_force?] "fresh-token"))
                  http-client/get (fn [_url opts]
                                    (swap! gets inc)
                                    (swap! tokens conj (get-in opts [:headers :authorization]))
                                    (if (= 1 @gets)
                                      (throw (ex-info "clj-http"
                                                      {:type :clj-http.client/unexceptional-status
                                                       :status 401
                                                       :headers {}
                                                       :body ""}))
                                      (listing-response [])))]
      (testing "a rejected token is renewed and the request retried exactly once"
        (is (= {:data {:children []}} (uut/reddit-get "/r/clojure/new" {})))
        (is (= 2 @gets))
        (is (= ["bearer stale-token" "bearer fresh-token"] @tokens))))))

;; ------------------------------------------------------------------ fetching

(deftest fetch-source-normalizes-api-variants-and-drops-invalid-items-test
  (start-appconfig!)
  (let [children [valid-child
                  ;; thumbnail is absent on some posts
                  (dissoc valid-child :thumbnail)
                  ;; selftext is absent on crossposts
                  (dissoc valid-child :selftext)
                  ;; gallery and self posts can carry a non-absolute url
                  (assoc valid-child :url "/r/clojure/comments/xyz/")
                  ;; genuinely malformed data is still rejected
                  (dissoc valid-child :title)]]
    (with-redefs [uut/access-token (constantly "tok")
                  http-client/get (fn [_url _opts] (listing-response children))]
      (let [items (fetch/fetch-source (src/reddit "clojure" :top) {})]
        (testing "spec violations are dropped, not passed on as nil"
          (is (= 4 (count items)))
          (is (every? some? items) "a nil here would reach postprocessing and the store"))
        (testing "valid items are mapped as before"
          (let [entry (:entry (first items))]
            (is (= "An article" (:title entry)))
            (is (= 42 (:score entry)))
            (is (= ["someone"] (:authors entry)))
            (is (= "the body" (get-in entry [:contents "text/plain"]))))
          (is (nil? (get-in (second items) [:entry :thumbnail]))
              "Reddit legitimately omits thumbnails from some posts"))
        (testing "missing selftext and relative post URLs are normalized"
          (is (= "" (get-in (nth items 2) [:entry :contents "text/plain"])))
          (is (= "https://www.reddit.com/r/clojure/comments/xyz/"
                 (str (get-in (nth items 3) [:entry :url])))))))))

(deftest fetch-source-records-scores-test
  (start-appconfig!)
  (let [src (src/reddit "clojure" :top)
        children (map #(assoc valid-child :id (str "id" %) :score %) (range 1 101))]
    (with-redefs [uut/access-token (constantly "tok")
                  http-client/get (fn [_url _opts] (listing-response children))]
      (fetch/fetch-source src {})
      (testing "scores are recorded so the dynamic cutoff needs no second request"
        (is (= (set (range 1 101)) (set (get @uut/last-listing-scores src))))
        (is (= 96 (fetchutils/get-reddit-cutoff-score src))
            "top 5% of 1..100 starts at 96")))))

;; ---------------------------------------------------------------- score cutoff

(deftest top-percentile-score-test
  (testing "an empty listing yields no cutoff instead of throwing"
    (is (nil? (fetchutils/top-percentile-score [] 0.05)))
    (is (nil? (fetchutils/top-percentile-score nil 0.05))))
  (testing "cutoff bounds the top fraction"
    (is (= 96 (fetchutils/top-percentile-score (range 1 101) 0.05)))
    (is (= 91 (fetchutils/top-percentile-score (range 1 101) 0.1))))
  (testing "small listings still yield a usable cutoff"
    (is (= 5 (fetchutils/top-percentile-score [5] 0.05)))
    (is (= 9 (fetchutils/top-percentile-score [1 9] 0.05)))))

(deftest reddit-proc-filter-test
  (let [item (fn [score] {:entry {:score score}})
        filter-fn (fn [opts]
                    (:filter (fetchutils/make-reddit-proc (src/reddit "clojure" :top) opts)))]
    (testing "static min-score keeps items at or above the threshold"
      (let [f (filter-fn {:min-score 5 :dynamic? false})]
        (is (false? (boolean (f (item 5)))) "min-score is inclusive per its docstring")
        (is (true? (boolean (f (item 4)))))
        (is (false? (boolean (f (item 500)))))))
    (testing "min-score is honored even with the dynamic cutoff enabled"
      ;; regression: the cutoff used to collapse to the dynamic value alone, so a
      ;; configured min-score had no effect at all
      (reset! uut/last-listing-scores {})
      (let [f (filter-fn {:min-score 100 :dynamic? true})]
        (is (true? (boolean (f (item 50)))) "below min-score, dropped")
        (is (false? (boolean (f (item 100)))))))
    (testing "a source with no scores yet falls back to min-score without throwing"
      (reset! uut/last-listing-scores {})
      (let [f (filter-fn {:min-score 0 :dynamic? true})]
        (is (false? (boolean (f (item 0)))))))))
