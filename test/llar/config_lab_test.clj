(ns llar.config-lab-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is use-fixtures]]
   [llar.appconfig :as appconfig]
   [llar.blobstore :as blobstore]
   [llar.config :as config]
   [llar.fetch :as fetch]
   [llar.fetch.feed :as feed]
   [llar.config-lab :as lab]
   [llar.http :as http]))

(def test-settings
  {:enabled? true
   :max-concurrent-runs 2
   :run-timeout-ms 2000
   :session-ttl-minutes 5})

(use-fixtures :each
  (fn [test-fn]
    (with-redefs [lab/settings (constantly test-settings)]
      (lab/reset-state-for-tests!)
      (try
        (test-fn)
        (finally
          (lab/reset-state-for-tests!))))))

(deftest sandbox-compiles-supported-source-and-processors
  (let [source (lab/compile-source
                "(src/selector-feed \"https://example.com/\" {:urls (S/tag :a)} {} {})")
        processors (lab/compile-processors
                    {:pre "(fn [item] (assoc item :lab true))"
                     :filter "(constantly false)"
                     :post "[]"})]
    (is (instance? llar.src.SelectorFeed source))
    (is (= true (:lab ((first (:pre processors)) {}))))
    (is (false? ((first (:filter processors)) {})))
    (is (instance? llar.src.Custom
                   (lab/compile-source "(src/custom :trial (fn [] []))")))
    (is (instance? llar.src.HackerNews
                   (lab/compile-source "(src/hn :front_page :count 10)")))
    (is (instance? llar.src.GitHubSearch
                   (lab/compile-source "(src/github-issues \"repo:irq0/llar\")")))))

(deftest sandbox-exposes-safe-url-parser-helper
  (let [source (lab/compile-source
                (str "(src/selector-feed \"https://example.com/news/\" "
                     "{:urls (S/tag :a)} "
                     "{:urls (fn [nodes] "
                     "         (map (fn [node] "
                     "                ($parse-url (get-in node [:attrs :href]) "
                     "                            \"https://example.com/news/\")) "
                     "              nodes))} {})"))
        extract-urls (get-in source [:extractors :urls])]
    (is (= ["https://example.com/story"]
           (mapv str (extract-urls [{:attrs {:href "/story"}}]))))))

(deftest sandbox-exposes-the-complete-config-helper-environment
  (let [canonical-helpers (->> (config/config-engine-environment)
                               (filter #(= :helper (:llar.config/kind %)))
                               (map :symbol)
                               set)
        helper-references (->> canonical-helpers
                               (sort-by str)
                               (map str)
                               (string/join " "))]
    (is (= '#{$credentials $html2text $ellipsify $parse-ts $category-rm
              $extract $add-tag $add-tag-filter $exchange $html-to-hickory
              $hickory-sanitize-blobify $make-item-hash $fetch $parse-url
              $hickory-to-html $uri-path $http-cookie-store $http-get $http-post}
           canonical-helpers))
    (is (fn? (first (:pre (lab/compile-processors
                           {:pre (str "(let [_ [" helper-references "]] identity)")})))))))

(deftest sandbox-supports-uri-interop-used-by-selector-feeds
  (let [source (lab/compile-source
                "(src/selector-feed
                   \"https://www.snia.org/blog\"
                   {:urls (S/and
                            (S/tag :a)
                            (S/attr :href #(re-matches #\"/blog/[0-9]{4}/.+\" %)))
                    :title (S/tag :h1)
                    :author (S/class \"author\")
                    :content (S/and (S/tag :div) (S/class \"main\"))}
                   {:urls (fn [links]
                            (->> links
                                 (map #(get-in % [:attrs :href]))
                                 distinct
                                 (map #(str (java.net.URI/create
                                              (str \"https://www.snia.org\" %))))))}
                   {})")
        extract-urls (get-in source [:extractors :urls])]
    (is (= ["https://www.snia.org/blog/2026/storage"]
           (vec (extract-urls [{:attrs {:href "/blog/2026/storage"}}
                               {:attrs {:href "/blog/2026/storage"}}]))))))

(deftest sandbox-does-not-expose-credentials-through-helper
  (let [processor (first (:pre (lab/compile-processors
                                {:pre "(fn [item] ($credentials :private))"})))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"credentials are never exposed"
                          (processor {})))))

(deftest sandbox-does-not-expose-host-capabilities
  (doseq [form ["(slurp \"/etc/passwd\")"
                "(clojure.core/slurp \"/etc/passwd\")"
                "(eval '(+ 1 2))"
                "(future (loop [] (recur)))"
                "(java.lang.System/getenv)"]]
    (is (thrown? Exception (lab/compile-source form)) form)))

(deftest network-boundary-rejects-local-addresses
  (doseq [url ["file:///etc/passwd"
               "http://localhost:8080/"
               "http://127.0.0.1/"
               "http://[::1]/"]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Config Lab"
                          (lab/validate-public-url! url))
        url)))

(deftest sessions-export-without-changing-runtime-config
  (let [session (lab/create-session!
                 "tester"
                 {:source-key "example-feed"
                  :source-form "(src/feed \"https://example.com/feed.xml\")"
                  :tags-form "#{:news}"
                  :options-form "#{:mark-read-on-view}"
                  :processors {:pre "[]" :filter "(constantly false)" :post "[]"}})
        id (:id session)]
    (is (= :compiled (:stage (lab/compile-session! "tester" id {}))))
    (is (= (str "(fetch example-feed\n"
                "  (src/feed \"https://example.com/feed.xml\")\n"
                "  :tags #{:news}\n"
                "  :options #{:mark-read-on-view}\n"
                "  :rm-fn (constantly false))\n")
           (lab/export-form "tester" id)))
    (is (= (str "(fetch exported-feed\n"
                "  (src/feed \"https://example.com/feed.xml\")\n"
                "  :tags #{:later}\n"
                "  :rm-fn (constantly false))\n")
           (lab/export-form "tester" id {:source-key "exported-feed"
                                         :tags-form "#{:later}"
                                         :options-form "#{}"})))
    (is (thrown? clojure.lang.ExceptionInfo (lab/session-view "other-user" id)))
    (is (true? (lab/delete-session! "tester" id)))))

(deftest source-key-and-metadata-are-export-only
  (let [session (lab/create-session!
                 "tester"
                 {:source-key "not a valid key"
                  :source-form "(src/feed \"https://www.example.com/news/feed.xml\")"
                  :tags-form "not edn"
                  :processors {:pre "[]" :filter "[]" :post "[]"}})
        compiled (lab/compile-session! "tester" (:id session) {})]
    (is (= :compiled (:stage compiled)))
    (is (= "example-com-news-feed-xml" (:suggested-source-key compiled)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Source key"
                          (lab/export-form "tester" (:id session)
                                           {:tags-form "#{}" :options-form "#{}"})))))

(deftest empty-processor-vectors-are-not-exported
  (let [session (lab/create-session!
                 "tester"
                 {:source-form "(src/feed \"https://example.com/feed.xml\")"
                  :processors {:pre "[]" :filter "[]" :post "[]"}})]
    (is (= (str "(fetch example\n"
                "  (src/feed \"https://example.com/feed.xml\"))\n")
           (lab/export-form "tester" (:id session)
                            {:source-key "example"
                             :tags-form "#{}"
                             :options-form "#{}"})))))

(deftest session-blobs-are-owner-scoped
  (let [session (lab/create-session! "tester" {})
        hash (apply str (repeat 64 "a"))
        file (java.io.File/createTempFile "llar-config-lab-test" ".blob")]
    (try
      (with-redefs [blobstore/get-local-filename (constantly file)
                    blobstore/get-blob (fn [requested]
                                         {:hash requested :mime-type "image/png"})]
        (is (= hash (:hash (lab/session-blob "tester" (:id session) hash))))
        (is (thrown? clojure.lang.ExceptionInfo
                     (lab/session-blob "someone-else" (:id session) hash))))
      (finally
        (.delete file)))))

(deftest generic-workflow-uses-temporary-storage-and-processes-cached-items
  (let [session (lab/create-session!
                 "tester"
                 {:source-key "example"
                  :source-form "(src/website \"https://example.com/\")"
                  :tags-form "#{}"
                  :options-form "#{}"
                  :processors {:pre "[]" :filter "(constantly false)" :post "[]"}})
        id (:id session)
        isolated-dir (atom nil)
        guarded-blob-fetch? (atom false)
        item {:summary {:title "Before"}
              :entry {:url "https://example.com/"
                      :contents {"text/plain" "Body"}}
              :meta {:tags #{}}
              :hash "test-hash"}]
    (lab/compile-session! "tester" id {})
    (with-redefs [fetch/fetch-source (fn [& _]
                                       (reset! isolated-dir (appconfig/blob-store-dir))
                                       (reset! guarded-blob-fetch?
                                               (identical? blobstore/*http-get*
                                                           http/get-with-redirect-guard))
                                       [item])]
      (is (= :fetched (:stage (lab/fetch-session! "tester" id)))))
    (is (string? @isolated-dir))
    (is (not= (appconfig/blob-store-dir) @isolated-dir))
    (is @guarded-blob-fetch?)
    (let [processed (lab/process-session!
                     "tester" id
                     {:pre "[]"
                      :filter "(constantly false)"
                      :post "(fn [item] (assoc-in item [:summary :title] \"After\"))"})]
      (is (= :processed (:stage processed)))
      (is (= "After" (get-in processed [:items 0 :after :title]))))))

(deftest selector-workflow-exposes-the-http-hickory-tree
  (let [session (lab/create-session!
                 "tester"
                 {:source-form "(src/selector-feed \"https://example.com/\" {:urls (S/tag :a)} {} {})"})
        id (:id session)
        hickory {:type :document
                 :content [{:type :element
                            :tag :html
                            :attrs {}
                            :content []}]}
        selected-node {:type :element
                       :tag :a
                       :attrs {:href "/article"}
                       :content ["Article"]}
        fetched-index {:http {:raw {:status 200
                                    :headers {:content-type "text/html"}
                                    :body "<html></html>"}
                              :summary {:title "Example"}
                              :trace {:raw-html "<html></html>"
                                      :dompurify-html "<html></html>"
                                      :final-html "<html></html>"}
                              :hickory hickory}
                       :matches [selected-node]
                       :item-urls ["https://example.com/article"]}]
    (lab/compile-session! "tester" id {})
    (with-redefs [feed/fetch-selector-index (fn [& _] fetched-index)]
      (let [result (lab/fetch-session! "tester" id)]
        (is (= hickory (:hickory result)))
        (is (= [selected-node]
               (get-in result [:url-selector :selected-hickory])))
        (is (= "Example" (get-in result [:summary :title])))
        (is (= 200 (get-in result [:response :status])))))))

(deftest worker-errors-are-unwrapped-for-actionable-lab-responses
  (let [session (lab/create-session!
                 "tester"
                 {:source-form "(src/selector-feed \"https://example.com/\" {:urls (S/tag :a)} {} {})"})
        id (:id session)]
    (lab/compile-session! "tester" id {})
    (with-redefs [feed/fetch-selector-index
                  (fn [& _]
                    (throw (ex-info "Invalid selector value"
                                    {:type ::invalid-selector-value
                                     :field :ts})))]
      (try
        (lab/fetch-session! "tester" id)
        (is false "Expected the worker error")
        (catch clojure.lang.ExceptionInfo exception
          (is (= ::invalid-selector-value (:type (ex-data exception))))
          (is (= :ts (:field (ex-data exception)))))))))

(deftest selector-item-returns-partial-diagnostics-when-item-is-invalid
  (let [session (lab/create-session!
                 "tester"
                 {:source-form "(src/selector-feed \"https://example.com/\" {:urls (S/tag :a)} {} {})"})
        id (:id session)
        index {:http {:raw {:status 200 :headers {} :body "index"}
                      :summary {:title "Index"}
                      :hickory {:type :document :content []}}
               :matches []
               :item-urls ["https://example.com/article"]}
        fetched {:item-url "https://example.com/article"
                 :http {:raw {:status 200 :headers {} :body "article"}
                        :summary {:title "Article"}
                        :hickory {:type :document :content []}}}
        diagnostic {:item nil
                    :valid? false
                    :errors [{:field :ts :message "Invalid timestamp"}]
                    :fields {:ts {:value "August 10"
                                  :valid? false
                                  :validation-error "Invalid timestamp"
                                  :expected "java.time.ZonedDateTime"
                                  :actual-type "java.lang.String"}}}]
    (lab/compile-session! "tester" id {})
    (with-redefs [feed/fetch-selector-index (fn [& _] index)]
      (lab/fetch-session! "tester" id))
    (with-redefs [feed/fetch-selector-item (fn [& _] fetched)
                  feed/extract-selector-item (fn [& _] diagnostic)]
      (let [result (lab/fetch-selector-item! "tester" id 0)]
        (is (false? (:valid? result)))
        (is (nil? (:item result)))
        (is (= "August 10" (get-in result [:fields :ts :value])))
        (is (= "Invalid timestamp" (get-in result [:errors 0 :message])))))))
