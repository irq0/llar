(ns llar.fetch.github-test
  (:require
   [llar.fetch.github :as uut]
   [llar.postproc :as postproc]
   [llar.persistency :as persistency]
   [llar.fetch :as fetch]
   [llar.src :as src]
   [llar.specs]
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is testing]]))

(s/check-asserts true)

(deftest expand-date-tokens-test
  (testing "expands known tokens to YYYY-MM-DD format"
    (let [result (uut/expand-date-tokens "created:>{{today}}")]
      (is (string? result))
      (is (re-find #"created:>\d{4}-\d{2}-\d{2}" result))
      (is (not (.contains result "{{")))))
  (testing "expands multiple tokens"
    (let [result (uut/expand-date-tokens "created:>{{last-week}} pushed:>{{yesterday}}")]
      (is (not (.contains result "{{")))
      (is (= 2 (count (re-seq #"\d{4}-\d{2}-\d{2}" result))))))
  (testing "leaves unknown tokens alone"
    (let [result (uut/expand-date-tokens "foo:>{{unknown-token}}")]
      (is (.contains result "{{unknown-token}}"))))
  (testing "passes through plain strings"
    (is (= "repo:ceph/ceph is:pr" (uut/expand-date-tokens "repo:ceph/ceph is:pr")))))

(deftest github-source-test
  (testing "creates github-issues source with defaults"
    (let [src (src/github-issues "repo:ceph/ceph is:pr")]
      (is (src/github-search? src))
      (is (src/source? src))
      (is (= :issues (:search-type src)))
      (is (= "repo:ceph/ceph is:pr" (:query src)))
      (is (= 30 (get-in src [:args :per-page])))
      (is (= :desc (get-in src [:args :order])))))
  (testing "creates github-repos source with custom args"
    (let [src (src/github-repos "language:clojure stars:>20" :sort :stars :per-page 50)]
      (is (src/github-search? src))
      (is (= :repos (:search-type src)))
      (is (= :stars (get-in src [:args :sort])))
      (is (= 50 (get-in src [:args :per-page])))))
  (testing "toString is informative"
    (let [src (src/github-issues "repo:ceph/ceph is:pr")]
      (is (.contains (str src) "GitHubSearch"))
      (is (.contains (str src) "issues"))
      (is (.contains (str src) "ceph")))))

(def sample-issue-hit
  {:id 123456789
   :number 42
   :title "Fix RGW multipart upload"
   :html_url "https://github.com/ceph/ceph/pull/42"
   :state "closed"
   :draft false
   :user {:login "testuser" :avatar_url "https://avatars.githubusercontent.com/u/123"}
   :labels [{:name "rgw" :color "00ffe4"} {:name "bug" :color "d73a4a"}]
   :assignees [{:login "reviewer1"} {:login "reviewer2"}]
   :comments 5
   :reactions {:total_count 3}
   :pull_request {:merged_at "2026-03-15T10:00:00Z"
                  :diff_url "https://github.com/ceph/ceph/pull/42.diff"}
   :created_at "2026-03-10T08:00:00Z"
   :closed_at "2026-03-15T10:00:00Z"
   :body "Fixed the multipart upload issue.\n\nThis addresses the race condition in rgw_op.cc."})

(def sample-repo-hit
  {:id 987654321
   :full_name "cool/project"
   :description "A cool Clojure project"
   :html_url "https://github.com/cool/project"
   :stargazers_count 150
   :forks_count 12
   :language "Clojure"
   :topics ["clojure" "web"]
   :owner {:login "cool" :avatar_url "https://avatars.githubusercontent.com/u/456"}
   :license {:spdx_id "MIT"}
   :homepage "https://cool-project.dev"
   :created_at "2026-03-15T00:00:00Z"})

(deftest issue-html-summary-test
  (testing "generates HTML with PR metadata and body"
    (let [html (#'uut/issue-html-summary sample-issue-hit)]
      (is (string? html))
      (is (.contains html "PR"))
      (is (.contains html "merged"))
      (is (.contains html "testuser"))
      (is (.contains html "rgw"))
      (is (.contains html "bug"))
      (is (.contains html "Fixed the multipart upload issue."))
      (is (.contains html "race condition"))
      (is (.contains html "5 comments"))
      (is (.contains html "3 reactions"))
      (is (.contains html "Diff"))))
  (testing "shows repo name"
    (let [html (#'uut/issue-html-summary sample-issue-hit)]
      (is (.contains html "ceph/ceph"))))
  (testing "shows merge date and open duration"
    (let [html (#'uut/issue-html-summary sample-issue-hit)]
      (is (.contains html "2026-03-15"))
      (is (.contains html "open 5d"))))
  (testing "shows assignees"
    (let [html (#'uut/issue-html-summary sample-issue-hit)]
      (is (.contains html "reviewer1"))
      (is (.contains html "reviewer2"))))
  (testing "shows issue type for non-PR, no diff link"
    (let [html (#'uut/issue-html-summary (dissoc sample-issue-hit :pull_request))]
      (is (.contains html "Issue"))
      (is (not (.contains html "Diff"))))))

(deftest repo-html-summary-test
  (testing "generates HTML with repo info"
    (let [html (#'uut/repo-html-summary sample-repo-hit)]
      (is (string? html))
      (is (.contains html "A cool Clojure project"))
      (is (.contains html "150 stars"))
      (is (.contains html "12 forks"))
      (is (.contains html "Clojure"))
      (is (.contains html "MIT"))
      (is (.contains html "clojure"))
      (is (.contains html "web"))
      (is (.contains html "Homepage"))
      (is (.contains html "cool-project.dev")))))

(defn- make-test-meta []
  {:source (src/github-issues "repo:test/test is:pr")
   :source-name "[GitHubSearch/issues: repo:test/test is:pr]"
   :source-key :test
   :fetch-ts (java.time.ZonedDateTime/now)
   :tags #{}
   :version 2})

(deftest make-github-item-test
  (testing "creates valid GitHubSearchItem"
    (let [meta (make-test-meta)
          summary {:ts (java.time.ZonedDateTime/now) :title "Test PR"}
          hash (fetch/make-item-hash "github-issues-123")
          entry {:url "https://github.com/test/test/pull/1" :title "#1 Test PR"
                 :authors ["tester"] :contents {"text/plain" "body"}}
          item (uut/make-github-item meta summary hash entry)]
      (is (instance? llar.fetch.github.GitHubSearchItem item))
      (is (= "Test PR" (get-in item [:summary :title]))))))

(deftest to-couch-test
  (testing "converts GitHubSearchItem to couch format"
    (let [item (uut/make-github-item
                (make-test-meta)
                {:ts (java.time.ZonedDateTime/now) :title "Test"}
                (fetch/make-item-hash "github-issues-123")
                {:url "https://github.com/test/test/pull/1" :title "Test"
                 :authors ["x"] :contents {}})
          couched (persistency/to-couch item)]
      (is (= :link (:type couched)))
      (is (nil? (get-in couched [:meta :source :args]))))))

(deftest make-issue-entry-test
  (testing "builds entry from issue hit"
    (let [entry (#'uut/make-issue-entry sample-issue-hit)]
      (is (= "#42 Fix RGW multipart upload" (:title entry)))
      (is (= ["testuser"] (:authors entry)))
      (is (= ["rgw" "bug"] (:labels entry)))
      (is (= true (:is-pr entry)))
      (is (= true (:merged entry)))
      (is (= 5 (:num-comments entry)))
      (is (= 3 (:score entry)))
      (is (some? (:pub-ts entry)))
      (is (contains? (:contents entry) "text/html"))
      (is (contains? (:contents entry) "text/plain")))))

(deftest make-issue-entry-nil-fields-test
  (testing "handles missing optional fields"
    (let [minimal-hit {:id 111 :number 1 :title "Minimal"
                       :html_url "https://github.com/a/b/issues/1"
                       :state "open" :user {:login "u"} :comments 0
                       :created_at "2026-03-10T00:00:00Z"}
          entry (#'uut/make-issue-entry minimal-hit)]
      (is (= "" (get-in entry [:contents "text/plain"])))
      (is (= [] (:labels entry)))
      (is (= 0 (:score entry)))
      (is (false? (:is-pr entry)))
      (is (false? (:merged entry))))))

(deftest make-repo-entry-nil-fields-test
  (testing "handles missing optional fields"
    (let [minimal-hit {:id 222 :full_name "a/b"
                       :html_url "https://github.com/a/b"
                       :stargazers_count 0 :forks_count 0
                       :owner {:login "a"} :created_at "2026-01-01T00:00:00Z"}
          entry (#'uut/make-repo-entry minimal-hit)]
      (is (= "" (get-in entry [:contents "text/plain"])))
      (is (nil? (:language entry)))
      (is (= [] (:topics entry)))
      (is (= 0 (:score entry))))))

(deftest make-repo-entry-test
  (testing "builds entry from repo hit"
    (let [entry (#'uut/make-repo-entry sample-repo-hit)]
      (is (= "cool/project" (:title entry)))
      (is (= ["cool"] (:authors entry)))
      (is (= 150 (:score entry)))
      (is (= "Clojure" (:language entry)))
      (is (= ["clojure" "web"] (:topics entry)))
      (is (= 12 (:num-forks entry)))
      (is (some? (:pub-ts entry))))))
