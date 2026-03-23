(ns llar.export.url-handler-test
  (:require
   [clojure.string]
   [clojure.test :refer [deftest is testing]]
   [llar.export.url-handler :as url-handler]))

(def test-item
  {:title "Test Article"
   :url "\"https://example.com/article\""
   :id 42
   :source-key "test-src"})

(def test-highlight-1
  {:id 1
   :selector {:position {:type "TextPositionSelector" :start 10 :end 24}
              :quote {:type "TextQuoteSelector"
                      :exact "important text"
                      :prefix "some "
                      :suffix " here"}}
   :body nil})

(def test-highlight-2
  {:id 3
   :selector {:position {:type "TextPositionSelector" :start 50 :end 67}
              :quote {:type "TextQuoteSelector"
                      :exact "another highlight"
                      :prefix ""
                      :suffix ""}}
   :body nil})

(def test-note-1
  {:id 2
   :selector nil
   :body "This is my note"})

(def test-note-2
  {:id 4
   :selector nil
   :body "Second note"})

(deftest test-format-body-full
  (testing "notes on top as list items, highlights as paragraphs"
    (let [body (url-handler/format-body
                test-item
                [test-highlight-1 test-highlight-2 test-note-1 test-note-2])]
      (is (clojure.string/includes? body "- This is my note"))
      (is (clojure.string/includes? body "- Second note"))
      (is (clojure.string/includes? body "important text"))
      (is (clojure.string/includes? body "another highlight"))
      (is (not (clojure.string/includes? body "> ")))
      (testing "notes come before highlights"
        (is (< (.indexOf body "- This is my note")
               (.indexOf body "important text")))))))

(deftest test-format-body-highlights-only
  (testing "highlights as plain paragraphs"
    (let [body (url-handler/format-body test-item [test-highlight-1])]
      (is (= "important text\n" body)))))

(deftest test-format-body-highlights-separated-by-blank-lines
  (testing "multiple highlights separated by blank lines"
    (let [body (url-handler/format-body test-item
                                        [test-highlight-1 test-highlight-2])]
      (is (= "important text\n\nanother highlight\n" body)))))

(deftest test-format-body-notes-only
  (testing "notes as list items"
    (let [body (url-handler/format-body test-item [test-note-1])]
      (is (= "- This is my note\n" body)))))

(deftest test-format-body-empty
  (testing "empty annotations produce empty string"
    (let [body (url-handler/format-body test-item [])]
      (is (= "" body)))))

(deftest test-build-export-url-source-placeholder
  (testing "{source} placeholder is substituted"
    (with-redefs [url-handler/url-handler-config
                  (constantly {:template "app://save?src={source}&title={title}"})]
      (let [url (url-handler/build-export-url test-item [])]
        (is (clojure.string/includes? url "src=test-src"))))))

(deftest test-url-encode
  (testing "spaces become %20"
    (is (= "hello%20world" (url-handler/url-encode "hello world"))))
  (testing "special characters are encoded"
    (is (= "a%26b" (url-handler/url-encode "a&b")))
    (is (= "a%3Db" (url-handler/url-encode "a=b")))
    (is (= "%22quoted%22" (url-handler/url-encode "\"quoted\""))))
  (testing "plain ascii passes through"
    (is (= "hello" (url-handler/url-encode "hello")))))
