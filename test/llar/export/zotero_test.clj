(ns llar.export.zotero-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llar.export.zotero :as zotero]))

(def +test-item+
  {:title "Test Article"
   :url "\"https://example.com/article\""
   :id 42
   :source-key "test-src"})

(def +test-item-plain-url+
  {:title "Test Article"
   :url "https://example.com/article"
   :id 43
   :source-key "test-src"})

(def +highlight-annotations+
  [{:selector {:quote {:exact "Important text passage"}}
    :body nil}
   {:selector {:quote {:exact "Another highlight"}}
    :body nil}])

(def +note-annotations+
  [{:selector nil
    :body "This is a general note"}
   {:selector nil
    :body "Another note about the article"}])

(def +mixed-annotations+
  [{:selector {:quote {:exact "Highlighted text"}}
    :body nil}
   {:selector nil
    :body "A note without highlight"}
   {:selector {:quote {:exact "Second highlight"}}
    :body nil}])

(deftest test-format-note-html-highlights-only
  (testing "annotations with highlights only produce blockquotes, no Notes section"
    (let [html (zotero/format-note-html +test-item+ +highlight-annotations+)]
      (is (re-find #"<blockquote><p>Important text passage</p></blockquote>" html))
      (is (re-find #"<blockquote><p>Another highlight</p></blockquote>" html))
      (is (re-find #"<h3>Highlights</h3>" html))
      (is (not (re-find #"<h3>Notes</h3>" html)))
      (is (re-find #"<h2>Test Article</h2>" html))
      (is (re-find #"https://example.com/article" html)))))

(deftest test-format-note-html-notes-only
  (testing "annotations with notes only produce paragraphs, no Highlights section"
    (let [html (zotero/format-note-html +test-item+ +note-annotations+)]
      (is (re-find #"<p>This is a general note</p>" html))
      (is (re-find #"<p>Another note about the article</p>" html))
      (is (re-find #"<h3>Notes</h3>" html))
      (is (not (re-find #"<h3>Highlights</h3>" html)))
      (is (not (re-find #"<blockquote>" html))))))

(deftest test-format-note-html-mixed
  (testing "both highlights and notes sections are present"
    (let [html (zotero/format-note-html +test-item+ +mixed-annotations+)]
      (is (re-find #"<h3>Highlights</h3>" html))
      (is (re-find #"<h3>Notes</h3>" html))
      (is (re-find #"<blockquote><p>Highlighted text</p></blockquote>" html))
      (is (re-find #"<blockquote><p>Second highlight</p></blockquote>" html))
      (is (re-find #"<p>A note without highlight</p>" html)))))

(deftest test-format-note-html-empty
  (testing "empty annotations produce basic structure without sections"
    (let [html (zotero/format-note-html +test-item+ [])]
      (is (re-find #"<h2>Test Article</h2>" html))
      (is (re-find #"https://example.com/article" html))
      (is (not (re-find #"<h3>Highlights</h3>" html)))
      (is (not (re-find #"<h3>Notes</h3>" html))))))

(deftest test-format-note-html-escapes-html
  (testing "HTML special characters in annotations are escaped"
    (let [annotations [{:selector {:quote {:exact "<script>alert('xss')</script>"}}
                        :body nil}]
          html (zotero/format-note-html +test-item+ annotations)]
      (is (not (re-find #"<script>" html)))
      (is (re-find #"&lt;script&gt;" html)))))

(deftest test-make-webpage-item
  (testing "webpage item has correct structure with collection"
    (let [result (zotero/make-webpage-item +test-item+ "ABC123")]
      (is (= "webpage" (:itemType result)))
      (is (= "Test Article" (:title result)))
      (is (= "https://example.com/article" (:url result)))
      (is (string? (:accessDate result)))
      (is (= "llar-id: 42" (:extra result)))
      (is (= [{:tag "llar"}] (:tags result)))
      (is (= ["ABC123"] (:collections result)))))
  (testing "webpage item without collection key"
    (let [result (zotero/make-webpage-item +test-item+ nil)]
      (is (nil? (:collections result)))))
  (testing "webpage item handles plain URL"
    (let [result (zotero/make-webpage-item +test-item-plain-url+ "ABC123")]
      (is (= "https://example.com/article" (:url result))))))

(deftest test-make-child-note
  (testing "child note has correct structure"
    (let [result (zotero/make-child-note "ABC123" "<h2>Test</h2>")]
      (is (= "note" (:itemType result)))
      (is (= "ABC123" (:parentItem result)))
      (is (= "<h2>Test</h2>" (:note result)))
      (is (= [{:tag "llar-annotations"}] (:tags result))))))
