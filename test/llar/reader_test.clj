(ns llar.reader-test
  (:require
   [clojure.test :refer [are deftest is]]
   [llar.reader :as uut]))

(deftest item-path-encodes-reader-context
  (are [expected context]
       (= expected (uut/item-path context 42))
    ["/reader/group" "type" "bookmark" "source" "all" "item/by-id" 42]
    uut/bookmark-context

    ["/reader/group" "default" "all" "source" "feed" "item/by-id" 42]
    (uut/source-context "feed"))
  (is (= "feed/name"
         (:source-key (uut/source-context "feed/name")))))

(deftest path-string-and-absolute-url-use-one-canonical-separator
  (let [path (uut/item-path uut/bookmark-context 42)]
    (is (= path (uut/bookmark-item-path 42)))
    (is (= "/reader/group/type/bookmark/source/all/item/by-id/42"
           (uut/path-string path)))
    (is (= "https://reader.example/reader/group/type/bookmark/source/all/item/by-id/42"
           (uut/absolute-url "https://reader.example///" path)))))

(deftest collection-item-action-and-tool-paths-share-the-route-grammar
  (let [context (uut/source-context :feed)]
    (is (= "/reader/group/default/all/source/feed/items"
           (uut/path-string (uut/items-path context))))
    (is (= "/reader/group/default/all/source/feed/update"
           (uut/path-string (uut/update-path context))))
    (is (= "/reader/group/default/all/source/feed/item/by-id/42/related"
           (uut/path-string (uut/item-action-path context 42 :related))))
    (is (= "/reader/tools/search"
           (uut/path-string (uut/tool-path :search))))
    (is (= "/reader/tools/todays-vibe/seen"
           (uut/path-string (uut/tool-action-path :todays-vibe :seen))))
    (is (= "/reader/item/by-id/42/state"
           (uut/path-string (uut/short-item-action-path 42 :state))))))
