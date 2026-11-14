(ns llar.urls-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require
   [clojure.test :refer [are deftest is testing]]
   [org.bovinegenius [exploding-fish :as uri]]
   [slingshot.test :refer :all]
   [llar.specs]
   [llar.http :as uut]))

(defn- assert-url-repairs []
  (testing
   "Basics"
    (are [x y] (= (str x) (str y))
      "https://example.com/a" (uut/absolutify-url "https%3A%2F%2Fexample.com%2Fa")
      "https://example.com/article?id=1" (uut/absolutify-url "%2Farticle%3Fid%3D1" "https://example.com/dir/page")
      "https://example.com/baz/foo.txt" (uut/absolutify-url "foo.txt" "https://example.com/baz/")
      "https://example.com/foo.txt" (uut/absolutify-url "/foo.txt" "https://example.com/baz/")
      "https://example.com/foo/" (uut/absolutify-url "/foo/" "https://example.com/baz/")
      "https://example.com/foo" (uut/absolutify-url "/foo" "https://example.com/baz/")
      "http://example.com/foo" (uut/absolutify-url "http://example.com/foo" "")
      "http://example.com/foo" (uut/absolutify-url "http://example.com/foo" nil)

      "http://example.com/?ts=foobar" (uut/absolutify-url "?ts=foobar" "http://example.com")
      "http://example.com/#foobar" (uut/absolutify-url "/#foobar" "http://example.com")

      "http://example.com/foo" (uut/absolutify-url "/foo" "http://example.com")
      "http://example.com/foo/bar" (uut/absolutify-url "/foo/bar" "http://example.com")))
  (testing
   "fragments"
    (are [x y] (= (str x) (str (uut/absolutify-url y "http://example.com/")))
      "#fn-1" "#fn-1"))
  (testing
   "leave special urls alone"
    (are [x] (= (str x) (str (uut/absolutify-url x "http://example.com/")))
      "data:text/plain;base64,SGVsbG8sIFdvcmxkIQ=="
      "data:text/html,<script>alert('hi');</script>"
      "data:,Hello%2C%20World!"
      "data:text/html,%3Ch1%3EHello%2C%20World!%3C%2Fh1%3E"
      "mailto:majordomo@example.com?body=subscribe%20bamboo-l"
      "mailto:infobot@example.com?body=send%20current-issue"
      "mailto:chris@example.com"
      "mailto:?to=joe@example.com&cc=bob@example.com&body=hello"))

  (testing
   "Half kaputt, but quirks-mode parsable"

    (is (= "https://example.com/3/extending/extending.html"
           (str (uut/absolutify-url "https://example.com/3/extending/extending.html\"" nil))))
    (is (= "https://example.com/"
           (str (uut/absolutify-url "//" "https://example.com/"))))
    (is (= "https://example.com/972"
           (str (uut/absolutify-url "(https://example.com/972)" nil)))))
  (testing
   "Absolute urls"
    (is
     (= "http://example.com/" (str (uut/absolutify-url "http://example.com"
                                                       "http://some-other-base-url.com")))
     "Absolute urls are returned as is regardless of base url"))
  (testing "Meaningless input should throw"
    (is (thrown+? [:type :llar.http/absolutify-impossible
                   :reason :llar.http/base-url-useless]
                  (uut/absolutify-url "/foo" "")))
    (is (thrown+? [:type :llar.http/absolutify-impossible
                   :reason :llar.http/base-url-relative]
                  (uut/absolutify-url "/example.com" "/foo"))))
  (testing "all urls should have a path"
    (is (= "/"
           (uri/path (uut/absolutify-url "https://example.com" nil)))))

  (testing "don't mess with filenames"
    (is (= "https://example.com/baz/foo.txt"
           (str (uut/absolutify-url "foo.txt" "https://example.com/baz/")))))

  (testing "path traversing should top at top level (quirk)"
    (is (= "https://example.com/foo/bar"
           (str (uut/absolutify-url "../../../foo/bar" "https://example.com/first/second/"))))
    (is (= "https://example.com/foo/bar/"
           (str (uut/absolutify-url "../../../foo/bar/" "https://example.com/first/second/")))))

  (testing "trailing slash preserved"
    (is (= "https://example.com/path/"
           (str (uut/absolutify-url "/path/" "https://example.com/")))))
  (testing "no trailing slash preserved"
    (is (= "https://example.com/path"
           (str (uut/absolutify-url "/path" "https://example.com/")))))
  (testing "query string only input"
    (is (= "http://example.com/?q=test"
           (str (uut/absolutify-url "?q=test" "http://example.com")))))
  (testing "fragment only input"
    (is (= "#section" (str (uut/absolutify-url "#section" "http://example.com/page")))))

  (testing "protocol-relative URLs use base URL's scheme"
    (is (= "https://cdn.example.com/asset.js"
           (str (uut/absolutify-url "//cdn.example.com/asset.js" "https://example.com/"))))
    (is (= "https://example.com/"
           (str (uut/absolutify-url "//" "https://example.com/")))))

  (testing "Path traversing should be resolved"
    (is (= "https://example.com/baz"
           (str (uut/absolutify-url "../../baz" "https://example.com/foo/bar/"))))))

;; Frozen parse-href from 0172119 so compatibility tests exercise both behaviors.
(defn- legacy-parse-href [s]
  (let [decoded (or (uut/swallow-exceptions (java.net.URLDecoder/decode s)) s)]
    (or (uut/swallow-exceptions (#'uut/parse-url decoded))
        (uut/swallow-exceptions (#'uut/parse-url (str "/" decoded))))))

(deftest absolutify-url-compatible-repairs-test
  (doseq [[label parser] [["legacy parser" legacy-parse-href]
                          ["fixed parser" @#'uut/parse-href]]]
    (testing label
      (with-redefs-fn {#'uut/parse-href parser} assert-url-repairs))))

(deftest absolutify-url-compatibility-changes-test
  (doseq [[label raw base legacy expected]
          [["spaces in path, query, and text fragment"
            "https://example.com/a b?q=hello world#:~:text=some text" nil
            "https://example.com/a b?q=hello world#:~:text=some text"
            "https://example.com/a%20b?q=hello%20world#:~:text=some%20text"]
           ["plus signs in query parameters"
            "http://www.google.com/search?hl=en&q=2^20+*+2^12+bytes+in+GB" nil
            "http://www.google.com/search?hl=en&q=2^20 * 2^12 bytes in GB"
            "http://www.google.com/search?hl=en&q=2^20+*+2^12+bytes+in+GB"]
           ["plus signs in path segments"
            "https://example.com/a+b" nil
            "https://example.com/a b" "https://example.com/a+b"]
           ["escaped slash stays within one path segment"
            "https://example.com/a%2Fb" nil
            "https://example.com/a/b" "https://example.com/a%2Fb"]
           ["escape spelling stays unchanged"
            "https://example.com/a%2fb" nil
            "https://example.com/a/b" "https://example.com/a%2fb"]
           ["escaped query delimiters stay within one parameter"
            "https://example.com/?value=a%26b%3Dc" nil
            "https://example.com/?value=a&b=c" "https://example.com/?value=a%26b%3Dc"]
           ["escaped fragment and query markers stay in the path"
            "https://example.com/a%23b%3Fc" nil
            "https://example.com/a#b?c" "https://example.com/a%23b%3Fc"]
           ["double escapes are not decoded on repeated calls"
            "/literal%252Fslash" "https://example.com/base/"
            "https://example.com/literal%2Fslash" "https://example.com/literal%252Fslash"]
           ["escaped Unicode retains its original spelling"
            "/squeezing-cybersecurity-lemons-%E2%80%93-labeling-regime-iot-products"
            "https://example.com"
            "https://example.com/squeezing-cybersecurity-lemons-–-labeling-regime-iot-products"
            "https://example.com/squeezing-cybersecurity-lemons-%E2%80%93-labeling-regime-iot-products"]
           ["relative URL with spaces and escaped plus"
            "/a b?q=%2B+value" "https://example.com/base/"
            "https://example.com/a b?q=+ value" "https://example.com/a%20b?q=%2B+value"]
           ["protocol-relative URL with spaces"
            "//cdn.example.com/a b" "https://example.com/base/"
            "https://cdn.example.com/a b" "https://cdn.example.com/a%20b"]
           ["parenthesis repair no longer truncates at a space"
            "(https://example.com/a b)" nil
            "https://example.com/a" "https://example.com/a%20b"]
           ["trailing quote repair also encodes spaces"
            "https://example.com/a b\"" nil
            "https://example.com/a b" "https://example.com/a%20b"]]]
    (testing label
      (is (= legacy (with-redefs-fn {#'uut/parse-href legacy-parse-href}
                      #(str (uut/absolutify-url raw base))))
          "the old output is measured, not inferred from the new implementation")
      (let [url (uut/absolutify-url raw base)]
        (is (= expected (str url)))
        (is (= expected (str (uut/absolutify-url (str url))))
            "normalizing the string twice preserves the destination")
        (is (= expected (str (uut/absolutify-url url)))
            "callers also pass parsed URI objects")))))

(deftest absolutify-url-repairs-entirely-encoded-urls-test
  (doseq [[raw expected]
          [["https%3A%2F%2Fexample.com%2Fa" "https://example.com/a"]
           ["%2Farticle%3Fid%3D1" "https://base.example/article?id=1"]
           ["HTTP%3a%2f%2fexample.com%2fa" "HTTP://example.com/a"]
           ["%2f%2fcdn.example.com%2fimage.jpg" "https://cdn.example.com/image.jpg"]
           ["https%3A%2F%2Fexample.com%2Fa%252Fb%3Fq%3Da%2Bb%26value%3D%2526%253D"
            "https://example.com/a%2Fb?q=a+b&value=%26%3D"]
           ["%2Farticle%3Fq%3Dtwo+words%26literal%3Da%2Bb"
            "https://base.example/article?q=two%20words&literal=a+b"]
           ["https%3A%2F%2Fexample.com%2Fa%20b" "https://example.com/a%20b"]
           ["https%3A%2F%2Fexample.com%2Fa%2520b" "https://example.com/a%20b"]
           ["part%2Fname?q=a+b%26c" "https://base.example/dir/part%2Fname?q=a+b%26c"]
           ["/part%2Fname?q=a+b%26c" "https://base.example/part%2Fname?q=a+b%26c"]
           ["https%3A%2F%2Fexample.com%2Fbad%escape"
            "https://base.example/dir/https%3A%2F%2Fexample.com%2Fbad%escape"]]]
    (testing raw
      (let [url (uut/absolutify-url raw "https://base.example/dir/page")]
        (is (= expected (str url)))
        (is (= expected (str (uut/absolutify-url (str url)))))
        (is (= expected (str (uut/absolutify-url url))))))))

(deftest absolutify-url-compatible-edge-cases-test
  (doseq [raw ["https://example.com/café"
               "https://example.com/bad%escape"
               "https://example.com/ends%"
               "#section with space"
               "mailto:a@example.com?subject=hello world"
               "data:,Hello%2C%20World!"]]
    (testing raw
      (is (= raw (with-redefs-fn {#'uut/parse-href legacy-parse-href}
                   #(str (uut/absolutify-url raw)))))
      (is (= raw (str (uut/absolutify-url raw)))))))
