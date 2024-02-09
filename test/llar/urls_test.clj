(ns llar.urls-test
  (:require
   [clojure.test :refer [are deftest is testing]]
   [org.bovinegenius [exploding-fish :as uri]]
   [slingshot.test :refer :all]
   [llar.http :as uut]))

(deftest absolutify-url
  (testing
   "Basics"
    (are [x y] (= (str x) (str y))
      "https://example.com/squeezing-cybersecurity-lemons-–-labeling-regime-iot-products"
      (uut/absolutify-url "/squeezing-cybersecurity-lemons-%E2%80%93-labeling-regime-iot-products" "https://example.com")
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

    (is (= "http://www.google.com/search?hl=en&q=2^20 * 2^12 bytes in GB"
           (str (uut/absolutify-url "http://www.google.com/search?hl=en&q=2^20+*+2^12+bytes+in+GB" nil))))
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

  (testing "Path traversing should be resolved"
    (is (= "https://example.com/baz"
           (str (uut/absolutify-url "../../baz" "https://example.com/foo/bar/"))))))
