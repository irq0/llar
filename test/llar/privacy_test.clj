(ns llar.privacy-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llar.privacy :as uut]))

(deftest strip-tracking-params
  (testing "removes common tracking params and preserves useful URL parts"
    (is (= "https://example.com/path?a=1&b=2#frag"
           (uut/strip-tracking-params
            "https://example.com/path?utm_source=news&a=1&fbclid=x&b=2#frag"))))

  (testing "removes all utm params without leaving a blank query"
    (is (= "https://example.com/path"
           (uut/strip-tracking-params
            "https://example.com/path?utm_medium=email&utm_campaign=x"))))

  (testing "leaves non-http URLs untouched"
    (is (= "mailto:test@example.com?utm_source=x"
           (uut/strip-tracking-params "mailto:test@example.com?utm_source=x"))))

  (testing "is idempotent"
    (let [url "https://example.com/path?a=1&utm_source=x&b=2"]
      (is (= (uut/strip-tracking-params url)
             (uut/strip-tracking-params (uut/strip-tracking-params url)))))))

(deftest secure-link-attrs
  (is (= {:href "https://example.com"
          :rel "external noopener noreferrer"
          :referrerpolicy "no-referrer"}
         (uut/secure-link-attrs {:href "https://example.com"
                                 :rel "external"})))
  (is (= {:href "/local" :rel "tag"}
         (uut/secure-link-attrs {:href "/local" :rel "tag"}))))
