(ns u1f596.http-test
  (:require
   [u1f596.http :as uut]
   [hickory.select :as s]
   [clojure.test :refer :all]
   [org.bovinegenius [exploding-fish :as uri]]
   [clojure.set :refer [union intersection]]
   [clojure.string :as string]
   [slingshot.test :refer :all]
   [mount.core :as mount]))

(def hick
  {:type :document,
   :content
   [{:type :document-type,
     :attrs {:name "html", :publicid "", :systemid ""}}
    {:type :element,
     :attrs {:lang "de"},
     :tag :html,
     :content
     [{:type :element,
       :attrs nil,
       :tag :head,
       :content
       [{:type :element,
         :attrs {:charset "utf-8"},
         :tag :meta,
         :content nil}
        "\n"
        {:type :element,
         :attrs
         {:rel "alternate",
          :type "application/rss+xml",
          :title "Text-Feed",
          :href "/rss.xml"},
         :tag :link,
         :content nil}
        {:type :element,
         :attrs
         {:rel "alternate",
          :type "application/rss+xml",
          :title "HTML-Feed",
          :href "/rss.xml?html"},
         :tag :link,
         :content nil}
        {:type :element,
         :attrs nil,
         :tag :title,
         :content ["Document Title"]}
        {:type :element,
         :attrs {:href "https://example.com/javascript.js"}
         :tag :script
         :content nil}
        {:type :element,
         :attrs {:rel "stylesheet"
                 :type "text/css"
                 :href "https://example.com/stylesheet"}
         :tag :link
         :content nil}
        "\n\n"]}
      {:type :element,
       :attrs nil,
       :tag :body,
       :content
       [{:type :element,
         :attrs nil,
         :tag :h2,
         :content
         [{:type :element,
           :attrs
           {:href "https://example.com/",
            :style "text-decoration:none;color:black"},
           :tag :a,
           :content ["Headline with inline style"]}]}
        "\n\n"
        {:type :element,
         :attrs nil,
         :tag :p,
         :content
         ["Paragraph"]}
        "\n\n"
        {:type :element,
         :attrs {:style "text-align:right"},
         :tag :p,
         :content
         ["Paragraph with style  "
          {:type :element,
           :attrs {:href "https://example.org"},
           :tag :a,
           :content ["Link"]}]}
        {:type :element,
         :attrs nil,
         :tag :p,
         :content nil}
        {:type :element,
         :attrs nil,
         :tag :h3,
         :content ["Content Headline"]}
        "\n\n"
        {:type :element,
         :attrs nil,
         :tag :ul,
         :content
         ["\n"
          {:type :element,
           :attrs nil,
           :tag :li,
           :content
           [{:type :element,
             :attrs {:href "https://example.com/content-item-link"},
             :tag :a,
             :content ["[l]"]}
            " Pellentesque dapibus suscipit ligula.  Donec posuere augue in quam.  Etiam vel tortor sodales tellus ultricies commodo.  Suspendisse potenti.  Aenean in sem ac leo mollis blandit.  Donec neque quam, dignissim in, mollis nec, sagittis eu, wisi.  Phasellus lacus.  Etiam laoreet quam sed arcu.  Phasellus at dui in ligula mollis ultricies.  Integer placerat tristique nisl.  Praesent augue.  Fusce commodo.  Vestibulum convallis, lorem a tempus semper, dui dui euismod elit, vitae placerat urna tortor vitae lacus.  Nullam libero mauris, consequat quis, varius et, dictum id, arcu.  Mauris mollis tincidunt felis.  Aliquam feugiat tellus ut neque.  Nulla facilisis, risus a rhoncus fermentum, tellus tellus lacinia purus, et dictum nunc justo sit amet elit.
"
            {:type :element,
             :attrs {:src "https://example.com/some-image.png"},
             :tag :img,
             :content [""]}
            {:type :element,
             :attrs {:src "https://blacklisted.example.com/image-from-blacklisted-domain.png"},
             :tag :img,
             :content [""]}
            {:type :element,
             :attrs nil,
             :tag :p,
             :content
             ["Lorem ipsum dolor sit amet, consectetuer adipiscing elit.  Donec hendrerit tempor tellus.  Donec pretium posuere tellus.  Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus.  Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.  Nulla posuere.  Donec vitae dolor.  Nullam tristique diam non turpis.  Cras placerat accumsan nulla.  Nullam rutrum.  Nam vestibulum accumsan nisl."]}]}]}
        "\n"
        {:type :element, :attrs nil, :tag :p, :content nil}]}]}]})

(def testing-domain-blocklist #{"blacklisted.example.com"})

(defn start-testing-http-state [f]
  (mount/start-with {#'u1f596.http/domain-blocklist (atom testing-domain-blocklist)})
  (f))

(use-fixtures :once start-testing-http-state)

(deftest sanitize
  (let [sanitized (uut/sanitize hick)
        hrefs (map #(get-in % [:attrs :href])
                   (s/select
                    (s/descendant
                     (s/tag :href))
                    sanitized))
        img-srcs (map #(get-in % [:attrs :src])
                      (s/select
                       (s/descendant
                        (s/tag :img))
                       sanitized))
        urls (remove nil? (union hrefs img-srcs))]
    (is (every? #(nil? (get-in % [:attrs :href]))
                (s/select (s/descendant (s/tag :script)) sanitized))
        "every script href should be cleared")
    (is (not-any? #(string/includes? % "blacklisted") urls)
        "contains no blacklisted link or image")))

(deftest sanitize-css
  (let [sanitized (uut/sanitize hick :remove-css? true)
        style-attrs (remove nil?
                            (map #(get-in % [:attrs :style])
                                 (s/select
                                  (s/descendant
                                   (s/attr :style))
                                  sanitized)))
        stylesheet-links (s/select
                          (s/descendant
                           (s/and
                            (s/tag :link)
                            (s/attr :rel (partial = "stylesheet"))))
                          sanitized)]
    (is (empty? style-attrs) "All style attrs cleared")
    (is (empty? stylesheet-links) "All style sheet links should be removed")))

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
    (is (thrown+? [:type :u1f596.http/absolutify-impossible
                   :reason :u1f596.http/base-url-useless]
                  (uut/absolutify-url nil "")))
    (is (thrown+? [:type :u1f596.http/absolutify-impossible
                   :reason :u1f596.http/base-url-relative]
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
           (str (uut/absolutify-url "../../baz" "https://example.com/foo/bar/")))))
  )
