(ns llar.http-test
  (:require
   [clojure.set :refer [union]]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [hickory.select :as s]
   [clj-http.client]
   [llar.appconfig :as appconfig]
   [llar.http :as uut]))

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
            " Pellentesque dapibus suscipit ligula.  Donec posuere augue in quam.  Etiam vel tortor sodales tellus ultricies commodo.  Suspendisse potenti.  Aenean in sem ac leo mollis blandit.  Donec neque quam, dignissim in, mollis nec, sagittis eu, wisi.  Phasellus lacus.  Etiam laoreet quam sed arcu.  Phasellus at dui in ligula mollis ultricies.  Integer placerat tristique nisl.  Praesent augue.  Fusce commodo.  Vestibulum convallis, lorem a tempus semper, dui dui euismod elit, vitae placerat urna tortor vitae lacus.  Nullam libero mauris, consequat quis, various et, dictum id, arcu.  Mauris mollis tincidunt felis.  Aliquam feugiat tellus ut neque.  Nulla facilisis, risus a rhoncus fermentum, tellus tellus lacinia purus, et dictum nunc justo sit amet elit.
"
            {:type :element,
             :attrs {:src "https://example.com/some-image.png"},
             :tag :img,
             :content [""]}
            {:type :element,
             :attrs {:src "https://blocklisted.example.com/image-from-blocklisted-domain.png"},
             :tag :img,
             :content [""]}
            {:type :element,
             :attrs nil,
             :tag :p,
             :content
             ["Lorem ipsum dolor sit amet, consectetuer adipiscing elit.  Donec hendrerit tempor tellus.  Donec pretium posuere tellus.  Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus.  Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculous mus.  Nulla posuere.  Donec vitae dolor.  Nullam tristique diam non turpis.  Cras placerat accumsan nulla.  Nullam rutrum.  Name vestibulum accumsan nisl."]}]}]}
        "\n"
        {:type :element, :attrs nil, :tag :p, :content nil}]}]}]})

(def testing-domain-blocklist #{"blocklisted.example.com"})

(deftest sanitize
  (binding [llar.http/*domain-blocklist* testing-domain-blocklist]
    (let [sanitized (uut/sanitize hick)
          hrefs (->> (s/select (s/descendant (s/tag :href)) sanitized)
                     (map #(get-in % [:attrs :href]))
                     (hash-set))
          img-srcs (->> (s/select (s/descendant (s/tag :img)) sanitized)
                        (map #(get-in % [:attrs :src]))
                        (hash-set))
          urls (remove nil? (union hrefs img-srcs))]
      (is (every? #(nil? (get-in % [:attrs :href]))
                  (s/select (s/descendant (s/tag :script)) sanitized))
          "every script href should be cleared")
      (is (not-any? #(string/includes? % "blocklisted") urls)
          "contains no blocklisted link or image"))))

(deftest sanitize-external-link-privacy-attrs
  (let [sanitized (uut/sanitize hick)
        link (first (s/select (s/descendant
                               (s/and
                                (s/tag :a)
                                (s/attr :href #(= "https://example.com/" %))))
                              sanitized))
        attrs (:attrs link)]
    (is (= "no-referrer" (:referrerpolicy attrs)))
    (is (string/includes? (:rel attrs) "noopener"))
    (is (string/includes? (:rel attrs) "noreferrer"))))

(deftest absolutify-links-strips-trackers
  (let [doc {:type :document
             :content [{:type :element
                        :tag :html
                        :attrs nil
                        :content [{:type :element
                                   :tag :body
                                   :attrs nil
                                   :content [{:type :element
                                              :tag :a
                                              :attrs {:href "/article?utm_source=x&id=1&fbclid=y"}
                                              :content ["link"]}
                                             {:type :element
                                              :tag :img
                                              :attrs {:src "/img.png?utm_medium=x&v=1"
                                                      :srcset "/small.png?utm_campaign=x&w=1 1x, /big.png?fbclid=y&w=2 2x"}
                                              :content []}]}]}]}
        rewritten (uut/absolutify-links-in-hick doc
                                                (uut/get-base-url
                                                 (#'uut/parse-url "https://example.com/base/page.html")))
        href (-> (s/select (s/descendant (s/tag :a)) rewritten) first :attrs :href)
        img (-> (s/select (s/descendant (s/tag :img)) rewritten) first :attrs)]
    (is (= "https://example.com/article?id=1" href))
    (is (= "https://example.com/img.png?v=1" (:src img)))
    (is (= "https://example.com/small.png?w=1 1x, https://example.com/big.png?w=2 2x"
           (:srcset img)))))

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
    (is (empty? style-attrs) "all style attrs cleared")
    (is (empty? stylesheet-links) "all style sheet links should be removed")))

;; Property-based tests

(def simple-hick-doc
  {:type :document
   :content
   [{:type :element
     :attrs nil
     :tag :html
     :content
     [{:type :element :attrs nil :tag :head :content []}
      {:type :element
       :attrs nil
       :tag :body
       :content
       [{:type :element
         :attrs {:href "https://example.com/link"}
         :tag :a
         :content ["Link text"]}
        {:type :element
         :attrs {:src "https://example.com/img.png"}
         :tag :img
         :content []}]}]}]})

(defspec sanitize-is-idempotent 20
  ;; Running sanitize twice should produce the same result as running it once
  (prop/for-all [_ gen/boolean]
                (let [once (uut/sanitize simple-hick-doc)
                      twice (uut/sanitize once)]
                  (= once twice))))

(deftest fetch-rejects-oversized-body
  (with-redefs [appconfig/appconfig {:http {:max-body-bytes 4}}
                clj-http.client/get (fn [& _]
                                      {:status 200
                                       :headers {"Content-Length" "5"}
                                       :body "12345"})]
    (try
      (uut/fetch "https://example.com" :sanitize? false :blobify? false)
      (is false "expected oversized response to throw")
      (catch clojure.lang.ExceptionInfo ex
        (is (= :llar.http/request-error (:type (ex-data ex))))
        (is (= :body-too-large (:reason-class (ex-data ex))))))))

(deftest fetch-bounds-streams-without-content-length
  (with-redefs [appconfig/appconfig {:http {:max-body-bytes 4}}
                clj-http.client/get (fn [& _]
                                      {:status 200
                                       :headers {}
                                       :body (java.io.ByteArrayInputStream.
                                              (.getBytes "12345"))})]
    (try
      (uut/fetch "https://example.com" :sanitize? false :blobify? false)
      (is false "expected oversized stream to throw")
      (catch clojure.lang.ExceptionInfo ex
        (is (= :llar.http/request-error (:type (ex-data ex))))
        (is (= :body-too-large (:reason-class (ex-data ex))))))))

(deftest bounded-stream-preserves-response-charset
  (with-redefs [appconfig/appconfig {:http {:max-body-bytes 32}}
                clj-http.client/get (fn [& _]
                                      {:status 200
                                       :headers {"Content-Type" "text/html; charset=ISO-8859-1"}
                                       :body (java.io.ByteArrayInputStream.
                                              (byte-array [(unchecked-byte 0xE4)]))})]
    (is (= "ä" (get-in (uut/fetch "https://example.com"
                                  :sanitize? false
                                  :blobify? false)
                       [:raw :body])))))

(deftest body-read-io-error-is-classified
  (with-redefs [appconfig/appconfig {:http {:max-body-bytes 1024}}
                clj-http.client/get (fn [& _]
                                      {:status 200
                                       :headers {}
                                       :body (proxy [java.io.InputStream] []
                                               (read
                                                 ([] (throw (java.io.IOException. "boom")))
                                                 ([_b] (throw (java.io.IOException. "boom")))
                                                 ([_b _off _len] (throw (java.io.IOException. "boom")))))})]
    (try
      (uut/fetch "https://example.com" :sanitize? false :blobify? false)
      (is false "expected mid-download IO error to be classified")
      (catch clojure.lang.ExceptionInfo ex
        (is (= :llar.http/server-error-retry-later (:type (ex-data ex))))
        (is (= :io (:reason-class (ex-data ex))))))))
