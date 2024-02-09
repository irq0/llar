(ns llar.http-test
  (:require
   [clojure.set :refer [union]]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [hickory.select :as s]
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
