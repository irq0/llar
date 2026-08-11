(ns llar.fetch.demo
  "Deterministic, network-free editorial fixtures for demos and UI development."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [hiccup2.core :refer [html]]
   [java-time.api :as time]
   [llar.analysis :as analysis]
   [llar.fetch :refer [FetchSource item-to-string make-item-hash make-meta]]
   [llar.item]
   [llar.persistency :refer [CouchItem]]
   [llar.postproc :refer [ItemProcessor]])
  (:import
   (java.util Locale Random)
   (net.datafaker Faker)))

(def ^:private content-version "1")

(def ^:private publication-order
  [:signal-wire :field-notes :common-ground :after-hours])

(def ^:private publication-indices
  (zipmap publication-order (range)))

(def ^:private publication-details
  {:signal-wire {:name "Signal Wire"
                 :seed-offset 101
                 :section "Systems"}
   :field-notes {:name "Field Notes"
                 :seed-offset 211
                 :section "Practice"}
   :common-ground {:name "Common Ground"
                   :seed-offset 307
                   :section "Public Life"}
   :after-hours {:name "After Hours"
                 :seed-offset 419
                 :section "Culture"}})

(def ^:private stories
  [{:slug "local-first"
    :tag :local-first
    :terms ["local-first" "sync" "ownership" "offline"]
    :lead "Local-first software is moving data ownership, synchronization, and offline work from implementation details into the product itself."
    :quote "A tool earns trust when the network can disappear without taking the work with it."
    :titles ["Local-first tools are turning sync into a product decision"
             "Designing for ownership when the network disappears"
             "Community archives rediscover the offline web"
             "The software that keeps working after the cloud goes quiet"]}
   {:slug "cooler-cities"
    :tag :climate
    :terms ["shade" "heat" "streets" "climate"]
    :lead "Cities are treating shade, cooler streets, and shared climate infrastructure as everyday public services rather than emergency measures."
    :quote "The most useful climate technology this summer may simply be a place to sit out of the sun."
    :titles ["A street-level sensor network maps the city’s missing shade"
             "Designing cooler blocks one awning at a time"
             "Public shade is becoming essential climate infrastructure"
             "The midnight walk revealing where the city still holds heat"]}
   {:slug "repair-culture"
    :tag :repair
    :terms ["repair" "hardware" "workshop" "maintenance"]
    :lead "Repair groups are combining open hardware, patient maintenance, and neighborhood workshops to keep useful objects in circulation."
    :quote "Maintenance is not the opposite of invention; it is invention stretched across time."
    :titles ["Open hardware turns repair manuals into living documents"
             "What product teams can learn from a neighborhood repair table"
             "The workshop making maintenance a shared civic skill"
             "A quiet Saturday with the people who refuse to throw things away"]}
   {:slug "library-networks"
    :tag :libraries
    :terms ["libraries" "archives" "community" "knowledge"]
    :lead "Libraries and small archives are building resilient community knowledge networks around lending, preservation, and careful discovery."
    :quote "An archive becomes alive when people can find themselves inside it."
    :titles ["Libraries prototype a slower, more durable discovery network"
             "The interface lessons hidden in a well-used card catalog"
             "Community archives are becoming neighborhood infrastructure"
             "After closing time, the library’s smallest collection comes alive"]}
   {:slug "calm-interfaces"
    :tag :attention
    :terms ["attention" "interfaces" "calm" "reading"]
    :lead "Calm interfaces are giving readers more control over attention by making state visible and reducing pressure to keep scrolling."
    :quote "Good reading software should remember your place without demanding your presence."
    :titles ["The case for software that knows when to become quiet"
             "Designing a reading queue that does not feel like a debt"
             "What calm interfaces return to public attention"
             "In praise of the button that waits until tomorrow"]}
   {:slug "small-models"
    :tag :machine-learning
    :terms ["models" "local" "research" "energy"]
    :lead "Smaller machine-learning models are opening practical research paths where local control, modest energy use, and inspectable behavior matter."
    :quote "The interesting benchmark is not always scale; sometimes it is whether a model fits where the work happens."
    :titles ["Small models find a useful edge outside the data center"
             "Designing research tools around local machine learning"
             "Shared labs test lower-energy models for public work"
             "The tiny model running all night beneath a workbench"]}
   {:slug "night-trains"
    :tag :mobility
    :terms ["trains" "night" "routes" "travel"]
    :lead "A renewed network of night trains is changing how travelers understand distance, time, and the shape of a useful route."
    :quote "The journey feels different when the timetable includes a full night’s sleep."
    :archived? true
    :titles ["A new night-train map treats time as part of the route"
             "Designing the overnight journey from platform to pillow"
             "Night trains reconnect regions the flight map leaves apart"
             "Notes from the last carriage before sunrise"]}
   {:slug "handmade-web"
    :tag :independent-web
    :terms ["web" "handmade" "publishing" "links"]
    :lead "Small handmade websites are renewing a culture of personal publishing built around durable links, distinct voices, and human-scale collections."
    :quote "A website can be a place again, not merely a delivery mechanism."
    :archived? true
    :titles ["The handmade web returns with better tools and fewer metrics"
             "What a personal homepage can teach us about durable design"
             "Small publishers rebuild a web of deliberate links"
             "An evening spent wandering through websites made by hand"]}])

(defrecord DemoItem [meta summary hash entry]
  Object
  (toString [item] (item-to-string item)))

(defn- now []
  (time/zoned-date-time))

(defn- publication-index [publication]
  (get publication-indices publication))

(defn- story-time [base-time publication index archived?]
  (if archived?
    (time/minus base-time (time/days (if (= index 6) 45 120)))
    (time/minus base-time
                (time/minutes (+ 12
                                 (* index 47)
                                 (* (publication-index publication) 7))))))

(defn- faker-paragraphs [^Faker faker]
  (vec (repeatedly 3 #(-> faker .lorem (.paragraph 4)))))

(defn- story-text [{:keys [lead quote terms]} paragraphs]
  (string/join
   "\n\n"
   (concat [lead]
           paragraphs
           [quote
            (str "The reporting follows " (string/join ", " terms)
                 " and the people putting those ideas into practice.")])))

(defn- story-html [{:keys [lead quote terms]} publication author paragraphs]
  (str
   (html
    [:article
     [:p {:class "lead"} lead]
     (for [paragraph paragraphs]
       [:p paragraph])
     [:blockquote
      [:p quote]]
     [:h2 "What to watch next"]
     [:p "The next phase will be shaped by ordinary use, careful maintenance, and whether the work remains understandable to the people relying on it."]
     [:ul
      (for [term terms]
        [:li (string/capitalize term)])]
     [:p [:em "Filed by " author " for " (:name publication) "."]]])))

(defn- initial-tags [story index]
  (cond-> #{:demo (:tag story)}
    (= index 1) (conj :highlight)
    (= index 2) (conj :saved :in-progress)
    (= index 4) (conj :saved)
    (:archived? story) (conj :archive)))

(defn- make-demo-item [src ^Faker faker base-time index story]
  (let [{:keys [publication]} src
        publication (get publication-details publication)
        title (nth (:titles story) (publication-index (:publication src)))
        author (-> faker .name .fullName)
        paragraphs (faker-paragraphs faker)
        text (story-text story paragraphs)
        timestamp (story-time base-time (:publication src) index (:archived? story))
        entry {:pub-ts timestamp
               :url (str "https://demo.llar.dev/" (name (:publication src)) "/" (:slug story))
               :title title
               :authors [author]
               :language :en
               :section (:section publication)
               :lead-image-url (str "/static/demo/" (:slug story) ".svg")
               :descriptions {"text/plain" (:lead story)}
               :contents {"text/html" (story-html story publication author paragraphs)
                          "text/plain" text}}
        meta (update (make-meta src) :tags into (initial-tags story index))]
    (->DemoItem meta
                {:ts timestamp :title title}
                (make-item-hash "llar-demo" content-version
                                (name (:publication src)) (:slug story))
                entry)))

(extend-protocol FetchSource
  llar.src.Demo
  (fetch-source [src _conditional-tokens]
    (let [{:keys [publication args]} src
          {:keys [count seed]} args
          publication (get publication-details publication)
          faker (Faker. Locale/ENGLISH
                        (Random. (long (+ seed (:seed-offset publication)))))
          base-time (now)]
      (mapv (partial make-demo-item src faker base-time)
            (range count)
            (take count stories)))))

(extend-protocol ItemProcessor
  DemoItem
  (post-process-item [item _src _state]
    (let [item (update item :entry merge (analysis/analyze-entry (:entry item)))]
      (if (contains? (get-in item [:meta :tags]) :archive)
        (update-in item [:meta :tags] disj :unread)
        item)))
  (filter-item [_item _src _state] false))

(extend-protocol CouchItem
  DemoItem
  (to-couch [item]
    (-> item
        (assoc :type :link)
        (assoc-in [:meta :source :args] nil))))

(s/fdef make-demo-item
  :args (s/cat :src #(instance? llar.src.Demo %)
               :faker #(instance? Faker %)
               :base-time :irq0-fetch-item/ts
               :index nat-int?
               :story map?)
  :ret #(instance? DemoItem %))
