(ns llar.value-inspector
  "Bounded, Clojure-first value presentation for the web inspector.

  The browser receives display nodes rather than arbitrary JSON values. This
  keeps Clojure collection shapes, typed map keys, tagged values and JVM class
  information intact while ensuring lazy or very large values are never fully
  realized just to render an administration page."
  (:require
   [cheshire.core :as cheshire]
   [hiccup2.core :as h]
   [org.bovinegenius.exploding-fish])
  (:import
   [java.io Writer]
   [java.time Duration Instant OffsetDateTime ZonedDateTime]
   [java.time.temporal TemporalAccessor]
   [java.util Date]
   [java.util.regex Pattern]
   [org.bovinegenius.exploding_fish Uri]))

(def default-limits
  {:max-depth 10
   :max-children 100
   :max-nodes 5000
   :max-string-length 20000
   :max-printed-length 2000
   :max-total-printed-length 250000})

(defn- runtime-type [value]
  (when (some? value)
    (.getName (class value))))

(defn- bounded-string [value limit]
  (if (<= (count value) limit)
    {:value value :truncated? false}
    {:value (str (subs value 0 limit) "…") :truncated? true}))

(defn- safe-pr-str [value limit]
  (let [buffer (StringBuilder.)
        truncated? (atom false)
        limit-reached (Object.)
        append-text! (fn [text]
                       (let [available (- limit (.length buffer))
                             accepted (max 0 (min available (count text)))]
                         (when (pos? accepted)
                           (.append buffer (subs text 0 accepted)))
                         (when (< accepted (count text))
                           (reset! truncated? true)
                           (throw (ex-info "print limit reached"
                                           {:type limit-reached})))))
        writer (proxy [Writer] []
                 (write
                   ([value]
                    (append-text! (if (number? value)
                                    (str (char value))
                                    (str value))))
                   ([chars offset length]
                    (append-text! (String. ^chars chars offset length))))
                 (flush [])
                 (close []))]
    (try
      (binding [*out* writer
                *print-length* 100
                *print-level* 12
                *print-meta* false]
        (pr value))
      {:value (str buffer (when @truncated? "…"))
       :truncated? @truncated?}
      (catch clojure.lang.ExceptionInfo exception
        (if (identical? limit-reached (:type (ex-data exception)))
          {:value (str buffer "…") :truncated? true}
          {:value (str "#<print-error " (or (runtime-type value) "nil")
                       ": " (or (ex-message exception) "unknown error") ">")
           :truncated? false
           :print-error? true}))
      (catch StackOverflowError _
        {:value (str "#<print-error " (or (runtime-type value) "nil")
                     ": stack overflow>")
         :truncated? false
         :print-error? true})
      (catch Exception exception
        {:value (str "#<print-error " (or (runtime-type value) "nil")
                     ": " (or (ex-message exception) "unknown error") ">")
         :truncated? false
         :print-error? true}))))

(defn- semantic-type [value]
  (cond
    (nil? value) :nil
    (keyword? value) :keyword
    (symbol? value) :symbol
    (string? value) :string
    (char? value) :character
    (boolean? value) :boolean
    (ratio? value) :ratio
    (integer? value) :integer
    (number? value) :number
    (uuid? value) :uuid
    (instance? Pattern value) :regex
    (fn? value) :function
    :else :object))

(defn- count-info [value]
  (when (counted? value)
    (try
      (count value)
      (catch Throwable _ nil))))

(defn- collection-shape [value]
  (cond
    (record? value) {:kind "map"
                     :open (str "#" (.getName (class value)) "{")
                     :close "}"
                     :semantic-type "record"}
    (map? value) {:kind "map" :open "{" :close "}" :semantic-type "map"}
    (set? value) {:kind "collection" :open "#{" :close "}" :semantic-type "set"}
    (vector? value) {:kind "collection" :open "[" :close "]" :semantic-type "vector"}
    (list? value) {:kind "collection" :open "(" :close ")" :semantic-type "list"}
    (sequential? value) {:kind "collection" :open "(" :close ")" :semantic-type "sequence"}
    (coll? value) {:kind "collection" :open "(" :close ")" :semantic-type "collection"}))

(defn- exception-context? [value]
  (= "llar.converter.ExceptionContext" (runtime-type value)))

(defmulti scalar-presentation
  "Return a readable tagged scalar presentation for a JVM value, or nil to
  use Clojure's ordinary printer. Applications can extend this multimethod for
  their own opaque value classes without changing the browser renderer."
  class)

(defmethod scalar-presentation :default [_] nil)

(defmethod scalar-presentation ZonedDateTime [value]
  {:semantic-type :zoned-date-time
   :printed (str "#java.time/zoned-date-time " (pr-str (str value)))})

(defmethod scalar-presentation OffsetDateTime [value]
  {:semantic-type :offset-date-time
   :printed (str "#java.time/offset-date-time " (pr-str (str value)))})

(defmethod scalar-presentation Instant [value]
  {:semantic-type :instant
   :printed (str "#inst " (pr-str (str value)))})

(defmethod scalar-presentation Date [value]
  {:semantic-type :instant
   :printed (str "#inst " (pr-str (str (.toInstant value))))})

(defmethod scalar-presentation Duration [value]
  {:semantic-type :duration
   :printed (str "#duration " (pr-str (str value)))})

(defmethod scalar-presentation TemporalAccessor [value]
  {:semantic-type :temporal
   :printed (str "#" (runtime-type value) " " (pr-str (str value)))})

(defmethod scalar-presentation Uri [value]
  {:semantic-type :uri
   :printed (str "#uri " (pr-str (str value)))})

(defmethod scalar-presentation java.net.URI [value]
  {:semantic-type :uri
   :printed (str "#uri " (pr-str (str value)))})

(defmethod scalar-presentation java.net.URL [value]
  {:semantic-type :uri
   :printed (str "#url " (pr-str (str value)))})

(declare present*)

(defn- take-bounded [value limit]
  (let [sample (vec (take (inc limit) value))]
    {:items (if (> (count sample) limit) (pop sample) sample)
     :more? (> (count sample) limit)}))

(defn- metadata-node [value context depth]
  (when (instance? clojure.lang.IMeta value)
    (let [metadata (meta value)]
      (when (seq metadata)
        (present* metadata context (inc depth))))))

(defn- claim-printed [{:keys [remaining-characters]} printed-result]
  (let [available (max 0 @remaining-characters)
        printed (:value printed-result)
        exhausted? (> (count printed) available)
        accepted (min available (count printed))
        claimed (if exhausted?
                  (if (pos? available)
                    (str (subs printed 0 (dec available)) "…")
                    "")
                  printed)]
    (swap! remaining-characters - accepted)
    (assoc printed-result
           :value claimed
           :truncated? (or (:truncated? printed-result) exhausted?)
           :character-budget-exhausted? exhausted?)))

(defn- scalar-node [value {:keys [limits] :as context}]
  (let [{:keys [max-string-length max-printed-length]} limits
        known (scalar-presentation value)
        string-result (when (string? value) (bounded-string value max-string-length))
        printed-result (claim-printed
                        context
                        (cond
                          known
                          (bounded-string (:printed known) max-printed-length)

                          string-result
                          (assoc (safe-pr-str (:value string-result)
                                              (+ max-string-length 2))
                                 :source-truncated? (:truncated? string-result))

                          :else
                          (safe-pr-str value max-printed-length)))
        truncated? (or (:truncated? printed-result)
                       (:source-truncated? printed-result))]
    (cond-> {:kind "scalar"
             :semantic-type (name (or (:semantic-type known)
                                      (semantic-type value)))
             :runtime-type (runtime-type value)
             :printed (:value printed-result)}
      truncated?
      (assoc :truncated true
             :truncation-reason (cond
                                  (:character-budget-exhausted? printed-result) "characters"
                                  (:source-truncated? printed-result) "string-length"
                                  :else "printed-length"))

      (string? value)
      (assoc :length (count value)
             :captured-length (min (count value) max-string-length))

      (:print-error? printed-result)
      (assoc :print-error true))))

(defn- present-collection-children [items map-value? context depth]
  (loop [remaining-items (seq items)
         children []]
    (if-let [item (first remaining-items)]
      (if (> @(get context :remaining) @(get context :reserved-nodes))
        (if map-value?
          (let [[key child] item
                key-node (present* key context (inc depth))
                child-node (present* child context (inc depth))]
            (if (and key-node child-node)
              (recur (next remaining-items)
                     (conj children {:key key-node :value child-node}))
              {:children children :budget-exhausted? true}))
          (if-let [child-node (present* item context (inc depth))]
            (recur (next remaining-items) (conj children child-node))
            {:children children :budget-exhausted? true}))
        {:children children :budget-exhausted? true})
      {:children children :budget-exhausted? false})))

(defn- collection-node [value context depth]
  (let [{:keys [limits]} context
        {:keys [max-depth max-children]} limits
        shape (collection-shape value)
        total-count (count-info value)]
    (if (>= depth max-depth)
      (merge shape
             {:runtime-type (runtime-type value)
              :count total-count
              :count-known (some? total-count)
              :children []
              :truncated true
              :truncation-reason "depth"})
      (let [available-nodes (- @(get context :remaining)
                               @(get context :reserved-nodes))
            sample-limit (max 0 (min max-children available-nodes))
            {:keys [items more?]} (take-bounded (seq value) sample-limit)
            {:keys [children budget-exhausted?]}
            (present-collection-children items (map? value) context depth)
            displayed-count (count children)
            omitted (when total-count (max 0 (- total-count displayed-count)))
            metadata-present? (and (instance? clojure.lang.IMeta value)
                                   (seq (meta value)))
            meta-value (when-not budget-exhausted?
                         (metadata-node value context depth))
            metadata-omitted? (and metadata-present? (nil? meta-value))
            node-limited? (and more? (< sample-limit max-children))
            child-truncated? (or more?
                                 budget-exhausted?
                                 metadata-omitted?
                                 (and omitted (pos? omitted)))]
        (cond-> (merge shape
                       {:runtime-type (runtime-type value)
                        :count total-count
                        :count-known (some? total-count)
                        :children children})
          child-truncated?
          (assoc :truncated true
                 :omitted-count omitted
                 :truncation-reason (if (or budget-exhausted?
                                            metadata-omitted?
                                            node-limited?)
                                      "nodes"
                                      "children"))

          meta-value
          (assoc :metadata meta-value))))))

(defn- exception-context-node [value context depth]
  (let [display-value (-> value
                          (select-keys [:message :data :cause])
                          (assoc :note "redacted - see exception details"))]
    (assoc (collection-node display-value context depth)
           :open "#llar-exception-context{"
           :semantic-type "record"
           :runtime-type (runtime-type value))))

(defn- present* [value {:keys [remaining reserved-nodes] :as context} depth]
  (when (> @remaining @reserved-nodes)
    (swap! remaining dec)
    (cond
      (exception-context? value) (exception-context-node value context depth)
      (coll? value) (collection-node value context depth)
      :else (scalar-node value context))))

(defn- presentation-context [options minimum-nodes]
  (let [limits (merge default-limits options)]
    {:limits limits
     :remaining (atom (max minimum-nodes (:max-nodes limits)))
     :reserved-nodes (atom 0)
     :remaining-characters
     (atom (max 1 (:max-total-printed-length limits)))}))

(defn present
  "Convert one Clojure/JVM value into a bounded display-node tree."
  ([value]
   (present value {}))
  ([value options]
   (present* value (presentation-context options 1) 0)))

(defn payload
  "Prepare named root values for one browser inspector."
  ([roots]
   (payload roots {}))
  ([roots options]
   (let [context (presentation-context options (count roots))
         roots-left (atom (count roots))]
     {:version 1
      :show-types true
      :roots (mapv (fn [[id label value]]
                     (reset! (:reserved-nodes context) (swap! roots-left dec))
                     {:id (name id)
                      :label label
                      :node (present* value context 0)})
                   roots)})))

(defn- safe-json [value]
  (-> (cheshire/generate-string value)
      (.replace "&" "\\u0026")
      (.replace "<" "\\u003c")
      (.replace ">" "\\u003e")
      (.replace "\u2028" "\\u2028")
      (.replace "\u2029" "\\u2029")))

(defn value-inspector
  "Render a mount point containing named `[id label value]` roots.

  `:variant :compact` renders the value directly without the inspector toolbar;
  the remaining options are presentation limits."
  ([roots]
   (value-inspector roots {}))
  ([roots options]
   (let [variant (:variant options)
         data (payload roots (dissoc options :variant))]
     [:div (cond-> {:class "clojure-value-inspector"
                    :data-clojure-value-inspector "true"}
             variant
             (assoc :data-clojure-inspector-variant (name variant)))
      [:script {:type "application/json"
                :class "clojure-value-inspector-payload"}
       (h/raw (safe-json data))]])))
