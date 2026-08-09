(ns llar.item-state
  (:require
   [clojure.set :as set]))

(def workflow-tags
  "Reserved workflow tags, including the retired in-progress compatibility tag."
  #{:unread :saved :archive :in-progress})

(def semantic-actions
  #{:seen :mark-unread :save :unsave :done :archive :unarchive :dequeue})

(def actions
  (into semantic-actions
        #{:save-checkpoint :clear-checkpoint :add-tag :remove-tag}))

(defn tag-set [item]
  (into #{}
        (map (fn [tag]
               (cond
                 (keyword? tag) tag
                 (string? tag) (keyword tag)
                 :else tag)))
        (:tags item)))

(defn bookmark? [item]
  (= :item-type/bookmark (:type item)))

(defn checkpoint [item]
  (let [nested (:checkpoint item)
        progress (if (contains? item :checkpoint-progress)
                   (:checkpoint-progress item)
                   (:progress nested))]
    (when (some? progress)
      {:selector (if (contains? item :checkpoint-selector)
                   (:checkpoint-selector item)
                   (:selector nested))
       :progress (double progress)})))

(defn state [item]
  {:id (:id item)
   :type (:type item)
   :tags (tag-set item)
   :checkpoint (checkpoint item)})

(defn queue-reasons [item]
  (let [{:keys [tags checkpoint] :as item} (state item)]
    (if (contains? tags :archive)
      []
      (cond-> []
        (contains? tags :saved) (conj :saved)
        checkpoint (conj :continue-reading)
        (and (bookmark? item) (contains? tags :unread))
        (conj :unread-bookmark)))))

(defn queued? [item]
  (boolean (seq (queue-reasons item))))

(defn- ensure-command [{:keys [action tag progress] :as command}]
  (when-not (contains? actions action)
    (throw (ex-info "Unknown item state action"
                    {:type ::unknown-action :action action})))
  (when (and (#{:add-tag :remove-tag} action)
             (or (not (keyword? tag)) (contains? workflow-tags tag)))
    (throw (ex-info "Workflow tags require a semantic item state action"
                    {:type ::reserved-tag :action action :tag tag})))
  (when (and (= :save-checkpoint action)
             (or (not (number? progress))
                 (not (Double/isFinite (double progress)))
                 (not (<= 0.0 (double progress) 1.0))))
    (throw (ex-info "Checkpoint progress must be between zero and one"
                    {:type ::invalid-progress :progress progress})))
  command)

(defn transition
  "Pure item-state reducer. It contains every user-facing transition rule;
  persistence code only stores the returned tags and checkpoint."
  [item command]
  (let [{:keys [action tag selector progress]} (ensure-command command)
        {:keys [type tags] :as before} (state item)
        without #(apply disj tags %)]
    (case action
      :seen (assoc before :tags (disj tags :unread))
      :mark-unread (assoc before :tags (-> tags (disj :archive) (conj :unread)))
      :save (assoc before :tags (-> tags (disj :archive) (conj :saved)))
      :unsave (assoc before :tags (disj tags :saved))
      :done (-> before
                (assoc :tags (without [:unread :saved :in-progress]))
                (assoc :checkpoint nil))
      :archive (-> before
                   (assoc :tags (conj (without [:unread :saved :archive :in-progress])
                                      :archive))
                   (assoc :checkpoint nil))
      :unarchive (assoc before :tags (disj tags :archive))
      :dequeue (-> before
                   (assoc :tags (cond-> (without [:saved :in-progress])
                                  (= type :item-type/bookmark) (disj :unread)))
                   (assoc :checkpoint nil))
      :save-checkpoint
      (-> before
          (assoc :tags (without [:unread :archive :in-progress]))
          (assoc :checkpoint {:selector selector
                              :progress (double progress)}))
      :clear-checkpoint (assoc before :checkpoint nil)
      :add-tag (assoc before :tags (conj tags tag))
      :remove-tag (assoc before :tags (disj tags tag)))))

(defn differences [before after]
  {:add-tags (set/difference (:tags after) (:tags before))
   :remove-tags (set/difference (:tags before) (:tags after))
   :checkpoint-changed? (not= (:checkpoint before) (:checkpoint after))})

(defn canonical [item]
  (let [{:keys [id type tags checkpoint] :as item} (state item)
        reasons (queue-reasons item)]
    {:id id
     :type type
     :unread (contains? tags :unread)
     :saved (contains? tags :saved)
     :archived (contains? tags :archive)
     :checkpoint checkpoint
     :queued (boolean (seq reasons))
     :queue-reasons reasons
     :tags (->> tags (map name) sort vec)}))
