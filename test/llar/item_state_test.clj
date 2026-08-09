(ns llar.item-state-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llar.item-state :as state]))

(def base
  {:id 42
   :type :item-type/link
   :tags #{:unread :research}
   :checkpoint nil})

(deftest attention-and-saved-are-independent
  (let [saved (state/transition base {:action :save})
        seen (state/transition saved {:action :seen})]
    (is (= #{:unread :saved :research} (:tags saved)))
    (is (= #{:saved :research} (:tags seen)))
    (is (= [:saved] (state/queue-reasons seen)))))

(deftest checkpoint-is-explicit-and-independent-of-saved
  (let [checkpoint {:position {:type "TextPositionSelector" :start 10 :end 20}
                    :quote {:type "TextQuoteSelector" :exact "abcdefghij"}}
        started (state/transition (assoc base :tags #{:unread :saved :research})
                                  {:action :save-checkpoint
                                   :selector checkpoint
                                   :progress 0.25})]
    (is (= #{:saved :research} (:tags started)))
    (is (= {:selector checkpoint :progress 0.25}
           (:checkpoint started)))
    (is (= [:saved :continue-reading] (state/queue-reasons started)))))

(deftest done-and-archive-have-distinct-retention
  (let [started (assoc base
                       :tags #{:unread :saved :research}
                       :checkpoint {:selector nil :progress 0.5})
        done (state/transition started {:action :done})
        archived (state/transition started {:action :archive})]
    (is (= #{:research} (:tags done)))
    (is (nil? (:checkpoint done)))
    (is (= #{:archive :research} (:tags archived)))
    (is (nil? (:checkpoint archived)))))

(deftest unread-bookmark-is-an-implicit-queue-reason
  (let [bookmark (assoc base :type :item-type/bookmark)
        dequeued (state/transition bookmark {:action :dequeue})]
    (is (= [:unread-bookmark] (state/queue-reasons bookmark)))
    (is (= #{:research} (:tags dequeued)))
    (is (empty? (state/queue-reasons dequeued)))))

(deftest archive-dominates-queue-membership
  (is (empty? (state/queue-reasons
               (assoc base
                      :tags #{:archive :saved}
                      :checkpoint {:progress 0.5})))))

(deftest attention-commands-do-not-destroy-reading-intent
  (let [started (assoc base
                       :tags #{:unread :saved}
                       :checkpoint {:progress 0.5})
        seen (state/transition started {:action :seen})
        unsaved (state/transition started {:action :unsave})
        unread-again (state/transition
                      (assoc started :tags #{:archive :saved})
                      {:action :mark-unread})]
    (is (= #{:saved} (:tags seen)))
    (is (some? (:checkpoint seen)))
    (is (= #{:unread} (:tags unsaved)))
    (is (some? (:checkpoint unsaved)))
    (is (= #{:saved :unread} (:tags unread-again)))
    (is (some? (:checkpoint unread-again)))))

(deftest clearing-a-checkpoint-is-not-done
  (let [started (assoc base
                       :tags #{:unread :saved}
                       :checkpoint {:progress 0.5})
        cleared (state/transition started {:action :clear-checkpoint})]
    (is (= #{:unread :saved} (:tags cleared)))
    (is (nil? (:checkpoint cleared)))
    (is (= [:saved] (state/queue-reasons cleared)))))

(deftest save-and-mark-unread-restore-an-archived-item
  (let [archived (assoc base :tags #{:archive :research})]
    (is (= #{:saved :research}
           (:tags (state/transition archived {:action :save}))))
    (is (= #{:unread :research}
           (:tags (state/transition archived {:action :mark-unread}))))))

(deftest differences-are-storage-agnostic
  (let [after (state/transition base {:action :save})]
    (is (= {:add-tags #{:saved}
            :remove-tags #{}
            :checkpoint-changed? false}
           (state/differences base after)))))

(deftest canonical-state-separates-workflow-state-from-item-tags
  (is (= {:unread true
          :read false
          :saved true
          :archived false
          :tags ["research" "saved" "unread"]
          :item-tags ["research"]}
         (select-keys
          (state/canonical (assoc base :tags #{:unread :saved :research}))
          [:unread :read :saved :archived :tags :item-tags]))))

(deftest invalid-actions-are-rejected-in-the-domain
  (testing "SQL never decides what an unknown action means"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Unknown item state action"
                          (state/transition base {:action :explode}))))
  (testing "reserved tags cannot bypass semantic transitions"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Workflow tags require"
                          (state/transition base {:action :add-tag :tag :saved}))))
  (testing "checkpoint bounds are domain invariants"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"between zero and one"
                          (state/transition base {:action :save-checkpoint
                                                  :selector nil
                                                  :progress 1.01})))))
