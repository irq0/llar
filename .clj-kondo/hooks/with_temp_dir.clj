(ns hooks.with-temp-dir
  (:require [clj-kondo.hooks-api :as api]))

(defn with-temp-dir [{:keys [node]}]
  ;; (with-temp-dir dir body...) => (let [dir nil] body...)
  (let [[dir-sym & body] (next (:children node))
        new-node (api/list-node
                  [(api/token-node 'let)
                   (api/vector-node [dir-sym (api/token-node nil)])
                   (api/list-node
                    (list* (api/token-node 'do) body))])]
    {:node new-node}))
