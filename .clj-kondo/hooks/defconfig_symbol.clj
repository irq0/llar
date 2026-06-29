(ns hooks.defconfig-symbol
  (:require [clj-kondo.hooks-api :as api]))

(defn- quote-node [node]
  (api/list-node [(api/token-node 'quote) node]))

(defn defconfig-symbol [{:keys [node]}]
  (let [[_ & args] (:children node)]
    {:node (api/list-node
            (list* (api/token-node 'do)
                   (map quote-node args)))}))
