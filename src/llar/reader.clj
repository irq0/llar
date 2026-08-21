(ns llar.reader
  (:require
   [clojure.string :as string]))

(def bookmark-context
  {:group-name :type
   :group-item :bookmark
   :source-key :all})

(def archive-context
  {:group-name :item-tags
   :group-item :archive
   :source-key :all})

(defn source-context [source-key]
  {:group-name :default
   :group-item :all
   :source-key (or source-key :all)})

(defn group-path
  [{:keys [group-name group-item source-key]}]
  ["/reader/group"
   (name (or group-name :default))
   (name (or group-item :all))
   "source"
   (name (or source-key :all))])

(defn items-path [context]
  (conj (group-path context) "items"))

(defn item-path [context item-id]
  (conj (group-path context) "item/by-id" item-id))

(defn item-action-path [context item-id action]
  (conj (item-path context item-id) (name action)))

(defn update-path [context]
  (conj (group-path context) "update"))

(defn tool-path [view]
  ["/reader/tools" (name view)])

(defn tool-action-path [view action]
  (conj (tool-path view) (name action)))

(defn short-item-action-path [item-id action]
  ["/reader/item/by-id" item-id (name action)])

(defn group-action-path [group-prefix action]
  [group-prefix (name action)])

(defn prefixed-item-path
  ([group-prefix item-id]
   [group-prefix "item/by-id" item-id])
  ([group-prefix item-id action]
   [group-prefix "item/by-id" item-id (name action)]))

(defn bookmark-item-path [item-id]
  (item-path bookmark-context item-id))

(defn path-string [path]
  (string/join "/" path))

(defn absolute-url [base-url path]
  (str (string/replace base-url #"/+$" "") (path-string path)))
