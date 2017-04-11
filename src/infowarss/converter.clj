(ns infowarss.converter
  (:require
   [clojure.java.shell :as shell]))


(defn html2text [html]
  "Convert html to text"
  (let [{:keys [exit out]}
        (shell/sh "w3m" "-T" "text/html" "-dump" :in html)]
    (if (= exit 0)
      out
      "")))
