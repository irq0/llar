(ns infowarss.converter
  (:require
   [clojure.java.shell :as shell]))


(defn html2text
  "Convert html to text"
  [html]
  (let [{:keys [exit out]}
        (shell/sh "w3m" "-T" "text/html" "-dump" :in html)]
    (if (zero? exit)
      out
      "")))
