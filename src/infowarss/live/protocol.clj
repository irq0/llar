(defprotocol LiveSource
  "Protocol to work with live sources"
  (start-collecting! [src chan])
  (status? [src])
  (stop-collecting! [src]))

;;; Utilities

(s/defn make-meta :- schema/Metadata
  "Make meta entry from source and optional initial tags"
  [src :- s/Any]
  {:source src
   :source-name (str src)
   :source-key :unkown  ; get added later by postprocessing
   :app "infowarss"
   :ns (str *ns*)
   :fetch-ts (time/now)
   :tags #{}
   :version 1})
