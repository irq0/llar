(ns llar.events)

(defn context
  ([surface trigger]
   (context surface trigger {}))
  ([surface trigger metadata]
   {:surface surface
    :trigger trigger
    :metadata metadata}))
