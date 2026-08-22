(ns llar.http.middleware)

(defn mark-private [response]
  (if (map? response)
    (update response :headers
            #(merge {"Cache-Control" "private"}
                    %
                    {"X-Robots-Tag" "noindex, nofollow, noarchive"
                     "Referrer-Policy" "no-referrer"}))
    response))

(defn wrap-private [handler]
  (fn [request]
    (mark-private (handler request))))
