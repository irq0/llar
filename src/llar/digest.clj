(ns llar.digest
  "Digest magazines: collect :digest-tagged items into an EPUB and email it to an
  e-reader's email-to-device address (Kindle, PocketBook, Onyx Boox, ...) on a
  schedule. The EPUB is a standard ebook, so any reader works (sideload if it has
  no email-to-device service).

  State machine lives entirely in tags (no state file):
    :digest             -> queued to be sent
    :digest-issue-<N>   -> sent in issue N (the EPUB title is \"LLAR Digest #N\")
  On send we add :digest-issue-N and remove :digest, so the selection query
  (:with-tag :digest) naturally excludes already-sent items. The next issue
  number is derived from the highest existing :digest-issue-* tag.

  Read-state (clearing :unread) is handled issue-windowed by the autoread
  scheduler in llar.update; see remove-unread-for-items-with-tag!."
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [java-time.api :as time]
   [llar.appconfig :as appconfig]
   [llar.blobstore :as blobstore]
   [llar.email :as email]
   [llar.item :as item]
   [llar.persistency :as persistency]
   [llar.sched :refer [defsched]]
   [llar.store :as store])
  (:import
   [io.documentnode.epub4j.domain Author Book Date Resource TOCReference]
   [io.documentnode.epub4j.epub EpubWriter]
   [java.io ByteArrayOutputStream File FileOutputStream]
   [org.jsoup Jsoup]
   [org.jsoup.nodes Document Document$OutputSettings$Syntax Element]))

;;;; Issue numbering (derived from tags, no state file)

(def ^:private issue-tag-prefix "digest-issue-")

(defn issue-tag
  "The item tag marking inclusion in issue n, e.g. :digest-issue-7"
  [n]
  (keyword (str issue-tag-prefix n)))

(defn parse-issue-number
  "Return the integer issue number encoded in a :digest-issue-N tag, or nil."
  [tag]
  (let [s (name tag)]
    (when (string/starts-with? s issue-tag-prefix)
      (parse-long (subs s (count issue-tag-prefix))))))

(defn current-max-issue
  "Highest existing digest issue number across all tags (0 if none)."
  []
  (->> (persistency/get-tags store/backend-db)
       (keep parse-issue-number)
       (reduce max 0)))

(defn next-issue-number []
  (inc (current-max-issue)))

(defn issues-outside-window
  "Existing issue numbers that fall outside the most-recent keep-recent issues.
  Used by the autoread scheduler to clear :unread on older issues."
  [keep-recent]
  (let [issues (->> (persistency/get-tags store/backend-db)
                    (keep parse-issue-number))
        cutoff (- (reduce max 0 issues) keep-recent)]
    (filter #(<= % cutoff) issues)))

;;;; Item selection

(defn select-items
  "Items queued for the next digest: tagged :digest, oldest first so the issue
  reads chronologically. Each item is loaded via get-item-by-id so it carries the
  assembled :data content tree (get-items-recent does not build it)."
  []
  (->> (persistency/get-items-recent
        store/backend-db
        {:with-tag :digest
         :sort-order :oldest
         :limit (or (appconfig/digest :limit) 200)})
       (map :id)
       (map #(persistency/get-item-by-id store/backend-db %))))

;;;; EPUB rendering

(def ^:private max-image-bytes (* 5 1024 1024))

(defn- image-extension [content-type url]
  (or (case (some-> content-type (string/split #";") first string/trim string/lower-case)
        "image/jpeg" "jpg"
        "image/jpg" "jpg"
        "image/png" "png"
        "image/gif" "gif"
        "image/webp" "webp"
        "image/svg+xml" "svg"
        nil)
      (second (re-find #"\.(jpe?g|png|gif|webp|svg)(?:\?|$)" (str url)))
      "jpg"))

(defn- blob-hash
  "Content hash for an <img> src that references the local blobstore
  (\"/blob/<hash>\", possibly made absolute by a base URL), or nil otherwise."
  [src]
  (second (re-find #"/blob/([0-9a-fA-F]+)" (str src))))

(defn- inline-images!
  "Embed locally-blobstored <img> images into book as EPUB resources and rewrite
  the src to the local href. Images are NEVER downloaded: by fetch time item HTML
  has its images rewritten to the blobstore (see llar.http/blobify), so we read
  the bytes straight from the blobstore. Any <img> that is not a blobstore
  reference, or whose blob is missing or too large, is left untouched."
  [^Document doc ^Book book counter]
  (doseq [^Element img (.select doc "img")]
    (when-let [hash (blob-hash (.attr img "src"))]
      (try
        (let [{:keys [data mime-type size]} (blobstore/get-blob hash)]
          (when (and data (<= (or size 0) max-image-bytes))
            (let [^bytes body (with-open [^java.io.InputStream in data] (.readAllBytes in))
                  href (str "images/img-" (swap! counter inc) "."
                            (image-extension mime-type hash))]
              (.add (.getResources book) (Resource. body href))
              (.attr img "src" href))))
        (catch Exception e
          (log/debugf "digest: failed to embed blob image %s: %s" hash (ex-message e)))))))

(defn llar-item-url
  "Absolute URL to view the item in the LLAR reader, or nil if no reader
  base-url is configured."
  [item-id]
  (when-let [base (appconfig/reader :base-url)]
    (str base "/reader/group/default/none/source/all/item/by-id/" item-id "/")))

(defn- esc
  "Minimal XML text escaping."
  [s]
  (-> (str s)
      (string/replace "&" "&amp;")
      (string/replace "<" "&lt;")
      (string/replace ">" "&gt;")))

(defn- clean-text
  "Strip any HTML tags from s and escape the result for embedding as XML text."
  [s]
  (esc (.text ^Document (Jsoup/parse (str s)))))

(defn- source-label [item]
  (if-let [sk (:source-key item)] (name sk) "unknown"))

(defn- pluralize [n what]
  (str n " " what (when (not= 1 n) "s")))

(defn- xhtml-resource
  "Wrap body-html into a clean XHTML Resource at href. doc-title-html must already
  be escaped. Inlines <img> resources into book when inline? is true."
  [href doc-title-html body-html base-url book counter inline?]
  (let [^Document doc (Jsoup/parse (str "<!DOCTYPE html><html><head><meta charset=\"utf-8\"/><title>"
                                        doc-title-html "</title></head><body>"
                                        body-html "</body></html>")
                                   (or base-url ""))]
    (when inline?
      (inline-images! doc book counter))
    (-> (.outputSettings doc) (.syntax Document$OutputSettings$Syntax/xml) (.charset "UTF-8"))
    (-> (.selectFirst doc "html") (.attr "xmlns" "http://www.w3.org/1999/xhtml"))
    (Resource. (.getBytes (.outerHtml doc) "UTF-8") href)))

(defn- chapter-resource
  "Build one XHTML chapter Resource for item at href, inlining images when enabled."
  [book counter href item inline-images?]
  (let [{:keys [id title author entry ts]} item
        url (:url entry)
        llar-url (llar-item-url id)
        {:keys [estimate]} (item/reading-time-estimate item)
        links (remove nil?
                      [(when url (str "<a href=\"" (esc url) "\">Original</a>"))
                       (when llar-url (str "<a href=\"" (esc llar-url) "\">Open in LLAR</a>"))])
        body (item/best-content item)
        body-html (cond
                    (nil? body) "<p><em>(no content)</em></p>"
                    (= "text/plain" (:mime body)) (str "<pre style=\"white-space: pre-wrap\">"
                                                       (clean-text (:data body))
                                                       "</pre>")
                    :else (:data body))
        header (str "<h1>" (clean-text (or title "Untitled")) "</h1>"
                    "<p style=\"color:#666;font-size:0.9em\">"
                    (string/join " &middot; "
                                 (remove string/blank?
                                         [(some-> author str esc)
                                          (esc (source-label item))
                                          (some->> ts (time/format "yyyy-MM-dd HH:mm"))
                                          (when (pos? estimate) (str estimate " min read"))]))
                    (when (seq links)
                      (str "<br/>" (string/join " &middot; " links)))
                    "</p><hr/>")]
    (xhtml-resource href (clean-text (or title "Untitled"))
                    (str header body-html) url book counter inline-images?)))

(defn- group-with-hrefs
  "Group items by source (in first-seen order), assigning each item a chapter
  href, escaped title and reading estimate. Returns an ordered seq of
  [source-label [{:item :href :title :estimate} ...]]."
  [items]
  (let [labelled (map (fn [it] [(source-label it) it]) items)
        order (distinct (map first labelled))
        by-src (group-by first labelled)
        c (atom 0)]
    (mapv (fn [label]
            [label (mapv (fn [[_ item]]
                           {:item item
                            :href (str "chapter-" (swap! c inc) ".xhtml")
                            :title (clean-text (or (:title item) "Untitled"))
                            :estimate (:estimate (item/reading-time-estimate item))})
                         (by-src label))])
          order)))

(defn- cover-resource
  "Cover / table-of-contents page: title, date, article count and a per-source
  list of links to each chapter."
  [issue-n date n-articles groups book counter]
  (let [toc (apply str
                   (for [[label entries] groups]
                     (str "<h3>" (esc label) "</h3><ol>"
                          (apply str
                                 (for [{:keys [href title estimate]} entries]
                                   (str "<li><a href=\"" href "\">" title "</a>"
                                        (when (pos? estimate)
                                          (str " <span style=\"color:#888\">" estimate " min</span>"))
                                        "</li>")))
                          "</ol>")))
        body (str "<h1>LLAR Digest #" issue-n "</h1>"
                  "<p style=\"color:#666\">" (esc date) " &middot; " (pluralize n-articles "article") "</p>"
                  "<h2>Contents</h2>" toc)]
    (xhtml-resource "digest-cover.xhtml" (str "LLAR Digest #" issue-n) body nil book counter false)))

(defn- divider-resource
  "Section divider page for a source group."
  [idx label n book counter]
  (xhtml-resource (str "section-" idx ".xhtml") (esc label)
                  (str "<h1>" (esc label) "</h1><p style=\"color:#666\">" (pluralize n "article") "</p>")
                  nil book counter false))

(defn render-epub!
  "Render items into an EPUB titled \"LLAR Digest #N\", with a cover/TOC page and
  chapters grouped by source. Returns a temp File. opts: {:inline-images? bool}."
  [issue-n items {:keys [inline-images?] :or {inline-images? true}}]
  (let [book (Book.)
        md (.getMetadata book)
        counter (atom 0)
        date (time/format "yyyy-MM-dd" (time/local-date))
        groups (group-with-hrefs items)]
    (.addTitle md (str "LLAR Digest #" issue-n))
    (.addAuthor md (Author. "LLAR" "Digest"))
    (.addDate md (Date. date))
    (.addSection book "Contents" (cover-resource issue-n date (count items) groups book counter))
    (doseq [[idx [label entries]] (map-indexed vector groups)]
      (let [^TOCReference parent (.addSection book label
                                              (divider-resource idx label (count entries) book counter))]
        (doseq [{:keys [item href title]} entries]
          (.addSection book parent title (chapter-resource book counter href item inline-images?)))))
    (let [tmp (File/createTempFile (str "llar-digest-" issue-n "-") ".epub")
          baos (ByteArrayOutputStream.)]
      (.write (EpubWriter.) book baos)
      (with-open [out (FileOutputStream. tmp)]
        (.write out (.toByteArray baos)))
      (log/infof "digest #%d rendered: %d items in %d sources, %d bytes"
                 issue-n (count items) (count groups) (.length tmp))
      tmp)))

;;;; Delivery

(defn send-digest!
  "Email the rendered EPUB to the configured e-reader address via the general
  mail transport. :from defaults to the :mail config but can be overridden
  per-digest (Kindle requires the From to be an approved sender)."
  [issue-n ^File epub-file]
  (email/send-message!
   (cond-> {:to (appconfig/digest :to)
            :subject (str "LLAR Digest #" issue-n)
            :body [{:type "text/plain"
                    :content (str "LLAR Digest #" issue-n " attached.")}
                   {:type :attachment
                    :content epub-file
                    :file-name (str "LLAR Digest #" issue-n ".epub")
                    :content-type "application/epub+zip"}]}
     (appconfig/digest :from) (assoc :from (appconfig/digest :from)))))

;;;; Orchestration

(defn run-digest!
  "Build and send the next digest issue from queued :digest items. Returns the
  issue number sent, or nil if there was nothing to send / not configured."
  []
  (cond
    (not (appconfig/digest)) (log/info "digest: not configured, skipping")
    (not (email/configured?)) (log/warn "digest: no :mail config, skipping")
    :else
    (let [items (select-items)]
      (if (empty? items)
        (log/info "digest: no :digest items queued, skipping")
        (let [n (next-issue-number)
              ^File epub (render-epub! n items {:inline-images? (get (appconfig/digest) :inline-images? true)})]
          (send-digest! n epub)
          (doseq [{:keys [id]} items]
            (persistency/item-set-tags! store/backend-db id [(issue-tag n)])
            (persistency/item-remove-tags! store/backend-db id [:digest]))
          (.delete epub)
          (log/infof "digest #%d sent to %s: %d items"
                     n (appconfig/digest :to) (count items))
          n)))))

;;;; Scheduler

(defsched digest-scheduler (or (appconfig/digest :schedule) :sundays)
  (when (appconfig/digest)
    (run-digest!)))
