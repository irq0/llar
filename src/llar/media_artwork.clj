(ns llar.media-artwork
  (:require
   [clojure.java.io :as io]
   [clojure.tools.logging :as log])
  (:import
   [com.google.common.cache Cache CacheBuilder Weigher]
   [java.awt Color Font Graphics2D RenderingHints]
   [java.awt.image BufferedImage]
   [java.io ByteArrayOutputStream]
   [javax.imageio ImageIO]))

(def ^:private max-cache-bytes (* 32 1024 1024))
(defonce ^:private ^Cache artwork-cache
  (-> (CacheBuilder/newBuilder)
      (.maximumWeight max-cache-bytes)
      (.weigher (reify Weigher
                  (weigh [_ _ bytes]
                    (alength ^bytes bytes))))
      (.build)))
(defonce ^:private background-cache (atom nil))

(defn image-to-png
  "Convert supported image bytes to PNG without changing their dimensions."
  [image-bytes]
  (let [source (ImageIO/read (io/input-stream image-bytes))
        _ (when-not source
            (throw (ex-info "ImageIO could not decode media artwork" {})))
        output (ByteArrayOutputStream.)]
    (ImageIO/write source "png" output)
    (.toByteArray output)))

(defn pad-image-png
  "Center image bytes on a dark target-sized background and return PNG bytes."
  [image-bytes width height]
  (let [source (ImageIO/read (io/input-stream image-bytes))
        _ (when-not source
            (throw (ex-info "ImageIO could not decode media artwork" {})))
        source-width (.getWidth source)
        source-height (.getHeight source)
        scale (min (/ (double width) source-width)
                   (/ (double height) source-height))
        drawn-width (int (* source-width scale))
        drawn-height (int (* source-height scale))
        x (int (/ (- width drawn-width) 2))
        y (int (/ (- height drawn-height) 2))
        image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics (.createGraphics image)
        output (ByteArrayOutputStream.)]
    (try
      (.setRenderingHint graphics RenderingHints/KEY_INTERPOLATION
                         RenderingHints/VALUE_INTERPOLATION_BILINEAR)
      (.setColor graphics (Color. 20 20 40))
      (.fillRect graphics 0 0 width height)
      (.drawImage graphics source x y drawn-width drawn-height nil)
      (finally
        (.dispose graphics)))
    (ImageIO/write image "png" output)
    (.toByteArray output)))

(defn- background-image []
  (or @background-cache
      (try
        (let [image (ImageIO/read (io/resource "podcast/nimoy-salute.jpg"))]
          (when-not image
            (throw (ex-info "ImageIO could not decode media artwork background" {})))
          (reset! background-cache image)
          image)
        (catch Exception e
          (log/warn e "media artwork: failed to load background image")
          nil))))

(defn- draw-cover-background! [^Graphics2D graphics width height]
  (if-let [source (background-image)]
    (let [source-width (.getWidth source)
          source-height (.getHeight source)
          target-ratio (/ (double width) height)
          source-ratio (/ (double source-width) source-height)
          crop-width (if (> source-ratio target-ratio)
                       (int (* source-height target-ratio))
                       source-width)
          crop-height (if (> source-ratio target-ratio)
                        source-height
                        (int (/ source-width target-ratio)))
          source-x (int (/ (- source-width crop-width) 2))
          source-y (int (/ (- source-height crop-height) 2))]
      (.drawImage graphics source
                  0 0 width height
                  source-x source-y
                  (+ source-x crop-width) (+ source-y crop-height)
                  nil))
    (do
      (.setColor graphics (Color. 20 20 40))
      (.fillRect graphics 0 0 width height))))

(defn- draw-label! [^Graphics2D graphics label width height]
  (let [label (subs (str label) 0 (min (count (str label)) 36))
        bar-height (max 90 (int (* height 0.23)))
        horizontal-padding (max 20 (int (* width 0.06)))
        max-text-width (- width (* 2 horizontal-padding))
        initial-font-size (max 24 (int (* height 0.12)))
        font-size (loop [font-size initial-font-size]
                    (.setFont graphics (Font. Font/SANS_SERIF Font/BOLD font-size))
                    (if (or (<= font-size 20)
                            (<= (.stringWidth (.getFontMetrics graphics) label)
                                max-text-width))
                      font-size
                      (recur (dec font-size))))
        baseline (- height (max 24 (int (* bar-height 0.25))))]
    (.setColor graphics (Color. 0 0 0 175))
    (.fillRect graphics 0 (- height bar-height) width bar-height)
    (.setFont graphics (Font. Font/SANS_SERIF Font/BOLD font-size))
    (.setColor graphics Color/WHITE)
    (let [metrics (.getFontMetrics graphics)
          x (max horizontal-padding
                 (int (/ (- width (.stringWidth metrics label)) 2)))]
      (.drawString graphics label x baseline))))

(defn- render-cover [label width height]
  (let [image (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        graphics (.createGraphics image)
        output (ByteArrayOutputStream.)]
    (try
      (.setRenderingHint graphics RenderingHints/KEY_INTERPOLATION
                         RenderingHints/VALUE_INTERPOLATION_BILINEAR)
      (.setRenderingHint graphics RenderingHints/KEY_ANTIALIASING
                         RenderingHints/VALUE_ANTIALIAS_ON)
      (.setRenderingHint graphics RenderingHints/KEY_TEXT_ANTIALIASING
                         RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
      (draw-cover-background! graphics width height)
      (draw-label! graphics label width height)
      (finally
        (.dispose graphics)))
    (ImageIO/write image "png" output)
    (.toByteArray output)))

(defn cover
  "Generate deterministic PNG cover bytes and retain a bounded in-memory cache."
  [label width height]
  (let [key [(str label) width height]]
    (or (.getIfPresent artwork-cache key)
        (let [bytes (render-cover (str label) width height)]
          (.put artwork-cache key bytes)
          bytes))))
