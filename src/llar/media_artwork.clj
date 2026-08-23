(ns llar.media-artwork
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [com.google.common.cache Cache CacheBuilder Weigher]
   [java.awt AlphaComposite Color Font GradientPaint Graphics2D RenderingHints]
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

(defn- color-pair [seed]
  (let [hash-code (hash seed)
        hue (/ (double (bit-and hash-code 0xffff)) 65535.0)
        hue-2 (mod (+ hue 0.12) 1.0)]
    [(Color/getHSBColor (float hue) 0.68 0.36)
     (Color/getHSBColor (float hue-2) 0.82 0.16)]))

(defn- draw-cover-background! [^Graphics2D graphics seed width height]
  (let [[start end] (color-pair seed)]
    (.setPaint graphics (GradientPaint. 0.0 0.0 start
                                        (float width) (float height) end))
    (.fillRect graphics 0 0 width height)
    (.setComposite graphics (AlphaComposite/getInstance AlphaComposite/SRC_OVER 0.10))
    (.setColor graphics Color/WHITE)
    (doseq [n (range 5)]
      (let [diameter (int (* width (+ 0.18 (* n 0.09))))
            x (int (- (* width (+ 0.72 (* n 0.08))) (/ diameter 2)))
            y (int (- (* height (+ 0.12 (* n 0.17))) (/ diameter 2)))]
        (.drawOval graphics x y diameter diameter)))
    (.setComposite graphics AlphaComposite/SrcOver)))

(defn- fit-font [^Graphics2D graphics text max-width initial-size minimum-size]
  (loop [size initial-size]
    (.setFont graphics (Font. Font/SANS_SERIF Font/BOLD size))
    (if (or (<= size minimum-size)
            (<= (.stringWidth (.getFontMetrics graphics) text) max-width))
      size
      (recur (dec size)))))

(defn- fit-text [^Graphics2D graphics text max-width]
  (let [metrics (.getFontMetrics graphics)]
    (if (<= (.stringWidth metrics text) max-width)
      text
      (loop [length (dec (count text))]
        (let [candidate (str (subs text 0 (max 0 length)) "…")]
          (if (or (zero? length)
                  (<= (.stringWidth metrics candidate) max-width))
            candidate
            (recur (dec length))))))))

(defn- draw-cover-text! [^Graphics2D graphics {:keys [title subtitle details]}
                         width height]
  (let [padding (max 24 (int (* width 0.08)))
        max-width (- width (* 2 padding))
        title (subs (str title) 0 (min 72 (count (str title))))
        subtitle (some-> subtitle str str/trim not-empty)
        details (->> details (map str) (remove str/blank?) (str/join "  •  "))
        title-size (fit-font graphics title max-width
                             (max 30 (int (* height 0.11)))
                             (max 20 (int (* height 0.055))))
        title-y (int (* height 0.46))]
    (.setColor graphics (Color. 255 255 255 190))
    (.setFont graphics (Font. Font/SANS_SERIF Font/BOLD
                              (max 13 (int (* height 0.035)))))
    (.drawString graphics "LLAR MEDIA" padding (int (* height 0.14)))
    (.setColor graphics Color/WHITE)
    (.setFont graphics (Font. Font/SANS_SERIF Font/BOLD title-size))
    (.drawString graphics (fit-text graphics title max-width) padding title-y)
    (when subtitle
      (.setColor graphics (Color. 255 255 255 190))
      (.setFont graphics (Font. Font/SANS_SERIF Font/PLAIN
                                (max 16 (int (* height 0.05)))))
      (.drawString graphics
                   (fit-text graphics subtitle max-width)
                   padding
                   (+ title-y (max 28 (int (* height 0.09))))))
    (when-not (str/blank? details)
      (.setColor graphics (Color. 255 255 255 210))
      (.setFont graphics (Font. Font/SANS_SERIF Font/PLAIN
                                (max 14 (int (* height 0.04)))))
      (.drawString graphics (fit-text graphics details max-width)
                   padding (- height padding)))))

(defn- render-cover [content width height]
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
      (draw-cover-background! graphics content width height)
      (draw-cover-text! graphics content width height)
      (finally
        (.dispose graphics)))
    (ImageIO/write image "png" output)
    (.toByteArray output)))

(defn cover
  "Generate deterministic abstract PNG cover bytes with optional supporting text."
  [content width height]
  (let [content (if (map? content) content {:title (str content)})
        key [content width height]]
    (or (.getIfPresent artwork-cache key)
        (let [bytes (render-cover content width height)]
          (.put artwork-cache key bytes)
          bytes))))
