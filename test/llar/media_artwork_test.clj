(ns llar.media-artwork-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [llar.media-artwork :as uut])
  (:import
   [java.awt Color]
   [java.awt.image BufferedImage]
   [java.io ByteArrayOutputStream]
   [javax.imageio ImageIO]))

(defn- png-bytes [width height]
  (let [image (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        graphics (.createGraphics image)
        output (ByteArrayOutputStream.)]
    (try
      (.setColor graphics Color/RED)
      (.fillRect graphics 0 0 width height)
      (finally
        (.dispose graphics)))
    (ImageIO/write image "png" output)
    (.toByteArray output)))

(defn- dimensions [bytes]
  (let [image (ImageIO/read (io/input-stream bytes))]
    [(.getWidth image) (.getHeight image)]))

(deftest image-conversion-preserves-or-targets-dimensions
  (let [source (png-bytes 160 90)]
    (testing "plain conversion preserves dimensions"
      (is (= [160 90] (dimensions (uut/image-to-png source)))))
    (testing "padded conversion uses the requested canvas"
      (is (= [300 300] (dimensions (uut/pad-image-png source 300 300)))))))

(deftest generated-covers-have-requested-dimensions
  (is (= [614 346] (dimensions (uut/cover "LLAR Media" 614 346))))
  (is (= [500 750]
         (dimensions
          (uut/cover {:title (apply str (repeat 100 "Long channel name "))
                      :subtitle "source-key"
                      :details ["100 items" "Latest Aug 2026"]}
                     500 750)))))
