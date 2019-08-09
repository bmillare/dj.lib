;; '[[ar.com.hjg/pngj "2.1.0"]]
(ns dj.image.png
  (:require [dj.dispatch.treefn :as tf]))

(defn image-info
  ^ar.com.hjg.pngj.ImageInfo
  [width height]
  (ar.com.hjg.pngj.ImageInfo. (int width)
                              (int height)
                              (int 8)
                              false
                              false
                              true))

(defn image-line
  ^ar.com.hjg.pngj.ImageLineByte
  [^ar.com.hjg.pngj.ImageInfo image-info]
  (ar.com.hjg.pngj.ImageLineByte. image-info))

(defn indexed-png-writer
  ^ar.com.hjg.pngj.PngWriter
  [^java.io.OutputStream os
   ^ar.com.hjg.pngj.chunks.PngChunkPLTE palette
   ^ar.com.hjg.pngj.ImageInfo image-info]
  (let [w (ar.com.hjg.pngj.PngWriter. os image-info)]
    (.queueChunk (.getMetadata w) palette)
    w))

(defn make-palette
  ^ar.com.hjg.pngj.chunks.PngChunkPLTE
  [^ar.com.hjg.pngj.ImageInfo image-info
   palette-data]
  (let [palette (ar.com.hjg.pngj.chunks.PngChunkPLTE. image-info)]
    (.setNentries palette (count palette-data))
    (doall
     (map-indexed (fn [i [r g b]]
                    (.setEntry palette (int i)
                               (int (* 255 (double r)))
                               (int (* 255 (double g)))
                               (int (* 255 (double b)))))
                  palette-data))
    palette))
