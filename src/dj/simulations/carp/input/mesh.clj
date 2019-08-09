(ns dj.simulations.carp.input.mesh
  (:require [dj]
            [clojure.java.io :as io]))

;;; Mesh generation

;; May want to split this into abstract mesh modeling, and the carp specific formats, simulation.input.carp

(defn into-text [x]
  (apply str (apply concat (interpose "\n" x))))

(defn coordinates-to-idx-2d
  "given coordinates and counts, return idx in pts file"
  [[i j] [x y]]
  (+ i (* j (inc x))))

(defn regular-mesh-2d [sizes h shape fiber-orientation point-classifier]
  (let [[x y :as counts] (map #(quot % h) sizes)
	start (map #(quot (- %) 2) sizes)
	points-count (apply * (map inc counts))
	points (into-text (list* [points-count]
				 (for [j (range (inc y))
				       i (range (inc x))]
				   (interpose " " (concat (map #(+ %1 (* %2 h)) start [i j])
							  [0])))))
	idx-to-um (fn [idx] (map + start (map #(* h %) idx)))
	type-classifier (fn [indexes]
			  (apply dj/plurality
				 (map #(point-classifier (idx-to-um %))
				      indexes)))
	elements-fn (case shape
			  "Qd" (fn [i j]
				 (let [corners [[i j]
						[(inc i) j]
						[(inc i) (inc j)]
						[i (inc j)]]]
				   (interpose " " (concat ["Qd"]
							  (map #(coordinates-to-idx-2d % counts) corners)
							  [(type-classifier corners)]))))
			  "Tr" (fn [i j]
				 (let [corners-1 [[i j]
						  [(inc i) j]
						  [(inc i) (inc j)]]
				       corners-2 [[i j]
						  [(inc i) (inc j)]
						  [i (inc j)]]]
				   (concat (interpose " " (concat ["Tr"]
								  (map #(coordinates-to-idx-2d % counts) corners-1)
								  [(type-classifier corners-1)]))
					   ["\n"]
					   (interpose " " (concat ["Tr"]
								  (map #(coordinates-to-idx-2d % counts) corners-2)
								  [(type-classifier corners-2)]))))))
	elements-count (* (case shape "Qd" 1 "Tr" 2) (apply * counts))
	elements (into-text (list* [elements-count]
				   (for [j (range y)
					 i (range x)]
				     (elements-fn i j))))
	lon (into-text (for [x (range elements-count)]
			 (interpose " " fiber-orientation)))
	tris elements]
    {:points points
     :elements elements
     :lon lon
     :tris tris}))

(defn euler-distance [x0 y0 x1 y1]
  (Math/pow (+ (Math/pow (- x0 x1) 2)
	       (Math/pow (- y0 y1) 2))
	    1/2))

(defn single-sink-classifier [radius position]
  (let [[x0 y0] position]
    (fn [[x y]]
      (if (<= (euler-distance x y x0 y0) radius)
	2
	1))))

(defn double-sink-classifier [radius-1 position-1 radius-2 position-2]
  (let [[x1 y1] position-1
	[x2 y2] position-2]
    (fn [[x y]]
      (if (or (<= (euler-distance x y x1 y1) radius-1)
	      (<= (euler-distance x y x2 y2) radius-2))
	2
	1))))

(defn parse-mesh-pts-file
  "efficient file based pts parser, uses readers under the hood to ensure memory usage is minimized

  possible better implementation using java.nio"
  [f]
  (with-open [r (io/reader f)]
    (let [[specified-size & rest] (line-seq r)
          pts (into [] (map (fn [^String line]
                              (mapv #(Double/parseDouble %) (.split #"\s+" line))))
                    rest)
          counted-size (count pts)]
      (if (= counted-size
             (Integer/parseInt specified-size))
        pts
        (throw (ex-info "specified size does not match counted size"
                        {:specified-size specified-size
                         :counted-size counted-size}))))))

(defn parse-mesh-elem-file
  "efficient file based elem parser, uses readers under the hood to ensure memory usage is minimized

  possible better implementation using java.nio"
  [f]
  (with-open [r (io/reader f)]
    (let [[specified-size & rest] (line-seq r)
          pts (into [] (map (fn [^String line]
                              (let [[t idx1 idx2 idx3 idx4] (.split #"\s+" line)]
                                [t
                                 (Integer/parseInt idx1)
                                 (Integer/parseInt idx2)
                                 (Integer/parseInt idx3)
                                 (Integer/parseInt idx4)])))
                    rest)
          counted-size (count pts)]
      (if (= counted-size
             (Integer/parseInt specified-size))
        pts
        (throw (ex-info "specified size does not match counted size"
                        {:specified-size specified-size
                         :counted-size counted-size}))))))
