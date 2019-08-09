(ns dj.template.svg
  (:require [dj.template.sgml :as sgml]
            [clojure.string :as cs]))

(defn emit-points
  "converts point data into svg formatted point data"
  [x y]
  (cs/join " "
           (map (fn [xi yi]
                  (str xi "," yi))
                x
                y)))

(defrecord point-pairs [ps]
  sgml/IEmitForm
  (-emit-form [_]
    (cs/join " "
             (map
              (fn [[x y]]
                (str x "," y))
              ps))))

(defrecord path-spec [s]
  sgml/IEmitForm
  (-emit-form [_]
    (cs/join " "
             (for [[command & args] s]
               (str (name command) " " (cs/join "," args))))))

(defrecord points [x y]
  sgml/IEmitForm
  (-emit-form [_]
    (emit-points x y)))

;; Paths make lines with stroke-widths that are immune to scaling in
;; inkscape
(defrecord line-path [x y]
  sgml/IEmitForm
  (-emit-form [_]
    (str "M" (first x) "," (first y) " L" (emit-points (rest x) (rest y)))))

(defrecord pair-line-path [ps]
  sgml/IEmitForm
  (-emit-form [_]
    (let [[x0 y0] (first ps)]
      (str "M" x0 "," y0 " L"
           (cs/join " "
                    (for [[x y] (rest ps)]
                      (str x "," y)))))))

(defn emit-pair-sequence [s]
  (cs/join " "
           (for [[x y] (partition 2 s)]
             (str x "," y))))

;; pair sequence
(defrecord ps [s]
  sgml/IEmitForm
  (-emit-form [_]
    (emit-pair-sequence s)))

(def svg-header {:xmlns "http://www.w3.org/2000/svg"
                 :version "1.1"})
(def xlink-header {:xmlns:xlink "http://www.w3.org/1999/xlink"})
(def svg [:svg svg-header])


(defn emit-transform-seq [transform-seq]
  (cs/join " "
           (for [[op & args] transform-seq]
             (str (name op) "(" (cs/join "," args) ")"))))

;; applied right to left in SVG spec
(defrecord ts [transform-seq]
  sgml/IEmitForm
  (-emit-form [_]
    (emit-transform-seq transform-seq)))

;;; Note on style attribute
;; - you can also use dj.template.css/->style to use a map
;; [:rect {:width "300" :height "100" :style (css/->style {"fill" "black" "stroke-width" 3})}]

;;; SVG Notes
;; <svg height="image-height" width="image-width"
;;  viewBox="x0 y0 width height">
;;
;; viewBox is for redefining the local coordinate system
;;
;; x0 y0 is the coordinates of the top left corner of the viewport
;;
;; width height is for the dimensions of the viewport
;;
;; - units are rescaled on width and height, 0-width corresponds to
;;   0-image-width

;; - you can use nested svgs to have multiple coordinate systems
