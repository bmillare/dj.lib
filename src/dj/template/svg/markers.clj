(ns dj.template.svg.markers
  (:require [dj.template.svg :as svg]
            [dj.template.sgml :as sgml]))

(def simple-arrow-id "simple-arrow")

(def simple-arrow-ref (str "url(#" simple-arrow-id ")"))

(def simple-arrow
  [:marker {:id simple-arrow-id
            :markerHeight 5
            :markerWdith 5
            :refX 2.5
            :refY 2.5
            :orient "auto"}
   [:path {:d (svg/->path-spec [[:M 0 0]
                                [:L 5 2.5]
                                [:L 0 5]
                                [:L 2.5 2.5]
                                [:Z]])
           :fill "#000"}]])

(comment
  (let [width 210
        height 297
        units "mm"]
    (->
     svg/svg
     (update 1 assoc
             "xmlns:xlink" "http://www.w3.org/1999/xlink"
             :width (str width units)
             :height (str height units)
             :viewBox (str "0 0 " width " " height))
     (into [[:defs {}
             simple-arrow]
            [:line {:x1 0
                    :y1 0
                    :x2 100
                    :y2 100
                    :stroke "#000"
                    :stroke-width 5
                    :marker-end simple-arrow-ref}]])
     vector
     sgml/emit
     (->> (dj.io/poop (dj.io/file "/home/username/tmp/test.svg"))))))
