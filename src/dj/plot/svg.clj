(ns dj.plot.svg
  (:require [dj.template.svg :as ts]
            [dj.template.css :as tc]
            [dj.plot.colormap :as cm]
            [dj.dispatch.treefn :as dt]))

(defn ticks
  [start limit step-size]
  (loop [result []
         current-tick start]
    (if (<= current-tick limit)
      (recur (conj result current-tick)
             (+ current-tick step-size))
      result)))

(defn gradient-stops [color-data]
  (let [total-count (count color-data)]
    (map-indexed (fn [n [r g b]]
                   [:stop {:offset (str (double (* 100 (/ n total-count))) "%")
                           :stop-color (tc/-emit-form (cm/->FloatRGB r g b))}])
                 color-data)))

(def gradients
  {:svg.component/svg-def-horizontal-gradient
   (dt/fm
    [:plot.input/color-map
     :plot.gradient/id]
    (into [:linearGradient (let [zp (str "0%")]
                             {:id id
                              :x1 zp
                              :x2 (str "100%")
                              :y1 zp
                              :y2 zp})]
          (gradient-stops color-map)))})

(def color-histogram-2D-fms
  {:svg.plot/color-histogram-2D
   (dt/fm
    [:svg.component/svg-def-horizontal-gradient
     :svg.component/svg-legend
     :svg.component/svg-tick-lines-y
     :svg.component/svg-y-axis-label
     :svg.component/svg-y-axis-scale-labels ; TODO:1 :new {:tags [:plotting :enhancement] :priority :low :description "how to reduce print precision to make room for y-axis-label"} 
     :svg.component/svg-2D-histogram-graph ; change just to graph
     :svg.component/svg-x-axis-scale-labels
     :svg.component/svg-tick-lines-x
     :svg.component/svg-x-axis-label
     :svg.component/svg-title-above

     :plot.style/figure-background-style
     :plot.spatial/width
     :plot.spatial/height]
    [:svg (merge
           ts/svg-header
           {:width (str width)
            :height (str height)})
     [:defs {}
      svg-def-horizontal-gradient]
     [:rect {:width width
             :height height
             :style (tc/->style figure-background-style)}]
     svg-2D-histogram-graph
     svg-legend
     svg-tick-lines-y
     svg-y-axis-label
     svg-y-axis-scale-labels
     svg-x-axis-scale-labels
     svg-tick-lines-x
     svg-x-axis-label
     svg-title-above])
   :svg.component/svg-legend
   (dt/fm
    [:plot.gradient/id
     :plot.legend/width
     :plot.legend/height
     :plot.legend/margin-bottom
     :plot.legend/text-margin-side
     :plot.legend/text-margin-bottom
     :plot.input/z-label
     :plot.scale/z-limit
     :svg.structure.fn/graph-coor->svg-coor-x
     :svg.structure.fn/graph-coor->svg-coor-y]
    (let [legend-x-center (graph-coor->svg-coor-x 0.75)
          graph-top (graph-coor->svg-coor-y 1)
          half-width (/ width 2.0)
          legend-y-center (- graph-top
                             margin-bottom
                             (/ height 2.0))]
      [:g {}
       [:text {:x (- legend-x-center
                     half-width
                     text-margin-side)
               :y legend-y-center
               :text-anchor "end"
               :alignment-baseline "middle"}
        0]
       [:rect {:x (- legend-x-center
                     half-width)
               :y (- graph-top
                     height
                     margin-bottom)
               :width width
               :height height
               :fill (str "url(#" id ")")}]
       [:text {:x legend-x-center
               :y (- graph-top
                     height
                     margin-bottom
                     text-margin-bottom)
               :text-anchor "middle"}
        z-label]
       [:text {:x (+ legend-x-center
                     half-width
                     text-margin-side)
               :y legend-y-center
               :alignment-baseline "middle"}
        z-limit]]))
   :plot.spatial/extents
   (dt/fm
    [:plot.input/data
     :plot.input/var-bin-width
     :plot.input/slope-bin-width]
    (loop [min-var Double/POSITIVE_INFINITY
           max-var Double/NEGATIVE_INFINITY
           min-slope Double/POSITIVE_INFINITY
           max-slope Double/NEGATIVE_INFINITY
           indices (keys data)]
      (let [index (first indices)]
        (if index
          (let [[v s] index]
            (recur (if (< v min-var)
                     v min-var)
                   (if (> v max-var)
                     v max-var)
                   (if (< s min-slope)
                     s min-slope)
                   (if (> s max-slope)
                     s max-slope)
                   (rest indices)))
          {:min-var (* var-bin-width min-var)
           :max-var (* var-bin-width max-var)
           :min-slope (* slope-bin-width min-slope)
           :max-slope (* slope-bin-width max-slope)}))))
   :plot.scale/y-lower-limit
   (dt/fm
    [:plot.spatial/extents]
    (:min-var extents))
   :plot.scale/y-upper-limit
   (dt/fm
    [:plot.spatial/extents
     :plot.input/var-bin-width]
    (+ var-bin-width (:max-var extents)))
   :plot.scale/x-lower-limit
   (dt/fm
    [:plot.spatial/extents]
    (:min-slope extents))
   :plot.scale/x-upper-limit
   (dt/fm
    [:plot.spatial/extents
     :plot.input/slope-bin-width]
    (+ slope-bin-width (:max-slope extents)))
   :color-map
   (dt/fm
    [:plot.scale/z-limit]
    (cm/float-interpolated-color-mapper {:clip-min 0.0
                                         :clip-max z-limit
                                         :color-vec cm/viridis-data}))
   :svg.component/svg-2D-histogram-graph
   (dt/fm
    [:color-map
     :plot.input/data
     :plot.input/var-bin-width
     :plot.input/slope-bin-width
     :plot.scale/y-lower-limit
     :plot.scale/y-upper-limit
     :plot.scale/x-lower-limit
     :plot.scale/x-upper-limit
     :plot.spatial/graph-width
     :plot.spatial/graph-height
     :svg.structure.fn/graph-coor->svg-coor-x
     :svg.structure.fn/graph-coor->svg-coor-y
     :plot.style/graph-background-style]
    (into [:g {}]
          (let [y-span (- y-upper-limit y-lower-limit)
                recep_var (* y-upper-limit
                             y-span)
                m_var (/ y-upper-limit
                         recep_var)
                b_var (/ (* (- y-lower-limit) y-upper-limit)
                         recep_var)
                x-span (- x-upper-limit x-lower-limit)
                recep_slope (* x-upper-limit
                               x-span)
                m_slope (/ x-upper-limit
                         recep_slope)
                b_slope (/ (* (- x-lower-limit) x-upper-limit)
                           recep_slope)
                width (- (graph-coor->svg-coor-x (/ slope-bin-width x-span))
                         (graph-coor->svg-coor-x 0))
                height (- (graph-coor->svg-coor-y 0)
                          (graph-coor->svg-coor-y (/ var-bin-width y-span)))]
            (for [[[v_i s_i] c] data]
              (let [v (+ (* var-bin-width v_i)
                         var-bin-width)
                    s (* slope-bin-width s_i)]
                [:rect {:x (graph-coor->svg-coor-x (+ (* m_slope s) b_slope))
                        :y (graph-coor->svg-coor-y (+ (* m_var v) b_var))
                        :fill (tc/-emit-form (color-map (Math/log c)))
                        :width width
                        :height height}])))))

   :svg.component/svg-x-axis-label
   (dt/fm
    [:plot.input/x-axis-label
     :svg.structure.fn/graph-coor->svg-coor-y
     :svg.structure.fn/graph-coor->svg-coor-x
     :plot.spatial/x-axis-margin-top
     :plot.style/x-axis-style]
    [:text {:x (graph-coor->svg-coor-x 0.5)
            :y (+ (graph-coor->svg-coor-y 0)
                  x-axis-margin-top)
            :text-anchor "middle"
            :dominant-baseline "text-before-edge"
            :style (tc/->style x-axis-style)}
     x-axis-label])
   :svg.component/svg-y-axis-label
   (dt/fm
    [:plot.input/y-axis-label
     :svg.structure.fn/graph-coor->svg-coor-y
     :svg.structure.fn/graph-coor->svg-coor-x
     :plot.spatial/y-axis-margin-right
     :plot.style/y-axis-style]
    (let [x (- (graph-coor->svg-coor-x 0)
               y-axis-margin-right)
          y (graph-coor->svg-coor-y 0.5)]
      [:text {:x x
              :y y
              :text-anchor "middle"
              :transform (str "rotate(270," x "," y ")")
              :style (tc/->style y-axis-style)}
       y-axis-label]))
   :svg.component/svg-title-above
   (dt/fm
    [:plot.input/title
     :svg.structure.fn/graph-coor->svg-coor-y
     :svg.structure.fn/graph-coor->svg-coor-x
     :plot.spatial/title-margin-below
     :plot.style/title-style]
    [:text {:x (graph-coor->svg-coor-x 0.25)
            :y (- (graph-coor->svg-coor-y 1)
                  title-margin-below)
            :text-anchor "middle"
            :style (tc/->style title-style)}
     title])
   })

(def color-histogram-2D-defaults
  {:plot.spatial/y-axis-margin-right 40
   :plot.style/y-axis-style {:font-size 15}
   :plot.spatial/x-axis-margin-top 20
   :plot.style/x-axis-style {:font-size 15}
   :plot.spatial/title-margin-below 20
   :plot.style/title-style {:font-size 20}
   :plot.spatial/margin-left 60.0
   :plot.spatial/margin-top 50
   :plot.spatial/margin-right 20
   :plot.gradient/id "legend-gradient"
   :plot.legend/width 300
   :plot.legend/height 20
   :plot.legend/margin-bottom 5
   :plot.legend/text-margin-side 5
   :plot.legend/text-margin-bottom 5
   })

(def scale-ticks
  {:svg.component/svg-tick-lines-y
   (dt/fm
    [:plot.spatial/y-tick-width
     :plot.style/y-tick-style
     :svg.structure/y-ticks-svg-coor
     :svg.structure.fn/graph-coor->svg-coor-x
     :svg.structure.fn/graph-coor->svg-coor-y]
    (let [x-zero (graph-coor->svg-coor-x 0)]
      (into [:g {}
             [:line {:x1 x-zero
                     :y1 (graph-coor->svg-coor-y 0)
                     :x2 x-zero
                     :y2 (graph-coor->svg-coor-y 1)
                     :style (tc/->style y-tick-style)}]]
            (map (let [offset (/ y-tick-width
                                 2.0)
                       x1 (- x-zero
                             offset)
                       x2 (+ x-zero
                             offset)]
                   (fn [y]
                     [:line {:x1 x1
                             :y1 y
                             :x2 x2
                             :y2 y
                             :style (tc/->style y-tick-style)}]))
                 y-ticks-svg-coor))))
   :svg.component/svg-tick-lines-x
   (dt/fm
    [:plot.spatial/x-tick-width
     :plot.style/x-tick-style
     :svg.structure/x-ticks-svg-coor
     :svg.structure.fn/graph-coor->svg-coor-x
     :svg.structure.fn/graph-coor->svg-coor-y]
    (let [y-zero (graph-coor->svg-coor-y 0)]
      (into [:g {}
             [:line {:x1 (graph-coor->svg-coor-x 0)
                     :y1 y-zero
                     :x2 (graph-coor->svg-coor-x 1)
                     :y2 y-zero
                     :style (tc/->style x-tick-style)}]]
            (map (let [offset (/ x-tick-width
                                 2.0)
                       y1 (- y-zero
                             offset)
                       y2 (+ y-zero
                             offset)]
                   (fn [x]
                     [:line {:x1 x
                             :y1 y1
                             :x2 x
                             :y2 y2
                             :style (tc/->style x-tick-style)}]))
                 x-ticks-svg-coor))))
   ;; simple zero ticks, BigDecimal recommended for arbitrary precision
   :plot.structure/y-ticks
   (dt/fm
    [:plot.scale/y-step-size
     :plot.scale/y-upper-limit
     :plot.scale/y-lower-limit]
    (ticks y-lower-limit y-upper-limit y-step-size))
   :plot.structure/x-ticks
   (dt/fm
    [:plot.scale/x-step-size
     :plot.scale/x-upper-limit
     :plot.scale/x-lower-limit]
    (ticks x-lower-limit x-upper-limit x-step-size))
   :svg.structure/y-ticks-svg-coor
   (dt/fm
    [:svg.structure.fn/graph-coor->svg-coor-y
     :plot.structure/y-ticks
     :plot.scale/y-upper-limit
     :plot.scale/y-lower-limit]
    (mapv (let [y-span (- y-upper-limit y-lower-limit)]
            (fn [tick]
              (graph-coor->svg-coor-y (/ (- tick y-lower-limit)
                                         y-span))))
          y-ticks))
   :svg.structure/x-ticks-svg-coor
   (dt/fm
    [:svg.structure.fn/graph-coor->svg-coor-x
     :plot.structure/x-ticks
     :plot.scale/x-upper-limit
     :plot.scale/x-lower-limit]
    (mapv (let [x-span (- x-upper-limit x-lower-limit)]
            (fn [tick]
              (graph-coor->svg-coor-x (/ (- tick x-lower-limit)
                                         x-span))))
          x-ticks))})

(def scale-tick-defaults
  {:plot.style/y-tick-style {:stroke "black"
                             :stroke-width 2}
   :plot.spatial/y-tick-width 5
   :plot.style/x-tick-style {:stroke "black"
                             :stroke-width 2}
   :plot.spatial/x-tick-width 5})

(def axis-scale-labels
  {:svg.component/svg-y-axis-scale-labels
   (dt/fm
    [:svg.axis/y-printer
     :svg.structure/y-ticks-svg-coor
     :plot.structure/y-ticks
     :plot.spatial/y-axis-scale-labels-margin-right
     :plot.style/y-axis-scale-labels-style
     :svg.structure.fn/graph-coor->svg-coor-x]
    (into [:g {}]
          (map (let [x (- (graph-coor->svg-coor-x 0)
                          y-axis-scale-labels-margin-right)]
                 (fn [y-tick y-tick-svg-coor]
                   [:text {:x x
                           :y y-tick-svg-coor
                           :text-anchor "end"
                           :alignment-baseline "middle"
                           :style (tc/->style y-axis-scale-labels-style)}
                    (y-printer y-tick)]))
               y-ticks
               y-ticks-svg-coor)))
   :svg.component/svg-x-axis-scale-labels
   (dt/fm
    [:svg.axis/x-printer
     :svg.structure/x-ticks-svg-coor
     :plot.structure/x-ticks
     :plot.spatial/x-axis-scale-labels-margin-top
     :plot.style/x-axis-scale-labels-style
     :svg.structure.fn/graph-coor->svg-coor-y]
    (into [:g {}]
          (map (let [y (+ (graph-coor->svg-coor-y 0)
                          x-axis-scale-labels-margin-top)]
                 (fn [x-tick x-tick-svg-coor]
                   [:text {:x x-tick-svg-coor
                           :y y
                           :text-anchor "middle"
                           :dominant-baseline "text-before-edge"
                           :style (tc/->style x-axis-scale-labels-style)}
                    (x-printer x-tick)]))
               x-ticks
               x-ticks-svg-coor)))
   })

(def axis-scale-label-defaults
  {:plot.style/y-axis-scale-labels-style {:fill (cm/->IntRGB 0 0 0)
                                          :font-size 15}
   :plot.spatial/y-axis-scale-labels-margin-right 5
   :plot.spatial/x-axis-scale-labels-margin-top 5
   :plot.style/x-axis-scale-labels-style {:fill (cm/->IntRGB 0 0 0)
                                          :font-size 15}
   :svg.axis/y-printer str #_ #(format "%.3f" %)
   :svg.axis/x-printer str})

(def color-bar-plot-fms
  {:color-map
   (dt/fm
    [:plot.scale/y-upper-limit]
    (cm/float-interpolated-color-mapper {:clip-min 0.0
                                         :clip-max y-upper-limit
                                         :color-vec cm/carp-data}))
   :plot.structure/bar-spatials
   (dt/fm
    [:color-map
     :plot.input/bar-data
     :plot.spatial/bar-width-fraction
     :plot.spatial/graph-width
     :plot.spatial/graph-height
     :plot.scale/y-upper-limit
     :svg.structure.fn/graph-coor->svg-coor-y
     :svg.structure.fn/graph-coor->svg-coor-x]
    ;; bar-width-fraction is the amount of space the bars
    ;; will take up compared to the full width of the graph

    ;; the individual bar width will be split between the
    ;; this fraction

    ;; the spacing will be split from the remaining, note
    ;; that there is always one more space more than the
    ;; number of bars
    (let [bar-data-count (count bar-data)
          bar-data-clipped (mapv (fn [y]
                                   (if (< y y-upper-limit)
                                     y
                                     y-upper-limit))
                                 bar-data)
          f (* graph-width
               bar-width-fraction)
          f| (/ f bar-data-count)
          r (- graph-width f)
          offset (/ r (inc bar-data-count))
          full-bar-width (+ f|
                            offset)
          x0 (graph-coor->svg-coor-x 0)]
      (map-indexed (fn [n v]
                     (let [normalized-v (double (/ v y-upper-limit))]
                       {:width (double f|)
                        :height (double (* normalized-v graph-height))
                        :x (double (+ x0
                                      (* n full-bar-width)
                                      offset))
                        :y (graph-coor->svg-coor-y normalized-v)
                        :style (tc/->style {:fill (color-map v)})}))
                   bar-data-clipped)))})

(def bar-plot-fms
  {:plot.spatial/graph-width
   (dt/fm
    [:plot.spatial/width
     :plot.spatial/margin-left
     :plot.spatial/margin-right]
    (- width margin-left margin-right))

   :plot.spatial/graph-height
   (dt/fm
    [:plot.spatial/height
     :plot.spatial/margin-bottom
     :plot.spatial/margin-top]
    (- height margin-top margin-bottom))

   ;; graph-coor is just 0-1
   :svg.structure.fn/graph-coor->svg-coor-x
   (dt/fm
    [:plot.spatial/margin-left
     :plot.spatial/graph-width]
    (let [m graph-width
          b margin-left]
      (fn [x]
        (+ (* m x) b))))

   :svg.structure.fn/graph-coor->svg-coor-y
   (dt/fm
    [:plot.spatial/margin-top
     :plot.spatial/graph-height]
    (let [m (- graph-height)
          b (+ margin-top
               graph-height)]
      (fn [y]
        (+ (* m y) b))))

   :svg.component/svg-grid-lines-y
   (dt/fm
    [:plot.style/grid-lines-y-style
     :svg.structure/y-ticks-svg-coor
     :svg.structure.fn/graph-coor->svg-coor-x]
    (into [:g {}]
          (map (let [x-zero (graph-coor->svg-coor-x 0)
                     x-end (graph-coor->svg-coor-x 1)]
                 (fn [y-tick-svg-coor]
                   [:line {:x1 x-zero
                           :y1 y-tick-svg-coor
                           :x2 x-end
                           :y2 y-tick-svg-coor
                           :style (tc/->style grid-lines-y-style)}]))
               y-ticks-svg-coor)))

   :svg.component/svg-graph-background
   (dt/fm
    [:plot.spatial/graph-width
     :plot.spatial/graph-height
     :svg.structure.fn/graph-coor->svg-coor-x
     :svg.structure.fn/graph-coor->svg-coor-y
     :plot.style/graph-background-style]
    [:rect {:width graph-width
            :height graph-height
            :x (graph-coor->svg-coor-x 0)
            :y (graph-coor->svg-coor-y 1)
            :style (tc/->style graph-background-style)}])

   :plot.structure/bar-spatials
   (dt/fm
    [:plot.input/bar-data
     :plot.spatial/bar-width-fraction
     :plot.spatial/graph-width
     :plot.spatial/graph-height
     :plot.scale/y-upper-limit
     :svg.structure.fn/graph-coor->svg-coor-y
     :svg.structure.fn/graph-coor->svg-coor-x]
    ;; bar-width-fraction is the amount of space the bars
    ;; will take up compared to the full width of the graph

    ;; the individual bar width will be split between the
    ;; this fraction

    ;; the spacing will be split from the remaining, note
    ;; that there is always one more space more than the
    ;; number of bars
    (let [bar-data-count (count bar-data)
          bar-data-clipped (mapv (fn [y]
                                   (if (< y y-upper-limit)
                                     y
                                     y-upper-limit))
                                 bar-data)
          f (* graph-width
               bar-width-fraction)
          f| (/ f bar-data-count)
          r (- graph-width f)
          offset (/ r (inc bar-data-count))
          full-bar-width (+ f|
                            offset)
          x0 (graph-coor->svg-coor-x 0)]
      (map-indexed (fn [n v]
                     (let [normalized-v (double (/ v y-upper-limit))]
                       {:width (double f|)
                        :height (double (* normalized-v graph-height))
                        :x (double (+ x0
                                      (* n full-bar-width)
                                      offset))
                        :y (graph-coor->svg-coor-y normalized-v)}))
                   bar-data-clipped)))
   :svg.component/svg-bars
   (dt/fm
    [:plot.structure/bar-spatials]
    (into [:g {}]
          (map (fn [spatial]
                 [:rect spatial])
               bar-spatials)))

   :svg.component/svg-bar-labels
   (dt/fm
    [:plot.structure/bar-spatials
     :plot.input/bar-labels
     :plot.spatial/bar-labels-margin-top
     :plot.style/bar-labels-style
     :svg.structure.fn/graph-coor->svg-coor-y
     :plot.style/bar-labels-style]
    (into [:g {}]
          (map (let [y (+ (graph-coor->svg-coor-y 0)
                          bar-labels-margin-top)]
                 (fn [{:keys [x width]} label]
                   [:text {:x (+ x (/ width 2.0))
                           :y y
                           :text-anchor "middle"
                           :dominant-baseline "text-before-edge"
                           :style (tc/->style bar-labels-style)}
                    label]))
               bar-spatials
               bar-labels)))
   :svg.component/svg-title
   (dt/fm
    [:plot.input/title
     :svg.structure.fn/graph-coor->svg-coor-y
     :svg.structure.fn/graph-coor->svg-coor-x
     :plot.spatial/title-margin-top
     :plot.style/title-style]
    [:text {:x (graph-coor->svg-coor-x 0.5)
            :y (+ (graph-coor->svg-coor-y 0)
                  title-margin-top)
            :text-anchor "middle"
            :dominant-baseline "text-before-edge"
            :style (tc/->style title-style)}
     title])

   :svg.plot/bar-plot
   (dt/fm
    [:svg.component/svg-grid-lines-y
     :svg.component/svg-y-axis-scale-labels
     :svg.component/svg-graph-background
     :svg.component/svg-bars
     :svg.component/svg-bar-labels
     :svg.component/svg-title

     :plot.style/figure-background-style
     :plot.spatial/width
     :plot.spatial/height]
    [:svg (merge
           ts/svg-header
           {:width (str width)
            :height (str height)})
     [:rect {:width width
             :height height
             :style (tc/->style figure-background-style)}]
     svg-graph-background
     svg-grid-lines-y
     svg-y-axis-scale-labels
     svg-bars
     svg-bar-labels
     svg-title])})

(def bar-plot-defaults
  (let [black (cm/->IntRGB 0 0 0)]
    {:plot.style/grid-lines-y-style {:stroke (cm/->IntRGB 60 60 60)
                                     :stroke-width 2}
     :plot.style/graph-background-style {:fill (cm/->IntRGB 200 200 200)}
     :plot.style/figure-background-style {:fill (cm/->IntRGB 200 200 255)}

     :plot.spatial/bar-labels-margin-top 5
     :plot.style/bar-labels-style {:fill black
                                   :font-size 15}
     :plot.spatial/bar-width-fraction 0.75

     :plot.spatial/title-margin-top 20
     :plot.style/title-style {:font-size 20}

     :plot.scale/y-lower-limit 0.0

     :plot.spatial/width 600
     :plot.spatial/height 300
     :plot.spatial/margin-bottom 50
     :plot.spatial/margin-top 10
     :plot.spatial/margin-left 50
     :plot.spatial/margin-right 10}))

(def line-plot-fms "point-sequence is seq of xy pairs ie [x y]"
  {:svg.component/svg-line-fn
   (dt/fm
    [:svg.structure.fn/graph-coor->svg-coor-x
     :svg.structure.fn/graph-coor->svg-coor-y
     :plot.scale/x-upper-limit
     :plot.scale/y-upper-limit]
    (fn [point-sequence]
      [:path {:d (dj.template.svg/->pair-line-path (map (fn [[x y]]
                                                           [(graph-coor->svg-coor-x (double
                                                                                     (/ x
                                                                                        x-upper-limit)))
                                                            (graph-coor->svg-coor-y (double
                                                                                     (/ y
                                                                                        y-upper-limit)))])
                                                         point-sequence))}]))})

(def line-plot-fms-split "instead of taking a seq of pairs, takes two seqs that correspond to x and y"
  {:svg.component/svg-line-fn
   (dt/fm
    [:svg.structure.fn/graph-coor->svg-coor-x
     :svg.structure.fn/graph-coor->svg-coor-y
     :plot.scale/x-upper-limit
     :plot.scale/x-lower-limit
     :plot.scale/y-upper-limit
     :plot.scale/y-lower-limit]
    (fn [xs ys]
      [:path {:d (dj.template.svg/->pair-line-path (map (fn [x y]
                                                           [(graph-coor->svg-coor-x (double
                                                                                     (let [x-range (- x-upper-limit
                                                                                                      x-lower-limit)]
                                                                                       (/ (- x x-lower-limit)
                                                                                          x-range))))
                                                            (graph-coor->svg-coor-y (double
                                                                                     (let [y-range (- y-upper-limit
                                                                                                      y-lower-limit)]
                                                                                       (/ (- y y-lower-limit)
                                                                                          y-range))))])
                                                         xs
                                                         ys))}]))})

(defn local-date 
  (^java.time.LocalDate [[year month day]]
   (java.time.LocalDate/of (int year) (int month) (int day)))
  (^java.time.LocalDate [year month day]
   (java.time.LocalDate/of (int year) (int month) (int day))))

(defn date-range
  [^java.time.LocalDate start ^java.time.LocalDate end days]
  (loop [accum []
         dx start]
    (if (pos? (.compareTo dx end))
      accum
      (recur (conj accum dx)
             (.plusDays dx (long days))))))

(defn local-date->v [^java.time.LocalDate d]
  [(.getYear d) (.getMonthValue d) (.getDayOfMonth d)])

(defn days-between [^java.time.LocalDate start ^java.time.LocalDate end]
  (.between java.time.temporal.ChronoUnit/DAYS start end))

(def date-range-fms
  {:plot.structure/x-date-ticks
   (dt/fm
    [:plot.input/start-date
     :plot.input/end-date
     :plot.input/days-per-tick]
    (date-range start-date
                end-date
                days-per-tick))
   :plot.scale/x-step-size
   (dt/fm
    [:plot.input/days-per-tick]
    days-per-tick)
   :plot.scale/x-upper-limit
   (dt/fm
    [:plot.input/start-date
     :plot.input/end-date
     :plot.input/days-per-tick]
    #_(+ days-per-tick (days-between start-date end-date))
    (days-between start-date end-date))
   :svg.component/svg-x-axis-scale-labels
   (dt/fm
    [:svg.axis/x-printer
     :svg.structure/x-ticks-svg-coor
     :plot.structure/x-date-ticks
     :plot.spatial/x-axis-scale-labels-margin-top
     :plot.style/x-axis-scale-labels-style
     :svg.structure.fn/graph-coor->svg-coor-y]
    (into [:g {}]
          (map (let [y (+ (graph-coor->svg-coor-y 0)
                          x-axis-scale-labels-margin-top)]
                 (fn [x-date-tick x-tick-svg-coor]
                   [:text {:x (double x-tick-svg-coor)
                           :y y
                           :text-anchor "middle"
                           :dominant-baseline "text-before-edge"
                           :style (tc/->style x-axis-scale-labels-style)}
                    (x-printer x-date-tick)]))
               x-date-ticks
               x-ticks-svg-coor)))})

(def date-range-defaults
  {:plot.scale/x-lower-limit 0
   :svg.axis/x-printer (fn [^java.time.LocalDate date]
                         (str (.getMonthValue date) "/" (.getDayOfMonth date)))})

(def lift-history-fms
  {:plot.input/all-dates
   (dt/fm
    [:data/lift-history]
    (sort (keys lift-history)))
   :plot.input/start-date
   (dt/fm
    [:plot.input/all-dates]
    (local-date (first all-dates)))
   :plot.input/end-date
   (dt/fm
    [:plot.input/all-dates]
    (local-date (last all-dates)))
   :plot.input/main-movement-history
   (dt/fm
    [:plot.input.criteria/movement-id
     :plot.input.criteria/quantity-selector
     :plot.input/start-date
     :plot.input/end-date
     :plot.input/all-dates
     :data/lift-history]
    (reduce (fn [ret k]
              (let [m (lift-history k)]
                (if (contains? m movement-id)
                  (into ret
                        (map (fn [s]
                               [(days-between start-date (local-date k))
                                (quantity-selector s)]))
                        (movement-id m))
                  ret)))
            []
            all-dates))
   :plot.input/weight-history
   (dt/fm
    [:plot.input/start-date
     :plot.input/end-date
     :plot.input/all-dates
     :data/lift-history]
    (let [body-weight-id :body-weight] (reduce (fn [ret k]
                      (let [m (lift-history k)]
                        (if (contains? m body-weight-id)
                          (conj ret
                                (let [w (body-weight-id m)]
                                  [(days-between start-date (local-date k))
                                   w]))
                          ret)))
                    []
                    all-dates)))
   :svg.plot/lift-history-plot
   (dt/fm
    [:svg.component/svg-grid-lines-y
     :svg.component/svg-graph-background
     :svg.component/svg-y-axis-scale-labels
     :svg.component/svg-x-axis-scale-labels
     :svg.component/svg-line-fn
     :plot.input/main-movement-history
     :plot.input/weight-history
     
     :plot.style/figure-background-style
     :plot.spatial/width
     :plot.spatial/height]
    [:svg (merge
           ts/svg-header
           {:width (str width)
            :height (str height)})
     [:rect {:width width
             :height height
             :style (tc/->style figure-background-style)}]
     svg-graph-background
     svg-grid-lines-y
     svg-y-axis-scale-labels
     svg-x-axis-scale-labels
     (assoc-in (svg-line-fn main-movement-history)
               [1 :style]
               (dj.template.css/->style {"fill" "none"
                                          "stroke-width" 3
                                          "stroke" "#f00"}))
     (assoc-in (svg-line-fn weight-history)
               [1 :style ]
               (dj.template.css/->style {"fill" "none"
                                          "stroke-width" 3
                                          "stroke" "#00f"}))])})

(def basic-xy-plot-fms
  {:svg.plot/xy-plot
   (dt/fm
    [:svg.component/svg-graph-background
     :svg.component/svg-grid-lines-y
     :svg.component/svg-y-axis-scale-labels
     :svg.component/svg-x-axis-scale-labels
     
     :plot.style/figure-background-style
     :plot.spatial/width
     :plot.spatial/height]
    [:svg (merge
           ts/svg-header
           {:width (str width)
            :height (str height)})
     [:rect {:width width
             :height height
             :style (tc/->style figure-background-style)}]
     svg-graph-background
     svg-grid-lines-y
     svg-y-axis-scale-labels
     svg-x-axis-scale-labels])})
