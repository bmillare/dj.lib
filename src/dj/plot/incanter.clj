(ns dj.plot.incanter
  (:require [incanter.charts :as ic]))

(defn chart->svg [^org.jfree.chart.JFreeChart chart width height file]
  (let [bounds (java.awt.Rectangle. width height)
        dom-impl (org.apache.batik.dom.GenericDOMImplementation/getDOMImplementation)
        doc (.createDocument dom-impl nil "svg" nil)
        svg-generator (org.apache.batik.svggen.SVGGraphics2D. doc)]
    (.draw chart svg-generator bounds)
    (let [os (java.io.FileOutputStream. file)]
      (try
        (.stream svg-generator
                 (java.io.OutputStreamWriter. os "UTF-8")
                 true)
        (finally
          (.flush os)
          (.close os)))
      nil)))

(defn multiplot
  "

chart-properties: {:title, :x-label, :y-label}
lines is a seq of {:x data, :y data, :series-label str, :width 3}
"
  [chart-properties lines]
  (let [color-map {:BLACK java.awt.Color/BLACK 
                   :BLUE java.awt.Color/BLUE 
                   :CYAN java.awt.Color/CYAN 
                   :DARK_GRAY java.awt.Color/DARK_GRAY
                   :GRAY java.awt.Color/GRAY 
                   :GREEN java.awt.Color/GREEN 
                   :LIGHT_GRAY java.awt.Color/LIGHT_GRAY
                   :MAGENTA java.awt.Color/MAGENTA 
                   :ORANGE java.awt.Color/ORANGE 
                   :PINK java.awt.Color/PINK 
                   :RED java.awt.Color/RED 
                   :WHITE java.awt.Color/WHITE 
                   :YELLOW java.awt.Color/YELLOW}
        y-range (:y-range chart-properties)
        chart (let [{:keys [title x-label y-label]} chart-properties
                    {:keys [x y series-label width color points]} (first lines)
                    y-range (or (:y-range (first lines))
                                y-range)
                    chart (ic/xy-plot x
                                      y
                                      :title title
                                      :x-label x-label
                                      :y-label y-label
                                      :legend true
                                      :series-label series-label
                                      :points points)]
                (ic/set-stroke chart :width width :series 0 :dataset 0)
                (when color
                  (ic/set-stroke-color chart (color-map color)))
                (when y-range
                  (ic/set-y-range chart (first y-range) (second y-range)))
                chart)]
    ;; because of the way add-line works and set-stroke, we can't be as
    ;; declarative and must seperate the first line from the remaining
    ;; lines
    (dotimes [line-idx (dec (count lines))]
      (let [line-idx (inc line-idx)
            line (nth lines line-idx)
            {:keys [x y width series-label color points]} line
            y-range (or (:y-range line)
                        y-range)]
        (ic/add-lines chart
                      x
                      y
                      :series-label series-label
                      :points points)
        (ic/set-stroke chart :width width :series 0 :dataset line-idx)
        (when color
          (ic/set-stroke-color chart (color-map color) :series 0 :dataset line-idx))
        (when y-range
          (ic/set-y-range chart (first y-range) (second y-range)))))
    chart))
