(ns dj.simulations.carp.input.adjustments
  (:require [dj]
            [clojure.java.io :as io]))

(defn emit-adjustments-file
  "efficient adjustment emitter, uses writers under the hood to ensure memory usage is minimized

  adjustment-map: node-id -> parameter value

  possible better implementation using java.nio"
  [adjustment-map f]
  (with-open [w (io/writer f)]
    (let [ordered-ids (sort (keys adjustment-map))
          number-of-adjustments (count ordered-ids)]
      (.write w (str number-of-adjustments "\n"))
      (.write w (str "intra\n"))
      (doseq [nid ordered-ids]
        (.write w (str nid " " (adjustment-map nid) "\n"))))))
