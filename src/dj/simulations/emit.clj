(ns dj.simulations.emit
  (:require [dj.math.cemit :as dmc]))

(defn model-binding
  "
helper fn to emit ods binding pair from data
"
  [pair]
  (let [emit (dmc/c-emitter)
        [s-name e] pair]
    [(str s-name) (str (emit e) ";")]))
