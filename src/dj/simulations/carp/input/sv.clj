(ns dj.simulations.carp.input.sv
  (:require [dj.algorithms.peg :as ap]
            [clojure.string :as cs]))

(let [ws (ap/t #"\s*")
      word (ap/t #"\S+")
      hashtag (ap/t #"\#")
      row (ap/alt (ap/s word ws hashtag ws word)
                  (fn [[value _ _ _ label]]
                    {:value value
                     :label label}))]
  (defn parse-sv [txt header-length]
    (let [[model-name-line & rest-lines]
          (drop header-length
                (cs/split-lines txt))]
      (loop [label-order []
             map {}
             rlines rest-lines]
        (if (empty? rlines)
          {:label-order label-order
           :label->value map
           :model-name (cs/trim model-name-line)}
          (let [{:keys [label value]} (:result (ap/parse row (first rlines)))]
            (recur (conj label-order label)
                   (assoc map
                          label
                          value)
                   (rest rlines))))))))
