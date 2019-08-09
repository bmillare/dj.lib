(ns dj.simulations.cuda.input)

;; * Purpose
;; - common input data manipulation and creation for running simulations

(defn peak-gradient-hashmap-1D
  "produce fake gradients per node basis peaked at node-ids"
  [{:keys [num-nodes node-ids max-value decrease-per-node]}]
  (apply merge-with
         max
         (map (fn [shunt-node-id]
                (reduce (fn [ret node-id]
                          (assoc ret
                            node-id
                            (double
                             (- max-value
                                (* decrease-per-node
                                   (Math/abs (- node-id shunt-node-id)))))))
                        {}
                        (range num-nodes)))
              node-ids)))

(defn ->initial-conditions-fn [config initial-conditions]
  (fn [var-name node-id block-id]
    (let [num-nodes (:num-nodes config)]
      (get (nth initial-conditions
                (+ (* block-id num-nodes)
                   node-id))
           var-name
           (double 0)))))
