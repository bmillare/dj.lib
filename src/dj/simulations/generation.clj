(ns dj.simulations.generation)

(defn base-stim-nodemap
  "build map of node-ids -> value, typically used by mito network
  models, the pattern is one node has a stim value, and the remaining
  nodes have a base value"
  [{:keys [base stim stim-id num-nodes]}]
  (merge
   (reduce (fn [m nid]
             (assoc m
                    nid
                    base))
           {}
           (range num-nodes))
   {stim-id stim}))
