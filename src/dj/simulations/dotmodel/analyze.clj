(ns dj.simulations.dotmodel.analyze
  (:require [dj.dispatch.treefn :as tf]
            [dj.math.expression :as me]
            [clojure.set :as cs]
            [dj.algorithms.dag :as dag]
            [dj.math.cemit :as e]))

(def scope
  {:dotmodel/all-def
   (tf/fm
    [:integrator/differential-exp-map
     :integrator/algebra-exp-map]
    (concat (keys differential-exp-map)
            (keys algebra-exp-map)))
   :dotmodel/dependents-map
   (tf/fm
    [:dotmodel/all-def
     :integrator/differential-exp-map
     :integrator/algebra-exp-map
     :dotmodel/all-state-vars]
    (merge
     (me/recursive-dependents-map algebra-exp-map
                                  all-def)
     (me/unbound-dependents-map all-state-vars)))
   :dotmodel/core-algebra
   (tf/fm
    [:dotmodel/all-def
     :integrator/differential-exp-map]
    (reduce-kv
     (fn [ret sv-name alg-names]
       (reduce (fn [ret' alg-name]
                 (update ret'
                         alg-name
                         (fn [x]
                           (conj (or x #{})
                                 sv-name))))
               ret
               alg-names))
     {}
     (me/recursive-dependents-map differential-exp-map
                                  all-def)))
   :input.dotmodel/query-lhs
   (tf/fm
    [:dotmodel/core-algebra]
    (set (keys
          core-algebra)))
   :dotmodel/shaken-keys
   (tf/fm
    [:input.dotmodel/query-lhs
     :dotmodel/dependents-map]
    (let [walk-accumulate
          (memoize
           (fn -walk-accumulate [check-v]
             (let [direct-dependents (dependents-map check-v)]
               (apply cs/union direct-dependents (map -walk-accumulate direct-dependents)))))]
      (reduce (fn [ret v]
                (assoc ret
                       v
                       (conj (walk-accumulate v)
                             v)))
              {}
              query-lhs)))
   :dotmodel/s-all-state-vars
   (tf/fm
    [:dotmodel/all-state-vars]
    (set all-state-vars))
   :dotmodel/sorted-keys
   (tf/fm
    [:dotmodel/shaken-keys
     :dotmodel/dependents-map
     :input.dotmodel/query-lhs
     :dotmodel/core-algebra
     :dotmodel/s-all-state-vars]
    (let [s-query-lhs (set query-lhs)
          all-shaken-keys (apply cs/union (vals shaken-keys))
          only-roots
          (filter (set query-lhs)
                  (dag/topological-sort (select-keys dependents-map all-shaken-keys)))]
      (loop [emitted s-all-state-vars
             shared #{}
             order []
             roots-left only-roots]
        (let [r (first roots-left)]
          (if r
            (let [vname-order (dag/topological-sort (select-keys dependents-map (shaken-keys r)))
                  s-vname-order (set vname-order)
                  shared' (cs/union
                           shared
                           (cs/intersection emitted
                                            s-vname-order))]
              (recur (cs/union
                      emitted
                      s-vname-order)
                     shared'
                     (-> order
                         (into (map (fn [vn]
                                      [vn :sv-uses]))
                               (vec (cs/intersection s-all-state-vars
                                                     s-vname-order)))
                         (into (map (fn [vn]
                                      [vn :alg-uses]))
                               (vec (cs/difference (cs/intersection s-vname-order
                                                                    shared')
                                                   s-all-state-vars)))
                         (into (map (fn [vn]
                                      [vn :output]))
                               (core-algebra r))
                         (into (map (fn [vn]
                                      [vn (if (s-query-lhs vn)
                                            :root
                                            nil)]))
                               (remove emitted vname-order)))
                     (rest roots-left)))
            order)))))
   :dotmodel/emit-scoped
   (tf/fm
    [:dotmodel/sorted-keys
     :integrator/algebra-exp-map
     :dotmodel/var-alias
     :dotmodel/op-alias]
    (let [emit (e/c-emitter (if var-alias
                              {"var"
                               (fn [emit]
                                 (fn [{:keys [name]}]
                                   (or (var-alias name)
                                       name)))}
                              {})
                            (if op-alias
                              op-alias
                              {}))]
      (apply str
             (for [[vname decoration] sorted-keys]
               (case decoration
                 :sv-uses (str "// uses state: " vname "\n")
                 :alg-uses (str "// uses alg: " vname "\n")
                 :output (str "// outputs: " vname "\n")
                 :root (str vname " = " (emit (algebra-exp-map vname)) ";\n\n")
                 (str vname " = " (emit (algebra-exp-map vname)) ";\n"))))))})

(def scope-defaults
  {:dotmodel/var-alias nil
   :dotmodel/op-alias {"copy-sign" "copysign"
                       "ln" "log"}})

;; Maybe add other keys for meta data like sv uses, and sv outputs,
;; then we can diff over them
(def diff
  {:dotmodel/single-sorted-keys
   (tf/fm
    [:dotmodel/shaken-keys
     :dotmodel/dependents-map
     :input.dotmodel/single-query-lhs]
    (dag/topological-sort (select-keys dependents-map (shaken-keys single-query-lhs))))
   :dotmodel/sv-uses
   (tf/fm
    [:dotmodel/s-all-state-vars
     :dotmodel/single-sorted-keys]
    (cs/intersection s-all-state-vars
                     (set single-sorted-keys)))
   :dotmodel/outputs
   (tf/fm
    [:dotmodel/core-algebra
     :input.dotmodel/single-query-lhs]
    (or (core-algebra single-query-lhs)
        #{}))})
