(ns dj.simulations.integrator
  (:require [dj]
            [dj.math.symbolics :as dm]
            [dj.math.expression :as dme]
            [dj.math.matrix :as dmm]
            [dj.math.parser :as dmp]
            [dj.math.bindings :as dmb]
            [dj.math.differentiation :as dmd]
            [dj.math.linearalgebra :as dml]
	    [dj.dispatch.compiledtreefn :as dc]
            [dj.dispatch.treefn :as tf]
            [clojure.set :as cs]))

;; Depending on iterations (rosenbrock is one), the forms may be different

;; I will create:
;; fn: ids, id (z,dz) -> input-name, id -> output-name, algebra, differential equations
;; Formula (not proven to converge, oh well)
;; Z1 = Z0 + (I-dt*Jf)'*dt*dZ0
;; Simplifes to Z0 + (I*dt'-Jf)'dZ0
;; but only for first step

;; create example with just forward euler

(defn pairs->map [kvs]
  (reduce (fn [m [k v]]
            (assoc m k v))
          {}
          kvs))

(defn pairs->exp-map [kvs]
  (-> kvs
      pairs->map
      (dj/update-vals (comp :result dmp/parse))))

(defn backward-formulation [dt I J]
  (dm/- (dm/d I
              dt)
        J))

(defn midpoint-formulation [dt I J]
  (dm/- (dm/d I
              dt)
        (dm/d J 2)))

;; option for rosenbrock formulation
(def backward-formulation-fns
  {:integrator/formulation-fn
   (tf/fm
    []
    backward-formulation)})

(def midpoint-formulation-fns
  {:integrator/formulation-fn
   (tf/fm
    []
    midpoint-formulation)})

;; Note, these base-fns will depend on transformed versions of
;; algebra-assignment and differential-assignment
(def base-fns
  {:integrator/shared-algebra-vars
   (tf/fm
    [:dotmodel/shared-algebra-assignment]
    (-> shared-algebra-assignment
        pairs->map
        keys))

   :integrator/algebra-singularities
   (tf/fm
    [:dotmodel/algebra-assignment
     :simulations/singularities]
    (reduce (fn [ret {:keys [dependent independent singularity-input min-input max-input]}]
              (update ret
                      dependent
                      (fn [expr]
                        (let [a-dep-map ret]
                          (-> expr
                              (dme/inline-expression a-dep-map)
                              (dme/linearize-singularity a-dep-map
                                                         independent
                                                         singularity-input
                                                         min-input
                                                         max-input))))))
            (pairs->exp-map algebra-assignment)
            singularities))

   :integrator/differential-exp-map
   (tf/fm
    [:dotmodel/differential-assignment]
    (pairs->exp-map differential-assignment))

   :integrator/algebra-exp-map
   (tf/fm
    [:integrator/algebra-singularities]
    algebra-singularities)

   :integrator/algebra-bindings
   (tf/fm
    [:dotmodel/algebra-assignment
     :integrator/algebra-exp-map]
          (dmb/pairs->bindings (reduce (fn [v s]
                                         (conj v
                                               [(dm/vare s) (algebra-exp-map s)]))
                                       []
                                       (map first algebra-assignment))))

   ;; helper datastructure for symbolic differentiation
   ;; - explains what sv an algebra symbol depends on
   ;; - also prevents recursive lookups of svs by stating they don't directly depend on anything
   ;; - further, we also turn off lookups for shared-state vars, I think we can undo this though
   :integrator/dep-map
   (tf/fm
    [:integrator/algebra-exp-map
     :dotmodel/all-state-vars
     :integrator/shared-algebra-vars]
    (merge (dme/recursive-dependents-map algebra-exp-map
                                         all-state-vars)
           (dme/unbound-dependents-map (concat shared-algebra-vars
                                               all-state-vars))))

   :integrator/jacobian-map
   (tf/fm
    [:integrator/differential-exp-map
     :integrator/algebra-exp-map
     :integrator/dep-map]
    (dmd/jacobian-map differential-exp-map
                      (dmd/symbolic-lookup-differentiation algebra-exp-map
                                                           dep-map)))

   :integrator/id->Ji
   (tf/fm []
          (fn [nid did]
            (str "d" nid "_d" did)))

   :integrator/jacobian-bindings
   (tf/fm
    [:integrator/jacobian-map
     :dotmodel/all-state-vars
     :integrator/id->Ji]
    (dmb/pairs->bindings (for [rsv all-state-vars
                               csv all-state-vars
                               :let [e ((jacobian-map rsv) csv)]
                               :when (not (number? e))]
                           [(dm/vare (id->Ji rsv csv))
                            e])))

   :integrator/J
   (tf/fm
    [:integrator/jacobian-map
     :dotmodel/all-state-vars
     :integrator/id->Ji]
    (dmm/symbolic-jacobian-template jacobian-map
                                    all-state-vars
                                    id->Ji))})

;; overrides [:algebra-exp-map :jacobian-map], adds inline and constant optimizations
(def optimized-fns
  ;; Needs to be updated to use :integrator/algebra-singularities
  {:integrator/algebra-exp-map
   (tf/fm
    [:integrator/algebra-singularities]
    (let [a algebra-singularities]
      (reduce (fn [m v]
                (-> v
                    dm/vare
                    (dme/inline-expression a)
                    (as-> v'
                        (let [constant? (dme/reduce-constants v')]
                          (if (dmp/symbolic? constant?)
                            (assoc m
                                   v
                                   (dme/reduce-constants
                                    (a v)))
                            (assoc m
                                   v
                                   constant?))))))
              {}
              (keys a))))
   :integrator/jacobian-map
   (tf/fm
    [:integrator/differential-exp-map
     :integrator/algebra-exp-map
     :integrator/dep-map]
    (let [j (dmd/jacobian-map differential-exp-map
                              (dmd/symbolic-lookup-differentiation algebra-exp-map
                                                                   dep-map))]
      (dj/update-vals j
                      (fn [m]
                        (dj/update-vals m
                                        dme/normalize-ratio)))))})

(def mapped-data-fns
  {:integrator/mapped-sv
   (let [direct-dependents
         (fn direct-dependents [e]
           (reduce (fn [m child]
                     (if (dmp/symbolic? child)
                       (if-let [children (:children child)]
                         (if (= (:op child) "map")
                           (conj m child)
                           (cs/union m (direct-dependents child)))
                         m)
                       m))
                   #{}
                   (:children e)))]
     (tf/fm
      [:integrator/algebra-exp-map]
      (direct-dependents {:children (vals algebra-exp-map)})))

   :integrator/algebra-parameters
   (let [find-parameters
         (fn find-parameters [m e]
           (if (dmp/symbolic? e)
             (if (= (:op e) "var")
               (let [vname (:name e)]
                 (if (re-find #"_parameter$" vname)
                   (conj m vname)
                   m))
               (reduce find-parameters
                       m
                       (:children e)))
             m))]
     (tf/fm
      [:integrator/algebra-exp-map]
      (reduce find-parameters
              #{}
              (vals algebra-exp-map))))
   :integrator/dep-map
   (tf/fm
    [:integrator/algebra-exp-map
     :dotmodel/all-state-vars
     :integrator/mapped-sv
     :integrator/algebra-parameters]
    (merge (dme/recursive-dependents-map algebra-exp-map
                                         (concat all-state-vars
                                                 (map :children mapped-sv)))
           (dme/unbound-dependents-map (concat all-state-vars algebra-parameters))))
   :integrator/jacobian-map
   (tf/fm
    [:integrator/differential-exp-map
     :integrator/algebra-exp-map
     :integrator/dep-map
     :integrator/mapped-sv]
    (let [j (dmd/jacobian-map (reduce (fn [ret e]
                                        (assoc ret
                                               e 0))
                                      differential-exp-map
                                      mapped-sv)
                              (dmd/symbolic-lookup-differentiation algebra-exp-map
                                                                   dep-map))]
      (dj/update-vals j
                      (fn [m]
                        (dj/update-vals m
                                        dme/normalize-ratio)))))})

;; input: requires mathematically transformed data on ODE system
;; output boundary: integrator fn used by cu generator
(def rosenbrock-integrator-fns
  "

  This is a 1-newton iteration rosenbrock semi-implicit ODE solver
  method. This allows a larger time step to improve runtime while
  maintaining model convergence. Uses dj.math.symbolics tools to
  compute symbolic partial derivatives and solves the linear system
  with a compile time expansion of matrix algebra (QR decomposition to
  perform inverse).

  requires formulation as input, aka backwards, midpoint, trapezoidal etc.

  "
  (merge
   base-fns
   optimized-fns
   {:simulations/integrator-fn
    (tf/fm
     [:integrator/formulation-fn
      :integrator/differential-exp-map
      :dotmodel/differential-assignment
      :dotmodel/all-state-vars
      :integrator/algebra-bindings
      :integrator/jacobian-bindings
      :integrator/J]
     (fn [dt id->dz]
       ;; solve returns a let'd vector (deltaZ)
       ;; I will need to wrap vector with a bounce -> deltaZ
       (let [differential-bindings (dmb/pairs->bindings (reduce (fn [v s]
                                                                  (conj v
                                                                        [(dm/vare (id->dz s)) (differential-exp-map s)]))
                                                                []
                                                                (map first differential-assignment)))
             deltaZ (dml/solve' (formulation-fn dt
                                                (dmm/identity-m (count all-state-vars))
                                                J)
                                (dmm/t
                                 (dmm/v
                                  [(mapv (fn [id]
                                           (dm/vare (id->dz id)))
                                         all-state-vars)])))]
         (-> deltaZ
             ;; Set the algebra & jacobian bindings
             (update-in [:bindings]
                        (fn [v]
                          (dmb/join algebra-bindings
                                    differential-bindings
                                    jacobian-bindings
                                    v)))
             ;; Generally speaking, the returns would have to be in the
             ;; code since tail-calls occur in if statements too
             (assoc-in [:children]
                       [{:op "return"
                         ;; How do we guarantee alignment of deltaZ to all-state-vars?
                         ;; (Probably due to ordering of all-state-vars, jacobian uses same order)

                         ;; Technically this is deltaZ but
                         ;; returns of map with symbols to the
                         ;; corresponding sv
                         :bindings (dmb/pairs->bindings (mapv (fn [result-e id]
                                                                [(dm/vare id)
                                                                 result-e])
                                                              (-> deltaZ
                                                                  :children
                                                                  first)
                                                              all-state-vars))}])))))}))



(def explicit-euler-integrator-fn
  (merge
   base-fns
   {:simulations/integrator-fn
    (tf/fm
     [:integrator/algebra-bindings
      :dotmodel/all-state-vars

      ;; This is for debugging for now
      :debug/differential-bindings]
     (fn [dt id->dz]
       (-> {:op "let"}
           ;; Set the algebra & jacobian bindings
           (assoc :bindings
                  (dmb/join algebra-bindings
                            differential-bindings))
           ;; deltaZ
           (assoc :children
                  [{:op "return"
                    :bindings (dmb/pairs->bindings (mapv (fn [id]
                                                           [(dm/vare id)
                                                            (dm/* dt
                                                                  (dm/vare (id->dz id)))])
                                                         all-state-vars))}]))))}))

(def basic-combiner-fns
  "
  z1 = z0 + deltaZ
  "
  {:simulations/combiner-fn
   (tf/fm
    []
    (fn [deltaZ id->z0 id->z1]
      (update-in deltaZ
                 [:children 0 :bindings]
                 (fn [bindings]
                   (dmb/pairs->bindings (mapv (fn [[s e]]
                                                [(update-in s [:name] id->z1)
                                                 (dm/+ (update-in s [:name] id->z0) e)])
                                              (seq bindings)))))))})

;; input: constrained vars
;; output boundary: just a utiilty fn for cu generator
(def constrained-precision-combiner-fns
  {:simulations/combiner-fn
   (tf/fm
    [:simulations/constrained-vars]
    (fn [deltaZ id->uz id->z1 id->bz]
      (update-in deltaZ
                 [:children 0 :bindings]
                 (fn [bindings]
                   (dmb/pairs->bindings (mapv (fn [[sv e]]
                                                (let [s (:name sv)]
                                                  [(dm/vare (id->z1 s))
                                                   (if (constrained-vars s)
                                                     {:op "if"
                                                      :children [{:op "<"
                                                                  :children [{:op "+"
                                                                              :children [(dm/vare (id->bz s))
                                                                                         (dm/vare (id->uz s))
                                                                                         e]}
                                                                             0.0]}
                                                                 (dm/vare (id->uz s))
                                                                 (dm/+ (dm/vare (id->uz s)) e)]} 
                                                     (dm/+ (dm/vare (id->uz s)) e))]))
                                              (seq bindings)))))))})

(def constrained-kahan-combiner-fns
  {:simulations/combiner-fn
   (tf/fm
    [:simulations/constrained-vars]
    (fn [deltaZ id->z0 id->z1 id->dze]
      (update-in deltaZ
                 [:children 0 :bindings]
                 (fn [bindings]
                   (dmb/pairs->bindings
                    (reduce into
                            []
                            (mapv (fn [[sv dz]]
                                    (let [s (:name sv)
                                          dze (dm/vare (id->dze s))
                                          dzc {:op "-"
                                               :children [dz
                                                          dze]}
                                          z0 (dm/vare (id->z0 s))
                                          z0+dzc {:op "+"
                                                  :children [dzc
                                                             z0]}]
                                      [[(dm/vare (id->z1 s))
                                        (if (constrained-vars s)
                                          {:op "if"
                                           :children [{:op "<"
                                                       :children [z0+dzc
                                                                  (constrained-vars s)]}
                                                      z0
                                                      z0+dzc]} 
                                          z0+dzc)]
                                       [dze
                                        {:op "-"
                                         :children [{:op "-"
                                                     :children [z0+dzc z0]}
                                                    dzc]}]]))
                                  (seq bindings))))))))})

(defn update-precision
  "
  recombines base and update vars into state variable
  "
  [result-bindings all-state-vars id->z1 id->bz id->uz]
  (update-in result-bindings
             [:children 0 :bindings]
             dmb/join
             (dmb/pairs->bindings (mapv (fn [s]
                                          [(dm/vare (id->z1 s))
                                           (dm/+ (dm/vare (id->bz s))
                                                 (dm/vare (id->uz s)))])
                                        all-state-vars))))
