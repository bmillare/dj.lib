(ns dj.simulations.cuda.output
  (:require [dj]))

;;; Purpose
;; - common simulation output data manipulation and analysis

(defn join-var-data
  "

takes two var-data (var-name->[vals...]) and joins the datas together
on key
"
  [o1 o2]
  (persistent!
   (reduce-kv (fn [ret k v]
                (assoc! ret
                        k 
                        (into (ret k)
                              v)))
              (transient o1)
              o2)))

(defn join-output
  ([d1 d2]
     (-> d1
         (update-in [:output]
                    (fn [nodes1]
                      (mapv join-var-data
                            nodes1
                            (:output d2))))))
  ([d1 d2 & args]
     (persistent!
      (reduce (fn [ret a]
                (assoc! ret
                        :output
                        (mapv join-var-data
                              (:output ret)
                              (:output a))))
              (transient (join-output d1 d2))
              args))))

(defn NaNs?
  "
given a snapshot state, detect if there are any NaNs
state: [{var 
"
  [snapshot-state]
  (some #(.isNaN %)
        (mapcat vals snapshot-state)))

(defn last-state
  "
return latest values
"
  [data]
  (mapv (fn [node-data]
          (dj/update-vals node-data
                          peek))
        (:output data)))

(defn nth-state
  "
return values at nth index
"
  [data n]
  (mapv (fn [node-data]
          (dj/update-vals node-data
                          (fn [v]
                            (nth v n))))
        (:output data)))

(defn nth-state*
  "
  return values at nth index, accepts direct data
  "
  [data n]
  (mapv (fn [node-data]
          (dj/update-vals node-data
                          (fn [v]
                            (nth v n))))
        data))

(let [dissoc-s #(apply dissoc %1 %2)]
  (defn block-parameters->parameter-bindings [bp {:keys [node-offset state-vars]}]
    (vec
     (for [bindings bp]
       (-> bindings
           (->> (reduce (fn [ret [v:name v:value]]
                          (assoc ret
                            v:name
                            (if (map? v:value)
                              (v:value node-offset)
                              v:value)))
                        {}))
           (dissoc-s state-vars))))))

(defn data-form->plot-form
  ([data]
     (let [{:keys [initial-conditions time-steps output]} data]
       (mapv (fn [node-id node]
               (reduce-kv (fn [ret k v]
                            (assoc ret
                              k
                              (if-let [ic ((nth initial-conditions
                                                node-id) k)]
                                {:x (into [0.0] time-steps)
                                 :y (into [ic] v)}
                                {:x time-steps
                                 :y v})))
                          {}
                          node))
             (range)
             output)))
  ([data args]
     (let [{:keys [select-steps nodes select-var]} args
           {:keys [initial-conditions time-steps output]} data
           cut (let [[start end] select-steps]
                 (if select-steps
                   (fn [d]
                     (subvec d start end))
                   identity))
           nodes-set (set nodes)]
       (vec (for [[node-id node] (map vector (range) output)
                  :when (nodes-set node-id)]
              (let [v (node select-var)]
                (if-let [ic ((nth initial-conditions
                                  node-id) select-var)]
                  {:x (cut (into [0.0] time-steps))
                   :y (cut (into [ic] v))
                   :series-label (str select-var "," node-id)
                   :width 3}
                  {:x (cut time-steps)
                   :y (cut v)
                   :series-label (str select-var "," node-id)
                   :width 3})))))))

(defn min-max
  "given a seq, returns min and max efficiently"
  [s]
  (loop [s (seq s)
         min (first s)
         max (first s)]
    (if s
      (let [fs (first s)]
        (recur (next s)
               (if (< fs min)
                 fs
                 min)
               (if (> fs max)
                 fs
                 max)))
      {:min min
       :max max})))

(defn min-max-analysis
  "produces a map of state-var to min and max found for all the data"
  [data]
  (let [merged-data (apply merge-with concat data)]
    (reduce (fn [m k]
              (assoc m
                k
                (min-max (merged-data k))))
            {}
            (keys (first data)))))

(defn min-max-output [data]
  (let [merged-data (apply merge-with concat (:output data))]
    (reduce (fn [m k]
              (assoc m
                k
                (min-max (merged-data k))))
            {}
            (keys (first (:output data))))))

(defn map-vars->line [result f node vout-name & var-names]
  (let [raw result
        time-steps (-> raw
                       :time-steps)
        line {:x time-steps
              :y (apply mapv
                        f
                        (map (fn [v-name]
                               (-> raw
                                   :output
                                   (nth node)
                                   (get v-name)))
                             var-names))
              :series-label vout-name
              :width 3}]
    line))

;; This should probably be deprecated
#_ (def state-context-context
  {:sd (dc/fnr [algebra-exp-map dep-map]
               (dj.math.differentiation/symbolic-lookup-differentiation algebra-exp-map
                                                                        dep-map))
   :state-context (dc/fnr [jacobian-map track-vars state-vars algebra-exp-map n-idx state-idx result]
                          (merge
                           (reduce (fn [m [v1 v2]]
                                     (assoc m
                                       (str "d" v1 "_d" v2)
                                       ((jacobian-map v1) v2)))
                                   {}
                                   (for [v1 state-vars
                                         v2 state-vars]
                                     [v1 v2]))
                           algebra-exp-map
                           {"n_idx" n-idx}
                           (apply dissoc
                                  (-> (nth-state result state-idx)
                                      (nth n-idx))
                                  (clojure.set/difference (set track-vars) (set state-vars)))))})

#_ (def state-context (dc/->let-fn (merge rse/base-fns
                                       state-context-context)
                                :state-context
                                [:result :config :n-idx :state-idx]))

(defn split-state-by-block [state num-blocks]
  (let [{:keys [initial-conditions output]} state
        nodes-per-block (/ (count output) num-blocks) ;; should divide evenly
        ]
    (mapv (fn [block-id]
            (let [start-idx (* block-id nodes-per-block)
                  end-idx (* (inc block-id) nodes-per-block)]
              {:initial-conditions (subvec initial-conditions
                                           start-idx
                                           end-idx)
               :output (subvec output
                               start-idx
                               end-idx)}))
          (range num-blocks))))

(defn cartesian-product
  "All the ways to take one item from each sequence"
  [seqs]
  (let [v-original-seqs (vec seqs)
	step
	(fn step [v-seqs]
	  (let [increment
		(fn [v-seqs]
		  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
		    (if (= i -1) nil
			(if-let [rst (next (v-seqs i))]
			  (assoc v-seqs i rst)
			  (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
	    (when v-seqs
              (cons (map first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? first seqs)
      (lazy-seq (step v-original-seqs)))))

(defn parameter-cartesian
  "
parameter-selection: vector of pairs
each pair is a vname and a vector of possible values

special-binding: [v bound-v bvf]

v is variable value we are grabbing

bound-v is variable that we will set

bvf is function that will be called with value of v, the value
returned by bvf will be set to bound-v

This enables setting state variables relative to other parameters

Note: Currently, a special-binding must exist

returns block-parameters
"
  ([parameter-selection special-binding]
   (let [[v bound-v bvf] special-binding
         parameters (map second parameter-selection)
         vnames (map first parameter-selection)]
     (mapv (fn [permutation]
             (conj (mapv (fn [vname parameter]
                           [vname parameter])
                         vnames
                         permutation)
                   [bound-v (bvf (nth permutation
                                      (.indexOf vnames v)))]))
           (cartesian-product parameters))))
  ([parameter-selection]
   (let [parameters (map second parameter-selection)
         vnames (map first parameter-selection)]
     (mapv (fn [permutation]
             (mapv (fn [vname parameter]
                     [vname parameter])
                   vnames
                   permutation))
           (cartesian-product parameters)))))

(defn dependencies->edges
  "
converts dependency graph into a graphviz friendly edge vector
"
  [direct-dependency-map]
  (reduce-kv (fn add [ret k v]
               (if (empty? v)
                 ret
                 (let [dep (first v)]
                   (recur (conj ret [k dep])
                          k
                          (disj v dep)))))
             #{}
             direct-dependency-map))

(defn cartesian-paths [precede-map start end]
  (letfn [(join-tail [id tail ignore]
            (let [parents (precede-map id)]
              (for [p parents
                    :when (not (ignore p))]
                (list (list* p tail) (conj ignore p)))))
          (paths [tails]
            (loop [completed []
                   tail-list tails]
              (if (empty? tail-list)
                completed
                (let [[tail visited-set] (first tail-list)
                      id (first tail)
                      next-tails (next tail-list)]
                  (if (= id
                         start)
                    (recur (conj completed tail)
                           next-tails)
                    (recur completed
                           (concat (join-tail id tail visited-set)
                                   next-tails)))))))]
    (paths (list (list (list end) #{end})))))

;; maybe unused?
#_ (defn precede-map [edges-map start end]
  (letfn [(precedes [precede-map explore-set explored-set]
            (if (empty? explore-set)
              precede-map
              (let [next-explored-set (apply cs/union
                                             explored-set
                                             (map #(get edges-map %)
                                                  explore-set))]
                (recur (reduce (fn [pm current-id]
                                 (reduce (fn [pm' child]
                                           (update-in pm'
                                                      [child]
                                                      (comp set conj)
                                                      current-id))
                                         pm
                                         (edges-map current-id)))
                               precede-map
                               explore-set)
                       (cs/difference next-explored-set
                                      explored-set)
                       next-explored-set))))]
    (precedes {} #{start} #{start end})))

;; overrides [:exp-maps], removes inline optimizations
#_ (def dependency-fns {:exp-maps (dc/fnr [config]
                                       (-> config
                                           (select-keys [:algebra-assignment :differential-assignment])
                                           (dj/update-vals rse/pairs->exp-map)))
                     :dependency-map (dc/fnr [differential-exp-map algebra-exp-map state-vars]
                                             (-> (dj.math.expression/recursive-dependents-map (merge algebra-exp-map differential-exp-map)
                                                                                              state-vars)
                                                 (select-keys state-vars)))
                     :dependencies->edges (dc/wrap-fnr dependencies->edges
                                                       [:dependency-map])
                     :graphviz (dc/fnr [dependencies->edges]
                                       (dj.plot.graphviz/graphviz {:type :digraph
                                                                   :name :dependency_graph
                                                                   :edges dependencies->edges}))
                     :dependent-paths (dc/fnr [dependency-map start-node end-node state-vars]
                                              (-> dependency-map
                                                  ;; (precede-map start-node end-node)
                                                  ;; dj.math.expression/reverse-dependencies
                                                  ;; (cartesian-paths (set state-vars) start-node end-node)
                                                  (cartesian-paths start-node end-node)
                                                  ))})

;; overrides [:dependency-map] to distinguish differential variables
;; in there differential and state-value form
#_ (def dependent-paths-fns
  {:dependency-map (dc/fnr [differential-exp-map algebra-exp-map]
                           (-> (merge algebra-exp-map
                                      (reduce-kv (fn [ret k v]
                                                   (assoc ret
                                                     (str "d_" k)
                                                     v))
                                                 {}
                                                 differential-exp-map))
                               (dj/update-vals dj.math.expression/direct-dependents)))})

#_ (def dependent-paths (dc/->let-fn (merge rse/base-fns
                                         dependency-fns
                                         dependent-paths-fns)
                                  :dependent-paths
                                  [:config :start-node :end-node]))

#_ (defn decompose-velocity-expressions
  "returns an expression-map of velocity var names -> flux components for a particular state-var but includes constant multiplicative factors"
  [exp-map state-var velocity-vars]
  (let [e (exp-map state-var)
        velocity-var-set (set velocity-vars)]
    (reduce (fn [ret velocity-var]
              (assoc ret
                velocity-var
                (dj.math.expression/inline-expression e
                                                      (fn [name]
                                                        (if (= name velocity-var)
                                                          nil ;; variable needs to be unbound so it can be set later
                                                          (if (velocity-var-set name)
                                                            0 ;; all other velocity variables needs to be set to 0 so we are left with only the velocity var of interest
                                                            (exp-map name)))))))
            {}
            velocity-vars)))

#_ (let [le (dj.math.lispemit/lisp-emitter)]
  (defn velocity-fns [velocity-exp-map]
    (reduce-kv (fn [ret k v]
                 (assoc ret
                   k
                   (eval `(fn [~(symbol k)]
                            ~(le v)))))
               {}
               velocity-exp-map)))

#_ (def telemetry-fns
  {:plot-data (dc/wrap-fnr data-form->plot-form
                           [:data])
   :line-width (dc/fnr [args]
                       (or (:line-width args)
                           3))
   :nodes (dc/fnr [args]
                  (:nodes args))
   :node-mod (dc/fnr [args]
                     (:node-mod args))
   :state-var (dc/fnr [args]
                      (:state-var args))
   :trace-filter-fn (dc/fnr [args]
                            (or (:trace-filter-fn args)
                                identity))
   :velocity-vars (dc/fnr [args]
                          (:velocity-vars args))
   :velocity-scaling-fn (dc/fnr [args]
                                (let [vs (or (:velocity-scaling args)
                                             1)]
                                  (fn [x]
                                    (* vs x))))
   :acceleration-scaling-fn (dc/fnr [args]
                                    (let [as (or (:acceleration-scaling args)
                                                 1)]
                                      (fn [x]
                                        (* as x))))
   ;; at the moment this must always exist
   :select-steps (dc/fnr [args]
                         (:select-steps args))
   :position-fn (dc/fnr [select-steps state-var line-width]
                        (fn [v-map n-id]
                          (let [sv-data (v-map state-var)
                                [step0 step1] select-steps]
                            {state-var
                             {:width line-width
                              :series-label (str state-var "," n-id)
                              :x (subvec (:x sv-data) step0 step1)
                              :y (subvec (:y sv-data) step0 step1)}})))
   :velocity-fn
   (dc/fnr [exp-maps select-steps line-width velocity-vars node-mod state-var velocity-scaling-fn]
           (fn [n-id]
             (let [fractional-velocity-fn-map (-> (decompose-velocity-expressions (assoc (:algebra-assignment exp-maps)
                                                                                    state-var
                                                                                    ((:differential-assignment exp-maps)
                                                                                     state-var)
                                                                                    "n_idx"
                                                                                    (mod n-id
                                                                                         node-mod))
                                                                                  state-var
                                                                                  velocity-vars)
                                                  velocity-fns)
                   [step0 step1] select-steps]
               (fn [v-map]
                 (let [fracs (reduce (fn [ret k]
                                       (let [v-data (v-map k)]
                                         (if v-data
                                           nil
                                           (throw (Exception. (str "variable " k " not found"))))
                                         (assoc ret
                                           (str "f_" k)
                                           {:width line-width
                                            :series-label (str "f_" k "," n-id)
                                            :x (subvec (:x v-data) step0 step1)
                                            :y (mapv (comp velocity-scaling-fn (fractional-velocity-fn-map k))
                                                     (subvec (:y v-data) step0 step1))})))
                                     {}
                                     velocity-vars)]
                   (assoc fracs
                     (str "velocity_" state-var)
                     {:width line-width
                      :series-label (str "velocity_" state-var "," n-id)
                      ;; if this is a v-map, should have example time
                      :x (-> fracs
                             (get (str "f_" (first velocity-vars)))
                             :x)
                      :y (apply map + (map (fn [k]
                                             (-> (fracs (str "f_" k))
                                                 :y))
                                           velocity-vars))}))))))
   :acceleration-fn (dc/fnr [line-width state-var select-steps acceleration-scaling-fn]
                            (let [[step0 step1] select-steps]
                              (fn [velocity-map n-id]
                                (assoc velocity-map
                                  (str "acceleration_" state-var)
                                  (let [velocity (-> velocity-map
                                                     (get (str "velocity_" state-var)))
                                        time (subvec (:x velocity)
                                                     0
                                                     (dec (- step1 step0)))]
                                    {:width line-width
                                     :series-label (str "acceleration_" state-var "," n-id)
                                     :x time
                                     :y (mapv (comp acceleration-scaling-fn
                                                    (fn [x0 x1 y0 y1]
                                                      (/ (- y1 y0)
                                                         (- x1 x0))))
                                              time
                                              (drop 1 time)
                                              (:y velocity)
                                              (drop 1 (:y velocity)))})))))
   :telemetry (dc/fnr [nodes plot-data position-fn velocity-fn acceleration-fn trace-filter-fn]
                      (mapcat (fn [n-id]
                                (let [v-map (-> plot-data
                                                (nth n-id))
                                      vf (velocity-fn n-id)]
                                  (vals
                                   (-> (merge (position-fn v-map
                                                           n-id)
                                              (-> (vf v-map)
                                                  (acceleration-fn n-id)))
                                       trace-filter-fn))))
                              nodes))})

#_ (def telemetry
  (dc/->let-fn
   (merge rse/base-fns
          dependency-fns
          dependent-paths-fns
          telemetry-fns)
   :telemetry
   [:config :data :args]))

#_ (def plot-with-context
  (dc/->let-fn (merge rse/base-fns
                      dependency-fns ;; don't want inline optimizations
                      {:input-variable (dc/fnr [args]
                                               (:input-variable args))
                       :input-domain (dc/fnr [args]
                                             (:input-domain args))
                       :output-variable (dc/fnr [args]
                                                (:output-variable args))
                       ;; line settings is a seq of maps with keys
                       ;; [:series-label :width :exp-map]
                       :line-settings (dc/fnr [args]
                                              (:line-settings args))
                       :body (dc/fnr [result exp-maps input-variable input-domain output-variable line-settings track-vars state-vars]
                                     (let [emit (dj.math.lispemit/lisp-emitter)]
                                       (mapv (fn [setting]
                                               (let [n-idx (:n-idx setting)
                                                     state-idx (:state-idx setting)
                                                     f (eval `(fn [~(symbol input-variable)]
                                                                ~(emit
                                                                  (dme/inline-expression (dj.math/vare output-variable)
                                                                                         (dissoc (merge (:algebra-assignment exp-maps)
                                                                                                        (:differential-assignment exp-maps)
                                                                                                        {"n_idx" n-idx}
                                                                                                        (:exp-map setting)
                                                                                                        (apply dissoc
                                                                                                               (-> (nth-state result state-idx)
                                                                                                                   (nth n-idx))
                                                                                                               (clojure.set/difference (set track-vars) (set state-vars))))
                                                                                                 input-variable)))))]
                                                 {:x input-domain
                                                  :y (mapv f input-domain)
                                                  :series-label (:series-label setting)
                                                  :width (or (:width setting)
                                                             3)}))
                                             line-settings)))})
               :body
               [:config
                :result
                :args]))

#_ (def time-surface-plot-with-context
  (dc/->let-fn (merge rse/base-fns
                      dependency-fns ;; don't want inline optimizations
                      {:input-variable (dc/fnr [args]
                                               (:input-variable args))
                       :input-domain (dc/fnr [args]
                                             (:input-domain args))
                       :output-variable (dc/fnr [args]
                                                (:output-variable args))
                       :state-domain (dc/fnr [args]
                                             (:state-domain args))
                       :n-idx (dc/fnr [args]
                                      (:n-idx args))
                       :body (dc/fnr [result n-idx exp-maps input-variable input-domain state-domain output-variable track-vars state-vars]
                                     (let [emit (dj.math.lispemit/lisp-emitter)
                                           exp-fn (fn [state-idx]
                                                    (eval `(fn [~(symbol input-variable)]
                                                             ~(emit
                                                               (dme/inline-expression (dj.math/vare output-variable)
                                                                                      (dissoc (merge (:algebra-assignment exp-maps)
                                                                                                     (:differential-assignment exp-maps)
                                                                                                     {"n_idx" n-idx}
                                                                                                     (apply dissoc
                                                                                                            (-> (nth-state result state-idx)
                                                                                                                (nth n-idx))
                                                                                                            (clojure.set/difference (set track-vars) (set state-vars))))
                                                                                              input-variable))))))
                                           state-domain-count (count state-domain)]
                                       (loop [states-left state-domain
                                              z []]
                                         (if states-left
                                           (let [state-idx (first states-left)]
                                             (recur (next states-left)
                                                    (into z (mapv (exp-fn state-idx) input-domain))))
                                           {:x input-domain
                                            :y state-domain
                                            :z z}))))})
               :body
               [:config
                :result
                :args]))

#_ (def surface-plot-with-context
  (dc/->let-fn (merge rse/base-fns
                      dependency-fns ;; don't want inline optimizations
                      {:input-variable-x (dc/fnr [args]
                                               (:input-variable-x args))
                       :input-domain-x (dc/fnr [args]
                                             (:input-domain-x args))
                       :input-variable-y (dc/fnr [args]
                                               (:input-variable-y args))
                       :input-domain-y (dc/fnr [args]
                                             (:input-domain-y args))
                       :output-variable (dc/fnr [args]
                                                (:output-variable args))
                       :state-idx (dc/fnr [args]
                                             (:state-idx args))
                       :n-idx (dc/fnr [args]
                                      (:n-idx args))
                       :body (dc/fnr [result n-idx exp-maps input-variable-x input-domain-x input-variable-y input-domain-y state-idx output-variable track-vars state-vars]
                                     (let [emit (dj.math.lispemit/lisp-emitter)
                                           exp-fn (eval `(fn [~(symbol input-variable-x) ~(symbol input-variable-y)]
                                                           ~(emit
                                                             (dme/inline-expression (dj.math/vare output-variable)
                                                                                    (dissoc (merge (:algebra-assignment exp-maps)
                                                                                                   (:differential-assignment exp-maps)
                                                                                                   {"n_idx" n-idx}
                                                                                                   (apply dissoc
                                                                                                          (-> (nth-state result state-idx)
                                                                                                              (nth n-idx))
                                                                                                          (clojure.set/difference (set track-vars) (set state-vars))))
                                                                                            input-variable-x
                                                                                            input-variable-y)))))]
                                       {:x input-domain-x
                                        :y input-domain-y
                                        :z (vec (for [y input-domain-y
                                                      x input-domain-x]
                                                  (exp-fn x y)))}))})
               :body
               [:config
                :result
                :args]))

#_ (def flux-stability
  (dc/->let-fn (merge rse/base-fns
                      dependency-fns ;; don't want inline optimizations
                      telemetry-fns
                      {:node (dc/fnr [args]
                                     (:node args))
                       :parameter-bindings (dc/fnr [args]
                                                   (:parameter-bindings args))
                       :state-idx (dc/fnr [args]
                                          (:state-idx args))
                       :state-var-domain (dc/fnr [args]
                                                 (:state-var-domain args))
                       :vars-from-result (dc/fnr [args]
                                            (:vars-from-result args))
                       :body (dc/fnr [result exp-maps node node-mod velocity-vars state-var state-var-domain vars-from-result track-vars state-vars state-idx parameter-bindings]
                                     (let [emit (dj.math.lispemit/lisp-emitter)
                                           n-idx (mod node node-mod)
                                           base-exps (assoc (merge (:algebra-assignment exp-maps)
                                                                   (:differential-assignment exp-maps)
                                                                   parameter-bindings)
                                                       "n_idx" n-idx)
                                           fractional-velocity-exps (decompose-velocity-expressions base-exps
                                                                                                    state-var
                                                                                                    velocity-vars)
                                           vel-lines (mapv (fn [vel-var]
                                                             (let [f (eval `(fn [~(symbol state-var)]
                                                                              ~(emit
                                                                                (dme/inline-expression (fractional-velocity-exps vel-var)
                                                                                                       (dissoc (merge base-exps
                                                                                                                      (apply dissoc
                                                                                                                             (-> (nth-state result state-idx)
                                                                                                                                 (nth node))
                                                                                                                             (clojure.set/difference (set track-vars) (set state-vars) (set vars-from-result))))
                                                                                                               state-var)))))]
                                                               
                                                               {:x state-var-domain
                                                                :y (mapv f state-var-domain)
                                                                :series-label (str vel-var "," node)
                                                                :width 3}))
                                                           velocity-vars)]
                                       (into vel-lines
                                             [{:x state-var-domain
                                               :y (apply mapv + (map :y vel-lines))
                                               :series-label (str "velocity_" state-var "," node )
                                               :width 3}
                                              (let [v (-> (nth-state result state-idx)
                                                          (nth node)
                                                          (get state-var))]
                                                {:x [v]
                                                 :y [0]
                                                 :series-label (str "model_point_" state-var ": " v)
                                                 :width 3
                                                 :points true})])))})
               :body
               [:config
                :result
                :args]))

#_ (def phase-plot
  (dc/->let-fn (merge rse/base-fns
                      dependency-fns ;; don't want inline optimizations
                      {:node (dc/fnr [args]
                                     (:node args))
                       :input-variable-x (dc/fnr [args]
                                               (:input-variable-x args))
                       :input-variable-y (dc/fnr [args]
                                               (:input-variable-y args))
                       :state-range (dc/fnr [args]
                                            (:state-range args))
                       :body (dc/fnr [result node state-range input-variable-x input-variable-y]
                                     (let [[state-min state-max] state-range]
                                       [{:x (-> result
                                                :output
                                                (nth node)
                                                (get input-variable-x)
                                                (subvec state-min state-max))
                                         :y (-> result
                                                :output
                                                (nth node)
                                                (get input-variable-y)
                                                (subvec state-min state-max))
                                         :series-label "Phase Line"
                                         :width 3}]))})
               :body
               [:config
                :result
                :args]))

;; goal is to get data to look like [{:x ... :y :series-label :width} ...]
(defn distance-plots [{:keys [output]}
                      {:keys [block-size
                              block-ids
                              time-indexes
                              variable-name]}]
  (reduce (fn [ret block-id]
            (into ret
                  (mapv (fn [time-index]
                          {:x (vec (range block-size))
                           :y (mapv (fn [node-id-relative]
                                      (-> output
                                          (get (+ node-id-relative
                                                  (* block-id block-size)))
                                          (get variable-name)
                                          (get time-index)))
                                    (range block-size))
                           :series-label (str block-id ":" time-index)
                           :width 3})
                        time-indexes)))
          []
          block-ids))

