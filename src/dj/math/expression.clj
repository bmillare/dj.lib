;; Depends on org.clojure/core.logic, 0.8.11 works for example
(ns dj.math.expression
  (:require [dj.math.symbolics :as dm]
            [dj.math.parser :as dmp]
            [dj.algorithms.peg :as peg]
            [dj.math.maxima :as dmm]
            [dj.dispatch.compiledtreefn :as ctf]
            [clojure.core.logic :as ccl]
            [clojure.set :as cs]
            [dj]))

(defn direct-dependents [e]
  (reduce (fn [m child]
            (if (dmp/symbolic? child)
              (if-let [children (:children child)]
                (if (= (:op child) "map")
                  (conj m children)
                  (cs/union m (direct-dependents child)))
                (if (= (:op child) "var")
                  (conj m (:name child))
                  m))
              m))
          #{}
          (:children e)))

(defn direct-dependents-map [dmap]
  (dj/update-vals dmap
                  (comp direct-dependents :result dmp/parse)))

(defn has-dependents
  "
  :var-uses-map
  symbols ('variable' names) -> sets of symbol names the expression uses

  :variables
  list of variables to check if an symbol depends on

  returns map
  symbols -> variables (passed as an arg) that the symbols depends on
  "
  [var-uses-map variables]
  (let [vs (set variables)]
    (dj/var-let [rdependents (memoize
                              (fn [s]
                                (let [ds (var-uses-map s)
                                      ds-vs (cs/difference ds vs)]
                                  (if (empty? ds-vs)
                                    (cs/intersection vs ds)
                                    (cs/intersection vs
                                                     (apply cs/union
                                                            ds
                                                            (map rdependents
                                                                 ds-vs)))))))]
                (reduce (fn [m s]
                          (assoc m
                                 s
                                 (rdependents s)))
                        {}
                        (keys var-uses-map)))))

(defn reverse-dependencies
  "
  converts map of
  symbols -> variables they depend on

  variables -> symbols that depend on variables
  "
  [dmap]
  (reduce-kv (fn [m k ds]
               (reduce (fn [m' d]
                         (update-in m'
                                    [d]
                                    cs/union
                                    #{k}))
                       m
                       ds))
             {}
             dmap))

(defn recursive-dependents-map
  "
  returns map
  symbols -> variables (passed as an arg) that the symbols depends on
  "
  [exp-map variables]
  (-> exp-map
      (dj/update-vals direct-dependents)
      (has-dependents variables)))

(defn unbound-dependents-map
  "
  convenience fn

  returns map
  symbols -> empty sets
" [variables]
(zipmap variables (repeat #{})))

(defn inline-expression
  "
  exp: any expression, ie. a number, var, or operator
  exp-map: hashmap from var names -> expressions

  returns expression with all nested expressions inlined
  "
  [exp exp-map]
  ((fn recur-inline-exp [exp]
     (if (dmp/symbolic? exp)
       (let [{:keys [op children name]} exp]
         (case op
           "var" (if-let [e (exp-map name)]
                   (recur-inline-exp e)
                   exp)
           (assoc exp
                  :children (mapv recur-inline-exp children))))
       (if (number? exp)
         exp
         (throw (ex-info (str "expression type not recognized: " (type exp))
                         (dj.repl/local-context))))))
   exp))

(defn arraymap->symbolic-expression [m]
  (if (number? m)
    m
    (if-let [c (:children m)]
      (assoc m
             :children
             (mapv arraymap->symbolic-expression c))
      m)))

(defn reduce-constants
  "
  evalutes expression as much as possible by calling math fn
  "
  [e]
  (if (dmp/symbolic? e)
    (if (:name e)
      e
      (let [c (map reduce-constants (:children e))
            commutative-simplify (fn [f]
                                   (let [{:keys [symbolic constant]}
                                         (group-by (fn [e]
                                                     (if (dmp/symbolic? e)
                                                       :symbolic
                                                       :constant))
                                                   c)
                                         all (concat constant symbolic)]
                                     (if (dm/one? (count all))
                                       (first all)
                                       (reduce f all))))]
        (case (:op e)
          "*" (commutative-simplify dm/*)
          "+" (commutative-simplify dm/+)
          "-" (if (= 1 (count c))
                (dm/- (first c))
                (reduce dm/- c))
          "/" (dm/d (first c) (second c))
          "sqrt" (dm/sqrt (first c))
          "pow" (apply dm/pow c)
          "copy-sign" (apply dm/pow c)
          "ln" (dm/ln (first c))
          "log" (dm/ln (first c))
          "exp" (dm/exp (first c))
          e)))
    e))

(letfn [ ;;e is guaranteed to be a normalized ratio if possible
        (split-ratio-expression [state e]
          (if (dmp/symbolic? e)
            (let [{:keys [op children name]} e]
              (case op
                "/" (let [[child-n child-d] children]
                      (-> state
                          (update-in [:numerator]
                                     conj
                                     child-n)
                          (update-in [:denominator]
                                     conj
                                     child-d)))
                (update-in state
                           [:numerator]
                           conj
                           e))),
            (update-in state
                       [:numerator]
                       conj
                       e)))
        (swap-ratio [state]
          {:numerator (:denominator state)
           :denominator (:numerator state)})
        (flatten-* [es]
          (reduce (fn [v e]
                    (if (dmp/symbolic? e)
                      (case (:op e)
                        "*" (into v (:children e))
                        (conj v e))
                      (conj v e)))
                  []
                  es))
        (multiply-expression [es]
          (case (count es)
            1 (first es)
            {:op "*"
             :children es}))
        (callo [f n1 d1 n2 d2]
          (ccl/fresh [ln1 ld1 ln2 ld2]
            (ccl/== ln1 n1)
            (ccl/== ld1 d1)
            (ccl/== ln2 n2)
            (ccl/== ld2 d2)
            (f ln1 ld1 ln2 ld2)))
        (copysign [number magnitude sign]
          (ccl/conda
           [(ccl/== number {:op "-" :children [magnitude]})
            (ccl/== sign -1)]
           [(ccl/== number magnitude)
            (ccl/== sign 1)]))
        (simplifyo [n1 d1 n2 d2]
          (ccl/conda
           [(ccl/== n1 {:op "-" :children [d1]})
            (ccl/== n2 -1)
            (ccl/== d2 1)]
           [(ccl/== {:op "-" :children [n1]} d1)
            (ccl/== n2 -1)
            (ccl/== d2 1)]
           [(ccl/fresh [exponent magnitude sign]
              (copysign d1 magnitude sign)
              (ccl/== {:op "pow" :children [magnitude exponent]} n1)
              (ccl/project [exponent magnitude]
                           (ccl/== n2 (dm/pow magnitude
                                              (dec exponent))))
              (ccl/== d2 sign))]
           [(ccl/fresh [exponent magnitude sign]
              (copysign n1 magnitude sign)
              (ccl/== {:op "pow" :children [magnitude exponent]} d1)
              (ccl/project [exponent magnitude]
                           (ccl/== d2 (dm/pow magnitude
                                              (dec exponent))))
              (ccl/== n2 sign))]
           [(ccl/fresh [nv dv n-exponent d-exponent]
              (ccl/== {:op "pow" :children [nv n-exponent]} n1)
              (ccl/== {:op "pow" :children [dv d-exponent]} d1)
              (ccl/project [nv n-exponent d-exponent]
                           (let [diff (- n-exponent
                                         d-exponent)]
                             (if (pos? diff)
                               (ccl/all
                                (ccl/== n2 (dm/pow nv diff))
                                (ccl/== d2 1))
                               (ccl/all
                                (ccl/== n2 1)
                                (ccl/== d2 (dm/pow nv (- diff))))))))]))
        (simplify [n d]
          (if (= n d)
            [1 1]
            (-> (ccl/run 1 [q]
                  (ccl/fresh [qn qd]
                    (ccl/== q [qn qd])
                    (callo simplifyo n d qn qd)))
                first)))
        (cancel [state]
          (let [{:keys [numerator denominator]} state
                n' (flatten-* numerator)
                d' (flatten-* denominator)]
            (loop [n n'
                   n-idx 0
                   d d'
                   d-idx 0]
              (if (< n-idx (count n))
                (if (< d-idx (count d))
                  (let [ne (n n-idx)]
                    (if (dmp/symbolic? ne)
                      (let [de (d d-idx)]
                        (if (dmp/symbolic? de)
                          (if-let [s (simplify ne de)]
                            (let [[ne' de'] s]
                              (recur (assoc n
                                            n-idx
                                            (arraymap->symbolic-expression ne')) (inc n-idx)
                                     (assoc d
                                            d-idx
                                            (arraymap->symbolic-expression de')) 0))
                            (recur n n-idx
                                   d (inc d-idx)))
                          (recur n n-idx
                                 d (inc d-idx))))
                      (recur n (inc n-idx)
                             d 0)))
                  (recur n (inc n-idx)
                         d 0))
                (dm/d (reduce dm/* n)
                      (reduce dm/* d))))))]
  (defn normalize-ratio
  "

  A normalized ratio is a division put into the form in prefix notation

  (/ (* numerator...) (* denominator...))

  and (* numerator...) and (* denominator) reduce to just numerator or
  denominator if singular

  "
  [e]
    (-> (if (dmp/symbolic? e)
          (let [{:keys [op children name]} e]
            (case op
              "/" (case (count children)
                    0 (throw (ex-info "empty division"
                                      (dj.repl/local-context)))
                    1 (normalize-ratio (first children))
                    (let [numerator (normalize-ratio (first children))
                          denominator (map normalize-ratio (rest children))]
                      (-> {:numerator []
                           :denominator []}
                          (as-> state
                              (reduce split-ratio-expression
                                      state
                                      denominator))
                          swap-ratio
                          (split-ratio-expression numerator)
                          cancel))),
              "var" e,
              (assoc e
                     :children
                     (mapv normalize-ratio children))))
          e)
        reduce-constants)))

(defn exponent-names [expr]
  ((fn walk [ret e]
     (if (dmp/symbolic? e)
       (case (:op e)
         "pow" (let [exponent (-> e
                                  :children
                                  second)]
                 (if (dmp/symbolic? exponent)
                   (conj ret
                         (:name exponent))
                   ret))
         (if (empty? (:children e))
           ret
           (reduce walk
                   ret
                   (:children e))))
       ret))
   #{}
   expr))

(defn pow-e->exp [e]
  (if (dmp/symbolic? e)
    (case (:op e)
      "pow" (let [[base exponent] (:children e)]
              (if (dmp/symbolic? base)
                (if (= (:name base)
                       "%e")
                  {:op "exp"
                   :children [exponent]}
                  (assoc e
                         :children (mapv pow-e->exp (:children e))))
                e))
      "var" e
      (assoc e
             :children (mapv pow-e->exp (:children e))))
    e))

(defn substitute-constants [expr exceptions end-cont]
  (letfn [(wrap-end [e _ m]
            (end-cont e m))
          (convert [e n m next-cont]
            (if (dmp/symbolic? e)
              (case (:op e)
                "var" (peg/bounce next-cont
                                  e
                                  n
                                  m)
                (peg/bounce walk
                            (:children e)
                            []
                            n
                            m
                            (fn [new-children n' m']
                              (peg/bounce next-cont
                                          (assoc e
                                                 :children new-children)
                                          n'
                                          m'))))
              (if (exceptions e)
                (peg/bounce next-cont
                            e
                            n
                            m)
                (if (== e 1.0)
                  (peg/bounce next-cont
                              (long 1)
                              n
                              m)
                  (let [vname (str "x" n)
                        v (dm/vare vname)
                        n' (inc n)]
                    (peg/bounce next-cont
                                v
                                n'
                                (assoc m
                                       vname e)))))))
          (walk [old-children
                 new-children
                 n
                 m
                 end-walk-cont]
            (if (empty? old-children)
              (peg/bounce end-walk-cont
                          new-children
                          n
                          m)
              (let [c (first old-children)]
                (peg/bounce convert
                            c
                            n
                            m
                            (fn [nc
                                 n'
                                 m']
                              (peg/bounce walk
                                          (next old-children)
                                          (conj new-children nc)
                                          n'
                                          m'
                                          end-walk-cont))))))]
    (trampoline convert
                expr
                0
                {}
                wrap-end)))

(def base-maxima
  {:exp-names (ctf/wrap-fn exponent-names
                           [:expr])
   :maxima-e (ctf/wrap-fn dmm/emit
                          [:expr])
   :declare-nonintegers (ctf/fn [exp-names]
                          (apply str (for [n exp-names]
                                       (str "declare(" n ",noninteger)$"))))
   :maxima-simp (ctf/fn [maxima-e]
                  (-> (str dmm/default-settings maxima-e ";")
                      dmm/call-maxima*
                      :out
                      dmm/parse
                      :result
                      pow-e->exp))})

(def maxima-simplify
  (ctf/compiled-treefn base-maxima
                       :maxima-simp
                       [:expr]))

(def linearize-singularity-fns
  {:singularity-output (ctf/fn [declare-nonintegers maxima-e vname singularity-input]
                         (-> (str dmm/default-settings declare-nonintegers "limit(" maxima-e "," vname "," singularity-input ");")
                             dmm/call-maxima*
                             :out
                             dmm/parse
                             :result
                             pow-e->exp))
   :singularity-slope (ctf/fn [declare-nonintegers maxima-e vname singularity-input]
                        (-> (str dmm/default-settings declare-nonintegers "limit(diff(" maxima-e "," vname ")," vname "," singularity-input ");")
                            dmm/call-maxima*
                            :out
                            dmm/parse
                            :result
                            pow-e->exp))
   :v (ctf/wrap-fn dm/vare
                   [:vname])
   :out-expression (ctf/fn [v expr min-input max-input singularity-slope singularity-input singularity-output]
                     (dm/? {:op "and"
                            :children [{:op ">"
                                        :children [v min-input]}
                                       {:op "<"
                                        :children [v max-input]}]}
                           (dm/+ (dm/* singularity-slope (dm/- v singularity-input))
                                 singularity-output)
                           expr))})

(def maxima-linearize-singularity
  (ctf/compiled-treefn (merge base-maxima
                              linearize-singularity-fns)
                       :out-expression
                       [:expr :vname :singularity-input :min-input :max-input]))

(defn linearize-singularity [expr exp-map vname singularity-input min-input max-input]
  (-> expr
      maxima-simplify
      (substitute-constants #{singularity-input} (fn [e m]
                                                   (-> e
                                                       (maxima-linearize-singularity vname
                                                                                     singularity-input
                                                                                     min-input
                                                                                     max-input)
                                                       (inline-expression m))))))
