(ns dj.math.solver
  (:require [dj.math.parser :as mp]))

(defn remove-child [children idx]
  (into (subvec children 0 idx)
        (subvec children (inc idx))))


(defn L=plus
  "+ operator assumed
  returns L'=T as [L' R']"
  [L R idx]
  (let [R-children (:children R)
        target (R-children idx)]
    [{:op "-"
      :children (into [L] (remove-child R-children idx))}
     target]))

(defn L=minus
  "- operator assumed
  returns L'=T as [L' R']"
  [L R idx]
  (let [R-children (:children R)
        target (R-children idx)]
    (case idx
      0 [{:op "+"
          :children (conj (remove-child R-children idx) L)}
         target]
      [{:op "-"
        :children (conj (remove-child R-children idx) L)}
       target])))

(defn L=mult
  "* operator assumed
  returns L'=T as [L' R']"
  [L R idx]
  (let [R-children (:children R)
        target (R-children idx)]
    (case (count R-children)
      1 [L target]
      2 [{:op "/"
          :children (into [L] (remove-child R-children idx))}
         target]
      [{:op "/"
        :children [L {:op "*"
                      :children (remove-child R-children idx)}]}
       target])))

(defn L=div
  "/ operator assumed
  returns L'=T as [L' R']"
  [L R idx]
  (let [R-children (:children R)
        target (R-children idx)]
    (case idx
      0 [{:op "*" :children (conj (remove-child R-children idx) L)}
         target]
      [{:op "/"
        :children (assoc R-children idx L)}
       target])))

(defn walk-op-chain
  "return chain of [op index] which describes path to target
  expression from top level expression"
  [expression target]
  (let [-walk-op-chain
        (fn -walk-op-chain [e]
          (if (= e target)
            ;; found sentinel
            [] 
            (if (mp/symbolic? e)
              (let [children (:children e)]
                (if (empty? children)
                  nil
                  (loop [cs children
                         i 0]
                    (let [c (first cs)]
                      (if c
                        (let [c-chain (-walk-op-chain c)]
                          (if c-chain
                            (into [[(:op e) i]]
                                  c-chain)
                            (recur (rest cs)
                                   (inc i))))
                        nil)))))
              nil)))]
    (-walk-op-chain expression)))

(defn walk-replace
  "return expression with all target sub-expressions replaced with
  supplied expression"
  [base target replacement]
  (let [-walk-replace
        (fn -walk-replace [e]
          (if (= e target)
            replacement
            (if (mp/symbolic? e)
              (let [children (:children e)]
                (if children
                  (update e
                          :children
                          (fn [children]
                            (mapv -walk-replace children)))
                  e))
              e)))]
    (-walk-replace base)))

(defn solve
  "solves for target expression assumed in the rhs of an equation

  Assumptions:
  - target is singular in rhs
  - handles only +, -, *, and /"
  [lhs rhs r-target]
  (let [chain (walk-op-chain rhs r-target)]
    (if chain
      (loop [links chain
             equation [lhs rhs]]
        (let [link (first links)]
          (if link
            (let [[op idx] link
                  [L R] equation
                  f (case op
                      "+" L=plus
                      "-" L=minus
                      "*" L=mult
                      "/" L=div)]
              (recur (rest links)
                     (f L R idx)))
            equation)))
      (throw (ex-info "target not found in rhs"
                      (dj.repl/local-context))))))

(comment

  (let [parse (fn [txt]
                (:result (dj.math.parser/parse txt)))
        emit (dj.math.cemit/c-emitter)
        L (parse "L")
        R (parse "5+3*(T*(2+3)-3)")
        T (parse "T")
        L' (-> (solve L
                      R
                      T)
               first)]
    {(str (emit L) " = " (emit R))
       (str (emit L') " = " (emit T))}
    )



  (let [parse (fn [txt]
                (:result (dj.math.parser/parse txt)))]
    (walk (parse
           "5+1/T-3")
          (parse "T"))
    )
  (let [emit (dj.math.cemit/c-emitter)
        
        parse (fn [txt]
                (:result (dj.math.parser/parse txt)))
        L (parse "L")
        emit= (fn [R-txt f idx]
                [(str (emit L) "=" R-txt)
                 (let [[L' R'] (f L (parse R-txt) idx)]
                         (str (emit L') "=" (emit R')))])]
    (into
     {}
     (for [[R-txt f idx] [["5+T+2" L=plus 1]
                          ["T-1-2-3" L=minus 0]
                          ["3*T" L=mult 1]
                          ["3*T*2" L=mult 1]
                          ["T/2" L=div 0]
                          ["2/T/3" L=didj.]]
       (emit= R-txt f idx))))

  (let [emit (dj.math.cemit/c-emitter)
        
        parse (fn [txt]
                (:result (dj.math.parser/parse txt)))
        e (parse "x+y+1-(z*T+(3*T))")
        T (parse "1")
        r (parse "2")]
    (emit (walk-replace e T r)))
  )
