(ns dj.math.parser
  (:require [dj.algorithms.peg :as ap]
            [dj.dispatch.graphfn :as gf]))

(def parse-base
  {:id (gf/fn #{} #{}
         (ap/t #"\p{Alpha}\w*"))
   :ws (gf/fn #{} #{} (ap/t #"(?:\s|;)*"))
   :wrap-ws (gf/fn #{ws} #{}
              (fn [t]
                (ap/s ws t ws)))
   :int-num (gf/fn #{} #{}
              (ap/alt (ap/t #"\d+")
                      (fn [x]
                        (long (Integer/parseInt x)))))
   :double-num (gf/fn #{} #{}
                 (ap/alt (ap/t #"(?:\d+(\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?")
                         (fn [x]
                           (Double/parseDouble x))))
   :plus-minus (gf/fn #{} #{}
                 (ap/t #"[+\-]"))
   :mult-div (gf/fn #{} #{}
               (ap/t #"[*/]"))
   :equalities (gf/fn #{} #{}
                 (ap/t #"==|\!=|>=|<=|[><]"))
   :logicals (gf/fn #{} #{}
               (ap/t #"\|\||&&"))
   :comma (gf/fn #{} #{}
            (ap/t #","))
   :lparen (gf/fn #{} #{}
             (ap/t #"\("))
   :rparen (gf/fn #{} #{}
             (ap/t #"\)"))
   :cast (gf/fn #{} #{}
           (ap/t #"double|float|long"))
   :carrot (gf/fn #{} #{}
             (ap/t #"\^"))
   :infix-node (gf/fn #{} #{}
                 (fn pm [[f r]]
                   (if r
                     {:op (let [op (first (first r))]
                            (case op
                              "||" "or"
                              "&&" "and"
                              "^" "pow"
                              op))
                      :children (vec (list* f (map second r)))}
                     f)))
   :infix-couple (gf/fn #{} #{}
                   (fn [c nc]
                     (fn [[f r]]
                       (if r
                         (let [ret (reduce (fn [ret [op e]]
                                             (if (= op c)
                                               (update-in ret
                                                          [c]
                                                          conj
                                                          e)
                                               (update-in ret
                                                          [nc]
                                                          conj
                                                          e)))
                                           {c [f]
                                            nc []}
                                           r)]
                           (let [rnc (ret nc)]
                             (if (empty? rnc)
                               {:op c
                                :children (ret c)}
                               {:op nc
                                :children (into [(let [rc (ret c)]
                                                   (if (= 1
                                                          (count rc))
                                                     (first rc)
                                                     {:op c
                                                      :children rc}))]
                                                rnc)})))
                         f))))
   
   :mult-expr (gf/fn #{mult-div infix-couple} #{atom}
                (ap/alt (ap/s atom
                              (ap/*
                               (ap/s mult-div
                                     atom)))
                        (infix-couple "*" "/")))
   :plus-expr (gf/fn #{plus-minus infix-couple} #{mult-expr}
                (ap/alt (ap/s mult-expr
                              (ap/*
                               (ap/s plus-minus
                                     mult-expr)))
                        (infix-couple "+" "-")))
   :equality-expr (gf/fn #{equalities infix-node} #{plus-expr}
                    (ap/alt (ap/s plus-expr
                                  (ap/* (ap/s equalities
                                              plus-expr)))
                            infix-node))
   :logicals-expr (gf/fn #{logicals infix-node} #{equality-expr}
                    (ap/alt (ap/s equality-expr
                                  (ap/* (ap/s logicals
                                              equality-expr)))
                            infix-node))
   :cond-expr (gf/fn #{wrap-ws} #{logicals-expr}
                (ap/alt (ap/s logicals-expr
                              (ap/? (ap/s (wrap-ws (ap/t #"\?"))
                                          logicals-expr
                                          (wrap-ws (ap/t #":"))
                                          logicals-expr)))
                        (fn [[c r]]
                          (if r
                            (let [[_ t _ f] r]
                              {:op "if"
                               :children [c t f]})
                            c))))
   :atom-no-ws (gf/fn #{int-num
                        double-num
                        lparen
                        rparen
                        ws
                        cast
                        comma
                        id}
                 #{atom
                   expr}
                 (ap/alt (ap/s (ap/? (ap/t #"-"))
                               (ap/| (ap/alt (ap/s int-num (ap/!? (ap/| (ap/t #"\.")
                                                                        (ap/t #"[eE]"))))
                                             first)
                                     double-num
                                     (ap/alt (ap/s lparen
                                                   ws
                                                   cast
                                                   ws
                                                   rparen
                                                   ws
                                                   atom)
                                             (fn [[_ _ id _ _ _ a]]
                                               {:op id
                                                :children [a]}))
                                     (ap/alt (ap/s id
                                                   ws
                                                   lparen
                                                   rparen)
                                             (fn [[id _ _ _]]
                                               {:op id
                                                :children []}))
                                     (ap/alt (ap/s id
                                                   ws
                                                   lparen
                                                   expr
                                                   (ap/* (ap/s comma expr))
                                                   rparen)
                                             (fn [[id _ _ f r _]]
                                               {:op id
                                                :children (vec (list* f (map second r)))}))
                                     (ap/alt id
                                             (fn [x]
                                               {:op "var"
                                                :name x}))
                                     (ap/alt (ap/s lparen
                                                   expr
                                                   rparen)
                                             second)))
                         (fn [[minus atom]]
                           (if minus
                             {:op "-"
                              :children [atom]}
                             atom))))
   :atom (gf/fn #{ws} #{atom-no-ws}
           (ap/alt (ap/s ws atom-no-ws ws)
                   second))})

(let [expr (-> parse-base
               (gf/mutual-graphfn :expr
                                  {:expr :cond-expr})
               :expr
               deref)]
  (defn parse [txt]
    (ap/parse expr txt)))

(defprotocol Isymbolic-expression-check
  (symbolic? [this])
  (math-type [this]))

(extend-protocol Isymbolic-expression-check
  nil
  (symbolic? [form] false)
  (math-type [form] nil)
  Number
  (symbolic? [form] false)
  (math-type [form] (type form))
  clojure.lang.IPersistentMap
  (symbolic? [form] (contains? form :op))
  (math-type [form] (if (contains? form :op)
                      :symbolic-expression
                      (type form)))
  Object
  (symbolic? [form] false)
  (math-type [form] (type form)))

;; this doesn't require metadata stating it is a :symbolic-expression,
;; however, much of the rest of dj.math does depend on the fact that
;; the type is :symbolic-expression, ideally in the future, we define
;; a different typer, like a typer protocol that then others can
;; extend to make sure the correct effective type is mapped
(defn symbolic-expression-dispatcher
  [x]
  (if (map? x)
    (if (and (contains? x :op)
             (or (contains? x :children)
                 (contains? x :name)))
      (:op x)
      (type x))
    (type x)))
