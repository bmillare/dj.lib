(ns dj.template.c
  (:require [clojure.string :as cs]))

(declare emit)

(defprotocol IEmitForm
  (-emit-form [form]))

(defprotocol IBodySemicolon
  (-body-semicolon? [form]))

(defn emit-body [forms]
  (try
    (apply str (map (fn [form]
                      (if (-body-semicolon? form)
                        (str (-emit-form form) ";\n")
                        (-emit-form form)))
                    forms))
    (catch Exception e
      (throw (ex-info "error emitting"
                      {:forms forms}
                      e)))))

(defn emit-tag [o]
  (try
    (case (first o)
      :declare (cs/join " " (map -emit-form (rest o)))
      :set (str (cs/join " " (map -emit-form (rest (drop-last o))))
                " = "
                (-emit-form (last o)))
      :block (if (= (count o) 2)
               (str "{" (emit-body (last o)) "}")
               (str (cs/join " " (map -emit-form (rest (drop-last o))))
                    "{" (emit-body (last o)) "}"))
      :body (emit-body (rest o))
      :aget (str (second o)
                 (apply str
                        (map (fn [index-v]
                               (str "[" index-v "]"))
                             (rest (rest o)))))
      (str (-emit-form (first o))
           "("
           (cs/join "," (map -emit-form (rest o)))
           ")"))
    (catch Exception e
      (throw (ex-info "error emitting tag"
                      {:form o}
                      e)))))

(extend-protocol IEmitForm
  clojure.lang.Symbol
  (-emit-form [form]
    (name form))
  clojure.lang.Keyword
  (-emit-form [form]
    (name form))
  String
  (-emit-form [form]
    form)
  nil
  (-emit-form [form]
    nil)
  Number
  (-emit-form [form]
    (str form))
  Object
  (-emit-form [form]
    (emit-tag form)))

(defn tag-semicolon? [form]
  (case (first form)
    :block false
    :body false
    true))

(extend-protocol IBodySemicolon
  clojure.lang.Symbol
  (-body-semicolon? [form]
    false)
  clojure.lang.Keyword
  (-emit-form [form]
    false)
  String
  (-body-semicolon? [form]
    false)
  nil
  (-body-semicolon? [form]
    false)
  Number
  (-body-semicolon? [form]
    false)
  Object
  (-body-semicolon? [form]
    (tag-semicolon? form)))

(defn emit [form]
  (try
    (-emit-form form)
    (catch Exception e
      (throw (ex-info "error emitting"
                      {:form form}
                      e)))))



(comment
  (emit [:body
         [:set 'const 'long 'NEQ '__num_vars]
         [:declare 'realtype 't]])
  "const long NEQ = __num_vars;\nrealtype t;\n"

  (emit [:body ['Ith 'abstol 1]])
  "Ith(abstol,1);\n"

  (emit [:body [:set ['Ith 'abstol 1] 'ATOL1]])
  "Ith(abstol,1) = ATOL1;\n"

  (emit [:block
         ['for [:body
                [:set 'int 'i 0]
                [:declare 'i '< 25]
                '++i]]
         [[:set ['Ith 'abstol 1] [:set 'ATOL1 0]]]])
  "for(int i = 0;\ni < 25;\n++i){Ith(abstol,1) = ATOL1 = 0;\n}"

  (emit [:aget 'J 15 20])
  "J[15][20]"
  )
