(ns dj.math.latexemit
  (:require [dj]
            [dj.math.parser :as dmp] ; [ ] will we need this?
            [dj.dispatch.plurality :as dp]))

(defn l [op & args]
  (str "\\" op (apply str (map
                           (fn [a]
                             (str "{" a "}"))
                           args))))

(defn emitter [raw-name->latex-name-map]
  (dp/->recursive-simple-multi-fn
   ;; implementations
   {"var"
    (fn [emit]
      (fn [{:keys [name]}]
        (raw-name->latex-name-map name)))
    java.lang.Long
    (fn [emit]
      (fn [x]
        #_(str (double x))
        (str x)))
    java.lang.Double
    (fn [emit]
      (fn [x]
        (str x)))}
   ;; base-default
   (fn [emit]
     (fn [{:keys [op children]}]
       (let [nowrap-interpose-children
             ;; for this, we won't wrap in parenthesis to allow
             ;; optimization of removal
             ;;  - intead we will lookahead nesting checks to see if
             ;;    they are necessary, its the caller's responsibility
             (fn [sep]
               (apply str (interpose sep (map emit children)))
               #_ (str "(" (apply str (interpose sep (map emit children))) ")"))
             interpose-children
             (fn [sep]
               (apply str (interpose sep (map
                                          (fn [child]
                                            (case (dmp/symbolic-expression-dispatcher child)
                                              "+" (str "(" (emit child) ")")
                                              "-" (if (= 1 (count (:children child)))
                                                    (emit child)
                                                    (str "(" (emit child) ")"))
                                              (emit child)))
                                          children))))]
         (case op
           "+" (nowrap-interpose-children "+")
           "-" (if (= (count children) 1)
                 (str "-" (let [child (first children)]
                            (case (dmp/symbolic-expression-dispatcher child)
                                              "+" (str "(" (emit child) ")")
                                              "-" (if (= 1 (count (:children child)))
                                                    (emit child)
                                                    (str "(" (emit child) ")"))
                                              (emit child))))
                 (nowrap-interpose-children "-"))
           "*" (interpose-children " ")
           "/" (l "frac"
                  (emit (first children))
                  (emit (second children)))
           "==" (interpose-children "==")
           ">" (interpose-children ">")
           "<" (interpose-children "<")
           "<=" (interpose-children "<=")
           ">=" (interpose-children ">=")
           "!=" (interpose-children "!=")
           "or" (interpose-children "||")
           "and" (interpose-children "&&")
           "float" (apply str (map emit children))
           "double" (apply str (map emit children))
           "long" (str (emit (first children)))
           "log" (str (l "log") " " (emit (first children)))
           "sqrt" (l "sqrt" (emit (first children)))
           "exp" (str (l "mathrm" "e") "^{" (emit (first children)) "}")
           "map" (str "{" (emit (first children)) "}_{" (emit (second children)) "}")
           ;; later switch this to "cases" https://tex.stackexchange.com/questions/32140/how-to-write-a-function-piecewise-with-bracket-outside
           "if" (let [[c t f] children]
                  (if (and (number? t)
                           (number? f)
                           (= (double t) (double f)))
                    t
                    (str "(("(emit (first children)) ") ? "
                         (emit (second children)) " : "
                         (emit (nth children 2)) ")")))
           ;; for pow, we might want to consider the two cases: 1)
           ;; superscript should be part of main symbol compoent, 2)
           ;; superscript should be for the whole complex symbol
           "pow" (str "{" (emit (first children)) "}^{" (emit (second children)) "}")))))
   dmp/symbolic-expression-dispatcher))

(comment
  (let [lhs->rhs {"ADPm"
                  {:op "-",
                   :children
                   [{:op "var", :name "VANT"}
                    {:op "var", :name "VATPase"}
                    {:op "var", :name "VSL"}]},
                  "H2O2gen" 0.0,
                  "Succ"
                  {:op "-",
                   :children [{:op "var", :name "VSL"} {:op "var", :name "VSDH"}]},
                  "ISOC"
                  {:op "-",
                   :children [{:op "var", :name "VACO"} {:op "var", :name "VIDH"}]},
                  "SOm"
                  {:op "-",
                   :children
                   [{:op "var", :name "VpSO"}
                    {:op "var", :name "VSODm"}
                    {:op "var", :name "Vt2SO2m"}]}}
        emit (emitter identity)]
    (mapv emit (vals lhs->rhs)))


  )
