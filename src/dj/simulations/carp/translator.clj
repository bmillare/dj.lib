(ns dj.simulations.carp.translator
  (:require [dj.algorithms.peg :as ap]
            [dj.dispatch.graphfn :as gf]))

(defn parse-dotmodel2
  "converts lines from a model file, into a list of ode's and algebra
  data"
  [txt]
  ;; this implementation is using the experimental parsing expression
  ;; grammar toolkit I created. It is a continuation style passing
  ;; design and uses trampolines for performance (does not blow
  ;; stack).
  (let [floating-point (ap/alt (ap/t #"[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?")
                               #(Double/parseDouble %))
	id (ap/t #"[a-zA-Z_][a-zA-Z_0-9]*")
	linecomment (ap/alt (ap/t #"\s*//.*?(\n|\r)")
                            #(vector :comment %))
	blockcomment (ap/alt (ap/t #"\s*/\*(?s).*?\*/")
                             #(vector :comment %))
	whitespace (ap/t #"\s*")
	equalsign (ap/t #"\s*=")
        assignment-sign (ap/t #"\s*(?:\*|\+)?=")
	expression (ap/t #"(?s).+?;")
	semicolon (ap/t #";")
	;; need to create different types of assignment, one for default initialize, and for differential variables
        ;; rush-larsen data in the .model file is messing things up, need to make a decision on how to proceed
	diff (ap/t #"diff_")
        trace (ap/t #"TRACE_")
	init-id (ap/t #"[a-zA-Z_][a-zA-Z_0-9]*?_init")
	assignment-type (ap/| (ap/alt (ap/s diff id)
                                      (fn [[_ v]]
                                        [:differential v]))
                              (ap/alt (ap/s trace id)
                                      (fn [[trace-name v]]
                                        [:trace (str trace-name v)]))
                              (ap/alt init-id
                                      (fn [v]
                                        [:initialize (apply str (drop-last 5 v))]))
                              (ap/alt id
                                      (fn [v]
                                        [:assignment v])))
        modifier (ap/alt (ap/s (ap/t #"\s*\.")
                               (ap/t #"[a-zA-Z_]+")
                               (ap/t #"\s*\(\s*")
                               (ap/t #"([a-zA-Z_0-9]|\.|-|,|/|\s)*")
                               (ap/t #"\s*\)\w*;"))
                         (fn [[_ m _ a _]]
                           ;; We want to create a modifier,
                           ;; its argument depends on what
                           ;; we define the modifier to do,
                           ;; so we need to dispatch on
                           ;; modifier name
                           (let [pd #(Double/parseDouble %)]
                             [:modifier [m
                                         ((case m
                                            "param" seq
                                            "external" identity
                                            "shared_algebra" seq
                                            "shared_sv" seq
                                            "nodal" seq
                                            "lb" pd
                                            "ub" pd
                                            "method" identity
                                            "store" seq
                                            "lookup" identity
                                            "regional" identity
                                            "trace" identity
                                            "units" identity
                                            identity)
                                          a)]])))
	assignment (ap/alt (ap/s whitespace assignment-type assignment-sign whitespace expression (ap/? modifier))
                           (fn [[_ [k v] as _ e m]]
                             (let [binding [k [v e]]]
                               (if m
                                 [:group [binding
                                          [:declare [v [m] as]]]]
                                 binding))))
        if-block (let [root-id :m-if-block
                       gfns {:lines
                             (gf/fn #{} #{:m-if-block}
                               (ap/+ (ap/| assignment m-if-block linecomment blockcomment)))
                             :if-elif-block
                             ;; not currently implementing recursive expressions
                             (gf/fn #{} #{:lines}
                               (ap/s (ap/t #"\s*(el)?if\s*\(")
                                     (ap/t #"[^\)]+") ;; note this line
                                     (ap/t #"\)\s*\{\s*")
                                     lines
                                     (ap/t #"\s*\}\s*")))
                               :else-block
                               (gf/fn #{} #{:lines}
                                 (ap/s (ap/t #"else\s*")
                                       (ap/? (ap/| linecomment blockcomment))
                                       (ap/t #"\s*\{\s*")
                                       lines
                                       (ap/t #"\s*\}\s*")))
                               :m-if-block
                               (gf/fn #{} #{:if-elif-block
                                            :else-block}
                                 (ap/s (ap/+ if-elif-block)
                                       (ap/? else-block)))}]
                       (-> gfns
                           (gf/mutual-graphfn root-id)
                           root-id
                           deref))
	           declaration (ap/alt (ap/s whitespace id whitespace semicolon)
                                       (fn [[_ i _]]
                                         i))
	           end-modifiers (ap/+ (ap/| modifier linecomment blockcomment))
	           group (ap/alt (ap/s (ap/t #"\s*group\s*\{")
                                       (ap/+ (ap/| declaration assignment if-block linecomment blockcomment))
                                       (ap/t #"\s*\}")
                                       end-modifiers)
                                 (fn [[_ ds _ ms]]
                                   [:group (for [d ds]
                                             [:declare [d (filter #(= (first %)
                                                                      :modifier)
                                                                  ms)]])]))
	           declare-modifier (ap/alt (ap/s declaration end-modifiers)
                                            (fn [[d ms]]
                                              [:declare [d (filter #(= (first %)
                                                                       :modifier)
                                                                   ms)]]))
	           block (ap/| assignment if-block group declare-modifier linecomment blockcomment)
	           blocks (ap/alt (ap/+ block)
                                  (fn [s]
                                    ;; flatten
                                    (vec
                                     (reduce (fn [coll x]
                                               (let [[k v] x]
                                                 (if (= k :group)
                                                   (concat coll v)
                                                   (concat coll (list x)))))
                                             (list)
                                             s))))]
    (ap/parse blocks txt)))

;; dotmodel file

;; Currently does not fully comply with current spec of .model file
;; missing a_* b_*

;; More specifically, its gating
;; -from carp-*/LIMPET/im.py, gates are prefixed like

;; gate_regex = { 'alpha': re.compile(r'(?:alpha_|a_)(.*)'),
;;                 'beta': re.compile(r'(?:beta_|b_)(.*)'),
;;                  'tau': re.compile(r'(?:t|tau)_(.*)'),
;;                  'inf': re.compile(r'(.*)_(?:inf|infinity|st)$'),
;;              }
;; basically, all rhs of _ is the same variable, and each prefix specifies a particular component
;; this notation only applies to state variables, algebra is not affected

;; basically, if the rhs of algebra label is a state variable (for alpha_|a_|beta_|b_|t_|tau_)
;; or if lhs is state variable and rhs is (_inf|_infinity|_st)

;; -note two groupings, alpha/beta, or tau/inf
;;
;; -then just add dependency that sv depends on that variable

;; Returns a list of statements of the form [:type content*]
;; types:
;; [:assignment [name value]]
;; [:comment string]
;; [:initialize [name value]]
;; [:differential [name expression]]
;; [:declare [name (list of modifiers)]]
;; [:modifier [type & args]]
;; ?[:group 
(defn parse-dotmodel
  "converts lines from a model file, into a list of ode's and algebra
  data"
  [txt]
  ;; this implementation is using the experimental parsing expression
  ;; grammar toolkit I created. It is a continuation style passing
  ;; design and uses trampolines for performance (does not blow
  ;; stack).
  (let [floating-point (ap/alt (ap/t #"[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?")
                               #(Double/parseDouble %))
	id (ap/t #"[a-zA-Z_][a-zA-Z_0-9]*")
	linecomment (ap/alt (ap/t #"\s*//.*?(\n|\r)")
                            #(vector :comment %))
	blockcomment (ap/alt (ap/t #"\s*/\*(?s).*?\*/")
                             #(vector :comment %))
	whitespace (ap/t #"\s*")
	equalsign (ap/t #"\s*=")
	expression (ap/t #"(?s).+?;")
	semicolon (ap/t #";")
	;; need to create different types of assignment, one for default initialize, and for differential variables
        ;; rush-larsen data in the .model file is messing things up, need to make a decision on how to proceed
	diff (ap/t #"diff_")
        trace (ap/t #"TRACE_")
	init-id (ap/t #"[a-zA-Z_][a-zA-Z_0-9]*?_init")
	assignment-type (ap/| (ap/alt (ap/s diff id)
                                      (fn [[_ v]]
                                        [:differential v]))
                              (ap/alt (ap/s trace id)
                                      (fn [[trace-name v]]
                                        [:trace (str trace-name v)]))
                              (ap/alt init-id
                                      (fn [v]
                                        [:initialize (apply str (drop-last 5 v))]))
                              (ap/alt id
                                      (fn [v]
                                        [:assignment v])))
        modifier (ap/alt (ap/s (ap/t #"\s*\.")
                               (ap/t #"[a-zA-Z_]+")
                               (ap/t #"\s*\(\s*")
                               (ap/t #"([a-zA-Z_0-9]|\.|-|,|\s)*")
                               (ap/t #"\s*\)\w*;"))
                         (fn [[_ m _ a _]]
                           ;; We want to create a modifier,
                           ;; its argument depends on what
                           ;; we define the modifier to do,
                           ;; so we need to dispatch on
                           ;; modifier name
                           (let [pd #(Double/parseDouble %)]
                             [:modifier [m
                                         ((case m
                                            "param" seq
                                            "external" identity
                                            "shared_algebra" seq
                                            "shared_sv" seq
                                            "nodal" seq
                                            "lb" pd
                                            "ub" pd
                                            "method" identity
                                            "store" seq
                                            "lookup" identity
                                            "regional" identity
                                            "trace" identity
                                            "units" identity
                                            identity)
                                          a)]])))
	assignment (ap/alt (ap/s whitespace assignment-type equalsign whitespace expression (ap/? modifier))
                           (fn [[_ [k v] _ _ e m]]
                             (let [binding [k [v e]]]
                               (if m
                                 [:group [binding
                                          [:declare [v [m]]]]]
                                 binding))))
	declaration (ap/alt (ap/s whitespace id whitespace semicolon)
                            (fn [[_ i _]]
                              i))
	end-modifiers (ap/+ (ap/| modifier linecomment blockcomment))
	group (ap/alt (ap/s (ap/t #"\s*group\s*\{")
                            (ap/+ (ap/| declaration assignment linecomment blockcomment))
                            (ap/t #"\s*\}")
                            end-modifiers)
                      (fn [[_ ds _ ms]]
                        [:group (for [d ds]
                                  [:declare [d (filter #(= (first %)
                                                           :modifier)
                                                       ms)]])]))
	declare-modifier (ap/alt (ap/s declaration end-modifiers)
                                 (fn [[d ms]]
                                   [:declare [d (filter #(= (first %)
                                                            :modifier)
                                                        ms)]]))
	block (ap/| assignment group declare-modifier linecomment blockcomment)
	blocks (ap/alt (ap/+ block)
                       (fn [s]
                         ;; flatten
                         (vec
                          (reduce (fn [coll x]
                                    (let [[k v] x]
                                      (if (= k :group)
                                        (concat coll v)
                                        (concat coll (list x)))))
                                  (list)
                                  s))))]
    (ap/parse blocks txt)))

(defn parse-dotmodel-c-global-data [txt]
  (let [[compute internal] (for [block (take-last 2 (.split txt (str #"[a-zA-Z_][a-zA-Z_0-9]* +[a-zA-Z_][a-zA-Z_0-9]*\([a-zA-Z*0-9,_ ]+\)\s*\{")))]
                             (let [lines (.split block "\n+")
                                   type-declaration (ap/t #"GlobalData_t")
                                   whitespace (ap/t #"\s*")
                                   id (ap/t #"[a-zA-Z_][a-zA-Z_0-9]*")
                                   binding (ap/alt (ap/s whitespace type-declaration whitespace id)
                                                   (fn [[_ _ _ id]]
                                                     id))]
                               (reduce (fn [ret2 line]
                                         (ap/parse binding
                                                   line
                                                   (fn [result rest-input]
                                                     (conj ret2
                                                           result))
                                                   (fn [result rest-input]
                                                     ret2)))
                                       #{}
                                       lines)))
        cvode-vars (->> txt
                        (re-find #"enum Cvode_Vars \{[^}]+\};")
                        (re-seq #"CVODE_[^,]+")
                        (map #(subs % 6)))]
    {:compute (set (remove #(re-find #"_markov_solve" %)
                           compute))
     :internal internal
     :cvode-vars cvode-vars}))

(defn cvode-dump-state [file-handle vars]
  (str "fprintf(" file-handle ", \"{"
       ":time %.17g,"
       ":node:id %d,"
       ":variables {" (apply str (for [v vars]
                                   (str "\\\"" v "\\\" %.17g,")))
       "}}\""
       ","
       "t," #_"current_global_time(),"
       "__i," #_"current_global_node(__i),"
       (apply str (interpose "," vars))
       ");"))

#_(defn cvode-check-state [vars]
  (str "if (!(" (apply str (interpose " && " (for [v vars]
                                               (str "isfinite(" v ")"))))
       "))" (cvode-dump-state vars)))
