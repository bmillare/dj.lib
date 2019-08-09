(ns dj.dispatch.plurality
  (:require [dj.repl :as dr]))

;;; Motivation
;; We wish to solve the expression problem: http://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt
;; We will accomplish this by creating a function that conditionally dispatches to one or more functions based on its inputs.
;; -The immutable function, also has metadata, that enables the creation of a new function with modifications
;; -the user then can use Clojure's concurrency coordination primitives according to their needs
;;  -note that extensibility is decomplected, functions are not tied to a particular storage mechanism (vars/namespaces)
;;   -we gain different coordination mechanisms

(defn update-implementation
  "
  returns a new plural-fn with updated implementations

  updates implementations with new value from call to f on implementations and args

  In detail:

  A modify-implementations fn must accept the new implementations and
  must return the same type of plural-fn but with new implementations

  "
  [plural-fn f & args]
  (let [{:keys [::implementations ::modify-implementations]} (meta plural-fn)]
    (modify-implementations (apply f implementations args))))

(defn convert-implementations
  "
  convenience fn

  returns application of f on implementations of plural-fn + args

  Used in combination with plural-fn constructors to convert one
  plural-fn to another
  "
  [plural-fn f & args]
  (apply f
         (-> (meta plural-fn)
             ::implementations)
         args))

(defn ->all-fn
  "
  plural-fn that returns a seq of the results of all methods
  "
  [implementations]
  (with-meta (fn [& args]
               (map #(apply % args)
                    implementations))
    {::modify-implementations ->all-fn
     ::implementations implementations}))

(defn ->broadcast-fn
  [implementations]
  (with-meta (fn [& args]
               (dorun
                (map #(apply % args)
                     implementations)))
    {::modify-implementations ->broadcast-fn
     ::implementations implementations}))

(defn ->random-fn
  ([implementations]
   (with-meta (fn [& args]
                (if (empty? implementations)
                  nil
                  (apply (rand-nth implementations)
                         args)))
     {::modify-implementations ->random-fn
      ::implementations implementations}))
  ([]
   (->random-fn [])))

(defn ->simple-multi-fn
  "
  Multimethods without hierarchies.

  TODO:1 :unsure {:tags [:programming]} Add hierarchies.

  implementations must be a map of dispatch-values -> method fns
  dispatch-fn is like a multimethod dispatch-fn

  Arity optimized for 5 or less args, but supports greater arity.
  "
  ([implementations dispatch-fn]
   (with-meta (fn
                ([a1]
                 (try
                   ((implementations (dispatch-fn a1))
                    a1)
                   (catch Exception e
                     (throw (ex-info "simple-multi-fn failed"
                                     (dr/local-context)
                                     e)))))
                ([a1 a2]
                 (try
                   ((implementations (dispatch-fn a1 a2))
                    a1
                    a2)
                   (catch Exception e
                     (throw (ex-info "simple-multi-fn failed"
                                     (dr/local-context)
                                     e)))))
                ([a1 a2 a3]
                 (try
                   ((implementations (dispatch-fn a1 a2 a3))
                    a1
                    a2
                    a3)
                   (catch Exception e
                     (throw (ex-info "simple-multi-fn failed"
                                     (dr/local-context)
                                     e)))))
                ([a1 a2 a3 a4]
                 (try
                   ((implementations (dispatch-fn a1 a2 a3 a4))
                    a1
                    a2
                    a3
                    a4)
                   (catch Exception e
                     (throw (ex-info "simple-multi-fn failed"
                                     (dr/local-context)
                                     e)))))
                ([a1 a2 a3 a4 a5]
                 (try
                   ((implementations (dispatch-fn a1 a2 a3 a4 a5))
                    a1
                    a2
                    a3
                    a4
                    a5)
                   (catch Exception e
                     (throw (ex-info "simple-multi-fn failed"
                                     (dr/local-context)
                                     e)))))
                ([a1 a2 a3 a4 a5 & args]
                 (try
                   (apply (implementations (apply dispatch-fn a1 a2 a3 a4 a5 args))
                          a1
                          a2
                          a3
                          a4
                          a5
                          args)
                   (catch Exception e
                     (throw (ex-info "simple-multi-fn failed"
                                     (dr/local-context)
                                     e))))))
     {::modify-implementations (fn [imps]
                                 (->simple-multi-fn imps dispatch-fn))
      ::implementations implementations}))
  ([implementations default-imp dispatch-fn]
   (with-meta (fn
                ([a1]
                 (try
                   (let [f (implementations (dispatch-fn a1))]
                     (if f
                       (f a1)
                       (default-imp a1)))
                   (catch Exception e
                     (throw (ex-info "simple-multi-fn failed"
                                     (dr/local-context)
                                     e)))))
                ([a1 a2]
                 (try
                   (let [f (implementations (dispatch-fn a1 a2))]
                     (if f
                       (f a1
                          a2)
                       (default-imp
                         a1
                         a2)))
                   (catch Exception e
                     (throw (ex-info "simple-multi-fn failed"
                                     (dr/local-context)
                                     e)))))
                ([a1 a2 a3]
                 (try
                   (let [f (implementations (dispatch-fn a1 a2 a3))]
                     (if f
                       (f a1
                          a2
                          a3)
                       (default-imp
                         a1
                         a2
                         a3)))
                   (catch Exception e
                     (throw (ex-info "simple-multi-fn failed"
                                     (dr/local-context)
                                     e)))))
                ([a1 a2 a3 a4]
                 (try
                   (let [f (implementations (dispatch-fn a1 a2 a3 a4))]
                     (if f
                       (f a1
                          a2
                          a3
                          a4)
                       (default-imp
                         a1
                         a2
                         a3
                         a4)))
                   (catch Exception e
                     (throw (ex-info "simple-multi-fn failed"
                                     (dr/local-context)
                                     e)))))
                ([a1 a2 a3 a4 a5]
                 (try
                   (let [f (implementations (dispatch-fn a1 a2 a3 a4 a5))]
                     (if f
                       (f a1
                          a2
                          a3
                          a4
                          a5)
                       (default-imp
                         a1
                         a2
                         a3
                         a4
                         a5)))
                   (catch Exception e
                     (throw (ex-info "simple-multi-fn failed"
                                     (dr/local-context)
                                     e)))))
                ([a1 a2 a3 a4 a5 & args]
                 (try
                   (let [f (implementations (apply dispatch-fn a1 a2 a3 a4 a5 args))]
                     (if f
                       (apply f
                              a1
                              a2
                              a3
                              a4
                              a5
                              args)
                       (apply default-imp
                              a1
                              a2
                              a3
                              a4
                              a5
                              args)))
                   (catch Exception e
                     (throw (ex-info "simple-multi-fn failed"
                                     (dr/local-context)
                                     e))))))
     {::modify-implementations (fn [imps]
                                 (->simple-multi-fn imps default-imp dispatch-fn))
      ::implementations implementations})))

(defn ->recursive-simple-multi-fn
  "
  Experimental!

  Multimethods with recursion off multi-fn but without using vars (but
  using very fast AtomicReferences for delayed binding)

  values of :implementations map must accept the multifn and return the implementation

  ex:
  {:some-key
  (fn [multi-fn] (fn [x y] (+ (multi-fn x) y)))}

  Note that the implementation must be a fn that accepts the
  multi-fn (what gives you recursive access) and then the body is the
  actual implementation

  ie. the called implementation will be generated using values in ::implementations

  Arity optimized for 5 or less args, but supports greater arity.
  "
  ([implementations dispatch-fn]
   ;; Since we never directly provide write access and we write before
   ;; we return a getter, this is always thread safe and we can use the
   ;; fastest mutable available, a non-volatile field.
   ;; See:
   ;; http://stackoverflow.com/questions/29001781/can-a-non-volatile-variable-that-is-delayed-assigned-to-by-a-method-of-the-class
   ;; To make sure this works you must compile v.java._1.NonVolatile.java with javac in that folder
   ;; - javac NonVolatile.java
   (let [implementations-ref (v.java._1.NonVolatile. nil)
         multi-fn (fn
                    ([a1]
                     (try
                       (let [f ((.get implementations-ref) (dispatch-fn a1))]
                         (f a1))
                       (catch Exception e
                         (throw (ex-info "recursive-simple-multi-fn failed"
                                         (dr/local-context)
                                         e)))))
                    ([a1 a2]
                     (try
                       (let [f ((.get implementations-ref) (dispatch-fn a1 a2))]
                         (f a1
                            a2))
                       (catch Exception e
                         (throw (ex-info "recursive-simple-multi-fn failed"
                                         (dr/local-context)
                                         e)))))
                    ([a1 a2 a3]
                     (try
                       (let [f ((.get implementations-ref) (dispatch-fn a1 a2 a3))]
                         (f a1
                            a2
                            a3))
                       (catch Exception e
                         (throw (ex-info "recursive-simple-multi-fn failed"
                                         (dr/local-context)
                                         e)))))
                    ([a1 a2 a3 a4]
                     (try
                       (let [f ((.get implementations-ref) (dispatch-fn a1 a2 a3 a4))]
                         (f a1
                            a2
                            a3
                            a4))
                       (catch Exception e
                         (throw (ex-info "recursive-simple-multi-fn failed"
                                         (dr/local-context)
                                         e)))))
                    ([a1 a2 a3 a4 a5]
                     (try
                       (let [f ((.get implementations-ref) (dispatch-fn a1 a2 a3 a4 a5))]
                         (f a1
                            a2
                            a3
                            a4
                            a5))
                       (catch Exception e
                         (throw (ex-info "recursive-simple-multi-fn failed"
                                         (dr/local-context)
                                         e)))))
                    ([a1 a2 a3 a4 a5 & args]
                     (try
                       (let [f ((.get implementations-ref) (apply dispatch-fn a1 a2 a3 a4 a5 args))]
                         (apply f
                                a1
                                a2
                                a3
                                a4
                                a5
                                args))
                       (catch Exception e
                         (throw (ex-info "recursive-simple-multi-fn failed"
                                         (dr/local-context)
                                         e))))))]
     (.set implementations-ref (reduce-kv (fn [ret k v]
                                            (assoc ret
                                                   k
                                                   (v multi-fn)))
                                          {}
                                          implementations))
     (with-meta multi-fn
       {::modify-implementations (fn [imps]
                                   (->recursive-simple-multi-fn imps dispatch-fn))
        ::implementations implementations})))
  ([implementations default-imp dispatch-fn]
   ;; Since we never directly provide write access and we write before
   ;; we return a getter, this is always thread safe and we can use the
   ;; fastest mutable available, a non-volatile field.
   ;; See:
   ;; http://stackoverflow.com/questions/29001781/can-a-non-volatile-variable-that-is-delayed-assigned-to-by-a-method-of-the-class
   ;; To make sure this works you must compile v.java._1.NonVolatile.java with javac in that folder
   ;; - javac NonVolatile.java
   (let [implementations-ref (v.java._1.NonVolatile. nil)
         default-imp-ref (v.java._1.NonVolatile. nil)
         multi-fn (fn
                    ([a1]
                     (try
                       (let [f ((.get implementations-ref) (dispatch-fn a1))]
                         (if f
                           (f a1)
                           ((.get default-imp-ref) a1)))
                       (catch Exception e
                         (throw (ex-info "recursive-simple-multi-fn failed"
                                         (dr/local-context)
                                         e)))))
                    ([a1 a2]
                     (try
                       (let [f ((.get implementations-ref) (dispatch-fn a1 a2))]
                         (if f
                           (f a1
                              a2)
                           ((.get default-imp-ref)
                            a1
                            a2)))
                       (catch Exception e
                         (throw (ex-info "recursive-simple-multi-fn failed"
                                         (dr/local-context)
                                         e)))))
                    ([a1 a2 a3]
                     (try
                       (let [f ((.get implementations-ref) (dispatch-fn a1 a2 a3))]
                         (if f
                           (f a1
                              a2
                              a3)
                           ((.get default-imp-ref)
                            a1
                            a2
                            a3)))
                       (catch Exception e
                         (throw (ex-info "recursive-simple-multi-fn failed"
                                         (dr/local-context)
                                         e)))))
                    ([a1 a2 a3 a4]
                     (try
                       (let [f ((.get implementations-ref) (dispatch-fn a1 a2 a3 a4))]
                         (if f
                           (f a1
                              a2
                              a3
                              a4)
                           ((.get default-imp-ref)
                            a1
                            a2
                            a3
                            a4)))
                       (catch Exception e
                         (throw (ex-info "recursive-simple-multi-fn failed"
                                         (dr/local-context)
                                         e)))))
                    ([a1 a2 a3 a4 a5]
                     (try
                       (let [f ((.get implementations-ref) (dispatch-fn a1 a2 a3 a4 a5))]
                         (if f
                           (f a1
                              a2
                              a3
                              a4
                              a5)
                           ((.get default-imp-ref)
                            a1
                            a2
                            a3
                            a4
                            a5)))
                       (catch Exception e
                         (throw (ex-info "recursive-simple-multi-fn failed"
                                         (dr/local-context)
                                         e)))))
                    ([a1 a2 a3 a4 a5 & args]
                     (try
                       (let [f ((.get implementations-ref) (apply dispatch-fn a1 a2 a3 a4 a5 args))]
                         (if f
                           (apply f
                                  a1
                                  a2
                                  a3
                                  a4
                                  a5
                                  args)
                           (apply (.get default-imp-ref)
                                  a1
                                  a2
                                  a3
                                  a4
                                  a5
                                  args)))
                       (catch Exception e
                         (throw (ex-info "recursive-simple-multi-fn failed"
                                         (dr/local-context)
                                         e))))))]
     (.set implementations-ref (reduce-kv (fn [ret k v]
                                            (assoc ret
                                                   k
                                                   (v multi-fn)))
                                          {}
                                          implementations))
     (.set default-imp-ref (default-imp multi-fn))
     (with-meta multi-fn
       {::modify-implementations (fn [imps]
                                   (->recursive-simple-multi-fn imps default-imp dispatch-fn))
        ::implementations implementations}))))

(defn ->simple-predicate-fn
  [implementations]
  (with-meta (fn [& args]
               (let [match (->> implementations
                                (filter (fn [[pred? implementation]]
                                          (apply pred? args)))
                                first)]
                 (apply match args)))
    {::modify-implementations ->simple-predicate-fn
     ::implementations implementations}))
