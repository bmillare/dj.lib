(ns dj.dispatch.plurality.predicate
  (:require [clojure.core.match :as m]
            [dj.dispatch.plurality :as p]))

(defmacro ->macro-predicate-fn
  "
  implementations should be a literal vector of pairs, matching form -> fn

  Only up to 5 arity is supported
  "
  [implementations]
  `(let [imps# ~implementations]
     (with-meta ~(if (empty? implementations)
                   `(fn [])
                   (case (count (first (first implementations)))
                     1 `(fn [a1#]
                          ((m/match [a1#]
                                    ~@(apply concat implementations))
                           a1#))
                     2 `(fn [a1# a2#]
                          ((m/match [a1# a2#]
                                    ~@(apply concat implementations))
                           a1# a2#))
                     3 `(fn [a1# a2# a3#]
                          ((m/match [a1# a2# a3#]
                                    ~@(apply concat implementations))
                           a1# a2# a3#))
                     4 `(fn [a1# a2# a3# a4#]
                          ((m/match [a1# a2# a3# a4#]
                                    ~@(apply concat implementations))
                           a1# a2# a3# a4#))
                     5 `(fn [a1# a2# a3# a4# a5#]
                          ((m/match [a1# a2# a3# a4# a5#]
                                    ~@(apply concat implementations))
                           a1# a2# a3# a4# a5#))))
       {:p/modify-implementations
        ~(let [imps (gensym "imps")]
           `(fn [~imps]
              ;; Since this is a
              ;; macro, a recursive
              ;; call would create an
              ;; infinite expansion,
              ;; we wrap this in an
              ;; eval to delay
              ;; computation.
              (-> `(dj.dispatch.plurality.predicate/->macro-predicate-fn
                    ~~imps)
                  macroexpand-1
                  eval)))
        :p/implementations imps#})))
