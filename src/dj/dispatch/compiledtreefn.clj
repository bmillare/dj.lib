(ns dj.dispatch.compiledtreefn
  (:refer-clojure :exclude [fn])
  (:require [clojure.set :as cs]
            [dj.repl]
            [dj.algorithms.dag :as dag]))

;; Given a mapping of input labels to the computations on those
;; inputs, and mapping the computations to an output label, compute
;; the call order (chaining output to input possible), also exclude
;; unused computations

(defmacro fn
  "adds dependency metadata, binding names must match up with callers
  and assigned label"
  [bindings & body]
  `(with-meta (clojure.core/fn ~bindings
                ~@body)
     {::dependencies '~(mapv keyword bindings)}))

(defmacro wrap-fn
  "
  Convenience macro:

  adds dependency metadata to an existing fn or var

  Takes a fn-or-var and a vector of keys
  "
  [fn-or-var vec-of-keys]
  `(with-meta ~fn-or-var
     {::dependencies ~vec-of-keys}))

(declare ^:dynamic eval-pass)

(defn compiled-treefn
  "
  given a map of functions that produce values, will compose them based on dependencies

  the composed function will return the value returned by the function mapped to the root-key

  input-keys are keys that the compiled-tree-fn will take as input
  "
  [fn-map root-key input-keys]
  (let [input-key-set (set input-keys)
        available-keys (set (keys fn-map))
        shaken-keys ((clojure.core/fn collect [all-dependents temp-key]
                       (let [the-fnr (temp-key fn-map)
                             dependents (-> the-fnr
                                            meta
                                            ::dependencies
                                            set
                                            (cs/difference input-key-set))]
                         (if (empty? dependents)
                           (conj all-dependents
                                 temp-key)
                           (reduce collect
                                   (cs/union dependents all-dependents)
                                   dependents))))
                     #{root-key}
                     root-key)
        undefined-keys (cs/difference shaken-keys available-keys)
        _ (if (empty? undefined-keys)
            nil
            (throw (ex-info (str "Unbound keys " undefined-keys)
                            (dj.repl/local-context))))
        shaken-map (select-keys fn-map shaken-keys)
        shaken-dag-with-inputs (reduce-kv (clojure.core/fn [ret k the-fnr]
                                            (let [dependents (-> the-fnr
                                                                 meta
                                                                 ::dependencies)]
                                              (assoc ret
                                                     k
                                                     (set dependents))))
                                          {}
                                          shaken-map)
        shaken-dag (reduce-kv (clojure.core/fn [ret k dependents]
                                (assoc ret
                                       k
                                       (cs/difference dependents
                                                      input-key-set)))
                              {}
                              shaken-dag-with-inputs)
        sorted-keys (dag/topological-sort shaken-dag)
        symbols (reduce (clojure.core/fn [ret k]
                          (assoc ret
                                 k (-> k
                                       name
                                       gensym)))
                        {}
                        shaken-keys)]
    (with-meta (binding [eval-pass shaken-map]
                 (eval `(let ~(vec
                               (mapcat (clojure.core/fn [k]
                                         (list (symbols k) `(~k eval-pass)))
                                       shaken-keys))
                          (clojure.core/fn ~(mapv (comp symbol name) input-keys)
                            (let ~(vec
                                   (mapcat (clojure.core/fn [k]
                                             (list (symbol (name k))
                                                   `(~(symbols k) ~@(map (comp symbol name)
                                                                         (-> k
                                                                             shaken-map
                                                                             meta
                                                                             ::dependencies)))))
                                           sorted-keys))
                              ~(symbol (name root-key)))))))
      {::dag shaken-dag-with-inputs})))

(defn dependencies->rev-edges
  "
  converts dependency graph into a graphviz friendly edge vector representing flow of data
  "
  [direct-dependency-map]
  (->> (reduce-kv (clojure.core/fn add [ret k v]
                    (if (empty? v)
                      ret
                      (let [dep (first v)]
                        (recur (conj ret [k dep])
                               k
                               (disj v dep)))))
                  []
                  direct-dependency-map)
       (mapv (comp vec rseq))
       set))

(defn graphviz-data [treefn]
  (let [edges (-> (meta treefn)
                  ::dag
                  dependencies->rev-edges)]
    {:type :digraph
     :name :tree_fn_flow
     :attributes {:graph {:size "10"}}
     :edges edges}))
