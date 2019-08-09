(ns dj.dispatch.graphfn
  (:refer-clojure :exclude [fn])
  (:require [clojure.set :as cs]
            [dj.repl]))

(defmacro fn
  "

  returns a function that is expected to be called during compiling
  step. Metadata is added to specify direct and late bound dependencies,
  which must be plain symbols with no namespaces.

  Typical usage would to have the value returned by this fnc to be a
  fn. Then at compile-time, mutual-graphfn would pass these values (fns) to
  fns that depend on them, thus enabling mutually recursive composition.

  "
  [direct-bindings late-bindings & body]
  `(with-meta (clojure.core/fn [{:keys ~(vec (cs/union direct-bindings
                                                       late-bindings))}]
                ~@body)
     {::direct-bindings ~(set (map keyword direct-bindings))
      ::late-bindings ~(set (map keyword late-bindings))}))

(defn mutual-graphfn
  "
  Provides a more composable construct than letfn.

  returns a hashmap of keywords -> values

  Usually these values are functions to take advantage of the late
  binding features but they can be plain values to take advantage of the
  compositional power of maps.

  fnc-map: keywords -> fncs

  See: dj.math.parser/parse
  "
  ([fnc-map root-key alias-map root-late? ref-fn ref-set-fn!]
   ((clojure.core/fn add-bind [references temp-root late?]
      (let [the-fnc (or (fnc-map temp-root)
                        (fnc-map (alias-map temp-root))
                        (throw (ex-info (str "keyword " temp-root " not found in fnc-map")
                                        (dj.repl/local-context))))
            {:keys [::direct-bindings ::late-bindings]} (-> the-fnc
                                                            meta)
            the-ref (when late?
                      (ref-fn))
            if-add (clojure.core/fn [l?]
                     (clojure.core/fn if-add [ret s]
                       (if (ret s)
                         ret
                         (add-bind ret s l?))))
            return (as-> references
                       references'
                     (reduce (if-add false)
                             references'
                             direct-bindings)
                     (assoc references'
                            temp-root
                            (if late?
                              the-ref
                              (the-fnc references')))
                     (reduce (if-add true)
                             references'
                             late-bindings))]
        (when late?
          (ref-set-fn! the-ref (the-fnc return)))
        return))
    {}
    root-key
    root-late?))
  ([fnc-map root-key]
   (mutual-graphfn fnc-map
                   root-key
                   {}))
  ([fnc-map root-key alias-map]
   (mutual-graphfn fnc-map
                   root-key
                   alias-map
                   true
                   #(clojure.lang.Var/create)
                   (clojure.core/fn [^clojure.lang.Var s v]
                     (.bindRoot s v)))))
