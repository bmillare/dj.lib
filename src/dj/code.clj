(ns dj.code
  (:require [clojure.walk :as w]))

(defmacro getenv [& body]
  (str (keys &env)))

(let [x 5]
  [3 x (getenv (let [ y 3] y))])


(defn all-qualified-symbols
  "returns all ns-qualified symbols from form, pass through
  clojure.walk/macroexpand-all to handle macros

  In the future, will want to get if this form declares a new var, if
  a form uses symbols that resolve while ignoring/marking if symbols
  are bounded
  "
  [code]
  (loop [ns-qualified #{}
         unhandled #{}
         code-stack [code]]
    (if (empty? code-stack)
      {:ns-qualified ns-qualified
       :unhandled unhandled}
      (let [curr-code (peek code-stack)] ; a nil curr-code means we can pop stack
        (if (coll? curr-code)
          (recur ns-qualified
                 unhandled
                 (conj (pop code-stack)
                       (next curr-code)
                       (first curr-code)))
          (if (symbol? curr-code)
            (if (namespace curr-code)
              (recur (conj ns-qualified curr-code)
                     unhandled
                     (pop code-stack))
              (recur ns-qualified
                     (conj unhandled curr-code)
                     (pop code-stack)))
            (recur ns-qualified
                   unhandled
                   (pop code-stack))))))))

(defn all-qualified-symbols-2
  "returns all ns-qualified symbols from form, pass through
  clojure.walk/macroexpand-all to handle macros

  In the future, will want to get if this form declares a new var, if
  a form uses symbols that resolve while ignoring/marking if symbols
  are bounded

  BUG: doesn't pop bindings and fn case is only simple case
  "
  [code]
  (loop [ns-qualified #{}
         unhandled #{}
         bound #{}
         code-stack [code]]
    (if (empty? code-stack)
      {:ns-qualified ns-qualified
       :unhandled unhandled
       :bound bound}
      (let [curr-code (peek code-stack)]
        (if (coll? curr-code)
          (let [sform (first curr-code)]
            (case sform
              let* (let [[_ bindens & body] curr-code]
                     (if (empty? bindens)
                       (recur ns-qualified
                              unhandled
                              bound
                              (conj (pop code-stack)
                                    body))
                       (let [[bsym nform & rbindens] bindens]
                         (recur ns-qualified
                                unhandled
                                (conj bound bsym)
                                (conj (pop code-stack)
                                      `(~'let* ~rbindens ~@body)
                                      nform)))))
              fn* (let [[_ [bindens & body]] curr-code]
                    (if (empty? bindens)
                      (recur ns-qualified
                             unhandled
                             bound
                             (conj (pop code-stack)
                                   body))
                      (recur ns-qualified
                             unhandled
                             (apply conj bound bindens)
                             (conj (pop code-stack)
                                   body))))
              (recur ns-qualified
                     unhandled
                     bound
                     (conj (pop code-stack)
                           (next curr-code)
                           (first curr-code)))))
          (if (symbol? curr-code)
            (if (namespace curr-code)
              (recur (conj ns-qualified curr-code)
                     unhandled
                     bound
                     (pop code-stack))
              (if (bound curr-code)
                (recur ns-qualified
                       unhandled
                       bound
                       (pop code-stack))
                (let [rsym-meta (-> curr-code
                                    resolve
                                    meta)
                      rsym (symbol (str (:ns rsym-meta)) (str (:name rsym-meta)))]
                  (if (namespace rsym)
                    (recur (conj ns-qualified rsym)
                           unhandled
                           bound
                           (pop code-stack))
                    (recur ns-qualified
                           (conj unhandled curr-code)
                           bound
                           (pop code-stack))))))
            (recur ns-qualified
                   unhandled
                   bound
                   (pop code-stack))))))))

(defn all-qualified-symbols-3
  "ideally, make this version based on a data-structure + finite state
  machine, and keep everything together with data, as in the state
  machine is encoded as maps to functions, and all the data structures
  are maintained in a map"
  [code]
  (loop [ns-qualified #{}
         unhandled #{}
         bound #{}
         code-stack [code]]))

(let [special-forms
      (into #{} (keys clojure.lang.Compiler/specials))]
  (defn all-qualified-symbols-4
    "version based on continuation passing style, instead of passing
  around a data-structure containing all the state, we call a function
  and pass each state individually"
    ([code]
     (all-qualified-symbols-4 code
                              #{}
                              #{}
                              #{}
                              #{}
                              (fn [ns-qualified unhandled bound bound-ever]
                                {:ns-qualified ns-qualified
                                 :unhandled unhandled
                                 :bound bound
                                 :bound-ever bound-ever})))
    ([code ns-qualified unhandled bound bound-ever ret-cont]
     (cond
       (symbol? code) (if (namespace code)
                        (ret-cont (conj ns-qualified code)
                                  unhandled
                                  bound
                                  bound-ever)
                        (if (bound code)
                          (ret-cont ns-qualified
                                    unhandled
                                    bound
                                    bound-ever)
                          (let [rsym-meta (-> code
                                              resolve
                                              meta)
                                s-ns (:ns rsym-meta)]
                            (if s-ns
                              (ret-cont (conj ns-qualified (symbol (str s-ns)
                                                                   (str (:name rsym-meta))))
                                        unhandled
                                        bound
                                        bound-ever)
                              (ret-cont ns-qualified
                                        (conj unhandled code)
                                        bound
                                        bound-ever)))))
       (seq? code) (let [f (first code)]
                     (case f
                       let* (let [[_ bindens & body] code]
                              (if (empty? bindens)
                                (recur (vec body)
                                       ns-qualified
                                       unhandled
                                       bound
                                       bound-ever
                                       ret-cont)
                                (let [[bsym nform & rbindens] bindens]
                                  (recur nform
                                         ns-qualified
                                         unhandled
                                         bound
                                         bound-ever
                                         (fn [ns-qualified' unhandled' bound' bound-ever']
                                           (all-qualified-symbols-4 `(~'let* ~rbindens ~@body)
                                                                    ns-qualified'
                                                                    unhandled'
                                                                    (conj bound'
                                                                          bsym)
                                                                    (conj bound-ever'
                                                                          bsym)
                                                                    (fn [ns-qualified'' unhandled'' bound'' bound-ever'']
                                                                      (ret-cont ns-qualified'' unhandled'' bound' bound-ever''))))))))
                       loop* (let [[_ bindens & body] code]
                               (if (empty? bindens)
                                 (recur (vec body)
                                        ns-qualified
                                        unhandled
                                        bound
                                        bound-ever
                                        ret-cont)
                                 (let [[bsym nform & rbindens] bindens]
                                   (recur nform
                                          ns-qualified
                                          unhandled
                                          bound
                                          bound-ever
                                          (fn [ns-qualified' unhandled' bound' bound-ever']
                                            (all-qualified-symbols-4 `(~'let* ~rbindens ~@body)
                                                                     ns-qualified'
                                                                     unhandled'
                                                                     (conj bound'
                                                                           bsym)
                                                                     (conj bound-ever'
                                                                           bsym)
                                                                     (fn [ns-qualified'' unhandled'' bound'' bound-ever'']
                                                                       (ret-cont ns-qualified'' unhandled'' bound' bound-ever''))))))))
                       fn* (let [[_ [bindens & body] & more] code]
                             (if (empty? bindens)
                               (recur (vec body)
                                      ns-qualified
                                      unhandled
                                      bound
                                      bound-ever
                                      (if more
                                        (fn [ns-qualified' unhandled' bound' bound-ever']
                                          (all-qualified-symbols-4 `(~'fn* ~@more)
                                                                   ns-qualified'
                                                                   unhandled'
                                                                   bound'
                                                                   bound-ever'
                                                                   ret-cont))
                                        ret-cont))
                               (recur (vec body)
                                      ns-qualified
                                      unhandled
                                      (apply conj bound bindens)
                                      (apply conj bound-ever bindens)
                                      (fn [ns-qualified' unhandled' bound' bound-ever']
                                        (if more
                                          (all-qualified-symbols-4 `(~'fn* ~@more)
                                                                   ns-qualified'
                                                                   unhandled'
                                                                   bound
                                                                   bound-ever'
                                                                   ret-cont)
                                          (ret-cont ns-qualified' unhandled' bound bound-ever'))))))
                       (recur (vec code)
                              ns-qualified
                              unhandled
                              bound
                              bound-ever
                              ret-cont)))
       (vector? code) (if (empty? code)
                        (ret-cont ns-qualified unhandled bound bound-ever)
                        (recur (first code)
                               ns-qualified
                               unhandled
                               bound
                               bound-ever
                               (fn [ns-qualified' unhandled' bound' bound-ever']
                                 (all-qualified-symbols-4 (subvec code 1)
                                                          ns-qualified'
                                                          unhandled'
                                                          bound'
                                                          bound-ever'
                                                          ret-cont))))
       (coll? code) (recur (vec code)
                           ns-qualified
                           unhandled
                           bound
                           bound-ever
                           ret-cont)
       :else (ret-cont ns-qualified
                       unhandled
                       bound
                       bound-ever)))))

(let [;;https://stackoverflow.com/questions/30947702/what-are-all-of-clojures-special-forms
      all-specials (set (keys (. clojure.lang.Compiler specials)))]
  (defn all-qualified-symbols-prototype
    "returns all ns-qualified symbols from form, pass through
  clojure.walk/macroexpand-all to handle macros

  Doesn't extract bounded symbols from let, fn, bound etc, might want
  to make a specific walker for that
  "
    [code]
    (loop [ns-qualified #{}
           unhandled #{}
           bound #{}
           code-stack [code]]
      (if (empty? code-stack)
        {:ns-qualified ns-qualified
         :unhandled unhandled
         :bound bound}
        (let [curr-code (peek code-stack)]
          (if (coll? curr-code)
            (recur ns-qualified
                       unhandled
                       bound
                       (conj (pop code-stack)
                             (next curr-code)
                             (first curr-code)))
            (if (symbol? curr-code)
              (cond
                (all-specials curr-code) (recur ns-qualified
                                                unhandled
                                                bound
                                                (pop code-stack))
                (namespace curr-code) (recur (conj ns-qualified curr-code)
                                             unhandled
                                             bound
                                             (pop code-stack))
                :else (recur ns-qualified
                             (conj unhandled curr-code)
                             bound
                             (pop code-stack)))
              (recur ns-qualified
                     unhandled
                     bound
                     (pop code-stack)))))))))

(comment
  (all-qualified-symbols-4 (w/macroexpand-all '(let [ ;;https://stackoverflow.com/questions/30947702/what-are-all-of-clojures-special-forms
                                                     all-specials (set (keys (. clojure.lang.Compiler specials)))]
                                                 (defn all-qualified-symbols
                                                   ([code]
                                                    (loop [ns-qualified #{}
                                                           unhandled #{}
                                                           bound #{}
                                                           code-stack [code]]
                                                      (if (empty? code-stack)
                                                        {:ns-qualified ns-qualified
                                                         :unhandled unhandled
                                                         :bound bound}
                                                        (let [curr-code (peek code-stack)]
                                                          (if (coll? curr-code)
                                                            (recur ns-qualified
                                                                   unhandled
                                                                   bound
                                                                   (conj (pop code-stack)
                                                                         (next curr-code)
                                                                         (first curr-code)))
                                                            (if (symbol? curr-code)
                                                              (cond
                                                                (all-specials curr-code) (recur ns-qualified
                                                                                                unhandled
                                                                                                bound
                                                                                                (pop code-stack))
                                                                (namespace curr-code) (recur (conj ns-qualified curr-code)
                                                                                             unhandled
                                                                                             bound
                                                                                             (pop code-stack))
                                                                :else (recur ns-qualified
                                                                             (conj unhandled curr-code)
                                                                             bound
                                                                             (pop code-stack)))
                                                              (recur ns-qualified
                                                                     unhandled
                                                                     bound
                                                                     (pop code-stack)))))))
                                                    asbestos)
                                                   ([asbestos]
                                                    (loop [ns-qualified #{}
                                                           unhandled #{}
                                                           bound #{}
                                                           code-stack [code]]
                                                      (if (empty? code-stack)
                                                        {:ns-qualified ns-qualified
                                                         :unhandled unhandled
                                                         :bound bound}
                                                        (let [curr-code (peek code-stack)]
                                                          (if (coll? curr-code)
                                                            (recur ns-qualified
                                                                   unhandled
                                                                   bound
                                                                   (conj (pop code-stack)
                                                                         (next curr-code)
                                                                         (first curr-code)))
                                                            (if (symbol? curr-code)
                                                              (cond
                                                                (all-specials curr-code) (recur ns-qualified
                                                                                                unhandled
                                                                                                bound
                                                                                                (pop code-stack))
                                                                (namespace curr-code) (recur (conj ns-qualified curr-code)
                                                                                             unhandled
                                                                                             bound
                                                                                             (pop code-stack))
                                                                :else (recur ns-qualified
                                                                             (conj unhandled curr-code)
                                                                             bound
                                                                             (pop code-stack)))
                                                              (recur ns-qualified
                                                                     unhandled
                                                                     bound
                                                                     (pop code-stack))))))))))))

  
  )
