(ns dj.parsers.c
  (:require [dj.dispatch.treefn :as tf]
            [dj.io]
            [clojure.string :as cs]))

(def collect-fms
  {::c-file-txt
   (tf/fm [::input-file]
          (dj.io/eat input-file))
   ::joined
   (tf/fm [::processed-lines]
          (cs/join "\n" (:new-lines processed-lines)))})

(defn process-lines [lines definitions]
  (let [num-lines (count lines)]
    (loop [defs definitions
           encountered #{}
           n 0
           new-lines []
           stack [:if-true]]
      (if (< n num-lines)
        (let [line (get lines n)
              [_ directive] (re-find #"#(define|ifdef|endif|ifndef|else|undef|if|elif)"
                                     line)]
          (println n stack directive)
          (case directive
            "endif"
            (recur defs
                   encountered
                   (inc n)
                   new-lines
                   (try
                     (pop stack)
                     (catch Exception e
                       (throw
                        (ex-info "blah"
                                 {:line line
                                  :n n}
                                 e)))))
            "elif"
            (let [[_ value] (re-find #"#elif\s+([^\n]+)"
                                     line)]
              (if value
                (let [eval-value (get defs value)]
                  (case (peek stack)
                    :if-true
                    (recur defs
                           (conj encountered value)
                           (inc n)
                           new-lines
                           (assoc stack
                                  (dec (count stack))
                                  :if-ignore))
                    :if-false
                    (if eval-value
                      (recur defs
                             (conj encountered value)
                             (inc n)
                             new-lines
                             (assoc stack
                                    (dec (count stack))
                                    :if-true))
                      (recur defs
                             (conj encountered value)
                             (inc n)
                             new-lines
                             (assoc stack
                                    (dec (count stack))
                                    :if-false)))
                    :if-ignore
                    (recur defs
                           (conj encountered value)
                           (inc n)
                           new-lines
                           (assoc stack
                                  (dec (count stack))
                                  :if-ignore))
                    (throw (ex-info (str "not supposed to encounter #" directive "during " (peek stack))
                                    {:line line
                                     :n n
                                     :new-lines new-lines
                                     :lines lines}))))
                (throw (ex-info (str "no value found for #" directive)
                                {:line line
                                 :n n
                                 :new-lines new-lines
                                 :lines lines}))))
            "else"
            (case (peek stack)
              :if-true
              (recur defs
                     encountered
                     (inc n)
                     new-lines
                     (assoc stack
                            (dec (count stack))
                            :if-ignore))
              :if-false
              (recur defs
                     encountered
                     (inc n)
                     new-lines
                     (assoc stack
                            (dec (count stack))
                            :if-true))
              :if-ignore
              (recur defs
                     encountered
                     (inc n)
                     new-lines
                     (assoc stack
                            (dec (count stack))
                            :if-ignore))
              (throw (ex-info (str "not supposed to encounter #" directive "during " (peek stack))
                              {:line line
                               :n n
                               :new-lines new-lines
                               :lines lines})))
            (if (= (peek stack) :if-true)
              (case directive
                "define"
                (let [[_ label value] (re-find #"#define\s+([a-zA-Z_][a-zA-Z0-9_]*)(?:\s+([^\n]+))?"
                                               line)]
                  (if label
                    (recur (assoc defs
                                  label
                                  (or value
                                      :exists))
                           encountered
                           (inc n)
                           new-lines
                           stack)
                    (throw (ex-info (str "no label found for #" directive)
                                    {:line line
                                     :n n
                                     :new-lines new-lines
                                     :lines lines}))))
                "undef"
                (let [[_ label] (re-find #"#undef\s+([a-zA-Z_][a-zA-Z0-9_]*)"
                                         line)]
                  (if label
                    (recur (dissoc defs
                                   label)
                           encountered
                           (inc n)
                           new-lines
                           stack)
                    (throw (ex-info (str "no label found for #" directive)
                                    {:line line
                                     :n n
                                     :new-lines new-lines
                                     :lines lines}))))
                "ifdef"
                (let [[_ label] (re-find #"#ifdef\s+([a-zA-Z_][a-zA-Z0-9_]*)"
                                         line)]
                  (if label
                    (if (contains? defs label)
                      (recur defs
                             (conj encountered label)
                             (inc n)
                             new-lines
                             (conj stack :if-true))
                      (recur defs
                             (conj encountered label)
                             (inc n)
                             new-lines
                             (conj stack :if-false)))
                    (throw (ex-info (str "no label found for #" directive)
                                    {:line line
                                     :n n
                                     :new-lines new-lines
                                     :lines lines}))))
                "ifndef"
                (let [[_ label] (re-find #"#ifndef\s+([a-zA-Z_][a-zA-Z0-9_]*)"
                                         line)]
                  (if label
                    (if (contains? defs label)
                      (recur defs
                             (conj encountered label)
                             (inc n)
                             new-lines
                             (conj stack :if-false))
                      (recur defs
                             (conj encountered label)
                             (inc n)
                             new-lines
                             (conj stack :if-true)))
                    (throw (ex-info (str "no label found for #" directive)
                                    {:line line
                                     :n n
                                     :new-lines new-lines
                                     :lines lines}))))
                "if"
                (let [[_ value] (re-find #"#if\s+([^\n]+)"
                                         line)]
                  (if value
                    (let [eval-value (get defs value)]
                      (if eval-value
                        (recur defs
                               (conj encountered value)
                               (inc n)
                               new-lines
                               (conj stack :if-true))
                        (recur defs
                               (conj encountered value)
                               (inc n)
                               new-lines
                               (conj stack :if-false))))
                    (throw (ex-info (str "no value found for #" directive)
                                    {:line line
                                     :n n
                                     :new-lines new-lines
                                     :lines lines}))))
                (if directive
                  (throw (ex-info (str "unhandled directive #" directive)
                                 {:line line
                                  :n n
                                  :new-lines new-lines
                                  :lines lines}))
                  (recur defs
                         encountered
                         (inc n)
                         (conj new-lines line)
                         stack)))
              (recur defs
                     encountered
                     (inc n)
                     (conj new-lines line)
                     stack))))
        {:new-lines new-lines
         :encountered encountered}))))

(def preprocessor-fms
  {::c-file-split-lines
   (tf/fm [::c-file-txt]
          (cs/split-lines c-file-txt))
   ::processed-lines
   (tf/fm [::c-file-split-lines
           ::definitions]
          (process-lines c-file-split-lines definitions))})

(comment

  (let [k ::joined
        the-tf (tf/treefm (merge collect-fms
                                 preprocessor-fms)
                          k)
        input {::input-file (dj.io/file "/home/username/Software/sources/carp-1.8-legacy/CARP/carp.c")
               ::definitions {"USE_PETSc" true
                              "defined(USE_PETSc) && defined(LOCAL_ELEM)" true
                              "defined(USE_PETSc)" true}}]

    (def v
      (-> input
          the-tf)))

  (-> v
      (get ::processed-lines)
      :encountered)
  #{"__osf__" "MPI_2" "USE_PETSc" "LOCAL_ELEM" "defined(USE_PETSc) && defined(LOCAL_ELEM)"}

  (-> v
      (get ::joined)
      (->> (dj.io/poop (dj.io/file "/home/username/Software/sources/understand-carp-filter/CARP/carp.c"))))

  (let [k ::joined
        the-tf (tf/treefm (merge collect-fms
                                 preprocessor-fms)
                          k)
        input {::input-file (dj.io/file "/home/username/Software/sources/understand-carp-post-build/LIMPET/MULTI_ION_IF.c")
               ::definitions {"USE_PETSc" true
                              "defined(USE_PETSc) && defined(LOCAL_ELEM)" true
                              "defined(USE_PETSc)" true
                              "!defined(__CUDACC__) || defined(USE_PETSc)" true
                              "defined(USE_PETSc) || defined(__CUDACC__)" true}}]

    (def v
      (-> input
          the-tf)))

  (-> v
      (get ::processed-lines)
      :encountered)
  

  (-> v
      (get ::joined)
      (->> (dj.io/poop (dj.io/file "/home/username/Software/sources/understand-carp-filter/LIMPET/MULTI_ION_IF.c"))))
  

  )
