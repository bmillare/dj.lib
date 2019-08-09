(ns dj.simulations.cvode.compile
  (:require [dj.dispatch.treefn :as tf]
            [clojure.java.shell :as sh]))

(def compile-fns
  {:simulation/emit-code
   (tf/fm
    [:cvode/c-code
     :simulations/label
     :simulations/output-dir]
    (dj.io/poop (dj.io/file output-dir (str label ".c"))
                c-code))
   :simulation/compile
   (tf/fm
    [:simulation/emit-code
     :simulations/label
     :simulations/output-dir]
    (let [args ["gcc" "-g" (str label ".c") "-o" (str label ".run") "-lsundials_cvode" "-lsundials_nvecserial" "-lm"
                :dir output-dir]
          sh-out (apply sh/sh args)]
      (if (zero? (:exit sh-out))
        sh-out
        (throw (ex-info "compile failed"
                        (assoc sh-out
                               :args
                               args))))))
   :simulation/run
   (tf/fm
    [:simulation/compile
     :simulations/label
     :simulations/output-dir]
    ;; execve semantics demands the "./" to ensure the executable path
    ;; includes the current directory
    (let [args [(str "./" label ".run")
                :dir output-dir]
          sh-out (apply sh/sh args)]
      (if (and (zero? (:exit sh-out))
               (empty? (:err sh-out)))
        (do
          (dj.io/poop (dj.io/file output-dir (str label ".log"))
                      (:out sh-out))
          (dissoc sh-out :out))
        (do
          (dj.io/poop (dj.io/file output-dir (str label ".log"))
                      (:out sh-out))
          (dj.io/poop (dj.io/file output-dir (str label ".error"))
                      (:err sh-out))
          (throw (ex-info (str "run failed, see " label ".log/.error")
                          (-> sh-out
                              (dissoc :out :err)
                              (assoc :args
                                     args))))))))})
