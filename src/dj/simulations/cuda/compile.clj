(ns dj.simulations.cuda.compile
  (:require [clojure.java.shell :as sh]
            [dj.dispatch.treefn :as tf]))

(defn sh
  "wrap shell to fail with exceptions"
  [args]
  (let [{:keys [exit out err] :as m} (apply sh/sh args)]
    (if (zero? exit)
      m
      (throw (ex-info "sh command failed"
                      {:exit exit
                       :out out
                       :err err
                       :args args})))))

;; input: cu & options
;; output boundary: cubin
(def compile-fns
  {:cuda.compile/ptx
   (tf/fm
    [:cuda/cu
     :cuda.compile/nvcc-path
     :cuda.compile/optimizations?
     :cuda.compile/gpu-architecture
     :cuda.compile/gpu-code]
    (dj.io/with-temp-file ptx-f {:suffix ".ptx"}
      (dj.io/with-temp-file cu-f {:suffix ".cu"}
	(dj.io/poop cu-f cu)
	;; compiler optimizations take longer for big code
        (sh (concat
             [nvcc-path
              (dj.io/get-path cu-f)]
             (when-not optimizations?
               ["-O0"          ; turn of optimizations for debugging
                "-Xcicc" "-O0" ; right now, and speed up compile time
                ])
             ["-m64"
              "--gpu-architecture" gpu-architecture
              "--gpu-code" gpu-code
              "-ptx"
              "-o" (dj.io/get-path ptx-f)
              :dir (dj.io/get-path (dj.io/parent cu-f))])))))
   :cuda.compile/cubin
   (tf/fm
    [:cuda.compile/ptx
     :cuda.compile/ptxas-path
     :cuda.compile/optimizations?
     :cuda.compile/gpu-code]
    (dj.io/with-temp-file cubin-f {:suffix ".cubin"
				   :read-fn dj.io/c-eat-binary-file}
      (dj.io/with-temp-file ptx-f {:suffix ".ptx"}
	(dj.io/poop ptx-f ptx)
	;; compiler optimizations take longer for big code
        (sh (concat
             [ptxas-path
              (dj.io/get-path ptx-f)]
             (when-not optimizations?
               ["-O0" ; turn of optimizations
                ])
             ["-m64"
              "--gpu-name" gpu-code
              "-o" (dj.io/get-path cubin-f)
              :dir (dj.io/get-path (dj.io/parent ptx-f))])))))})
