(ns dj.simulations.cuda.solvers
  (:require [dj.dispatch.treefn :as tf]
            [dj.math.cemit :as ce]
            [dj.math.parser :as p]
            [dj.classreloader :as dc]
            [dj.simulations.integrator :as i]))

;; input: variables, integrator method, combiner (summation method)
;; output boundary: cu code
(def solver-code-fns
  {:cuda/cu
   (tf/fm
    [:simulations/kernel-name
     :simulations/block-parameters
     :dotmodel/all-state-vars
     :dotmodel/shared-algebra-assignment
     :dotmodel/track-vars
     :dotmodel/shared-vars
     :simulations/integrator-fn
     :simulations/combiner-fn]
    (let [dt (p/s {:op "var"
                   :name "dt"})
          id->z0 identity
          id->z1 identity
          id->dz (fn [s] (str "d_" s))
          id->dze (fn [s] (str "d_" s "_error"))
          ;; ods-data, how do we deal with this?
          emit (ce/c-emitter)
          ;; block-parameters are set at runtime, we put
          ;; parameter_name instead of dummy value so that
          ;; preprocessing treats them as variables and not constants
          out-set-block-parameters (apply str
                                          (map-indexed (fn [v_idx v]
                                                         (str "float " v "_parameter = block_parameters[parameter_offset+" v_idx "];\n"))
                                                       (mapv first (first block-parameters))))
          out-state_variable_declarations (apply str
                                                 (for [d all-state-vars]
                                                   (str "float " d ";\n"
                                                        "float " (id->dze d) ";\n")))
          out-shared_algebra_declarations (apply str
                                                 (for [d (map first shared-algebra-assignment)]
                                                   (str "float " d ";\n")))
          out-initial_conditions (apply str
                                        (map-indexed (fn [sv_idx v]
                                                       (let [z0 (id->z0 v)
                                                             dze (id->dze v)]
                                                         (str z0 " = idata[n_mem_idx*num_vars+" sv_idx "];\n"
                                                              dze " = 0.0;\n")))
                                                     all-state-vars))
          out-shared-algebra (apply str
                                    (for [[id e] shared-algebra-assignment]
                                      (str id " = " e "\n")))
          out-compute (-> (integrator-fn dt id->dz)
                          (combiner-fn id->z0 id->z1 id->dze)
                          emit)
          out-shared-sv-indexes (apply str
                                       (map-indexed (fn [id-idx id]
                                                      (str "const unsigned int shared_" id "_idx = " id-idx ";\n"))
                                                    shared-vars))
          out-shared-sv-write (apply str
                                     (map-indexed (fn [sv-idx sv]
                                                    (str "sdata[n_mem_idx*num_shared_vars+" sv-idx "] = " sv ";\n"))
                                                  shared-vars))
          out-write_data (apply str
                                (map-indexed (fn [sv_idx id]
                                               (str "odata[record_ts_idx*(num_track_vars*num_nodes*num_blocks)+n_mem_idx*num_track_vars+" sv_idx "] = " id ";\n"))
                                             track-vars))]
      (dj/replace-map (dc/resource-as-str "dj.simulations/cuda/templates/kernel.cu")
                      {"__kernel_name" (or kernel-name
                                           "bm_solve")
                       "__set_block_parameters" out-set-block-parameters
                       "__num_vars" (str (count all-state-vars))
                       "__num_track_vars" (str (count track-vars))
                       "__num_shared_vars" (str (count shared-vars))
                       "__shared_sv_indexes;" out-shared-sv-indexes
                       "__state_variable_declarations;" out-state_variable_declarations
                       "__shared_algebra_declarations;" out-shared_algebra_declarations
                       "__initial_conditions;" out-initial_conditions
                       "__shared_sv_read;" out-shared-algebra
                       "__shared_sv_write" out-shared-sv-write
                       "__compute;" out-compute
                       "__write_data;" out-write_data})))})

(def debug-solver-code-fns
  {:cuda/cu
   (tf/fm
    [:dotmodel/algebra-assignment
     :dotmodel/differential-assignment

     :simulations/block-parameters
     :simulations/compute-initial-conditions
     :dotmodel/all-state-vars
     :dotmodel/shared-algebra-assignment
     :dotmodel/track-vars
     :dotmodel/shared-vars
     :simulations/integrator-fn
     :simulations/combiner-fn
     :debug/n-idx
     :simulations/dt]
    (let [id->z0 identity
          id->z1 identity
          id->dz (fn [s] (str "d_" s))
          id->dze (fn [s] (str "d_" s "_error"))
          emit (ce/debug-emitter {"sqrt" "sqrtf"
                                  "pow" "powf"
                                  "copy-sign" "copysignf"
                                  "ln" "logf"
                                  "exp" "expf"
                                  "abs" "fabsf"})
          out-set-block-parameters (apply str
                                          (map (fn [[vname vval]]
                                                 (str "float " vname "_parameter = " vval  ";\n"))
                                               (first block-parameters)))
          out-state_variable_declarations (apply str
                                                 (for [d all-state-vars]
                                                   (str "float " d ";\n"
                                                        "float " (id->dze d) ";\n")))
          out-shared_algebra_declarations (apply str
                                                 (for [d (map first shared-algebra-assignment)]
                                                   (str "float " d ";\n")))
          out-initial_conditions (apply str
                                        (map (fn [v]
                                               (let [z0 (id->z0 v)
                                                     dze (id->dze v)
                                                     ic (compute-initial-conditions v
                                                                                    n-idx
                                                                                    0)]
                                                 (str z0 " = " ic ";\n"
                                                      dze " = 0.0;\n")))
                                             all-state-vars))
          out-shared-algebra (apply str
                                    (for [[id e] shared-algebra-assignment]
                                      (str id " = " e "\n")))
          out-compute (-> (integrator-fn dt id->dz)
                          (combiner-fn id->z0 id->z1 id->dze)
                          emit)]
      (dj/replace-map (dc/resource-as-str "dj/cuda/debug_template.c")
                      {"__n_idx" (str n-idx)
                       "__dt" (str dt)
                       "__set_block_parameters" out-set-block-parameters
                       "__state_variable_declarations;" out-state_variable_declarations
                       "__shared_algebra_declarations;" out-shared_algebra_declarations
                       "__shared_algebra;" out-shared-algebra
                       "__initial_conditions;" out-initial_conditions
                       "__compute;" out-compute})))})
