(ns dj.simulations.cuda.main
  (:require [dj.dispatch.treefn :as tf]
            [dj.simulations.cuda.run :as r]
            [dj.simulations.cuda :as c]
            [dj.simulations.cuda.compile :as l]
            [dj.simulations.cuda.solvers :as s]
            [dj.simulations.integrator :as i]
            [dj.simulations.carp.input.dotmodel :as d]))

#_ (defn run
  "

runs solver & debug info

"
  [config]
  (let [{:keys [cuda-device-id]} config]
    ;; 16384, max shared memory per block in bytes
    ;; 8192, max 32-bit registers per block
    (log (in-cuda-ctx cuda-device-id
                      debug-info)
         "debug-info")
    (-> (in-cuda-ctx cuda-device-id
                  (solve-multiple-network (-> config
                                              t/add-iteration-data)))
        (dcd/cuda-data-form config))))

(def run
  (tf/treefm
   (merge
    r/cuda-simulate-fns              ; host data args → device data args, also device output data → host data
    r/simulate-fns                   ; high level simulation parameters → generic host data args
    c/invoke-kernel-fns              ; device data args → invoke kernel side effect

    l/compile-fns                    ; cu → cubin
    s/solver-code-fns                ; cu generator
    i/backward-formulation-fns       ; formulation for rosenbrock/integrator-fn
    i/rosenbrock-integrator-fns      ; integrator-fn for cu
    i/constrained-kahan-combiner-fns ; combiner-fn for cu
    c/setup-cuda                     ; MISSING DEPENDENCY something needs :cuda/context, generic cuda setup, cubin → kernel (fn)
    d/dotmodel-fns                   ; dotmodel txt → clj dotmodel data
    )
   :cuda.simulate/output-host-data))

;; MISSING DESTRUCTORS for run

#_ (defn managed-run
  "
A cuda call shouldn't take longer than a second

Limitation: :estimated-clockspeed * :dt must be >= 1
"
  
  [config]
  (let [{:keys [cuda-device-id estimated-clockspeed dt]} config
        manager-end-time (:end-time config)
        initial-run-time-step (Math/floor
                               (double (* dt
                                          estimated-clockspeed)))
        start-date (java.util.Date.)
        start-time (System/currentTimeMillis)
        add-simulation-metadata (fn [result]
                                  (assoc result
                                    :simulation-metadata {:start-date start-date
                                                          :run-time (/ (- (System/currentTimeMillis) start-time)
                                                                       1e3)}))]
    ;; 16384, max shared memory per block in bytes
    ;; 8192, max 32-bit registers per block
    (log (in-cuda-ctx cuda-device-id
                      debug-info)
         "debug-info")
    (loop [run-config (-> config
                          (assoc :end-time initial-run-time-step)
                          t/add-iteration-data)
           previous-result nil
           time-progressed 0.0
           ;; run-time-step appears to have previously been dynamic
           run-time-step initial-run-time-step]
      (let [result (-> (in-cuda-ctx cuda-device-id
                                    (solve-multiple-network run-config))
                       (dcd/cuda-data-form run-config))
            last-state (dcd/last-state result)
            next-time-progressed (+ time-progressed run-time-step)]
        (if (and previous-result
                 (dcd/NaNs? last-state))
          (add-simulation-metadata
           {:error "NaN detected"
            :last-state last-state
            :previous-result previous-result
            :result result})
          (let [joined-result (if previous-result
                                (dcd/join-output previous-result
                                                 result)
                                result)]
            (if (>= next-time-progressed manager-end-time)
              ;; Package Output
              (-> joined-result
                  (dcd/add-time-steps config)
                  add-simulation-metadata)
              (recur (-> run-config
                         t/add-iteration-data
                         (assoc :compute-initial-conditions (dcd/->initial-conditions-fn config last-state)))
                     joined-result
                     next-time-progressed
                     run-time-step))))))))
