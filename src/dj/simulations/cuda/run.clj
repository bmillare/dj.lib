(ns dj.simulations.cuda.run
  (:require [dj.dispatch.treefn :as tf]
            [dj.simulations.cuda :as c]))

;;; Purpose
;; - setup/run management

(defn block-parameters->host-block-parameters
  "
data is vector of vector of pairs
"
  [data]
  (let [block-size (count data)
        var-count (count (first data))
        the-array (float-array (* block-size var-count))]
    (doseq [block-id (range block-size)
            vid (range var-count)]
      (aset the-array
            (+ (* block-id var-count)
               vid)
            ;; handle case when value is a map, which dispatches on type of node
            ;; This ONLY works when the variable is a state variable initial condition, not when it is algebra
            (let [num (second ((data block-id) vid))]
              (if (number? num)
                (float num)
                (float 0.0)))))
    the-array))

;; input: host data args
;; output boundary: cuda data args
(def cuda-simulate-fns
  {:cuda.simulate/device-arg-dt
   (tf/fm
    [:simulations/host-arg-dt]
    (c/->float-array host-arg-dt))

   :cuda.simulate/device-arg-rest
   (tf/fm
    [:simulations/host-arg-rest]
    (c/->int-array host-arg-rest))

   :cuda.simulate/device-block-parameters
   (tf/fm
    [:simulations/host-block-parameters]
    (c/->float-array host-block-parameters))
   
   :cuda.simulate/input-device-data
   (tf/fm
    [:simulations/input-host-data]
    (c/->float-array input-host-data))
   :cuda.simulate/shared-device-data
   (tf/fm
    [:simulations/shared-host-data]
    (c/->float-array shared-host-data))

   :cuda.simulate/output-device-data
   (tf/fm
    [:simulations/output-host-data-pre]
    (c/->float-array output-host-data-pre))

   :cuda/kernel-parameters
   (tf/fm
    [:cuda.simulate/device-arg-dt
     :cuda.simulate/device-arg-rest
     :cuda.simulate/device-block-parameters
     :cuda.simulate/input-device-data
     :cuda.simulate/shared-device-data
     :cuda.simulate/output-device-data]
    [device-arg-dt
     device-arg-rest
     device-block-parameters
     input-device-data
     shared-device-data
     output-device-data])

   :cuda.simulate/output-host-data
   (let [F (c/constants :FLOAT)]
     (tf/fm
      [:cuda/invoke-kernel ; side effect dependency
       :cuda/stream
       :simulations/output-data-size
       :cuda.simulate/output-device-data]
      (jcuda.driver.JCudaDriver/cuStreamSynchronize stream)
      (let [output-host-data (float-array output-data-size)]
        (jcuda.driver.JCudaDriver/cuMemcpyDtoH (jcuda.Pointer/to output-host-data)
                                               output-device-data
                                               (* output-data-size F))
        output-host-data)))})

(def cuda-line-simulation-parameters
  {:cuda/extra
   (tf/fm
    []
    nil)
   :cuda/shared-memory-size
   (tf/fm
    []
    16000)
   :cuda/grid-dim-y
   (tf/fm
    []
    1)
   :cuda/grid-dim-z
   (tf/fm
    []
    1)
   :cuda/block-dim-y
   (tf/fm
    []
    1)
   :cuda/block-dim-z
   (tf/fm
    []
    1)})

(def simulation-data
  "Convert array from CUDA job data to clojure friendly format

  Output format is
 :output vector of size node
 - each entry is a map of state vars -> vector of size num-time-steps
"
  (tf/treefm
   {:simulations/output
    (tf/fm
     [:cuda.simulate/output-host-data
      :dotmodel/track-vars
      :dotmodel/all-state-vars
      :simulations/num-nodes
      :simulations/block-parameters]
     ;; time-steps -> nodes -> track-vars -> values
     (let [num-blocks (count block-parameters)
           num-nodes (* num-blocks num-nodes)
           num-track-vars (count track-vars)
           state-track-vars (set all-state-vars)
           time-step-groups (vec (partition (* num-track-vars
                                               num-nodes)
                                            output-host-data))
           initial-conditions (mapv (fn [node-data]
                                      (apply hash-map
                                             (->> (map list 
                                                       track-vars
                                                       node-data)
                                                  (filter (fn [[var-name _]]
                                                            (state-track-vars var-name)))
                                                  (apply concat))))
                                    (vec
                                     (partition num-track-vars
                                                (first time-step-groups))))
           output
           ;; array -> [[xn0yn0xn1yn1]...]
           (reduce (fn [ret time-step-data]
                     ;; [xn0yn0xn1yn1] -> [[xn0yn0][xn1yn1]]
                     (reduce (fn [ret' [node-data n-idx]]
                               ;; [xn0yn0] -> [x y]
                               (reduce (fn [ret'' [name value]]
                                         (update-in ret''
                                                    [n-idx
                                                     name]
                                                    conj
                                                    value))
                                       ret'
                                       (map list
                                            track-vars
                                            node-data)))
                             ret
                             (map list
                                  (partition num-track-vars
                                             time-step-data)
                                  (range num-nodes))))
                   (mapv (fn [_]
                           (reduce (fn [m k]
                                     (assoc m
                                            k
                                            []))
                                   {}
                                   track-vars))
                         (range num-nodes))
                   (rest time-step-groups))]
       {:initial-conditions initial-conditions
        :output output}))}
   :simulations/output))

;; Need an intermediate normal format that is easy to manipulate

;; data form
;; hashmap
;; :initial-conditions var-name->value
;; :output var-name->[values...]

;; Plotting form
;; a xy-data {:x <vector of x data>, :y <vector of y data>}
;; hashmap of xy-data var-name->xy-data
;; vector of hashmaps node-id->hashmap

(defn cuda-data-form
  "
cuda-data: cuda-array
args
 {
:track-vars ,
:state-vars ,
}

# returns

 [{
:initial-conditions var-name->value
:output var-name->[values...]
}...]

"
  [cuda-data {:keys [track-vars state-vars num-nodes block-parameters]}]
  ;; time-steps -> nodes -> track-vars -> values
  (let [num-blocks (count block-parameters)
        num-nodes (* num-blocks num-nodes)
        num-track-vars (count track-vars)
        state-track-vars (set state-vars)
        time-step-groups (vec (partition (* num-track-vars
                                            num-nodes)
                                         cuda-data))
        initial-conditions (mapv (fn [node-data]
                                   (apply hash-map
                                          (->> (map list 
                                                    track-vars
                                                    node-data)
                                               (filter (fn [[var-name _]]
                                                         (state-track-vars var-name)))
                                               (apply concat))))
                                 (vec
                                  (partition num-track-vars
                                             (first time-step-groups))))
        output
        ;; array -> [[xn0yn0xn1yn1]...]
        (reduce (fn [ret time-step-data]
                  ;; [xn0yn0xn1yn1] -> [[xn0yn0][xn1yn1]]
                  (reduce (fn [ret' [node-data n-idx]]
                            ;; [xn0yn0] -> [x y]
                            (reduce (fn [ret'' [name value]]
                                      (update-in ret''
                                                 [n-idx
                                                  name]
                                                 conj
                                                 value))
                                    ret'
                                    (map list
                                         track-vars
                                         node-data)))
                          ret
                          (map list
                               (partition num-track-vars
                                          time-step-data)
                               (range num-nodes))))
                (mapv (fn [_]
                        (reduce (fn [m k]
                                  (assoc m
                                    k
                                    []))
                                {}
                                track-vars))
                      (range num-nodes))
                (rest time-step-groups))]
    {:initial-conditions initial-conditions
     :output output}))

(defn add-time-steps [data {:keys [start-time record-dt]}]
  (let [start-time (or start-time record-dt)
        a-node (first (:output data))
        a-key (first (keys a-node))
        steps (count (a-node a-key))]
    (assoc data
      :time-steps
      (vec (take steps
                 (map (fn [s]
                        (+ start-time
                           (* record-dt s)))
                      (range steps)))))))

;; input: simulation parameters (high level), e.g. timing, dt, end-time
;; output boundary: simulation (non-cuda) arguments (formatted directly for simulation), e.g. num-record-time-steps
(def simulate-fns
  {;; iteration data
   :simulations/iterations-per-record
   (tf/fm
    [:simulations/record-dt :simulations/dt]
    (long (/ record-dt
             dt)))

   ;; iteration data
   :simulations/num-time-steps
   (tf/fm
    [:simulations/end-time :simulations/dt :simulations/iterations-per-record]
    (+ (long (/ end-time
                dt))
       iterations-per-record))

   ;; iteration data
   :simulations/num-record-time-steps
   (tf/fm
    [:simulations/num-time-steps :simulations/iterations-per-record]
    (/ num-time-steps
       iterations-per-record))

   :simulations/compute-initial-conditions
   (tf/fm
    [:simulations/block-parameters
     :dotmodel/initial-conditions-assignment]
    (let [block-parameters-vec (mapv (fn [c]
                                       (reduce (fn [m [k v]]
                                                 (assoc m
                                                        k v))
                                               {}
                                               c))
                                     block-parameters)
          default-ic (reduce (fn [ret [id v]]
                               (assoc ret
                                      id (Double/parseDouble (dj/substring v
                                                                           0
                                                                           -1))))
                             {}
                             initial-conditions-assignment)]
      (fn [id node-id block-id]
        (let [default-val (if-let [result (default-ic id)]
                            result
                            (double 0))]
          (if-let [block-parameters (get block-parameters-vec
                                         block-id)]
            (if-let [block-setting (block-parameters id)]
              (if (number? block-setting) ;; Dispatch on value type, maps correspond to vary on node-id
                (double block-setting)
                (double (block-setting node-id)))
              default-val)
            default-val)))))

   :simulations/host-arg-dt
   (tf/fm
    [:simulations/dt]
    (float-array [dt]))

   :simulations/host-block-parameters
   (tf/fm
    [:simulations/block-parameters]
    (block-parameters->host-block-parameters block-parameters))

   :simulations/num-blocks
   (tf/fm
    [:simulations/block-parameters]
    (count block-parameters))

   :cuda/block-dim-x
   (tf/fm
    [:simulations/num-nodes]
    num-nodes)

   :cuda/grid-dim-x
   (tf/fm
    [:simulations/num-blocks]
    num-blocks)

   :simulations/initialize-host-array-fn
   (tf/fm
    [:simulations/num-blocks
     :simulations/num-nodes
     :simulations/compute-initial-conditions]
    (fn [the-array vars]
      (doseq [block-id (range num-blocks)
              node-id (range num-nodes)
              [v v-idx] (map vector vars (range))]
        (aset the-array
              (+ (* block-id num-nodes (count vars))
                 (* node-id (count vars))
                 v-idx)
              (float (compute-initial-conditions v node-id block-id))))
      the-array))

   :simulations/num-parameters
   (tf/fm
    [:simulations/block-parameters]
    (count (first block-parameters)))

   :simulations/host-arg-rest
   (tf/fm
    [:simulations/num-nodes
     :simulations/num-time-steps
     :simulations/iterations-per-record
     :simulations/num-parameters]
    (int-array [num-nodes num-time-steps iterations-per-record num-parameters]))

   :simulations/num-state-vars
   (tf/fm
    [:dotmodel/all-state-vars]
    (count all-state-vars))

   :simulations/input-data-size
   (tf/fm
    [:simulations/num-state-vars
     :simulations/num-nodes
     :simulations/num-blocks]
    (* num-state-vars num-nodes num-blocks))

   :simulations/input-host-data
   (tf/fm
    [:dotmodel/all-state-vars
     :simulations/input-data-size
     :simulations/initialize-host-array-fn]
    (initialize-host-array-fn (float-array input-data-size)
                              all-state-vars))

   :simulations/shared-vars
   (tf/fm
    [:dotmodel/shared-vars]
    (or shared-vars [0.0]))

   :simulations/num-shared-vars
   (tf/fm
    [:simulations/shared-vars]
    (count shared-vars))

   :simulations/shared-data-size
   (tf/fm
    [:simulations/num-shared-vars
     :simulations/num-nodes
     :simulations/num-blocks]
    (* num-shared-vars num-nodes num-blocks))

   :simulations/shared-host-data
   (tf/fm
    [:simulations/shared-vars
     :simulations/shared-data-size
     :simulations/initialize-host-array-fn]
    (initialize-host-array-fn (float-array shared-data-size)
                              shared-vars))

   :simulations/num-track-vars
   (tf/fm
    [:dotmodel/track-vars]
    (count track-vars))

   :simulations/output-data-size
   (tf/fm
    [:simulations/num-track-vars
     :simulations/num-record-time-steps
     :simulations/num-nodes
     :simulations/num-blocks]
    (* num-track-vars num-record-time-steps num-nodes num-blocks))

   :simulations/output-host-data-pre
   (tf/fm
    [:dotmodel/track-vars
     :simulations/output-data-size
     :simulations/initialize-host-array-fn]
    (initialize-host-array-fn (float-array output-data-size)
                              track-vars))})
