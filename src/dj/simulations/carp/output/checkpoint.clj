(ns dj.simulations.carp.output.checkpoint
  (:require [dj.repl]))

;; state files are a pretty ridiculoussly stupid format
;; (float) time
;; (int) node-count
;; (byte * node-count) node-mask
;; sequence of global var values of type: (double * node-count) gvar-array
;; sequence of states of a particular region:
;;   each region looks like:
;;   (int) name-length
;;   (byte * name-length) imp-name
;;   ([((float | double) * sv-count) state-var-values] * local-region-node-count)

;; also there appears to be an intrinsic extra 4 byte 0 pad at the end
;; of each state-var set
(defn read-state
  "extracts state information from byte-array"
  [^bytes ba args]
  (let [{:keys [globals imps]} args
        zero (int 0)
        bb (-> ba
               java.nio.ByteBuffer/wrap
               (.order java.nio.ByteOrder/LITTLE_ENDIAN))
        fast-forward (fn [length]
                       (.position bb
                                  (+ (.position bb)
                                     length)))
        get-byte-vec (fn [size]
                       (let [ret (byte-array size)]
                         (.get bb
                               ret
                               zero
                               size)
                         (vec ret)))
        get-double-vec (fn [size]
                         (let [ret (double-array size)]
                           (-> (.asDoubleBuffer bb)
                               (.get ret
                                     zero
                                     size))
                           (fast-forward (* 8 size))
                           (vec ret)))
        get-float-vec (fn [size]
                         (let [ret (float-array size)]
                           (-> (.asFloatBuffer bb)
                               (.get ret
                                     zero
                                     size))
                           (fast-forward (* 4 size))
                           (vec ret)))
        get-string (fn [size]
                     (let [ret (byte-array size)]
                       (-> (.get bb
                                 ret
                                 zero
                                 size))
                       (String. ret)))
        state:time (.getFloat bb)
        state:node-count (.getInt bb)
        ;; node-mask: node:id -> region/imp-id
        state:node-mask (get-byte-vec state:node-count)
        region:id->node:ids (loop [ret {}
                                   node:id 0]
                              (if (< node:id state:node-count)
                                (recur (update-in ret
                                                  [(state:node-mask node:id)]
                                                  (fn [node:ids]
                                                    (if (empty? node:ids)
                                                      [node:id]
                                                      (conj node:ids node:id))))
                                       (inc node:id))
                                ret))
        state:globals (reduce (fn [ret global-name]
                                (assoc ret
                                  global-name
                                  (get-double-vec state:node-count)))
                              {}
                              globals)
        state:regions (reduce (fn [ret region:id]
                                (let [imp:name-length (.getInt bb)
                                      imp:name (try (get-string imp:name-length)
                                                    (catch Exception e
                                                      (throw (ex-info "crash"
                                                                      (dj.repl/local-context)))))]
                                  ;; state:next (get-double-vec 49)
                                  ;; state:next2 (get-float-vec 5)
                                  (assoc ret
                                    {:imp:name imp:name
                                     :region:id region:id}
                                    (let [imp (imps imp:name)
                                          state-var:types (:types imp)
                                          state-var:names (:order imp)
                                          node:ids (region:id->node:ids region:id)]
                                      (mapv (fn [node:id]
                                              (persistent!
                                               (reduce (fn [ret' state-var:name]
                                                         (assoc! ret'
                                                                 state-var:name
                                                                 (case (state-var:types state-var:name)
                                                                   :double (.getDouble bb)
                                                                   :float (.getFloat bb))))
                                                       (transient {})
                                                       state-var:names)))
                                            node:ids)))))
                              {}
                              (range (count (keys region:id->node:ids))))]
    {:time state:time
     :node-count state:node-count
     :node-mask state:node-mask
     :globals state:globals
     :regions state:regions}))
