(ns dj.simulations.cuda.unused)

;;; not used [2015-04-22]

(defn sub-state
  [data start end]
  (-> data
      (update-in [:time-steps]
                 subvec
                 start end)
      (assoc
       :output
       (mapv (fn [node-data]
               (dj/update-vals node-data
                               subvec
                               start
                               end))
             (:output data)))))

(defn join-time-steps [d1 d2]
  (-> d1
      (update-in [:time-steps]
                 into
                 (:time-steps d2))))

(defn NaN-variables [snapshot-state]
  (mapv (fn [node]
          (reduce (fn [s k]
                    (if (.isNaN (node k))
                      (conj s k)
                      s))
                  #{}
                  (keys node)))
        snapshot-state))

(defn index-of-NaN [data]
  (dj/index-of data NaNs? nth-state))

(defn select-vars [data vars]
  (let [{:keys [initial-conditions output]} data]
    (assoc data
      :initial-conditions (mapv (fn [n]
                                  (select-keys n vars))
                                initial-conditions)
      :output (mapv (fn [n]
                      (select-keys n vars))
                    output))))

(defn map-data
  "
  Applies f to all elements of d1 and d2 in pairs
  "
  [f d1 d2]
  {:initial-conditions (mapv (fn [n1 n2]
                               (reduce (fn [m k]
                                         (assoc m
                                                k
                                                (f (n1 k)
                                                   (n2 k))))
                                       {}
                                       (keys n1)))
                             (:initial-conditions d1)
                             (:initial-conditions d2))
   :output (mapv (fn [n1 n2]
                   (reduce (fn [m k]
                             (assoc m
                                    k
                                    (map f
                                         (n1 k)
                                         (n2 k))))
                           {}
                           (keys n1)))
                 (:output d1)
                 (:output d2))
   :time-steps (:time-steps d1)})

(defn difference-data [d1 d2]
  (map-data - d1 d2))

(defn normalized-difference-data [d1 d2]
  {:initial-conditions (mapv (fn [n1 n2]
                               (reduce (fn [m k]
                                         (assoc m
                                                k
                                                (/ (Math/abs (- (n1 k)
                                                                (n2 k)))
                                                   (max (n1 k)
                                                        (n2 k)))))
                                       {}
                                       (keys n1)))
                             (:initial-conditions d1)
                             (:initial-conditions d2))
   :output (mapv (fn [n1 n2]
                   (reduce (fn [m k]
                             (assoc m
                                    k
                                    (map (fn [e1 e2]
                                           (/ (Math/abs (- e1 e2))
                                              (max e1 e2)))
                                         (n1 k)
                                         (n2 k))))
                           {}
                           (keys n1)))
                 (:output d1)
                 (:output d2))
   :time-steps (:time-steps d1)})

(defn grouped-max-output [data]
  (mapv (fn [node]
          (reduce (fn [m k]
                    (assoc m
                      k
                      (apply max (node k))))
                  {}
                  (keys node)))
        (:output data)))

(defn grouped-mean-output [data]
  (mapv (fn [node]
          (reduce (fn [m k]
                    (assoc m
                      k
                      (let [e (node k)]
                        (/ (apply + e)
                           (count e)))))
                  {}
                  (keys node)))
        (:output data)))

(defn typecast-float [e]
  (case (type e)
    :symbolic-expression (case (:op e)
                           "var" e
                           (assoc e
                             :children (mapv typecast-float (:children e))))
    (float e)))

(def dependency-graph (dc/->let-fn (merge rse/base-fns
                                          dependency-fns)
                                   :graphviz
                                   [:config]))
