(ns dj.math.gauss-elim.alpha
  (:require [clojure.pprint]
            [clojure.set :as cs]
            [dj.math.gauss-elim.html-view :as hv]
            #_ [clojure.data.int-map :as i]))

;;; Datastructure Design 1
;; Idea: Make swap, multiply, and scalar*multiply+add as cheap as possible
;; 1) row-id-order (makes swap cheap)
;; - vector of row-ids that represents the current order
;; 2) original-order-rows
;; - vector of maps(x-ids->expressions)

(defn print-matrix [row-id-order
                    original-order-rows]
  (let [ks (sort (apply cs/union (map (comp set keys) original-order-rows)))
        new-order-rows (reduce (fn [ret row-id]
                                 (conj ret
                                       (original-order-rows row-id)))
                               []
                               row-id-order)]
    (clojure.pprint/print-table
     ks
     new-order-rows)))

(defn diy-gaussian-elimination [user-ops]
  (let [b [1 2 3]
        row-id-order [0 1 2]
        num-rows (count b)
        original-order-rows [{0 1
                              1 1
                              2 1}
                             {0 -1
                              1 3
                              2 2}
                             {0 2
                              1 1
                              2 1}]
        augmented-order-rows (mapv
                              (fn [ri bi]
                                (assoc ri
                                       num-rows
                                       bi))
                              original-order-rows
                              b)]
    (println "start")
    (print-matrix row-id-order
                  augmented-order-rows)
    (loop [ops #_ [[:comb 0 1 1]
                   [:swap 1 2]
                   [:mult 0 2]]
           user-ops
           rio row-id-order
           cor augmented-order-rows]
      (if (empty? ops)
        nil
        (let [op (first ops)
              op-name (first op)]
          (case op-name
            :swap
            (let [[_ r1 r2] op
                  rio' (-> rio
                           (assoc r1 r2)
                           (assoc r2 r1))]
              (println "swap" r1 r2)
              (print-matrix rio'
                            cor)
              (recur (rest ops)
                     rio'
                     cor))
            :mult
            (let [[_ rid alpha] op
                  o-rid (rio rid)
                  r (cor o-rid)
                  r' (reduce-kv (fn [ret k v]
                                  (assoc ret
                                         k
                                         (* alpha v)))
                                {}
                                r)
                  cor' (assoc cor
                              o-rid
                              r')]
              (println "multiply" rid alpha)
              (print-matrix rio
                            cor')
              (recur (rest ops)
                     rio
                     cor'))
            :comb
            (let [[_ rid-source alpha rid-target] op
                  o-source-rid (rio rid-source)
                  o-target-rid (rio rid-target)
                  r-source (cor o-source-rid)
                  r-target (cor o-target-rid)
                  r-target' (reduce-kv (fn [ret k sv]
                                         (update ret
                                                 k
                                                 (fn [tv]
                                                   (if tv
                                                     (+ (* alpha sv) tv)
                                                     (* alpha sv)))))
                                       r-target
                                       r-source)
                  cor' (assoc cor
                              o-target-rid
                              r-target')]
              (println "combine" rid-source alpha rid-target)
              (print-matrix rio
                            cor')
              (recur (rest ops)
                     rio
                     cor'))))))))

;;; Datastructure Design 2
;; Operations:
;; 1) need to keep track of non-zero entries for a given column
;; 2) want to get largest/smallest entry within a column (largest pivot)
;; - might want to just do linear scan and not maintain index, since
;;   at most, one scan per column, but multiple updates for index
;; Plan:
;; - need col-id->row-id->value
;;   - vector{row-id->value}
;; - no need for column-id indirection because no column swap
;; - but because of overhead, might just ignore value, and pull it
;;   from current-order-rows

(defn auto1-gaussian-elimination []
  (let [b [1 2 3]
        row-id-order [0 1 2]
        num-rows (count b)
        original-order-rows [{0 1
                              1 1
                              2 1}
                             {0 -1
                              1 3
                              2 2}
                             {0 2
                              1 1
                              2 1}]
        col-id->row-ids [#{0 1 2}
                         #{0 1 2}
                         #{0 1 2}]
        augmented-order-rows (mapv
                              (fn [ri bi]
                                (assoc ri
                                       num-rows
                                       bi))
                              original-order-rows
                              b)]
    (println "start")
    (print-matrix row-id-order
                  augmented-order-rows)
    (loop [state :swap-rows
           p-crid 0 ;; p-rid = p-cid for our stuff
           rio row-id-order
           coor augmented-order-rows
           cid->rids col-id->row-ids]
      (do
        (prn "state" state)
        (prn "p-crid" p-crid)
        (prn "rio" rio)
        (prn "coor" coor)
        (prn "cid->rids" cid->rids)
        (case state
          :swap-rows
          (let [p-crid-swap
                (loop [nz-rids (filter (fn [rid]
                                         (not (< rid p-crid)))
                                       (cid->rids p-crid))
                       best-pid p-crid
                       best-p-value ((coor (rio p-crid)) p-crid)]
                  (if (empty? nz-rids)
                    best-pid
                    (let [nz-rid (first nz-rids)
                          best-p-value' (Math/abs (double
                                                   (-> nz-rid
                                                       rio
                                                       coor
                                                       (get p-crid))))]
                      (if (and (< best-p-value'
                                  best-p-value)
                               (not (zero? best-p-value')))
                        (recur (rest nz-rids)
                               nz-rid
                               best-p-value')
                        (recur (rest nz-rids)
                               best-pid
                               best-p-value)))))]
            (if (= p-crid-swap p-crid)
              (do
                (prn "pivot the same")
                (recur :zero-below
                       p-crid
                       rio
                       coor
                       cid->rids))
              (let [rid1 p-crid
                    rid2 p-crid-swap
                    _ (println [rid1 rid2])
                    rio' (-> rio
                             (assoc rid1 rid2)
                             (assoc rid2 rid1))
                    r1 (coor (rio rid1))
                    r2 (coor (rio rid2))
                    r1-cols (keys r1)
                    r2-cols (keys r2)
                    union-cols (-> (into (set r1-cols) r2-cols)
                                   ;; exclude b column, b is always full
                                   (disj num-rows))
                    cid->rids' (reduce (fn [ret cid]
                                         (update ret
                                                 cid
                                                 (fn [rids]
                                                   (if (rids rid1)
                                                     (if (rids rid2)
                                                       rids
                                                       (-> rids
                                                           (conj rid2)
                                                           (disj rid1)))
                                                     (if (rids rid2)
                                                       (-> rids
                                                           (conj rid1)
                                                           (disj rid2))
                                                       rids)))))
                                       cid->rids
                                       union-cols)]
                (println "swap" rid1 rid2)
                (print-matrix rio'
                              coor)
                (recur :zero-below
                       p-crid
                       rio'
                       coor
                       cid->rids'))))
          :zero-below
          (let [p-crid' (inc p-crid)]
            (if (< p-crid' num-rows)
              (let [rid-source p-crid
                    o-source-rid (rio rid-source)
                    r-source (coor o-source-rid)
                    p-v (r-source rid-source)
                    [coor' cid->rids']
                    (loop [rid-targets (filter (fn [rid]
                                                 (> rid p-crid))
                                               (cid->rids p-crid))
                           coor coor
                           cid->rids cid->rids]
                      (if (empty? rid-targets)
                        [coor
                         cid->rids]
                        (let [rid-target (first rid-targets)
                              o-target-rid (rio rid-target)
                              r-target (coor o-target-rid)
                              alpha (- (/ (r-target p-crid)
                                          p-v))
                              r-target'
                              (-> (reduce-kv
                                   (fn [ret k sv]
                                     (update
                                      ret
                                      k
                                      (fn [tv]
                                        (if tv
                                          ;; Potentially, if the following is zero,
                                          ;; we could do further optimizations with
                                          ;; the indices but may not be worth the
                                          ;; benefit given how rare that is
                                          (+ (* alpha sv) tv)
                                          (* alpha sv)))))
                                   r-target
                                   r-source)
                                  ;; zero below pivot
                                  (dissoc rid-source))
                              coor' (assoc coor
                                           o-target-rid
                                           r-target')
                              cid->rids' (update cid->rids
                                                 rid-source
                                                 disj
                                                 rid-target)]
                          (println "rid-target" rid-target)
                          (println "o-target-rid" o-target-rid)
                          (println "r-target" r-target)
                          (println "p-v" p-v)
                          (println "combine" rid-source alpha rid-target)
                          (print-matrix rio
                                        coor')
                          (recur (rest rid-targets)
                                 coor'
                                 cid->rids'))))]
                (recur :swap-rows
                       p-crid'
                       rio
                       coor'
                       cid->rids'))
              (recur :pivots->1
                     p-crid
                     rio
                     coor
                     cid->rids)))
          :pivots->1
          (if (< p-crid 0)
            (recur :back-subst
                   (dec num-rows)
                   rio
                   coor
                   cid->rids)
            (let [rid p-crid
                  o-rid (rio rid)
                  r (coor o-rid)
                  p-v (r rid)
                  alpha (/ 1 p-v)
                  r' (reduce-kv (fn [ret k v]
                                  (assoc ret
                                         k
                                         (* alpha v)))
                                {}
                                r)
                  coor' (assoc coor
                               o-rid
                               r')]
              (println "multiply" rid alpha)
              (print-matrix rio
                            coor')
              (recur :pivots->1
                     (dec p-crid)
                     rio
                     coor'
                     cid->rids)))
          :back-subst
          (loop [p-crid p-crid
                 coor coor
                 cid->rids cid->rids]
            (prn "p-crid" p-crid)
            (prn "coor" coor)
            (prn "cid->rids" cid->rids)
            (print-matrix rio
                          coor)
            (if (< p-crid 1)
              nil
              (let [p-b (get (coor (rio p-crid)) num-rows)
                    coor' (reduce (fn [ret rid]
                                    (update ret
                                            (rio rid)
                                            (fn [r]
                                              (let [e (r p-crid)
                                                    bi (r num-rows)
                                                    alpha (- e)]
                                                (-> r
                                                    (dissoc p-crid)
                                                    (assoc num-rows
                                                           (+ (* alpha p-b)
                                                              bi)))))))
                                  coor
                                  (disj (cid->rids p-crid)
                                        p-crid))
                    cid->rids' (assoc cid->rids
                                      p-crid
                                      #{p-crid})]
                (recur (dec p-crid)
                       coor'
                       cid->rids'))))
          :end
          nil)))))

(defn auto2-gaussian-elimination [augmented-original-order-rows]
  (let [num-rows (count augmented-original-order-rows)
        row-id-order (vec (range num-rows))
        col-id->row-ids (let [row-ids (set row-id-order)]
                          (mapv (fn [_]
                                  row-ids)
                                row-id-order))]
    (println "start")
    (println "num-rows" num-rows)
    (print "row-id-order" row-id-order)
    (print-matrix row-id-order
                  augmented-original-order-rows)
    (loop [state :swap-rows
           p-crid 0 ;; p-rid = p-cid for our stuff
           rio row-id-order
           coor augmented-original-order-rows
           cid->rids col-id->row-ids]
      (do
        (prn "state" state)
        (prn "p-crid" p-crid)
        (prn "rio" rio)
        (prn "coor" coor)
        (prn "cid->rids" cid->rids)
        (case state
          :swap-rows
          (let [p-crid-swap
                (loop [nz-rids (filter (fn [rid]
                                         (not (< rid p-crid)))
                                       (cid->rids p-crid))
                       best-pid p-crid
                       best-p-value ((coor (rio p-crid)) p-crid)]
                  (if (empty? nz-rids)
                    best-pid
                    (let [nz-rid (first nz-rids)
                          best-p-value' (Math/abs (double
                                                   (-> nz-rid
                                                       rio
                                                       coor
                                                       (get p-crid))))]
                      (if (and (< best-p-value'
                                  best-p-value)
                               (not (zero? best-p-value')))
                        (recur (rest nz-rids)
                               nz-rid
                               best-p-value')
                        (recur (rest nz-rids)
                               best-pid
                               best-p-value)))))]
            (if (= p-crid-swap p-crid)
              (do
                (prn "pivot the same")
                (recur :zero-below
                       p-crid
                       rio
                       coor
                       cid->rids))
              (let [rid1 p-crid
                    rid2 p-crid-swap
                    _ (println [rid1 rid2])
                    o-rid1 (rio rid1)
                    o-rid2 (rio rid2)
                    rio' (-> rio
                             (assoc rid1 o-rid2)
                             (assoc rid2 o-rid1))
                    r1 (coor (rio rid1))
                    r2 (coor (rio rid2))
                    r1-cols (keys r1)
                    r2-cols (keys r2)
                    union-cols (-> (into (set r1-cols) r2-cols)
                                   ;; exclude b column, b is always full
                                   (disj num-rows))
                    cid->rids' (reduce (fn [ret cid]
                                         (update ret
                                                 cid
                                                 (fn [rids]
                                                   (if (rids rid1)
                                                     (if (rids rid2)
                                                       rids
                                                       (-> rids
                                                           (conj rid2)
                                                           (disj rid1)))
                                                     (if (rids rid2)
                                                       (-> rids
                                                           (conj rid1)
                                                           (disj rid2))
                                                       rids)))))
                                       cid->rids
                                       union-cols)]
                (println "swap" rid1 rid2)
                (print-matrix rio'
                              coor)
                (recur :zero-below
                       p-crid
                       rio'
                       coor
                       cid->rids'))))
          :zero-below
          (let [p-crid' (inc p-crid)]
            (if (< p-crid' num-rows)
              (let [rid-source p-crid
                    o-source-rid (rio rid-source)
                    r-source (coor o-source-rid)
                    p-v (r-source rid-source)
                    [coor' cid->rids']
                    (loop [rid-targets (filter (fn [rid]
                                                 (> rid p-crid))
                                               (cid->rids p-crid))
                           coor coor
                           cid->rids cid->rids]
                      (if (empty? rid-targets)
                        [coor
                         cid->rids]
                        (let [rid-target (first rid-targets)
                              o-target-rid (rio rid-target)
                              r-target (coor o-target-rid)
                              alpha (- (/ (r-target p-crid)
                                          p-v))
                              r-target'
                              (-> (reduce-kv
                                   (fn [ret k sv]
                                     (update
                                      ret
                                      k
                                      (fn [tv]
                                        (if tv
                                          ;; Potentially, if the following is zero,
                                          ;; we could do further optimizations with
                                          ;; the indices but may not be worth the
                                          ;; benefit given how rare that is
                                          (+ (* alpha sv) tv)
                                          (* alpha sv)))))
                                   r-target
                                   r-source)
                                  ;; zero below pivot
                                  (dissoc rid-source))
                              coor' (assoc coor
                                           o-target-rid
                                           r-target')
                              cid->rids' (update cid->rids
                                                 rid-source
                                                 disj
                                                 rid-target)]
                          (println "rid-target" rid-target)
                          (println "o-target-rid" o-target-rid)
                          (println "r-target" r-target)
                          (println "p-v" p-v)
                          (println "combine" rid-source alpha rid-target)
                          (print-matrix rio
                                        coor')
                          (recur (rest rid-targets)
                                 coor'
                                 cid->rids'))))]
                (recur :swap-rows
                       p-crid'
                       rio
                       coor'
                       cid->rids'))
              (recur :pivots->1
                     p-crid
                     rio
                     coor
                     cid->rids)))
          :pivots->1
          (if (< p-crid 0)
            (recur :back-subst
                   (dec num-rows)
                   rio
                   coor
                   cid->rids)
            (let [rid p-crid
                  o-rid (rio rid)
                  r (coor o-rid)
                  p-v (r rid)
                  alpha (/ 1 p-v)
                  r' (reduce-kv (fn [ret k v]
                                  (assoc ret
                                         k
                                         (* alpha v)))
                                {}
                                r)
                  coor' (assoc coor
                               o-rid
                               r')]
              (println "multiply" rid alpha)
              (print-matrix rio
                            coor')
              (recur :pivots->1
                     (dec p-crid)
                     rio
                     coor'
                     cid->rids)))
          :back-subst
          (loop [p-crid p-crid
                 coor coor
                 cid->rids cid->rids]
            (prn "p-crid" p-crid)
            (prn "coor" coor)
            (prn "cid->rids" cid->rids)
            (print-matrix rio
                          coor)
            (if (< p-crid 1)
              nil
              (let [p-b (get (coor (rio p-crid)) num-rows)
                    coor' (reduce (fn [ret rid]
                                    (update ret
                                            (rio rid)
                                            (fn [r]
                                              (let [e (r p-crid)
                                                    bi (r num-rows)
                                                    alpha (- e)]
                                                (-> r
                                                    (dissoc p-crid)
                                                    (assoc num-rows
                                                           (+ (* alpha p-b)
                                                              bi)))))))
                                  coor
                                  (disj (cid->rids p-crid)
                                        p-crid))
                    cid->rids' (assoc cid->rids
                                      p-crid
                                      #{p-crid})]
                (recur (dec p-crid)
                       coor'
                       cid->rids'))))
          :end
          nil)))))

;; quiet version
(defn auto3-gaussian-elimination [augmented-original-order-rows]
  (let [num-rows (count augmented-original-order-rows)
        row-id-order (vec (range num-rows))
        col-id->row-ids (let [row-ids (set row-id-order)]
                          (mapv (fn [_]
                                  row-ids)
                                row-id-order))]
    (println "start")
    (println "num-rows" num-rows)
    (print "row-id-order" row-id-order)
    (print-matrix row-id-order
                  augmented-original-order-rows)
    (loop [state :swap-rows
           p-crid 0 ;; p-rid = p-cid for our stuff
           rio row-id-order
           coor augmented-original-order-rows
           cid->rids col-id->row-ids]
      (case state
        :swap-rows
        (let [p-crid-swap
              (loop [nz-rids (filter (fn [rid]
                                       (not (< rid p-crid)))
                                     (cid->rids p-crid))
                     best-pid p-crid
                     best-p-value ((coor (rio p-crid)) p-crid)]
                (if (empty? nz-rids)
                  best-pid
                  (let [nz-rid (first nz-rids)
                        best-p-value' (Math/abs (double
                                                 (-> nz-rid
                                                     rio
                                                     coor
                                                     (get p-crid))))]
                    (if (and (< best-p-value'
                                best-p-value)
                             (not (zero? best-p-value')))
                      (recur (rest nz-rids)
                             nz-rid
                             best-p-value')
                      (recur (rest nz-rids)
                             best-pid
                             best-p-value)))))]
          (if (= p-crid-swap p-crid)
            (recur :zero-below
                   p-crid
                   rio
                   coor
                   cid->rids)
            (let [rid1 p-crid
                  rid2 p-crid-swap
                  o-rid1 (rio rid1)
                  o-rid2 (rio rid2)
                  rio' (-> rio
                           (assoc rid1 o-rid2)
                           (assoc rid2 o-rid1))
                  r1 (coor (rio rid1))
                  r2 (coor (rio rid2))
                  r1-cols (keys r1)
                  r2-cols (keys r2)
                  union-cols (-> (into (set r1-cols) r2-cols)
                                 ;; exclude b column, b is always full
                                 (disj num-rows))
                  cid->rids' (reduce (fn [ret cid]
                                       (update ret
                                               cid
                                               (fn [rids]
                                                 (if (rids rid1)
                                                   (if (rids rid2)
                                                     rids
                                                     (-> rids
                                                         (conj rid2)
                                                         (disj rid1)))
                                                   (if (rids rid2)
                                                     (-> rids
                                                         (conj rid1)
                                                         (disj rid2))
                                                     rids)))))
                                     cid->rids
                                     union-cols)]
              (recur :zero-below
                     p-crid
                     rio'
                     coor
                     cid->rids'))))
        :zero-below
        (let [p-crid' (inc p-crid)]
          (if (< p-crid' num-rows)
            (let [rid-source p-crid
                  o-source-rid (rio rid-source)
                  r-source (coor o-source-rid)
                  p-v (r-source rid-source)
                  [coor' cid->rids']
                  ;; This can be parallelized, but not a big deal for sparse
                  (loop [rid-targets (filter (fn [rid]
                                               (> rid p-crid))
                                             (cid->rids p-crid))
                         coor coor
                         cid->rids cid->rids]
                    (if (empty? rid-targets)
                      [coor
                       cid->rids]
                      (let [rid-target (first rid-targets)
                            o-target-rid (rio rid-target)
                            r-target (coor o-target-rid)
                            alpha (- (/ (r-target p-crid)
                                        p-v))
                            r-target'
                            (-> (reduce-kv
                                 (fn [ret k sv]
                                   (update
                                    ret
                                    k
                                    (fn [tv]
                                      (if tv
                                        ;; Potentially, if the following is zero,
                                        ;; we could do further optimizations with
                                        ;; the indices but may not be worth the
                                        ;; benefit given how rare that is
                                        (+ (* alpha sv) tv)
                                        (* alpha sv)))))
                                 r-target
                                 r-source)
                                ;; zero below pivot
                                (dissoc rid-source))
                            coor' (assoc coor
                                         o-target-rid
                                         r-target')
                            cid->rids' (update cid->rids
                                               rid-source
                                               disj
                                               rid-target)]
                        (recur (rest rid-targets)
                               coor'
                               cid->rids'))))]
              (recur :swap-rows
                     p-crid'
                     rio
                     coor'
                     cid->rids'))
            (recur :pivots->1
                   p-crid
                   rio
                   coor
                   cid->rids)))
        :pivots->1
        (if (< p-crid 0)
          (recur :back-subst
                 (dec num-rows)
                 rio
                 coor
                 cid->rids)
          (let [rid p-crid
                o-rid (rio rid)
                r (coor o-rid)
                p-v (r rid)
                alpha (/ 1 p-v)
                r' (reduce-kv (fn [ret k v]
                                (assoc ret
                                       k
                                       (* alpha v)))
                              {}
                              r)
                coor' (assoc coor
                             o-rid
                             r')]
            (recur :pivots->1
                   (dec p-crid)
                   rio
                   coor'
                   cid->rids)))
        :back-subst
        (loop [p-crid p-crid
               coor coor
               cid->rids cid->rids]
          (if (< p-crid 1)
            (print-matrix rio
                          coor)
            (let [p-b (get (coor (rio p-crid)) num-rows)
                  coor' (reduce (fn [ret rid]
                                  (update ret
                                          (rio rid)
                                          (fn [r]
                                            (let [e (r p-crid)
                                                  bi (r num-rows)
                                                  alpha (- e)]
                                              (-> r
                                                  (dissoc p-crid)
                                                  (assoc num-rows
                                                         (+ (* alpha p-b)
                                                            bi)))))))
                                coor
                                (disj (cid->rids p-crid)
                                      p-crid))
                  cid->rids' (assoc cid->rids
                                    p-crid
                                    #{p-crid})]
              (recur (dec p-crid)
                     coor'
                     cid->rids'))))
        
        :end
        nil))))

;; Really quiet version
(defn auto4-gaussian-elimination [augmented-original-order-rows]
  (let [num-rows (count augmented-original-order-rows)
        row-id-order (vec (range num-rows))
        col-id->row-ids (let [row-ids (set row-id-order)]
                          (mapv (fn [col-id]
                                  (reduce (fn [ret [row-id row]]
                                            (let [entry (get row col-id)]
                                              (if entry
                                                (conj ret row-id)
                                                ret)))
                                          #{}
                                          (map-indexed vector augmented-original-order-rows)))
                                row-id-order))]
    #_ (throw (ex-info "data"
                    {:col-id->row-ids (take 11 col-id->row-ids)
                     :rows (take 11 augmented-original-order-rows)}))
    (loop [state :swap-rows
           p-crid 0 ;; p-rid = p-cid for our stuff
           rio row-id-order
           coor augmented-original-order-rows
           cid->rids col-id->row-ids]
      (case state
        :swap-rows
        (let [
              p-crid-swap
              (loop [;; select row-ids below pivot
                     nz-rids (filter (fn [rid]
                                       (not (< rid p-crid)))
                                     (cid->rids p-crid))
                     best-pid p-crid
                     best-p-value ((coor (rio p-crid)) p-crid)]
                (if (empty? nz-rids)
                  best-pid
                  (let [nz-rid (first nz-rids)
                        best-p-value' (try
                                        (Math/abs (double
                                                   (-> nz-rid
                                                       rio
                                                       coor
                                                       ;; get entry in this col
                                                       (get p-crid))))
                                        (catch Exception e
                                          (throw (ex-info "error in Math/abs"
                                                          {:nz-rid nz-rid
                                                           :p-crid p-crid
                                                           :nz-rio (-> nz-rid rio)
                                                           :nz-rio-coor (-> nz-rid rio coor)
                                                           :cid->rids (take 11 cid->rids)
                                                           }
                                                          e))))]
                    (if (and (< best-p-value'
                                best-p-value)
                             (not (zero? best-p-value')))
                      (recur (rest nz-rids)
                             nz-rid
                             best-p-value')
                      (recur (rest nz-rids)
                             best-pid
                             best-p-value)))))]
          (if (= p-crid-swap p-crid)
            (recur :zero-below
                   p-crid
                   rio
                   coor
                   cid->rids)
            (let [rid1 p-crid
                  rid2 p-crid-swap
                  o-rid1 (rio rid1)
                  o-rid2 (rio rid2)
                  rio' (-> rio
                           (assoc rid1 o-rid2)
                           (assoc rid2 o-rid1))
                  r1 (coor (rio rid1))
                  r2 (coor (rio rid2))
                  r1-cols (keys r1)
                  r2-cols (keys r2)
                  union-cols (-> (into (set r1-cols) r2-cols)
                                 ;; exclude b column, b is always full
                                 (disj num-rows))
                  cid->rids' (reduce (fn [ret cid]
                                       (update ret
                                               cid
                                               (fn [rids]
                                                 (if (rids rid1)
                                                   (if (rids rid2)
                                                     rids
                                                     (-> rids
                                                         (conj rid2)
                                                         (disj rid1)))
                                                   (if (rids rid2)
                                                     (-> rids
                                                         (conj rid1)
                                                         (disj rid2))
                                                     rids)))))
                                     cid->rids
                                     union-cols)]
              (recur :zero-below
                     p-crid
                     rio'
                     coor
                     cid->rids'))))
        :zero-below
        (let [p-crid' (inc p-crid)]
          (if (< p-crid' num-rows)
            (let [rid-source p-crid
                  o-source-rid (rio rid-source)
                  r-source (coor o-source-rid)
                  p-v (r-source rid-source)
                  [coor' cid->rids']
                  ;; This can be parallelized, but not a big deal for sparse
                  (loop [rid-targets (filter (fn [rid]
                                               (> rid p-crid))
                                             (cid->rids p-crid))
                         coor coor
                         cid->rids cid->rids]
                    (if (empty? rid-targets)
                      [coor
                       cid->rids]
                      (let [rid-target (first rid-targets)
                            o-target-rid (rio rid-target)
                            r-target (coor o-target-rid)
                            alpha (try
                                    (- (/ (r-target p-crid)
                                          p-v))
                                    (catch Exception e
                                      (throw (ex-info "divide by zero"
                                                      {:rid-target rid-target
                                                       :r-target r-target
                                                       :r-source r-source
                                                       :p-crid p-crid
                                                       :p-v p-v}))))
                            r-target'
                            (-> (reduce-kv
                                 (fn [ret k sv]
                                   (update
                                    ret
                                    k
                                    (fn [tv]
                                      (if tv
                                        ;; Potentially, if the following is zero,
                                        ;; we could do further optimizations with
                                        ;; the indices but may not be worth the
                                        ;; benefit given how rare that is
                                        (+ (* alpha sv) tv)
                                        (* alpha sv)))))
                                 r-target
                                 r-source)
                                ;; zero below pivot
                                (dissoc rid-source))
                            zero-cids-of-target (reduce-kv (fn [ret k v]
                                                             (if (zero? v)
                                                               (conj ret (rio k))
                                                               ret))
                                                           []
                                                           r-target')

                            coor' (assoc coor
                                         o-target-rid
                                         (apply dissoc r-target'
                                                zero-cids-of-target))
                            cid->rids' (apply update cid->rids
                                              rid-source
                                              disj
                                              rid-target
                                              zero-cids-of-target)]
                        (recur (rest rid-targets)
                               coor'
                               cid->rids'))))]
              (recur :swap-rows
                     p-crid'
                     rio
                     coor'
                     cid->rids'))
            (recur :pivots->1
                   p-crid
                   rio
                   coor
                   cid->rids)))
        :pivots->1
        (if (< p-crid 0)
          (recur :back-subst
                 (dec num-rows)
                 rio
                 coor
                 cid->rids)
          (let [rid p-crid
                o-rid (rio rid)
                r (coor o-rid)
                p-v (r rid)
                alpha (/ 1 p-v)
                r' (reduce-kv (fn [ret k v]
                                (assoc ret
                                       k
                                       (* alpha v)))
                              {}
                              r)
                coor' (assoc coor
                             o-rid
                             r')]
            (recur :pivots->1
                   (dec p-crid)
                   rio
                   coor'
                   cid->rids)))
        :back-subst
        (loop [p-crid p-crid
               coor coor
               cid->rids cid->rids]
          (if (< p-crid 1)
            {:rio rio
             :coor coor
             :cid->rids cid->rids}
            (let [p-b (get (coor (rio p-crid)) num-rows)
                  coor' (reduce (fn [ret rid]
                                  (update ret
                                          (rio rid)
                                          (fn [r]
                                            (let [e (r p-crid)
                                                  bi (r num-rows)
                                                  alpha (- e)]
                                              (-> r
                                                  (dissoc p-crid)
                                                  (assoc num-rows
                                                         (+ (* alpha p-b)
                                                            bi)))))))
                                coor
                                (disj (cid->rids p-crid)
                                      p-crid))
                  cid->rids' (assoc cid->rids
                                    p-crid
                                    #{p-crid})]
              (recur (dec p-crid)
                     coor'
                     cid->rids'))))
        
        :end
        nil))))

(defn add-debug [accum-atom out-f & args]
  (swap! accum-atom into (apply out-f args)))

;; Web debug version
(defn auto5-gaussian-elimination [augmented-original-order-rows debug-accum-atom]
  (swap! debug-accum-atom conj [:div {} (pr-str (java.util.Date.))])
  (let [window-width 3
        num-rows (count augmented-original-order-rows)
        row-id-order (vec (range num-rows))
        col-id->row-ids (let [row-ids (set row-id-order)]
                          (mapv (fn [col-id]
                                  (reduce (fn [ret [row-id row]]
                                            (let [entry (get row col-id)]
                                              (if entry
                                                (conj ret row-id)
                                                ret)))
                                          #{}
                                          (map-indexed vector augmented-original-order-rows)))
                                row-id-order))]
    (swap! debug-accum-atom conj [:div {} "Start " (pr-str (java.util.Date.))])
    #_ (add-debug debug-accum-atom hv/out-row-id-order
               row-id-order
               0 6)
    #_ (add-debug debug-accum-atom hv/out-col-id->row-ids
               col-id->row-ids
               0 6)
    #_ (add-debug debug-accum-atom hv/out-current-rows
               augmented-original-order-rows
               row-id-order
               0 6)
    (loop [state :swap-rows
           p-crid 0 ;; p-rid = p-cid for our stuff
           rio row-id-order
           coor augmented-original-order-rows
           cid->rids col-id->row-ids]
      (swap! debug-accum-atom conj [:div {} (str state " pivot:" p-crid " ") (pr-str (java.util.Date.))])
      (println p-crid)
      (case state
        :swap-rows
        (let [test-best-p-value ((coor (rio p-crid)) p-crid)
              test-nz-rids (filter (fn [rid]
                                (not (< rid p-crid)))
                              (cid->rids p-crid))
              test-best-pid (if test-best-p-value
                              p-crid
                              (first test-nz-rids))
              test2-best-p-value (if test-best-p-value
                                   test-best-p-value
                                   (if test-best-pid
                                     ((coor (rio test-best-pid)) p-crid)
                                     (do
                                       (swap! debug-accum-atom conj
                                              [:div {} [:pre {}
                                                        (str "test-best-p-value " (pr-str test-best-p-value)
                                                             " test-nz-rids " (pr-str test-nz-rids)
                                                             " test-best-pid " (pr-str test-best-pid))]])
                                       (throw (Exception. "can't end here")))))
              p-crid-swap
              (loop [;; select row-ids below pivot
                     nz-rids test-nz-rids
                     best-pid test-best-pid
                     best-p-value test2-best-p-value]
                (if (empty? nz-rids)
                  best-pid
                  (let [nz-rid (first nz-rids)
                        best-p-value' (try
                                        (Math/abs (double
                                                   (-> nz-rid
                                                       rio
                                                       coor
                                                       ;; get entry in this col
                                                       (get p-crid))))
                                        (catch Exception e
                                          (add-debug debug-accum-atom hv/out-row-id-order
                                                     rio
                                                     p-crid (inc p-crid))
                                          (add-debug debug-accum-atom hv/out-row-id-order
                                                     rio
                                                     nz-rid (inc nz-rid))
                                          (add-debug debug-accum-atom hv/out-current-rows
                                                     coor
                                                     rio
                                                     p-crid (inc p-crid))
                                          (add-debug debug-accum-atom hv/out-current-rows
                                                     coor
                                                     rio
                                                     nz-rid (inc nz-rid))
                                          (add-debug debug-accum-atom hv/out-col-id->row-ids
                                                     cid->rids
                                                     p-crid (inc p-crid))
                                          (add-debug debug-accum-atom hv/out-col-id->row-ids
                                                     cid->rids
                                                     p-crid (inc p-crid))
                                          (throw e)))]
                    (if (and (try
                               (< best-p-value'
                                  best-p-value)
                               (catch Exception e
                                 (swap! debug-accum-atom conj
                                        [:div {} (str "nz-rids " (pr-str nz-rids)
                                                      " best-pid " best-pid)])
                                 (throw e)))
                             (not (zero? best-p-value')))
                      (recur (rest nz-rids)
                             nz-rid
                             best-p-value')
                      (recur (rest nz-rids)
                             best-pid
                             best-p-value)))))]
          (if (= p-crid-swap p-crid)
            (do
              (swap! debug-accum-atom conj
                     [:div {} "pivot " p-crid " already swap "
                      (pr-str (java.util.Date.))])
              (recur :zero-below
                     p-crid
                     rio
                     coor
                     cid->rids))
            (let [rid1 p-crid
                  rid2 p-crid-swap
                  o-rid1 (rio rid1)
                  o-rid2 (rio rid2)
                  rio' (-> rio
                           (assoc rid1 o-rid2)
                           (assoc rid2 o-rid1))
                  r1 (coor (rio rid1))
                  r2 (coor (rio rid2))
                  r1-cols (keys r1)
                  r2-cols (keys r2)
                  union-cols (-> (into (set r1-cols) r2-cols)
                                 ;; exclude b column, b is always full
                                 (disj num-rows))
                  cid->rids' (reduce (fn [ret cid]
                                       (update ret
                                               cid
                                               (fn [rids]
                                                 (if (rids rid1)
                                                   (if (rids rid2)
                                                     rids
                                                     (-> rids
                                                         (conj rid2)
                                                         (disj rid1)))
                                                   (if (rids rid2)
                                                     (-> rids
                                                         (conj rid1)
                                                         (disj rid2))
                                                     rids)))))
                                     cid->rids
                                     union-cols)]
              (do
                (swap! debug-accum-atom conj
                     [:div {} "swapping " rid1 " with " rid2 " "
                      (pr-str (java.util.Date.))])
                (let [start-id (if (zero? p-crid)
                                 0
                                 (dec p-crid))
                      end-id (if (< p-crid (- num-rows window-width))
                               (+ window-width p-crid)
                               num-rows)]
                  (add-debug debug-accum-atom hv/out-row-id-order
                             rio
                             start-id end-id)
                  (add-debug debug-accum-atom hv/out-row-id-order
                             rio'
                             start-id end-id)
                  (add-debug debug-accum-atom hv/out-col-id->row-ids
                             cid->rids
                             start-id end-id)
                  (add-debug debug-accum-atom hv/out-col-id->row-ids
                             cid->rids'
                             start-id end-id)
                  (swap! debug-accum-atom conj [:div {} "swapping " rid1])
                  (add-debug debug-accum-atom hv/out-current-rows
                             coor
                             rio
                             start-id end-id)
                  (add-debug debug-accum-atom hv/out-current-rows
                             coor
                             rio'
                             start-id end-id)
                  (swap! debug-accum-atom conj [:div {} "near swap-target " rid2])
                  (add-debug debug-accum-atom hv/out-current-rows
                             coor
                             rio
                             rid2 (inc rid2))
                  (add-debug debug-accum-atom hv/out-current-rows
                             coor
                             rio'
                             rid2 (inc rid2)))
                (recur :zero-below
                       p-crid
                       rio'
                       coor
                       cid->rids')))))
        :zero-below
        (let [p-crid' (inc p-crid)]
          (if (< p-crid' num-rows)
            (let [rid-source p-crid
                  o-source-rid (rio rid-source)
                  r-source (coor o-source-rid)
                  p-v (r-source rid-source)
                  [coor' cid->rids']
                  ;; This can be parallelized, but not a big deal for sparse
                  (loop [rid-targets (filter (fn [rid]
                                               (> rid p-crid))
                                             (cid->rids p-crid))
                         coor coor
                         cid->rids cid->rids]
                    (swap! debug-accum-atom conj [:div {} ":zero-below-inner " (pr-str (java.util.Date.))])
                    (if (empty? rid-targets)
                      (do
                        (swap! debug-accum-atom conj [:div {} ":finish-zero-below-inner"])
                        [coor
                         cid->rids])
                      (let [rid-target (first rid-targets)
                            o-target-rid (rio rid-target)
                            r-target (coor o-target-rid)
                            alpha (try
                                    (double (- (/ (r-target p-crid)
                                                  p-v)))
                                    (catch Exception e
                                      (throw (ex-info "divide by zero"
                                                      {:rid-target rid-target
                                                       :r-target r-target
                                                       :r-source r-source
                                                       :p-crid p-crid
                                                       :p-v p-v}))))
                            r-target'
                            (-> (reduce-kv
                                 (fn [ret cid sv]
                                   (update
                                    ret
                                    cid
                                    (fn [tv]
                                      (if tv
                                        ;; Potentially, if the following is zero,
                                        ;; we could do further optimizations with
                                        ;; the indices but may not be worth the
                                        ;; benefit given how rare that is
                                        (unchecked-add (unchecked-multiply alpha (double sv))
                                                       (double tv))
                                        (unchecked-multiply alpha (double sv))))))
                                 r-target
                                 r-source)
                                ;; zero below pivot
                                (dissoc rid-source))
                            zero-cids-of-target (reduce-kv (fn [ret cid v]
                                                             (if (zero? v)
                                                               (conj ret cid)
                                                               ret))
                                                           []
                                                           r-target')
                            new-cids-of-target (keys r-target')
                            coor' (assoc coor
                                         o-target-rid
                                         (apply dissoc r-target'
                                                zero-cids-of-target))
                            cid->rids+ (reduce (fn [ret cid]
                                                 (update ret
                                                         cid
                                                         (fn [cids]
                                                           (conj (or cids
                                                                     #{})
                                                                 rid-target))))
                                               cid->rids
                                               new-cids-of-target)
                            cid->rids' (reduce (fn [ret cid]
                                                 (update ret
                                                         cid
                                                         disj
                                                         rid-target))
                                               cid->rids+
                                               (into [rid-source]
                                                     zero-cids-of-target))]
                        #_ (update cid->rids
                                   rid-source
                                   disj
                                   rid-target)
                        (swap! debug-accum-atom into [[:div {} "rid-target " rid-target " pivot " p-crid]
                                                      [:div {} "alpha " alpha]
                                                      [:div {} "zero-cids-of-target " [:pre {}
                                                                                       (pr-str zero-cids-of-target)]]])
                        (let [start-id (if (zero? p-crid)
                                         0
                                         (dec p-crid))
                              end-id (if (< p-crid (- num-rows window-width))
                                       (+ window-width p-crid)
                                       num-rows)]
                          (add-debug debug-accum-atom hv/out-col-id->row-ids
                                     cid->rids
                                     start-id end-id)
                          (add-debug debug-accum-atom hv/out-col-id->row-ids
                                     cid->rids'
                                     start-id end-id)
                          (add-debug debug-accum-atom hv/out-current-rows
                                     coor
                                     rio
                                     start-id end-id)
                          (add-debug debug-accum-atom hv/out-current-rows
                                     coor'
                                     rio
                                     start-id end-id)
                          (swap! debug-accum-atom conj [:div {} "near rid-target " rid-target " rid-source " rid-source " alpha " alpha])
                          (add-debug debug-accum-atom hv/out-current-rows
                                     coor
                                     rio
                                     rid-target (inc rid-target))                          
                          (add-debug debug-accum-atom hv/out-current-rows
                                     coor'
                                     rio
                                     rid-target (inc rid-target)))
                        (recur (rest rid-targets)
                               coor'
                               cid->rids'))))]
              (recur :swap-rows
                     p-crid'
                     rio
                     coor'
                     cid->rids'))
            (recur :pivots->1
                   p-crid
                   rio
                   coor
                   cid->rids)))
        :pivots->1
        (if (< p-crid 0)
          (recur :back-subst
                 (dec num-rows)
                 rio
                 coor
                 cid->rids)
          (let [rid p-crid
                o-rid (rio rid)
                r (coor o-rid)
                p-v (r rid)
                alpha (/ 1 p-v)
                r' (reduce-kv (fn [ret k v]
                                (assoc ret
                                       k
                                       (* alpha v)))
                              {}
                              r)
                coor' (assoc coor
                             o-rid
                             r')]
            (recur :pivots->1
                   (dec p-crid)
                   rio
                   coor'
                   cid->rids)))
        :back-subst
        (loop [p-crid p-crid
               coor coor
               cid->rids cid->rids]
          (if (< p-crid 1)
            {:rio rio
             :coor coor
             :cid->rids cid->rids}
            (let [p-b (get (coor (rio p-crid)) num-rows)
                  coor' (reduce (fn [ret rid]
                                  (update ret
                                          (rio rid)
                                          (fn [r]
                                            (let [e (r p-crid)
                                                  bi (r num-rows)
                                                  alpha (- e)]
                                              (-> r
                                                  (dissoc p-crid)
                                                  (assoc num-rows
                                                         (+ (* alpha p-b)
                                                            bi)))))))
                                coor
                                (disj (cid->rids p-crid)
                                      p-crid))
                  cid->rids' (assoc cid->rids
                                    p-crid
                                    #{p-crid})]
              (recur (dec p-crid)
                     coor'
                     cid->rids'))))
        
        :end
        nil))))

;; optimizations attempts, transient col-id->row-ids (doesn't work,
;; transients don't work that way globally)
(defn auto7-gaussian-elimination [augmented-original-order-rows]
  (println (java.util.Date.)))

;; Optimize, simple row-id-order to long-array
;; - also optimized initialization of col-id->row-ids
;; - also use data.int-map (ended up being slower)

(defn auto8-gaussian-elimination [augmented-original-order-rows]
  (println (java.util.Date.))
  (let [window-width 3
        pre-coor augmented-original-order-rows
        #_ (vec (pmap (fn [row]
                              (into (i/int-map) row))
                            augmented-original-order-rows))
        num-rows (count pre-coor)
        rio (long-array (range num-rows)) ;; row-id-order
        col-id->row-ids (let [init-full-vec (loop [v (transient [])
                                                   n 0]
                                              (if (< n num-rows)
                                                (recur (conj! v (transient #{} #_ (i/int-set)))
                                                       (inc n))
                                                v))
                              t-cid->rids (reduce (fn [ret-v [row-id row]]
                                                    (reduce-kv (fn [ret-v' cid v]
                                                                 (if (not= cid num-rows)
                                                                   (let [rids (ret-v' cid)]
                                                                     (assoc! ret-v'
                                                                             cid
                                                                             (conj! rids row-id)))
                                                                   ret-v'))
                                                               ret-v
                                                               row))
                                                  init-full-vec
                                                  (map-indexed vector pre-coor))]
                          (loop [v t-cid->rids
                                 n 0]
                            (if (< n num-rows)
                              (recur (assoc! v
                                             n
                                             (persistent! (v n)))
                                     (inc n))
                              (persistent! v))))]
    (println (java.util.Date.))
    (loop [state :swap-rows
           p-crid 0 ;; p-rid = p-cid for our stuff
           coor pre-coor
           cid->rids col-id->row-ids]
      (println state p-crid)
      (if (< p-crid 1000)
        (case state
         :swap-rows
         (let [test-best-p-value ((coor (aget rio p-crid)) p-crid)
               test-nz-rids (filter (fn [rid]
                                      (not (< rid p-crid)))
                                    (cid->rids p-crid))
               test-best-pid (if test-best-p-value
                               p-crid
                               (first test-nz-rids))
               test2-best-p-value (if test-best-p-value
                                    test-best-p-value
                                    (if test-best-pid
                                      ((coor (aget rio test-best-pid)) p-crid)
                                      (throw (Exception. "can't end here"))))
               p-crid-swap
               (apply min
                      (for [rid test-nz-rids
                            :let [ks (coor (aget rio rid))]
                            :when (not (zero? (count ks)))]
                        rid))]
           (if (= p-crid-swap p-crid)
             (recur :zero-below
                    p-crid
                    coor
                    cid->rids)
             (let [rid1 p-crid
                   rid2 p-crid-swap
                   o-rid1 (aget rio rid1)
                   o-rid2 (aget rio rid2)
                   _rio' (-> rio
                             (aset rid1 o-rid2)
                             (aset rid2 o-rid1))
                   r1 (coor (aget rio rid1))
                   r2 (coor (aget rio rid2))
                   r1-cols (keys r1)
                   r2-cols (keys r2)
                   union-cols (-> (into (set r1-cols) #_ (i/int-set r1-cols) r2-cols)
                                  ;; exclude b column, b is always full
                                  (disj num-rows))
                   cid->rids' (reduce (fn [ret cid]
                                        (update ret
                                                cid
                                                (fn [rids]
                                                  (if (rids rid1)
                                                    (if (rids rid2)
                                                      rids
                                                      (-> rids
                                                          (conj rid2)
                                                          (disj rid1)))
                                                    (if (rids rid2)
                                                      (-> rids
                                                          (conj rid1)
                                                          (disj rid2))
                                                      rids)))))
                                      cid->rids
                                      union-cols)]
               (recur :zero-below
                      p-crid
                      coor
                      cid->rids'))))
         :zero-below
         (let [p-crid' (inc p-crid)]
           (if (< p-crid' num-rows)
             (let [rid-source p-crid
                   o-source-rid (aget rio rid-source)
                   r-source (coor o-source-rid)
                   p-v (double (r-source rid-source))
                   [coor' cid->rids']
                   ;; This can be parallelized, but not a big deal for sparse
                   (loop [rid-targets (filter (fn [rid]
                                                (> rid p-crid))
                                              (cid->rids p-crid))
                          coor coor
                          cid->rids cid->rids]
                     (if (empty? rid-targets)
                       [coor
                        cid->rids]
                       (let [rid-target (first rid-targets)
                             o-target-rid (aget rio rid-target)
                             r-target (coor o-target-rid)
                             alpha (try
                                     (double (-
                                              (/ (double
                                                  (r-target p-crid))
                                                 p-v)))
                                     (catch Exception e
                                       (throw (ex-info "divide by zero"
                                                       {:rid-target rid-target
                                                        :r-target r-target
                                                        :r-source r-source
                                                        :p-crid p-crid
                                                        :p-v p-v}))))
                             r-target'
                             ;; slow
                             (-> (reduce-kv
                                  (fn [ret cid sv]
                                    (let [tv (ret cid)]
                                      (assoc! ret
                                              cid
                                              (if tv
                                                (unchecked-add (unchecked-multiply alpha (double sv))
                                                               (double tv))
                                                (unchecked-multiply alpha (double sv))))))
                                  (transient r-target)
                                  r-source)
                                 ;; zero below pivot
                                 (dissoc! rid-source)
                                 persistent!)
                             zero-cids-of-target (persistent!
                                                  (reduce-kv (fn [ret cid v]
                                                               (if (zero? v)
                                                                 (conj! ret cid)
                                                                 ret))
                                                             (transient [])
                                                             r-target'))
                             new-cids-of-target (keys r-target')
                             coor' (assoc coor
                                          o-target-rid
                                          (apply dissoc r-target'
                                                 zero-cids-of-target))
                             ;; slow
                             cid->rids+ (persistent!
                                         (reduce (fn [ret cid]
                                                   (if (= cid num-rows)
                                                     ret
                                                     (let [rids (ret cid)]
                                                       (assoc! ret
                                                               cid
                                                               (conj (or rids
                                                                         #{})
                                                                     rid-target)))))
                                                 (transient cid->rids)
                                                 new-cids-of-target))
                             cid->rids' (persistent!
                                         (reduce (fn [ret cid]
                                                   (if (= cid num-rows)
                                                     ret
                                                     (let [rids (ret cid)]
                                                       (assoc! ret
                                                               cid
                                                               (disj rids rid-target)))))
                                                 (transient cid->rids+)
                                                 (into [rid-source]
                                                       zero-cids-of-target)))]
                         (recur (rest rid-targets)
                                coor'
                                cid->rids'))))]
               (recur :swap-rows
                      p-crid'
                      coor'
                      cid->rids'))
             (do
               (binding [*print-length* nil]
                 (dj.io/poop (dj.io/file "/home/username/tmp/test.txt")
                             (str {:rio rio
                                   :coor coor
                                   :cid->rids cid->rids})))
               (recur :pivots->1
                      p-crid
                      coor
                      cid->rids))))
         :pivots->1
         (if (< p-crid 0)
           (recur :back-subst
                  (dec num-rows)
                  coor
                  cid->rids)
           (let [rid p-crid
                 o-rid (aget rio rid)
                 r (coor o-rid)
                 p-v (r rid)
                 alpha (/ 1 p-v)
                 r' (reduce-kv (fn [ret k v]
                                 (assoc ret
                                        k
                                        (* alpha v)))
                               {}
                               r)
                 coor' (assoc coor
                              o-rid
                              r')]
             (recur :pivots->1
                    (dec p-crid)
                    coor'
                    cid->rids)))
         :back-subst
         (try
           (loop [p-crid p-crid
                  coor coor
                  cid->rids cid->rids]
             (if (< p-crid 1)
               {:rio rio
                :coor coor
                :cid->rids cid->rids}
               (let [p-b (get (coor (aget rio p-crid)) num-rows)
                     coor' (reduce (fn [ret rid]
                                     (update ret
                                             (aget rio rid)
                                             (fn [r]
                                               (let [e (r p-crid)
                                                     bi (r num-rows)
                                                     alpha (- e)]
                                                 (-> r
                                                     (dissoc p-crid)
                                                     (assoc num-rows
                                                            (+ (* alpha p-b)
                                                               bi)))))))
                                   coor
                                   (disj (cid->rids p-crid)
                                         p-crid))
                     cid->rids' (assoc cid->rids
                                       p-crid
                                       #{p-crid})]
                 (recur (dec p-crid)
                        coor'
                        cid->rids'))))
           (catch Exception e
             {:rio rio
              :coor coor
              :cid->rids cid->rids
              :e e}))
        
         :end
         nil)
        nil))))

(comment

  ;;; Notation
  ;; pivot - left most non-zero entry in row
  ;; zero'ing - make pivot zero via row operations
  ;; preduce'ing - make pivot 1 (partial reduce)
  (diy-gaussian-elimination
   [ ;; get row-echelon form
    [:comb 0 1 1] ;; easy zero a row
    [:comb 0 -2 2] ;; zero another row
    ;; don't want fractions
    [:swap 1 2]   ;; 1) choose pivot that's smaller so can multiply to zero the other
    [:comb 1 4 2] ;; 2) zero larger pivot
    [:mult 1 -1] ;; preduce row (sign)
    [:mult 2 -1] ;; preduce row (sign)
    ;; backward substitution
    [:comb 1 -1 0]
    [:comb 2 -1 1]])



  (auto1-gaussian-elimination)

  (-> [[2   1 -1  8]
       [-3 -1  2 -11]
       [-2  1  2 -3]]
      matrix->sparse-map
      (auto3-gaussian-elimination))
  [2
   3
   -1]

  (require 'dj.io)
  (let [m (random-matrix rand 32)]
    (dj.io/poop (dj.io/file "/home/username/tmp/testmatrix.out")
                (with-out-str
                  (try
                    (-> m
                        matrix->sparse-map
                        (auto3-gaussian-elimination))
                    (catch Exception e
                      (println e)))))
    m)

  (let [m (random-matrix rand 32)]
    (dj.io/poop (dj.io/file "/home/username/tmp/testmatrix.out")
                (with-out-str
                  (try
                    (-> m
                        matrix->sparse-map
                        (auto4-gaussian-elimination))
                    (catch Exception e
                      (println e)))))
    m)

  (+ 3 (* 1 1/2)) 7/2
  alpha 1/2

  (Math/abs 1/2)

  (+ 1/2 1/2)

  15N
  (let [rio [4 1 3 2 0 5]
        rid1 3
        rid2 5
        o-rid1 (rio rid1)
        o-rid2 (rio rid2)]
    (-> rio
        (assoc rid1 o-rid2)
        (assoc rid2 o-rid1)
        ))
  )

;;; Wikipedia high level explanation
;; Do row operations to get into row-echelon form, then backwards
;; propagation to get reduced-row-ecahalon form
;; 3 operations:
;; 1. swap
;; 2. multiply row by scalar
;; 3. add one row into another times a scalar

;;; Wikipedia presented pseudo code
;; h := 1 /* Initialization of the pivot row */
;; k := 1 /* Initialization of the pivot column */
;; while h ≤ m and k ≤ n
;;   /* Find the k-th pivot: */
;;   i_max := argmax (i = h ... m, abs(A[i, k]))
;;   if A[i_max, k] = 0
;;     /* No pivot in this column, pass to next column */
;;     k := k+1
;;   else
;;      swap rows(h, i_max)
;;      /* Do for all rows below pivot: */
;;      for i = h + 1 ... m:
;;         f := A[i, k] / A[h, k]
;;         /* Fill with zeros the lower part of pivot column: */
;;         A[i, k]  := 0
;;         /* Do for all remaining elements in current row: */
;;         for j = k + 1 ... n:
;;            A[i, j] := A[i, j] - A[h, j] * f
;;      /* Increase pivot row and column */
;;      h := h+1 
;;      k := k+1

;;; Interpreted Wikipedia presented pseudo code
;; row-id := 1 /* Initialization of the pivot row */
;; col-id := 1 /* Initialization of the pivot column */
;; while row-id ≤ row-size and col-id ≤ col-size
;;   /* Find the col-id-th pivot: */
;;   ;; Get absolute value largest pivot
;;   ;; Maybe we can have a data structure, column-id -> non-empty entries to quickly pull 
;;   big_pivot_id := argmax (i = row-id ... row-size, abs(A[i, col-id]))
;;   if A[big_pivot_id, col-id] = 0
;;     /* No pivot in this column, pass to next column */
;;     col-id := col-id+1
;;   else
;;      ;; Put biggest pivot on top
;;      swap rows(row-id, big_pivot_id)
;;      /* Do for all rows below pivot: */
;;      ;; Actually, I think we should only do this for rows that have entries
;;      ;; in the same column
;;      for sub-rid = row-id + 1 ... row-size:
;;         ;; alpha = sub-row-elem / pivot
;;         alpha := A[sub-rid, col-id] / A[row-id, col-id]
;;         ;; zero column below pivot
;;         ;; sub-row-elem - sub-row-elem/pivot*pivot = 0
;;         A[sub-rid, col-id]  := 0
;;         /* Do for all remaining elements in current row: */
;;         ;; Finish the subtraction for the rest of the sub-row
;;         for sub-cid = col-id + 1 ... col-size:
;;            A[sub-rid, sub-cid] := A[sub-rid, sub-cid] - A[row-id, sub-cid] * alpha
;;      /* Increase pivot row and column */
;;      row-id := row-id+1 
;;      col-id := col-id+1

;; Other notes:
;; - I think they pick biggest pivot so that you subtract small amounts to the other entries
;;   - small because a number/bigger number -> smaller number
;;   - subtracting small numbers means things won't blow up
;;     - but its possible subtracting small numbers means changes
;;       won't occur, but maybe that doesn't matter
;; - I say test biggest pivot and smallest pivot, using doubles and using BigDecimal
;;   - very likely we can't use rationals


;;; My plan
;; - Don't directly use pseudo code cause it assumes raw matrix
;;   encoding
;; - instead we should build algorithm based on the {node-id ->
;;   {node-id -> value...}} format
;;   - I feel like certain operations are free/cheap with this format,
;;     like swap could just mean, reassign node order? or just
;;     optimize order at the end?


;;; From discussions with Eric
;; - The A matrix depends on the conductivities (which are simple),
;;   but also depends on the edge length of the element, which means
;;   each point will have a unique entry in the A matrix
;; - Its to be explored if cutting that precision prior to doing GE
;;   incurrs numerical instabilities or significantly alters the
;;   result, my gut feeling is since it can be done once and not
;;   repeatedly (start low precision but iterative using high
;;   precision), we might be able to do some tricks
