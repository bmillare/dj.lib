(ns dj.math.gauss-elim
  (:require [clojure.set :as cs]
            [dj.math.gauss-elim.html-view :as hv]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn matrix->sparse-map [vec-of-vecs]
  (mapv (fn [row]
          (reduce (fn [ret [i e]]
                    (if (zero? (double e))
                      ret
                      (assoc ret
                             i
                             e)))
                  {}
                  (map-indexed vector row)))
        vec-of-vecs))

(defn random-matrix [r-f size]
  (mapv (fn [_]
          (mapv (fn [__]
                  (r-f 1000))
                (range (inc (long size)))))
        (range size)))

;; silent
(defn auto6-gaussian-elimination [augmented-original-order-rows]
  (println (java.util.Date.))
  (let [num-rows (count augmented-original-order-rows)
        row-id-order (vec (range num-rows))
        col-id->row-ids (let [row-ids (set row-id-order)
                              numbered-original-rows (map-indexed vector augmented-original-order-rows)]
                          (vec (pmap (fn [col-id]
                                       (persistent!
                                        (reduce (fn [ret [row-id row]]
                                                  (let [entry (get row col-id)]
                                                    (if entry
                                                      (conj! ret row-id)
                                                      ret)))
                                                (transient #{})
                                                numbered-original-rows)))
                                     ;; treated as getting all available columns
                                     row-id-order)))]
    (println (java.util.Date.))
    (loop [state :swap-rows
           p-crid 0 ;; p-rid = p-cid for our stuff
           rio row-id-order
           coor augmented-original-order-rows
           cid->rids col-id->row-ids]
      (println state p-crid)
      (case state
        :swap-rows
        (let [test-best-p-value ((coor (rio p-crid)) p-crid)
              test-nz-rids (filter (fn [rid]
                                     (not (< (long rid) (long p-crid))))
                                   (cid->rids p-crid))
              test-best-pid (if test-best-p-value
                              p-crid
                              (first test-nz-rids))
              test2-best-p-value (if test-best-p-value
                                   test-best-p-value
                                   (if test-best-pid
                                     ((coor (rio test-best-pid)) p-crid)
                                     (throw (Exception. "can't end here"))))
              p-crid-swap
              (apply min
                     (for [rid test-nz-rids
                           :let [ks (coor (rio rid))]
                           :when (not (zero? (count ks)))]
                       rid))]
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
                  p-v (double (r-source rid-source))
                  [coor' cid->rids']
                  ;; This can be parallelized, but not a big deal for sparse
                  (loop [rid-targets (filter (fn [rid]
                                               (> (long rid) (long p-crid)))
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
                            (-> (persistent!
                                 (reduce-kv
                                  (fn [ret cid sv]
                                    (let [tv (ret cid)]
                                      (assoc! ret
                                              cid
                                              (if tv
                                                (unchecked-add (unchecked-multiply (double alpha) (double sv))
                                                               (double tv))
                                                (unchecked-multiply (double alpha) (double sv))))))
                                  (transient r-target)
                                  r-source))
                                ;; zero below pivot
                                (dissoc rid-source))
                            zero-cids-of-target (persistent!
                                                 (reduce-kv (fn [ret cid v]
                                                              (if (zero? (double v))
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
                     rio
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
                     rio
                     coor
                     cid->rids))))
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
                p-v (double (r rid))
                alpha (double (/ 1 p-v))
                r' (reduce-kv (fn [ret k v]
                                (assoc ret
                                       k
                                       (* alpha (double v))))
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
        (try
          (loop [p-crid p-crid
                 coor coor
                 cid->rids cid->rids]
            (if (< p-crid 1)
              {:rio rio
               :coor coor
               :cid->rids cid->rids}
              (let [p-b (double (get (coor (rio p-crid)) num-rows))
                    coor' (try
                            (reduce (fn [ret rid]
                                      (update ret
                                              (rio rid)
                                              (fn [r]
                                                (let [e (r p-crid)
                                                      bi (double (r num-rows))
                                                      alpha (- (double e))]
                                                  (-> r
                                                      (dissoc p-crid)
                                                      (assoc num-rows
                                                             (+ (* alpha p-b)
                                                                bi)))))))
                                    coor
                                    (disj (cid->rids p-crid)
                                          p-crid))
                            (catch Exception e
                              (throw (ex-info "meh"
                                              {:rio rio
                                               :coor coor
                                               :cid->rids cid->rids
                                               :p-crid p-crid
                                               :num-rows num-rows
                                               :p-b p-b}
                                              e))))
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
        nil))))

;; Optimize, simple row-id-order to long-array
;; - also optimized initialization of col-id->row-ids
;; - also use data.int-map (ended up being slower)
;; - 8-1 used as reference

(def auto8-1-window (atom {}))

(def debug-start 11346)
(def debug-end 11336)

(defn auto8-1-gaussian-elimination
  ([augmented-original-order-rows]
   (println (java.util.Date.))
   (let [pre-coor augmented-original-order-rows
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
                               (persistent! v))))]))
  ([pre-coor rio cid->rids state]
   (auto8-1-gaussian-elimination
    pre-coor
    (count pre-coor)
    (long-array rio)
    cid->rids
    state
    (case state
      :swap-rows 0
      :pivots->1 (dec (count pre-coor)))))
  ([pre-coor num-rows ^longs rio col-id->row-ids state p-crid]
   (println (java.util.Date.))
   (let [num-rows (long num-rows)]
     (loop [state state
            p-crid p-crid ;; p-rid = p-cid for our stuff
            coor pre-coor
            cid->rids col-id->row-ids]
       (let [p-crid (long p-crid)]
         (println state p-crid
                  (when-not (< (long p-crid) 0)
                    (count (coor p-crid)))
                  (when-not (< (long p-crid) 0)
                    (count (cid->rids p-crid))))
         (if (and (> p-crid (long debug-end))
                  (< p-crid (long debug-start)))
           (swap! auto8-1-window
                  assoc
                  [p-crid state]
                  {:p-crid p-crid
                   :state state
                   :rio (into [] rio)
                   :coor coor
                   :cid->rids cid->rids})
           (when (not (> p-crid (long debug-end)))
             (throw (ex-info "quit early"
                             {:p-crid p-crid}))))
         (case state
           :swap-rows
           (let [test-best-p-value ((coor (aget rio p-crid)) p-crid)
                 test-nz-rids (filter (fn [rid]
                                        (not (< (double rid) p-crid)))
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
                     _rio' (doto rio
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
                     all-rid-targets (filter (fn [rid]
                                               (> (long rid) p-crid))
                                             (cid->rids p-crid))
                     [coor' cid->rids']
                     ;; This can be parallelized, but not a big deal for sparse
                     (loop [rid-targets all-rid-targets
                            coor coor
                            cid->rids cid->rids]
                       (if (empty? rid-targets)
                         [coor
                          cid->rids]
                         (let [rid-target (first rid-targets)
                               o-target-rid (aget rio rid-target)
                               r-target (coor o-target-rid)
                               alpha (double (-
                                              (/ (double
                                                  (r-target p-crid))
                                                 p-v)))
                               r-target'
                               ;; slow
                               (-> (reduce-kv
                                    (fn [tr-target cid sv]
                                      (let [tv (tr-target cid)]
                                        #_ (when (and (= rid-target 369)
                                                      (= p-crid 4)
                                                      (= cid 4))
                                             (def debug-v-8-1 {:pre-r-target (vec (sort r-target))
                                                               :rid-target rid-target
                                                               :r-target (vec (sort (persistent! tr-target)))
                                                               :rid-source rid-source
                                                               :r-source (vec (sort r-source))
                                                               :rid-targets all-rid-targets
                                                               :cid cid
                                                               :sv sv
                                                               :tv tv
                                                               :alpha alpha})
                                             (throw (ex-info "found it too"
                                                             {:alpha alpha
                                                              :sv sv
                                                              :tv tv
                                                              :rid-target rid-target
                                                              :p-crid p-crid})))
                                        (assoc! tr-target
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
                                                                 (if (zero? (double v))
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
                 (def pivot-help-ref
                   {:p-crid p-crid
                    :state state
                    :rio (into [] rio)
                    :coor (mapv
                           (fn [row]
                             (into {} row))
                           coor)
                    :cid->rids (mapv (fn [rids]
                                       (into #{} rids))
                                     cid->rids)})              
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
                   p-v (double (r rid))
                   alpha (/ 1.0 p-v)
                   r' (reduce-kv (fn [ret k v]
                                   (assoc ret
                                          k
                                          (* alpha (double v))))
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
                 (let [p-b (double (get (coor (aget rio p-crid)) num-rows))
                       coor' (reduce (fn [ret rid]
                                       (update ret
                                               (aget rio rid)
                                               (fn [r]
                                                 (let [e (r p-crid)
                                                       bi (double (r num-rows))
                                                       alpha (- (double e))]
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
           nil))))))

(def auto9-window (atom {}))

;; try using java data structures
;; - array versions
;; - hashset
;; - hashmap
(defn auto9-gaussian-elimination
  ([augmented-original-order-rows]
   (println (java.util.Date.))
   (let [num-rows (count augmented-original-order-rows)
         coor (object-array num-rows)
         _ (dotimes [i num-rows]
             (aset coor
                   i
                   (let [ret (java.util.HashMap.)]
                     (doseq [[k v] (augmented-original-order-rows i)]
                       (.put ret
                             (long k)
                             (double v)))
                     ret)))
         rio (long-array (range num-rows)) ;; row-id-order
         cid->rids (object-array num-rows)
         _ (dotimes [i num-rows]
             (aset cid->rids
                   i
                   (java.util.HashSet. (int 700))))
         _ (dotimes [rid num-rows]
             (if (< rid num-rows)
               (let [row ^java.util.HashMap (aget coor rid)]
                 (doseq [e (.entrySet row)]
                   (let [cid (.getKey ^java.util.HashMap$Node e)]
                     (when-not (= cid num-rows)
                       (let [rids ^java.util.HashSet (aget cid->rids cid)]
                         (.add rids (long rid)))))))))]
     (auto9-gaussian-elimination num-rows coor rio cid->rids :swap-rows 0)))
  ([pre-coor
    rio
    cid->rids
    state]
   (println "testing")
   (let [num-rows (count pre-coor)
         coor (object-array num-rows)
         _ (dotimes [i num-rows]
             (aset coor
                   i
                   (java.util.HashMap. ^java.util.Map (pre-coor i))))
         cid->rids (object-array num-rows)
         _ (dotimes [i num-rows]
             (aset cid->rids
                   i
                   (java.util.HashSet. (int 700))))
         _ (dotimes [rid num-rows]
             (if (< rid num-rows)
               (let [row ^java.util.HashMap (aget coor rid)]
                 (doseq [e (.entrySet row)]
                   (let [cid (.getKey ^java.util.HashMap$Node e)]
                     (when-not (= cid num-rows)
                       (let [rids ^java.util.HashSet (aget cid->rids cid)]
                         (.add rids (long rid)))))))))]
     (auto9-gaussian-elimination num-rows
                                 coor
                                 (long-array rio)
                                 cid->rids
                                 state
                                 (case state
                                   :swap-rows 0
                                   :pivots->1 (dec num-rows)
                                   ))))
  ([num-rows
    ^"[Ljava.lang.Object;" coor
    ^longs rio
    ^"[Ljava.lang.Object;" cid->rids
    state
    p-crid]
   (println (java.util.Date.))
   (loop [state state
          p-crid (long p-crid) ;; p-rid = p-cid for our stuff
          ]
     (println state p-crid
              (when-not (< (long p-crid) 0)
                (count (aget coor p-crid)))
              (when-not (< (long p-crid) 0)
                (count (aget cid->rids p-crid))))
     #_ (when (= p-crid 2000)
          (def a9 (aget coor p-crid))
          (throw (Exception. "nothing")))
     (if (and (< p-crid (long debug-end))
              (> p-crid (long debug-start)))
        (swap! auto9-window
               assoc
               [p-crid state]
               {:p-crid p-crid
                :state state
                :rio (into [] rio)
                :coor (mapv
                       (fn [row]
                         (into {} row))
                       coor)
                :cid->rids (mapv (fn [rids]
                                   (into #{} rids))
                                 cid->rids)})
        (when (not (< p-crid (long debug-end)))
          (throw (ex-info "quit early"
                         {:p-crid p-crid}))))
     (let [num-rows (long num-rows)
           p-crid (long p-crid)]
       (case state
         :swap-rows
         (let [test-best-p-value (.get ^java.util.HashMap (aget coor (aget rio p-crid))
                                       p-crid)
               test-nz-rids (filter (fn [rid]
                                      (not (< (long rid) p-crid)))
                                    (aget cid->rids p-crid))
               test-best-pid (if test-best-p-value
                               p-crid
                               (first test-nz-rids))
               test2-best-p-value (if test-best-p-value
                                    test-best-p-value
                                    (if test-best-pid
                                      (.get ^java.util.HashMap (aget coor (aget rio test-best-pid))
                                            p-crid)
                                      (throw (Exception. "can't end here"))))
               p-crid-swap
               (apply min
                      (for [rid test-nz-rids
                            :let [row ^java.util.HashMap (aget coor (aget rio rid))]
                            :when (not (zero? (.size row)))]
                        rid))]
           (if (= p-crid-swap p-crid)
             (recur :zero-below
                    p-crid)
             (let [rid1 p-crid
                   rid2 p-crid-swap
                   o-rid1 (aget rio rid1)
                   o-rid2 (aget rio rid2)
                   _rio' (doto rio
                           (aset rid1 o-rid2)
                           (aset rid2 o-rid1)) 
                   r1 ^java.util.HashMap (aget coor (aget rio rid1))
                   r2 ^java.util.HashMap (aget coor (aget rio rid2))
                   r1-cols (set (.keySet r1))
                   r2-cols (set (.keySet r2))
                   union-cols (-> (into r1-cols r2-cols)
                                  ;; exclude b column, b is always full
                                  (disj num-rows))
                   _cid->rids' (doseq [cid union-cols]
                                 (let [rids ^java.util.HashSet (aget cid->rids cid)]
                                   (if (rids rid1)
                                     (if (rids rid2)
                                       rids
                                       (doto rids
                                         (.add rid2)
                                         (.remove rid1)))
                                     (if (rids rid2)
                                       (doto rids
                                         (.add rid1)
                                         (.remove rid2))
                                       rids))))]
               (recur :zero-below
                      p-crid))))
         :zero-below
         (let [p-crid' (inc p-crid)]
           (if (< p-crid' num-rows)
             (let [rid-source p-crid
                   o-source-rid (aget rio rid-source)
                   r-source ^java.util.HashMap (aget coor o-source-rid)
                   p-v (double (.get r-source (long rid-source)))
                   rid-targets (doall
                                (filter (fn [rid]
                                          (> (long rid) p-crid))
                                        (into #{} (aget cid->rids p-crid))))
                   _coor_cid->rids'
                   (doseq [rid-target rid-targets]
                     (let [rid-target rid-target
                           o-target-rid (aget rio rid-target)
                           r-target ^java.util.HashMap (aget coor o-target-rid)
                           alpha (double (-
                                          (/ (double
                                              (.get r-target (long p-crid)))
                                             p-v)))
                           zero-cids-of-target (java.util.HashSet. )
                           new-cids-of-target (java.util.HashSet. )
                           pre-r-target (vec (sort (into {} r-target)))
                           _r-target'
                           ;; slow
                           (doseq [^java.util.HashMap$Node e (.entrySet r-source)]
                             (let [cid (long (.getKey e))
                                   sv (double (.getValue e))
                                   tv (.get r-target (long cid))]
                               (if tv
                                 (let [tv' (unchecked-add (unchecked-multiply alpha sv)
                                                          (double tv))]
                                   (.put r-target
                                         cid
                                         tv')
                                   (when (and (zero? tv')
                                              (not= cid num-rows))
                                     (.add zero-cids-of-target cid)
                                     (.remove r-target cid)))
                                 (do
                                   (.put r-target
                                         cid
                                         (unchecked-multiply alpha sv))
                                   (.add new-cids-of-target cid)))))
                           ;; zero below pivot
                           _ (.remove r-target
                                      (long rid-source))
                           _coor' nil
                           ;; slow
                           _ (.remove new-cids-of-target
                                      (long num-rows))
                           _cid->rids+ (doseq [cid new-cids-of-target]
                                         (let [rids ^java.util.HashSet (aget cid->rids cid)]
                                           (.add rids rid-target)))
                           _ (.remove zero-cids-of-target
                                      (long num-rows))
                           _cid->rids- (doseq [cid (into [rid-source]
                                                         zero-cids-of-target)]
                                         (when-not (= cid num-rows)
                                           (let [rids ^java.util.HashSet (aget cid->rids cid)]
                                             (.remove rids (long rid-target)))))])
                     )
                   ]
               (recur :swap-rows
                      p-crid'))
             (recur :pivots->1
                    p-crid)))
         :pivots->1
         (if (< p-crid 0)
           (recur :back-subst
                  (dec num-rows))
           (let [rid p-crid
                 o-rid (aget rio rid)
                 r ^java.util.HashMap (aget coor o-rid)
                 p-v (double (.get r rid))
                 alpha (/ 1 p-v)
                 entries (doall (map (fn [^java.util.HashMap$Node e]
                                       [(.getKey e) (.getValue e)])
                                     (.entrySet r)))
                 _r' (doseq [e entries]
                       (let [cid (long (first e))
                             v (long (second e))
                             v' (unchecked-multiply alpha v)]
                         (if (zero? v')
                           (when-not (= cid num-rows)
                             (.remove r cid)
                             (let [rids ^java.util.HashSet (aget cid->rids cid)]
                               (.remove rids (long rid))))
                           (.put r cid v'))))
                 _r' (.put r rid 1.0)
                 _coor' nil]
             (println (into {} r))
             (recur :pivots->1
                    (dec p-crid))))
         :back-subst
         (loop [p-crid p-crid]
           (if (< p-crid 1)
             {:rio rio
              :coor coor
              :cid->rids cid->rids}
             (let [p-b (double
                        (.get ^java.util.HashMap (aget coor (aget rio p-crid))
                              (long num-rows)))
                   rids (disj (into #{} (aget cid->rids p-crid))
                              p-crid)
                   coor' (doseq [rid rids]
                           (let [row ^java.util.HashMap (aget coor (aget rio rid))]
                             (try
                               (let [e (double (.get row (long p-crid)))
                                     bi (double (.get row (long num-rows)))
                                     alpha (- e)]
                                 (doto row
                                   (.remove (long p-crid))
                                   (.put (long num-rows)
                                         (+ (* alpha p-b)
                                            bi))))
                               (catch Exception e
                                 (throw (ex-info "blah"
                                                 {:p-crid p-crid
                                                  :rid rid
                                                  :row (into {} row)
                                                  :rids rids
                                                  :num-rows num-rows}
                                                 e))))))
                   
                   _cid->rids' (aset cid->rids
                                     p-crid
                                     #{p-crid})]
               (recur (dec p-crid)))))
         :end
         nil)))))

;; remove debug
(defn auto9-1-gaussian-elimination
  ([augmented-original-order-rows]
   (println (java.util.Date.))
   (let [num-rows (count augmented-original-order-rows)
         coor (object-array num-rows)
         _ (dotimes [i num-rows]
             (aset coor
                   i
                   (let [ret (java.util.HashMap.)]
                     (doseq [[k v] (augmented-original-order-rows i)]
                       (.put ret
                             (long k)
                             (double v)))
                     ret)))
         rio (long-array (range num-rows)) ;; row-id-order
         cid->rids (object-array num-rows)
         _ (dotimes [i num-rows]
             (aset cid->rids
                   i
                   (java.util.HashSet. (int 700))))
         _ (dotimes [rid num-rows]
             (if (< rid num-rows)
               (let [row ^java.util.HashMap (aget coor rid)]
                 (doseq [e (.entrySet row)]
                   (let [cid (.getKey ^java.util.HashMap$Node e)]
                     (when-not (= cid num-rows)
                       (let [rids ^java.util.HashSet (aget cid->rids cid)]
                         (.add rids (long rid)))))))))]
     (auto9-1-gaussian-elimination num-rows coor rio cid->rids :swap-rows 0)))
  ([pre-coor
    rio
    cid->rids
    state]
   (println "testing")
   (let [num-rows (count pre-coor)
         coor (object-array num-rows)
         _ (dotimes [i num-rows]
             (aset coor
                   i
                   (java.util.HashMap. ^java.util.Map (pre-coor i))))
         cid->rids (object-array num-rows)
         _ (dotimes [i num-rows]
             (aset cid->rids
                   i
                   (java.util.HashSet. (int 700))))
         _ (dotimes [rid num-rows]
             (if (< rid num-rows)
               (let [row ^java.util.HashMap (aget coor rid)]
                 (doseq [e (.entrySet row)]
                   (let [cid (.getKey ^java.util.HashMap$Node e)]
                     (when-not (= cid num-rows)
                       (let [rids ^java.util.HashSet (aget cid->rids cid)]
                         (.add rids (long rid)))))))))]
     (auto9-1-gaussian-elimination num-rows
                                 coor
                                 (long-array rio)
                                 cid->rids
                                 state
                                 (case state
                                   :swap-rows 0
                                   :pivots->1 (dec num-rows)
                                   ))))
  ([num-rows
    ^"[Ljava.lang.Object;" coor
    ^longs rio
    ^"[Ljava.lang.Object;" cid->rids
    state
    p-crid]
   (println (java.util.Date.))
   (loop [state state
          p-crid (long p-crid) ;; p-rid = p-cid for our stuff
          ]
     (println state p-crid
              (when-not (< (long p-crid) 0)
                (count (aget coor p-crid)))
              (when-not (< (long p-crid) 0)
                (count (aget cid->rids p-crid))))
     (let [num-rows (long num-rows)
           p-crid (long p-crid)]
       (case state
         :swap-rows
         (let [test-best-p-value (.get ^java.util.HashMap (aget coor (aget rio p-crid))
                                       p-crid)
               test-nz-rids (filter (fn [rid]
                                      (not (< (long rid) p-crid)))
                                    (aget cid->rids p-crid))
               test-best-pid (if test-best-p-value
                               p-crid
                               (first test-nz-rids))
               test2-best-p-value (if test-best-p-value
                                    test-best-p-value
                                    (if test-best-pid
                                      (.get ^java.util.HashMap (aget coor (aget rio test-best-pid))
                                            p-crid)
                                      (throw (Exception. "can't end here"))))
               p-crid-swap
               (apply min
                      (for [rid test-nz-rids
                            :let [row ^java.util.HashMap (aget coor (aget rio rid))]
                            :when (not (zero? (.size row)))]
                        rid))]
           (if (= p-crid-swap p-crid)
             (recur :zero-below
                    p-crid)
             (let [rid1 p-crid
                   rid2 p-crid-swap
                   o-rid1 (aget rio rid1)
                   o-rid2 (aget rio rid2)
                   _rio' (doto rio
                           (aset rid1 o-rid2)
                           (aset rid2 o-rid1)) 
                   r1 ^java.util.HashMap (aget coor (aget rio rid1))
                   r2 ^java.util.HashMap (aget coor (aget rio rid2))
                   r1-cols (set (.keySet r1))
                   r2-cols (set (.keySet r2))
                   union-cols (-> (into r1-cols r2-cols)
                                  ;; exclude b column, b is always full
                                  (disj num-rows))
                   _cid->rids' (doseq [cid union-cols]
                                 (let [rids ^java.util.HashSet (aget cid->rids cid)]
                                   (if (rids rid1)
                                     (if (rids rid2)
                                       rids
                                       (doto rids
                                         (.add rid2)
                                         (.remove rid1)))
                                     (if (rids rid2)
                                       (doto rids
                                         (.add rid1)
                                         (.remove rid2))
                                       rids))))]
               (recur :zero-below
                      p-crid))))
         :zero-below
         (let [p-crid' (inc p-crid)]
           (if (< p-crid' num-rows)
             (let [rid-source p-crid
                   o-source-rid (aget rio rid-source)
                   r-source ^java.util.HashMap (aget coor o-source-rid)
                   p-v (double (.get r-source (long rid-source)))
                   rid-targets (doall
                                (filter (fn [rid]
                                          (> (long rid) p-crid))
                                        (aget cid->rids p-crid)))
                   _coor_cid->rids'
                   (doseq [rid-target rid-targets]
                     (let [rid-target rid-target
                           o-target-rid (aget rio rid-target)
                           r-target ^java.util.HashMap (aget coor o-target-rid)
                           alpha (double (-
                                          (/ (double
                                              (.get r-target (long p-crid)))
                                             p-v)))
                           zero-cids-of-target (java.util.HashSet. )
                           new-cids-of-target (java.util.HashSet. )
                           _r-target'
                           ;; slow
                           (doseq [^java.util.HashMap$Node e (.entrySet r-source)]
                             (let [cid (long (.getKey e))
                                   sv (double (.getValue e))
                                   tv (.get r-target (long cid))]
                               (if tv
                                 (let [tv' (unchecked-add (unchecked-multiply alpha sv)
                                                          (double tv))]
                                   (.put r-target
                                         cid
                                         tv')
                                   (when (and (zero? tv')
                                              (not= cid num-rows))
                                     (.add zero-cids-of-target cid)
                                     (.remove r-target cid)))
                                 (do
                                   (.put r-target
                                         cid
                                         (unchecked-multiply alpha sv))
                                   (.add new-cids-of-target cid)))))
                           ;; zero below pivot
                           _ (.remove r-target
                                      (long rid-source))
                           _coor' nil
                           ;; slow
                           _ (.remove new-cids-of-target
                                      (long num-rows))
                           _cid->rids+ (doseq [cid new-cids-of-target]
                                         (let [rids ^java.util.HashSet (aget cid->rids cid)]
                                           (.add rids rid-target)))
                           _ (.remove zero-cids-of-target
                                      (long num-rows))
                           _cid->rids- (doseq [cid (into [rid-source]
                                                         zero-cids-of-target)]
                                         (when-not (= cid num-rows)
                                           (let [rids ^java.util.HashSet (aget cid->rids cid)]
                                             (.remove rids (long rid-target)))))])
                     )
                   ]
               (recur :swap-rows
                      p-crid'))
             (do
               (def pivot-help {:p-crid p-crid
                                :state state
                                :rio (into [] rio)
                                :coor (mapv
                                       (fn [row]
                                         (into {} row))
                                       coor)
                                :cid->rids (mapv (fn [rids]
                                                   (into #{} rids))
                                                 cid->rids)})
               (recur :pivots->1
                      p-crid))))
         :pivots->1
         (if (< p-crid 0)
           (recur :back-subst
                  (dec num-rows))
           (let [rid (long p-crid)
                 o-rid (aget rio rid)
                 r ^java.util.HashMap (aget coor o-rid)
                 p-v (double (.get r rid))
                 alpha (/ 1 p-v)
                 entries (doall (map (fn [^java.util.HashMap$Node e]
                                       [(.getKey e) (.getValue e)])
                                     (.entrySet r)))
                 _r' (doseq [e entries]
                       (let [cid (long (first e))
                             v (double (second e))
                             v' (unchecked-multiply alpha v)]
                         (if (zero? v')
                           (when-not (= cid num-rows)
                             (.remove r cid)
                             (let [rids ^java.util.HashSet (aget cid->rids cid)]
                               (.remove rids rid)))
                           (.put r cid v'))))
                 _r' (.put r rid 1.0)
                 _coor' nil]
             (recur :pivots->1
                    (dec p-crid))))
         :back-subst
         (loop [p-crid p-crid]
           (if (< p-crid 1)
             {:rio rio
              :coor coor
              :cid->rids cid->rids}
             (let [p-b (double
                        (.get ^java.util.HashMap (aget coor (aget rio p-crid))
                              (long num-rows)))
                   rids (disj (into #{} (aget cid->rids p-crid))
                              p-crid)
                   coor' (doseq [rid rids]
                           (let [row ^java.util.HashMap (aget coor (aget rio rid))]
                             (let [e (double (.get row (long p-crid)))
                                   bi (double (.get row (long num-rows)))
                                   alpha (- e)]
                               (doto row
                                 (.remove (long p-crid))
                                 (.put (long num-rows)
                                       (+ (* alpha p-b)
                                          bi))))))
                   
                   _cid->rids' (aset cid->rids
                                     p-crid
                                     #{p-crid})]
               (recur (dec p-crid)))))
         :end
         nil)))))

;; parallel java version
#_ (defn auto10-gaussian-elimination [augmented-original-order-rows]
     )

(defn map-rows->vec-rows [map-rows]
  (mapv (fn [i]
          (get map-rows
               i))
        (range (count map-rows))))

(defn make-augmented [matrix-vec-map b]
  (let [num-rows (count b)]
    (mapv
     (fn [ri bi]
       (assoc ri
              num-rows
              bi))
     matrix-vec-map
     b)))
