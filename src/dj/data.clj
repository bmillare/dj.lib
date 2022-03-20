(ns dj.data)

(defn find-in-fn
  "convenience for map/filter, like find-in but returns fn that takes
  coll with f already bound"
  [f]
  (fn [coll]
    (loop [q [coll]]
      (if (empty? q)
        nil
        (let [item (peek q)
              nq (pop q)]
          (if (f item)
            item
            (cond
              (sequential? item)
              (recur (into nq
                           item))
              (map? item)
              (recur (into nq
                           (vals item)))
              :else
              (recur (pop q)))))))))

(defn find-in
  "returns item in nested data if f returns true on that item"
  [coll f]
  ((find-in-fn f) coll))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.
  (deep-merge-with + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
                     {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))
