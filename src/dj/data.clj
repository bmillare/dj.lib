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
