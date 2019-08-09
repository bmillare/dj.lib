(ns dj.algorithms.diff)

(defn new-entries
  "returns keys of map that are new or different in b compared to a (does not handled removed)"
  [a b]
  (persistent!
   (reduce (fn [ret k]
             (if (contains? a k)
               (if (= (a k) (b k))
                 ret
                 (conj! ret k))
               (conj! ret k)))
           (transient [])
           (keys b))))
