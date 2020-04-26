(ns dj.template.graphviz.convert)

(defn direct-dependencies-map->edges
  "converts direct dependency graph (key->vec of keys) into a graphviz
  friendly set of edges (each edge is a vector representing a keyword
  pair) representing keyword1 \"needed by\" keyword2
  "
  [direct-dependency-map]
  (->> (reduce-kv (fn [ret k v]
                    (assoc ret
                           k
                           (set v)))
                  {}
                  direct-dependency-map)
       (reduce-kv (fn add [ret k v]
                    (if (empty? v)
                      ret
                      (let [dep (first v)]
                        (recur (conj ret [k dep])
                               k
                               (disj v dep)))))
                  [])
       (mapv (comp vec rseq))
       set))
