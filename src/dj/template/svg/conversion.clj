(ns dj.template.svg.conversion)

(defn pt->inches
  [n]
  (/ n 72.0))

(defn pt->mm
  [n]
  (* n 0.352778))

(def s_pt->mm (comp str pt->mm))
(def s_pt->inches (comp str pt->inches))

(defn split
  "n: number of partitions"
  ([n]
   (let [delta (double (/ 1 (inc n)))]
     (loop [ret []
            i 0]
       (if (< i n)
         (let [next-i (inc i)]
           (recur
            (conj ret
                  (* next-i delta))
            next-i))
         ret))))
  ([n start end]
   (let [diff (- end start)
         delta (double (/ diff (inc n)))]
     (loop [ret []
            i 0]
       (if (< i n)
         (let [next-i (inc i)]
           (recur
            (conj ret
                  (+ start
                     (* next-i delta)))
            next-i))
         ret)))))

(defn middle-idx
  [coll]
  (long
   (/ (dec (count coll))
      2)))

(defn middle-item
  [coll]
  (nth coll
       (middle-idx coll)))

(comment
  (split 5)
  (split 5 10 20)
  )
