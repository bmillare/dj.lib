(ns dj.simulations.carp.output.bench
  (:require [dj.algorithms.peg :as ap]
            [clojure.string :as cs]))

(let [ignore-line (ap/t #".*")
      ws (ap/t #"\s*")
      word (ap/t #"\S+")
      line (ap/alt (ap/s ws word ws word ws word ws)
                   (fn [[_ time _ vm _ ignore _]]
                     [(Double/parseDouble time)
                      (Double/parseDouble vm)]))
      lines (ap/alt (ap/s ignore-line
                          (ap/+ line))
                    second)]
  (defn parse-bench-vm [txt]
    (loop [times (transient [])
           vms (transient [])
           rows (:result (ap/parse lines txt))]
      (if (empty? rows)
        {:time (persistent! times)
         :voltage (persistent! vms)}
        (let [[t v] (first rows)]
          (recur (conj! times t)
                 (conj! vms v)
                 (rest rows)))))))

(let [ws (ap/t #"\s*")
      prefix (ap/t #"[^.]+")
      dot (ap/t #"\.")
      label (ap/t #"\S+")
      row (ap/alt (ap/s ws prefix dot label ws label ws label ws label ws)
                  (fn [[_ prefix _ sv-name _ sv-type _ sv-type-size _ trace-size _]]
                    {:prefix prefix
                     :sv-name sv-name
                     :sv-type sv-type
                     :sv-type-size (Long/parseLong sv-type-size)
                     :trace-size (Long/parseLong trace-size)}))]
  (defn parse-bench-header [txt]
    (reduce (fn [ret line]
              (let [data (:result (ap/parse row line))]
                (assoc ret
                       (:sv-name data)
                       data)))
            {}
            (drop 1
                  (cs/split-lines txt)))))

(defn file->real-vec
  "real-type: :float or :double"
  [^java.nio.channels.FileChannel file-channel
   real-type]
  (let [buf (.map file-channel
                  java.nio.channels.FileChannel$MapMode/READ_ONLY
                  0
                  (.size file-channel))]
    (.order buf java.nio.ByteOrder/LITTLE_ENDIAN)
    (case real-type
      :float (let [fb (.asFloatBuffer buf)
                   size (.limit fb)
                   ret-a (float-array size)]
               (.get fb ret-a)
               (vec ret-a))
      :double (let [db (.asDoubleBuffer buf)
                    size (.limit db)
                    ret-a (double-array size)]
                (.get db ret-a)
                (vec ret-a)))))

#_ (comment
  (let [file (dj.io/file "/home/username/mnt/kpurryvm/bench_test/BENCH_REG_MBRDR.m")]
    (io/with-file-channel [c file "r"]
      (vec
       (file->real-array c
                         :float)))
    )

  (let [file (dj.io/file "/home/username/mnt/kpurryvm/bench_test/BENCH_REG_header.txt")]
    (parse-bench-header (dj.io/eat file))
    )
  
  (let [input {:file nil}
        fms {}])


  )
