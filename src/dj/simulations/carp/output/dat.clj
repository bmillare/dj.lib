(ns dj.simulations.carp.output.dat
  (:require [clojure.string :as cs]
            [clojure.java.io :as cjio]))

(defn vec-dat-reader [txt]
  (let [lines (drop 2 (cs/split-lines txt))]
    (mapv (fn [line]
            (let [trimmed (cs/trim line)]
              (Float/parseFloat trimmed)))
          lines)))

(defn matrix-dat-lazy-reader
  "might not work on multiprocessed matrix output"
  [path]
  (with-open [rdr (cjio/reader path)]
    (loop [lines (drop 2 (line-seq rdr))
           ordered-rows {}]
      (if (empty? lines)
        ordered-rows
        (let [line (first lines)
              [first-half second-half] (cs/split line #":")
              row-id (try
                       (Integer/parseInt (re-find #"\d+" first-half))
                       (catch Exception e
                         (throw (ex-info "parse Int failed"
                                         {:first-half first-half}))))
              all-mat-txt-entries (re-seq #"\([^\)]+\)" second-half)
              ordered-rows' (reduce (fn [ret txt]
                                      (let [trimmed (subs txt 1 (dec (count txt)))
                                            [txt-col-id txt-entry] (cs/split trimmed #",")
                                            entry (Double/parseDouble txt-entry)]
                                        (if (zero? entry)
                                          ret
                                          (assoc-in ret
                                                    [row-id (Integer/parseInt txt-col-id)]
                                                    entry))))
                                    ordered-rows
                                    all-mat-txt-entries)]
          (recur (rest lines)
                 ordered-rows'))))))

(comment

  (-> (dj.io/file "/preCARP_LAPLACE_PRINT3/test3/LAPLACE_STEP_I_e.dat")
      dj.io/eat
      vec-dat-reader)

  (try
    (binding [*print-length* nil]
      (-> "/preCARP_LAPLACE_PRINT3/test3/LAPLACE_STEP_K_ie.dat"
          matrix-dat-lazy-reader
          pr-str
          (->> (dj.io/poop (dj.io/file "/tmp/K_ie.clj")))))
    (catch Exception e
      (ex-data e)))

  )
