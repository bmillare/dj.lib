(ns dj.algorithms.peg.lines)

(defn line-match [^java.util.regex.Pattern re]
  (fn [input succeed fail]
    (let [line (first input)
          match (re-find re line)]
      (if match
        (succeed match
                 (rest input))
        (fail line input)))))

(defn ignore-until-ahead [^java.util.regex.Pattern re]
  (fn [input succeed fail]
    (loop [rinput input]
      (let [line (first rinput)]
        (if line
          (let [match (re-find re line)]
            (if match
              (succeed nil
                       rinput)
              (recur (rest rinput))))
          (fail :end-of-input input))))))

(defn match-if-not [^java.util.regex.Pattern re]
  (fn [input succeed fail]
    (let [line (first input)
          match (re-find re line)]
      (if match
        (fail match input)
        (succeed line
                 (rest input))))))
