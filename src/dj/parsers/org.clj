(ns dj.parser.org
  (:require [dj.algorithms.peg2 :as p]
            [clojure.string :as cs]))

(p/defp ::pad (p/m (p/* " " 1)
                   cs/join))
(p/defp ::text-to-line (p/m (p/s :text (p/* (p/! \newline))
                             :newline (p/| :newline \newline
                                           :eof ::p/eof))
                            (comp
                             cs/join
                             :text)))

(defn bullet-parser [indent step]
  (fn bullet [input cursor]
    (let [new-indent (+ indent step)]
      ((p/m (p/s :prefix "-"
                 :pad ::pad
                 :content (p/m (p/s :line ::text-to-line
                                    :other-lines (p/* (p/m (p/s :indent (p/m (p/* " " new-indent new-indent)
                                                                             cs/join)
                                                                :not-dash (p/>? (p/! "-"))
                                                                :line ::text-to-line)
                                                           :line))
                                    :children (p/* (p/m (p/s :indent (p/m (p/* " " new-indent new-indent)
                                                                          cs/join)
                                                             :child (bullet-parser new-indent step))
                                                        :child)))
                               (fn [{:keys [:line :other-lines :children]}]
                                 {:body (into [line] other-lines)
                                  :children children})))
            :content)
       input cursor))))
(dissoc (p/parse (bullet-parser 0 2)
                 "- [X]   this is a dog
  abcd
  acd
  - markov")
        :last-parse-data)
