(ns dj.learning.human.scratch
  (:require [dj.learning.human.spaced-repetition :as srs]
            [dj.learning.human.srs-gui :as srsgui]
            [dj.dispatch.treefn :as tf]))

;; HSK1 Chinese -> English
(do
  (load "srs_gui")
  (let [input (merge srsgui/default-tfgui-inputs
                     {:master-list
                      (-> "/home/bmillare/Documents/learn-chinese/HSK Official With Definitions 2012 L1 freqorder.txt"
                          (srs/table-reader (fn [[simplified traditional tonenumber tonemark english]]
                                              [english
                                               (str simplified " " tonemark)])))
                      :score-path "/home/bmillare/Documents/learn-chinese/HSK Official With Definitions 2012 L1 freqorder scores.edn"
                      :start-idx 110})
        the-tf (tf/treefm (merge srsgui/tfgui-fms
                                 srsgui/tfgui-saver-fms)
                          :score-closer!)]
    (read-line)
    (the-tf input)))

;; HSK2 Chinese -> English
(do
  (load "srs_gui")
  (let [input (merge srsgui/default-tfgui-inputs
                     {:master-list
                      (-> "/home/bmillare/Documents/learn-chinese/HSK Official With Definitions 2012 L2 freqorder.txt"
                          (srs/table-reader (fn [[simplified traditional tonenumber tonemark english]]
                                              [english
                                               (str simplified " " tonemark)])))
                      :score-path "/home/bmillare/Documents/learn-chinese/HSK Official With Definitions 2012 L1 freqorder scores.edn"
                      :start-idx 0})
        the-tf (tf/treefm (merge srsgui/tfgui-fms
                                 srsgui/tfgui-saver-fms)
                          :score-closer!)]
    (read-line)
    (the-tf input)))

(clojure.repl/doc send-off)

;; airport region mapper
(do
  (load "srs_gui")
  (load "/dj/durable")
  (let [input (merge srsgui/default-tfgui-inputs
                     {:master-list
                      (-> "/home/bmillare/Documents/flashcards/aws_regions.txt"
                          (srs/table-reader (fn [[airport region descrp]]
                                              [region
                                               airport])))
                      :score-path "/home/bmillare/Documents/flashcards/aws_regions_scores.edn"})
        the-tf (tf/treefm (merge srsgui/tfgui-fms
                                 srsgui/tfgui-saver-fms)
                          :score-closer!)]
    (read-line)
    (the-tf input)))

(load "/dj/durable")
(dd/map->storage-dumb
 {:x 1
  :y 2}
 "/home/bmillare/tmp/test.edn")
