(ns dj.learning.human.scratch
  (:require [dj.learning.human.spaced-repetition :as srs]
            [dj.learning.human.srs-gui :as srsgui]
            [dj.dispatch.treefn :as tf]
            [dj.durable :as dd]))

;; HSK1 Chinese -> English
(do
  (load "srs_gui")
  (read-line)
  (-> "/home/bmillare/Documents/learn-chinese/HSK Official With Definitions 2012 L1 freqorder.txt"
      (srs/table-reader (fn [[simplified traditional tonenumber tonemark english]]
                          [(str simplified " " tonemark)
                           english]))
      (srsgui/rungui3)))

(clojure.repl/doc send-off)

;; airport region mapper
(do
  (load "srs_gui")
  (load "/dj/durable")
  (let [my-agent (dd/kv-agent)
        scores-path "/home/bmillare/Documents/flashcards/aws_regions_scores.edn"
        my-writer (dd/kv-writer scores-path)
        input (merge srsgui/default-tfgui-inputs
                     {:master
                      (-> "/home/bmillare/Documents/flashcards/aws_regions.txt"
                          (srs/table-reader (fn [[airport region descrp]]
                                              [airport
                                               region])))
                      :initial-scores (dd/read-kv-log-file scores-path)
                      :score-saver! (dd/assoc+save!-fn my-writer)})
        the-tf (tf/treefm srsgui/tfgui-fms
                          :run!)]
    (read-line)
    (the-tf input)
    (.close my-writer)))

(load "/dj/durable")
(dd/map->storage-dumb
 {:x 1
  :y 2}
 "/home/bmillare/tmp/test.edn")
