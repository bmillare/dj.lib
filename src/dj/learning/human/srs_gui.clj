(require '[dj.dependencies2])
(dj.dependencies2/add-dependencies '[[seesaw "1.5.0" :exclusions [org.clojure/clojure]]])
(ns dj.learning.human.srs-gui
  (:require [seesaw.core :as sc]
            [dj.learning.human.spaced-repetition :as srs]
            [dj.dispatch.treefn :as tf]
            [dj.durable :as dd]))

(sc/native!)

(def default-tfgui-inputs
  {:incorrect-symbol 'l
   :quit-symbol 'q
   :window-title "SRS"
   :window-width 1400
   :window-height 100
   :window-start-x 100
   :window-start-y 100
   :font "ARIAL-40"
   :start-idx 0
   :scaler
   (fn ^long scaler [^long x]
     (long (Math/floor (Math/pow (double 2) (double (dec x))))))})

(def tfgui-saver-fms
  {:score-writer
   (tf/fm
    [:score-path]
    (dd/kv-writer score-path))
   :save-agent
   (tf/fm
    []
    (dd/kv-agent))
   :score-saver!
   (tf/fm
    [:score-writer]
    (dd/assoc+save!-fn score-writer))
   :score-closer!
   (tf/fm
    [:run!
     ^java.io.Writer score-writer]
    (.close score-writer))
   :initial-scores
   (tf/fm
    [:score-path]
    (dd/read-kv-log-file score-path))})

(def tfgui-fms
  {:run!
   (tf/fm
    [:incorrect-symbol
     :quit-symbol
     :master-list
     :window-title
     :window-width
     :window-height
     :window-start-x
     :window-start-y
     :font
     :scaler
     :initial-scores
     :score-saver!
     :start-idx]
    (let [frame (sc/frame :title window-title
                          :width window-width
                          :height window-height)
          label (sc/label :text "")
          _ (do
              (sc/config! frame
                          :content label)
              (.setHorizontalAlignment label javax.swing.SwingConstants/CENTER)
              (sc/move! frame :to [window-start-x window-start-y])
              (sc/config! label :font font)
              (-> frame
                  sc/show!))
          lzero (long 0)
          lone (long 1)
          prompt
          (fn prompt [[a b]]
            (sc/config! label
                        :text a)
            
            (flush)
            (read-line)
            (println "====================")
            (println (pr-str quit-symbol) "quit," "\"\" or [0-9] correct," (pr-str incorrect-symbol) "incorrect")
            (sc/config! label
                        :text b)
            (flush))]
      (loop [scores initial-scores
             stack [nil]
             idx (long start-idx)]
        (print "scores ") (prn scores)
        ;;(print "stack " ) (prn stack)
        (print "idx ") (prn idx)
        (flush)
        (let [current (peek stack)]
          (if current
            (let [score (long (or (scores current)
                                  lzero))
                  _ (prompt current)
                  input (read-line)
                  read-input (if (empty? input)
                               lone
                               (read-string input))]
              (cond
                (number? read-input)
                (let [new-score (+ (long read-input) score)]
                  (recur (score-saver! scores
                                       current
                                       new-score)
                         (srs/place-at-or-next-free (pop stack) (scaler new-score) current)
                         idx))
                (= read-input incorrect-symbol)
                (let [new-score (max (dec score)
                                     lzero)]
                  (recur (score-saver! scores
                                       current
                                       new-score)
                         (srs/place-at-and-push-back (pop stack) (scaler new-score) current)
                         idx))
                (= read-input quit-symbol) nil
                :else
                (recur scores
                       stack
                       idx)))
            (let [idx (mod idx (count master-list))
                  current (master-list idx)
                  new-idx (inc idx)
                  score (long (or (scores current)
                                  lzero))
                  _ (prompt current)
                  input (read-line)
                  read-input (if (empty? input)
                               lone
                               (read-string input))]
              (println current)
              (cond
                (number? read-input)
                (let [add (if (empty? input)
                            lone
                            (long (read-string input)))
                      new-score (+ add score)]
                  (recur (score-saver! scores
                                       current
                                       new-score)
                         (srs/place-at-or-next-free (pop stack) (scaler new-score) current)
                         new-idx))
                (= read-input incorrect-symbol)
                (let [new-score (max (dec score)
                                     lzero)]
                  (recur (score-saver! scores
                                       current
                                       new-score)
                         (srs/place-at-and-push-back (pop stack) (scaler new-score) current)
                         new-idx))
                (= read-input quit-symbol) nil
                :else
                (recur scores
                       stack
                       idx))))))))})
