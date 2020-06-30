(require '[dj.dependencies2])
(dj.dependencies2/add-dependencies '[[seesaw "1.5.0" :exclusions [org.clojure/clojure]]])
(ns dj.learning.human.srs-gui
  (:require [seesaw.core :as sc]
            [dj.learning.human.spaced-repetition :as srs]
            [dj.dispatch.treefn :as tf]))

(sc/native!)

(let [correct-letter ""
      incorrect-letter "'"
      quit-letter "q"
      #_ #_ master [["我 wǒ" "I"][1 "yi"][2 "er"][3 "san"][4 "si"]]]
  (defn rungui1 [master]
    (let [frame (sc/frame :title "SRS"
                          :width 400
                          :height 400)
          label (sc/label :text "Start")
          _ (do
              (sc/config! frame
                          :content label)
              (.setHorizontalAlignment label javax.swing.SwingConstants/CENTER)
              (sc/move! frame :to [100 100])
              (sc/config! label :font "ARIAL-40")
              (-> frame
                  sc/show!))
          lzero (long 0)
          prompt
          (fn prompt [[a b]]
            (sc/config! label
                        :text a)
            
            (flush)
            (read-line)
            (println "====================")
            (println (pr-str quit-letter) "quit," (pr-str correct-letter) "correct," (pr-str incorrect-letter) "incorrect")
            (sc/config! label
                        :text b)
            (flush))]
      (loop [scores {}
             stack [nil]
             idx lzero]
        (print "scores ") (prn scores)
        (print "stack " ) (prn stack)
        (print "idx ") (prn idx)
        (let [current (peek stack)]
          (if current
            (let [score (long (or (scores current)
                                  lzero))
                  _ (prompt current)
                  input (read-line)]
              (condp = input
                correct-letter
                (let [new-score (inc score)]
                  (recur (assoc scores
                                current
                                new-score)
                         (srs/place-at-or-next-free (pop stack) new-score current)
                         idx))
                incorrect-letter
                (let [new-score (max (dec score)
                                     lzero)]
                  (recur (assoc scores
                                current
                                new-score)
                         (srs/place-at-and-push-back (pop stack) new-score current)
                         idx))
                "q" nil
                (recur scores
                       stack
                       idx)))
            (let [idx (mod idx (count master))
                  current (master idx)
                  new-idx (inc idx)
                  score (long (or (scores current)
                                  lzero))
                  _ (prompt current)
                  input (read-line)]
              (println current)
              (condp = input
                correct-letter
                (let [new-score (inc score)]
                  (recur (assoc scores
                                current
                                new-score)
                         (srs/place-at-or-next-free (pop stack) new-score current)
                         new-idx))
                incorrect-letter
                (let [new-score (max (dec score)
                                     lzero)]
                  (recur (assoc scores
                                current
                                new-score)
                         (srs/place-at-and-push-back (pop stack) new-score current)
                         new-idx))
                "q" nil
                (recur scores
                       stack
                       idx)))))))))

(let [correct-letter ""
      incorrect-letter "'"
      quit-letter "q"
      #_ #_ master [["我 wǒ" "I"][1 "yi"][2 "er"][3 "san"][4 "si"]]
      scaler (fn ^long scaler [^long x]
               (long (Math/floor (Math/pow (double 2) (double (dec x))))))]
  (defn rungui2 [master]
    (let [frame (sc/frame :title "SRS"
                          :width 1000
                          :height 200)
          label (sc/label :text "Start")
          _ (do
              (sc/config! frame
                          :content label)
              (.setHorizontalAlignment label javax.swing.SwingConstants/CENTER)
              (sc/move! frame :to [100 100])
              (sc/config! label :font "ARIAL-40")
              (-> frame
                  sc/show!))
          lzero (long 0)
          prompt
          (fn prompt [[a b]]
            (sc/config! label
                        :text a)
            
            (flush)
            (read-line)
            (println "====================")
            (println (pr-str quit-letter) "quit," (pr-str correct-letter) "correct," (pr-str incorrect-letter) "incorrect")
            (sc/config! label
                        :text b)
            (flush))]
      (loop [scores {}
             stack [nil]
             idx lzero]
        (print "scores ") (prn scores)
        (print "stack " ) (prn stack)
        (print "idx ") (prn idx)
        (let [current (peek stack)]
          (if current
            (let [score (long (or (scores current)
                                  lzero))
                  _ (prompt current)
                  input (read-line)]
              (condp = input
                correct-letter
                (let [new-score (inc score)]
                  (recur (assoc scores
                                current
                                new-score)
                         (srs/place-at-or-next-free (pop stack) (scaler new-score) current)
                         idx))
                incorrect-letter
                (let [new-score (max (dec score)
                                     lzero)]
                  (recur (assoc scores
                                current
                                new-score)
                         (srs/place-at-and-push-back (pop stack) (scaler new-score) current)
                         idx))
                "q" nil
                (recur scores
                       stack
                       idx)))
            (let [idx (mod idx (count master))
                  current (master idx)
                  new-idx (inc idx)
                  score (long (or (scores current)
                                  lzero))
                  _ (prompt current)
                  input (read-line)]
              (println current)
              (condp = input
                correct-letter
                (let [new-score (inc score)]
                  (recur (assoc scores
                                current
                                new-score)
                         (srs/place-at-or-next-free (pop stack) (scaler new-score) current)
                         new-idx))
                incorrect-letter
                (let [new-score (max (dec score)
                                     lzero)]
                  (recur (assoc scores
                                current
                                new-score)
                         (srs/place-at-and-push-back (pop stack) (scaler new-score) current)
                         new-idx))
                "q" nil
                (recur scores
                       stack
                       idx)))))))))

(let [incorrect-letter 'l
      quit-letter 'q
      scaler (fn ^long scaler [^long x]
               (long (Math/floor (Math/pow (double 2) (double (dec x))))))]
  (defn rungui3 [master]
    (let [frame (sc/frame :title "SRS"
                          :width 1200
                          :height 120)
          label (sc/label :text "Start")
          _ (do
              (sc/config! frame
                          :content label)
              (.setHorizontalAlignment label javax.swing.SwingConstants/CENTER)
              (sc/move! frame :to [100 100])
              (sc/config! label :font "ARIAL-40")
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
            (println (pr-str quit-letter) "quit," "\"\" or [0-9] correct," (pr-str incorrect-letter) "incorrect")
            (sc/config! label
                        :text b)
            (flush))]
      (loop [scores {}
             stack [nil]
             idx lzero]
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
                               (long 1)
                               (read-string input))]
              (cond
                (number? read-input)
                (let [new-score (+ (long read-input) score)]
                  (recur (assoc scores
                                current
                                new-score)
                         (srs/place-at-or-next-free (pop stack) (scaler new-score) current)
                         idx))
                (= read-input incorrect-letter)
                (let [new-score (max (dec score)
                                     lzero)]
                  (recur (assoc scores
                                current
                                new-score)
                         (srs/place-at-and-push-back (pop stack) (scaler new-score) current)
                         idx))
                (= read-input quit-letter) nil
                :else
                (recur scores
                       stack
                       idx)))
            (let [idx (mod idx (count master))
                  current (master idx)
                  new-idx (inc idx)
                  score (long (or (scores current)
                                  lzero))
                  _ (prompt current)
                  input (read-line)
                  read-input (if (empty? input)
                               (long 1)
                               (read-string input))]
              (println current)
              (cond
                (number? read-input)
                (let [add (if (empty? input)
                            lone
                            (long (read-string input)))
                      new-score (+ add score)]
                  (recur (assoc scores
                                current
                                new-score)
                         (srs/place-at-or-next-free (pop stack) (scaler new-score) current)
                         new-idx))
                (= read-input incorrect-letter)
                (let [new-score (max (dec score)
                                     lzero)]
                  (recur (assoc scores
                                current
                                new-score)
                         (srs/place-at-and-push-back (pop stack) (scaler new-score) current)
                         new-idx))
                (= read-input quit-letter) nil
                :else
                (recur scores
                       stack
                       idx)))))))))

(def default-tfgui-inputs
  {:incorrect-symbol 'l
   :quit-symbol 'q
   :window-title "SRS"
   :window-width 1400
   :window-height 100
   :window-start-x 100
   :window-start-y 100
   :font "ARIAL-40"
   :scaler
   (fn ^long scaler [^long x]
     (long (Math/floor (Math/pow (double 2) (double (dec x))))))
   :initial-scores {}
   })

(def tfgui-fms
  {:run!
   (tf/fm
    [:incorrect-symbol
     :quit-symbol
     :master
     :window-title
     :window-width
     :window-height
     :window-start-x
     :window-start-y
     :font
     :scaler
     :initial-scores
     :score-saver!]
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
             idx lzero]
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
            (let [idx (mod idx (count master))
                  current (master idx)
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
