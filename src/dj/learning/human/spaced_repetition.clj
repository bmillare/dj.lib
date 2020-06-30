(ns dj.learning.human.spaced-repetition
  (:require [clojure.java.io :as cji]))

;; - want to maximize exposure to all words
;; - want maximize correct answers to questions
;; - want the gaps between correct answers to be long

;; idea: exponential backoff on correct answers

;; other idea: queue of words to learn
;; have a queue
;; - goal is to learn target word first, with other words, regardless
;;   of competance, used as filler
;; - goal is to get this target word through exponential increase in
;;   time between sights
;; - in correct responses decrements the gap by exponential factor
;; - only if word is in waiting can we see other words in the list

;; master list
[[:a {:score 0 :scheduled true}]
 [:b {:score 0 :scheduled false}]
 [:c {:score 0 :scheduled false}]
 [:d]
 [:e]
 [:f]]

;; review stack
[[:a ]
 [:a]
 [:a]
 [:b]
 [:a]
 [:b]
 [:b]
 [:a]]

;; - have list/queue (review stack)
;; - can have nil entries
;; - can pop list after consuming
;; - can schedule a card based on score in advance
;; - if after popping, next slot is empty, fill with next item in master list that is not currently scheduled
;; - an item that was scheduled before, will always be scheduled in the future
;;   - after consuming an item, it is immediately scheduled again (based on result)
;;   - if ideal slot is full, delay scheduling by one card, keep retrying until success
;; - if correct, place in next avail slot = or after desired slot
;; - if incorrect, place in desired slot and push remaining back

(defn place-at-or-next-free [v relative-idx x]
  (let [relative-idx (long relative-idx)
        v-size (long (count v))
        last-idx (dec v-size)]
    (if (< relative-idx v-size)
      (let [idx (- last-idx relative-idx)]
        (if (v idx)
          (recur v (inc relative-idx) x)
          (assoc v idx x)))
      (-> [x]
          (into (repeat (- relative-idx last-idx (long 1)) nil))
          (into v)))))

(comment
  (clojure.repl/apropos "repeat")
  (clojure.repl/doc repeat)
  [nil] 0 :x

  (= [:x nil nil]
     (place-at-or-next-free [] 2 :x))

  (= [:x]
     (place-at-or-next-free [] 0 :x))

  (= [:x nil :b :a]
     (place-at-or-next-free [:b :a] 3 :x))

  (= [:x :b :a]
     (place-at-or-next-free [:b :a] 1 :x))

  (= [:x :b :a]
     (place-at-or-next-free [:b :a] 0 :x)))

(defn place-at-and-push-back
  [v relative-idx x]
  (try
    (let [relative-idx (long relative-idx)
          v-size (long (count v))
          last-idx (dec v-size)
          idx (- last-idx relative-idx)
          next-idx (inc idx)]
      (if (> relative-idx v-size)
        (-> [x]
            (into (repeat (- relative-idx last-idx (long 1)) nil))
            (into v))
        (let [a (subvec v
                        (max (long 0)
                             next-idx)
                        v-size)
              c (subvec v 0 (inc (min last-idx idx)))]
          (-> (conj c x)
              (into a)))))
    (catch Exception e
      (throw (ex-info "failed"
                      {:v v
                       :relative-idx relative-idx
                       :x x}
                      e)))))
(comment
  
 (= [:a :b :c :x]
    (place-at-and-push-back [:a :b :c] 0 :x))

 (= [:a :b :x :c]
    (place-at-and-push-back [:a :b :c] 1 :x))

 (= [:x]
    (place-at-and-push-back [] 0 :x))
 (= [:x nil]
    (place-at-and-push-back [] 1 :x)))

(defn runit []
  (let [master [:a :b :c :d]
        lzero (long 0)]
    (loop [scores {:a 0
                   :b 0
                   :c 0
                   :d 0}
           stack [nil]
           idx lzero]
      (print "scores ") (prn scores)
      (print "stack " ) (prn stack)
      (print "idx ") (prn idx)
      (println "q quit")
      (println "p correct")
      (println "n incorrect")
      (let [current (peek stack)]
        (if current
          (let [score (long (scores current))
                _ (println current)
                _ (flush)
                input (read-line)]
            (case input
              "p" (let [new-score (inc score)]
                    (recur (assoc scores
                                  current
                                  new-score)
                           (place-at-or-next-free (pop stack) new-score current)
                           idx))
              "n" (let [new-score (max (dec score)
                                       lzero)]
                    (recur (assoc scores
                                  current
                                  new-score)
                           (place-at-and-push-back (pop stack) new-score current)
                           idx))
              "q" nil
              (recur scores
                     stack
                     idx)))
          (let [idx (mod idx (count master))
                current (master idx)
                new-idx (inc idx)
                score (long (scores current))
                _ (println current)
                _ (flush)
                input (read-line)]
            (println current)
            (case input
              "p" (let [new-score (inc score)]
                    (recur (assoc scores
                                  current
                                  new-score)
                           (place-at-or-next-free (pop stack) new-score current)
                           new-idx))
              "n" (let [new-score (max (dec score)
                                       lzero)]
                    (recur (assoc scores
                                  current
                                  new-score)
                           (place-at-and-push-back (pop stack) new-score current)
                           new-idx))
              "q" nil
              (recur scores
                     stack
                     idx))))))))

(let [prompt
      (fn prompt [current]
        (println current)
        (flush)
        (read-line)
        (println "q quit")
        (println "p correct")
        (println "n incorrect")
        (flush))]
  (defn runit2 [master]
    (let [lzero (long 0)]
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
              (case input
                "p" (let [new-score (inc score)]
                      (recur (assoc scores
                                    current
                                    new-score)
                             (place-at-or-next-free (pop stack) new-score current)
                             idx))
                "n" (let [new-score (max (dec score)
                                         lzero)]
                      (recur (assoc scores
                                    current
                                    new-score)
                             (place-at-and-push-back (pop stack) new-score current)
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
              (case input
                "p" (let [new-score (inc score)]
                      (recur (assoc scores
                                    current
                                    new-score)
                             (place-at-or-next-free (pop stack) new-score current)
                             new-idx))
                "n" (let [new-score (max (dec score)
                                         lzero)]
                      (recur (assoc scores
                                    current
                                    new-score)
                             (place-at-and-push-back (pop stack) new-score current)
                             new-idx))
                "q" nil
                (recur scores
                       stack
                       idx)))))))))

(let [correct-letter ""
      incorrect-letter "'"
      quit-letter "q"
      prompt
      (fn prompt [[a b]]
        (println a)
        (flush)
        (read-line)
        (println "====================")
        (println (pr-str quit-letter) "quit," (pr-str correct-letter) "correct," (pr-str incorrect-letter) "incorrect")
        (println b)
        (flush))]
  (defn runit3 [master]
    (let [lzero (long 0)]
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
                         (place-at-or-next-free (pop stack) new-score current)
                         idx))
                incorrect-letter
                (let [new-score (max (dec score)
                                     lzero)]
                  (recur (assoc scores
                                current
                                new-score)
                         (place-at-and-push-back (pop stack) new-score current)
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
                         (place-at-or-next-free (pop stack) new-score current)
                         new-idx))
                incorrect-letter
                (let [new-score (max (dec score)
                                     lzero)]
                  (recur (assoc scores
                                current
                                new-score)
                         (place-at-and-push-back (pop stack) new-score current)
                         new-idx))
                "q" nil
                (recur scores
                       stack
                       idx)))))))))

;; Todo
;; - [X] be able to pull from word lists
;; - [X] convert to tf
;; - [X] be able to store scores and restore scores state
;; - [X] generate word lists for different topics
;;   - [X] airport codes / regions
;; - [X] improve interface, be able to see characters larger via swing
;; - [X] make dumb durable log compacter
;; - [ ] checkout progress
;; - [ ] cleanup checkout clean

(defn table-reader [path f]
  (with-open [table-reader (cji/reader path)]
    (mapv (fn [s]
            (f (clojure.string/split s #"\t")))
          (line-seq table-reader))))
