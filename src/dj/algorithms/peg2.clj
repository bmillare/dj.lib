(ns dj.algorithms.peg2
  (:refer-clojure :exclude [*]))

(defprotocol IParser
  "return a parser given arg type"
  (parser [this registry]))

(extend-type java.lang.Character
  IParser
  (parser [this registry]
    (fn char-parser [^String input cursor]
      (if (< cursor (count input))
        (let [input-char (.charAt input (int cursor))]
          (if (= input-char
                 this)
            {:operator 'char-parser
             :parsed? true
             :result this
             :old-cursor cursor
             :new-cursor (inc cursor)}
            {:operator 'char-parser
             :parsed? false
             :input-char input-char
             :candidate this
             :comment "failed to match character"
             :old-cursor cursor}))
        {:operator 'char-parser
         :parsed? false
         :comment "cannot parse since end of input reached"
         :old-cursor cursor}))))

(extend-type clojure.lang.PersistentHashSet
  IParser
  (parser [this registry]
    (fn set-parser [^String input cursor]
      (if (< cursor (count input))
        (let [input-char (.charAt input (int cursor))
              result (this input-char)]
          (if result
            {:operator 'set-parser
             :parsed? true
             :result result
             :old-cursor cursor
             :new-cursor (inc cursor)}
            {:operator 'set-parser
             :parsed? false
             :input-char input-char
             :candidate this
             :comment "failed to match set"
             :old-cursor cursor}))
        {:operator 'set-parser
         :parsed? false
         :comment "cannot parse since end of input reached"
         :old-cursor cursor}))))

(extend-type java.lang.String
  IParser
  (parser [this registry]
    (fn str-parser [^String input cursor]
      (let [input-size (count input)
            match-size (count this)]
        (if (< cursor input-size)
          (if (.startsWith input this (int cursor))
            {:operator 'str-parser
             :parsed? true
             :result this
             :old-cursor cursor
             :new-cursor (+ match-size
                            cursor)}
            {:operator 'str-parser
             :parsed? false
             :input-section (subs input cursor (min
                                                input-size
                                                (+ cursor
                                                   match-size)))
             :candidate this
             :comment "failed to match string"
             :old-cursor cursor})
          {:operator 'str-parser
           :parsed? false
           :comment "cannot parse since end of input reached"
           :old-cursor cursor})))))

(extend-type clojure.lang.Keyword
  IParser
  (parser [this registry]
    (fn registered-parser [input cursor]
      (let [delegate-parser-pair (find @registry this)]
        (if delegate-parser-pair
          (let [[k delegate-parser] delegate-parser-pair]
            (assoc (delegate-parser input cursor)
                   ::name this))
          (throw (ex-info "parser not found"
                          {:k this
                           :registry @registry})))))))

(extend-type java.lang.Object
  IParser
  (parser [this registry]
    this))

(defn ! [char-string-or-set]
  (condp =
      (type char-string-or-set)
    java.lang.Character
    (fn not-char-parser [^String input cursor]
      (if (< cursor (count input))
        (let [input-char (.charAt input (int cursor))]
          (if (= input-char
                 char-string-or-set)
            {:operator 'not-char-parser
             :parsed? false
             :input-char input-char
             :candidate char-string-or-set
             :comment "failed to NOT match character"
             :old-cursor cursor}
            {:operator 'not-char-parser
             :parsed? true
             :result input-char
             :candidate char-string-or-set
             :old-cursor cursor
             :new-cursor (inc cursor)}))
        {:operator 'not-char-parser
         :parsed? false
         :comment "cannot parse since end of input reached"
         :old-cursor cursor}))
    java.lang.String
    (fn not-str-parser [^String input cursor]
      (let [input-size (count input)
            match-size (count char-string-or-set)]
        (if (< cursor input-size)
          (if (.startsWith input char-string-or-set (int cursor))
            {:operator 'not-str-parser
             :parsed? false
             :input-section (subs input cursor (min
                                                input-size
                                                (+ cursor
                                                   match-size)))
             :candidate char-string-or-set
             :comment "failed to NOT match string"
             :old-cursor cursor}
            (let [new-cursor
                  (min
                   input-size
                   (+ cursor
                      match-size))]
              {:operator 'not-str-parser
               :parsed? true
               :candidate char-string-or-set
               :result (subs input
                             cursor
                             new-cursor)
               :old-cursor cursor
               :new-cursor new-cursor}))
          {:operator 'not-str-parser
           :parsed? false
           :comment "cannot parse since end of input reached"
           :old-cursor cursor})))
    clojure.lang.PersistentHashSet
    (fn not-set-parser [^String input cursor]
      (if (< cursor (count input))
        (let [input-char (.charAt input (int cursor))
              result (char-string-or-set input-char)]
          (if result
            {:operator 'not-set-parser
             :parsed? false
             :input-char input-char
             :candidate char-string-or-set
             :comment "failed to NOT match set"
             :old-cursor cursor}
            {:operator 'not-set-parser
             :parsed? true
             :candidate char-string-or-set
             :result input-char
             :old-cursor cursor
             :new-cursor (inc cursor)}))
        {:operator 'not-set-parser
         :parsed? false
         :comment "cannot parse since end of input reached"
         :old-cursor cursor}))))

(defn lookahead
  [registry p]
  (let [p (parser p registry)]
    (fn lookahead-parser [input cursor]
      (let [{:keys [:parsed? :old-cursor] :as parse-data} (p input cursor)]
        (if parsed?
          (assoc parse-data
                 :new-cursor
                 old-cursor)
          parse-data)))))

(defn star
  ([registry p]
   (star registry p 0 nil))
  ([registry p minimum]
   (star registry p minimum nil))
  ([registry p minimum maximum]
   (let [p (parser p registry)]
     (fn star-parser [input cursor]
       (loop [i 0
              c cursor
              last-parse-data nil
              result []]
         (let [parse-data (p input c)]
           (if (:parsed? parse-data)
             (recur (inc i)
                    (:new-cursor parse-data)
                    parse-data
                    (conj result (:result parse-data)))
             {:operator 'star
              :parsed? (and (>= i minimum)
                            (if maximum
                              (<= i maximum)
                              true))
              :result result
              :count i
              :old-cursor cursor
              :new-cursor c
              :terminate-parse-data parse-data
              :last-accept-parse-data last-parse-data})))))))

(defn chain
  [registry & pairs]
  (let [parser-pairs (mapv (fn [[k raw-p]]
                             (when-not (keyword? k)
                               (throw (ex-info "arguments must be pairs of keywords and parsers"
                                               {:k k
                                                :parser raw-p})))
                             [k (parser raw-p registry)])
                           (partition 2 2 pairs))]
    (fn chain-parser [input cursor]
      (loop [rpairs parser-pairs
             c cursor
             last-parse-data {}
             result {}]
        (if (empty? rpairs)
          {:operator 'chain
           :parsed? true
           :result result
           :old-cursor cursor
           :new-cursor c
           :last-parse-data last-parse-data}
          (let [[k p] (first rpairs)
                {:keys [:parsed?] :as parse-data} (p input c)]
            (if parsed?
              (let [{:keys [new-cursor]
                     p-result :result} parse-data]
                (recur (rest rpairs)
                       new-cursor
                       (assoc last-parse-data
                              k
                              parse-data)
                       (assoc result
                              k
                              p-result)))
              {:operator 'chain
               :parsed? false
               :k k
               :partial-result result
               :old-cursor cursor
               :last-accept-parse-data last-parse-data
               :terminate-parse-data parse-data})))))))

(defn choice
  [registry & pairs]
  (let [parser-pairs (mapv (fn [[k raw-p]]
                             (when-not (keyword? k)
                               (throw (ex-info "arguments must be pairs of keywords and parsers"
                                               {:k k
                                                :parser raw-p})))
                             [k (parser raw-p registry)])
                           (partition 2 2 pairs))]
    (fn choice-parser [input cursor]
      (loop [rpairs parser-pairs
             all-terminate-parse-data {}]
        (if (empty? rpairs)
          {:operator 'choice
           :parsed? false
           :all-terminate-parse-data all-terminate-parse-data
           :old-cursor cursor}
          (let [[k p] (first rpairs)
                {:keys [:parsed?] :as parse-data} (p input cursor)]
            (if parsed?
              (let [{:keys [new-cursor]
                     p-result :result} parse-data]
                {:operator 'choice
                 :parsed? true
                 :k k
                 :result [k p-result]
                 :old-cursor cursor
                 :new-cursor new-cursor
                 :accept-parse-data parse-data})
              (recur (rest rpairs)
                     (assoc all-terminate-parse-data
                            k
                            parse-data)))))))))

(defn m
  "modifies parser to modify result with f if parser is successful"
  [p f]
  (fn mod-parser [input cursor]
    (let [{:keys [:parsed?] :as parse-data} (p input cursor)]
      (if parsed?
        (update parse-data
                :result
                f)
        parse-data))))

(defn register-parser
  [registry k p]
  (swap! registry
         assoc
         k
         p))

(def default-components
  {::eof (fn [input cursor]
           (if (< cursor (count input))
             {:operator ::eof
              :parsed? false
              :old-cursor cursor}
             {:operator ::eof
              :parsed? true
              :result nil
              :old-cursor cursor
              :new-cursor cursor}))})

(defn new-registry []
  (atom default-components))

(defn clean-registry [registry]
  (reset! registry default-components))

(defn defutilities-helper
  [registry-sym]
  `(do
     (def ~registry-sym (new-registry))
     (def ~'get-parser (partial parser ~registry-sym))
     ~@(for [[new-s old-s] [['>? 'lookahead]
                            ['* 'star]
                            ['s 'chain]
                            ['| 'choice]
                            ['defp 'register-parser]
                            ['clean-default-registry 'clean-registry]]]
         `(def ~new-s (partial ~(symbol "dj.algorithms.peg2" (str old-s))
                               ~registry-sym)))))

(defmacro defutil [s]
  (defutilities-helper s))

(defutil default-registry)

(defn annotate-line [{:keys [old-cursor ^String input] :as parse-data}]
  (assoc parse-data
         :line (let [size (min old-cursor
                               (count input))]
                 (loop [n (int 0)
                        newlines (int 1)]
                   (if (< n size)
                     (let [c (.charAt input n)]
                       (recur (inc n)
                              (if (= c \newline)
                                (inc newlines)
                                newlines)))
                     newlines)))))

(defn parse
  "convenience fn, resolves p and run on input in one swoop"
  ([p input]
   (parse p input 0))
  ([p input cursor]
   (-> ((parser p default-registry)
        input
        cursor)
       annotate-line
       (assoc :input input
              :input-size (count input)))))

#_ (do
  (clean-default-registry)
  (defp :s (! "abc"))
  (defp :a (* :s))
  (parse :a "\nabc\n" 3))
