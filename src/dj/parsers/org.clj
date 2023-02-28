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

(defn analyze-table [table-result]
  (let [[header & entries] table-result
        {[entity-key] :body
         header-children :children} header
        ordered-keys (mapv (comp first
                                 :body)
                           header-children)]
    (mapv (fn [{[entity] :body
                :keys [:children]}]
            (loop [entry {entity-key entity}
                   rest-ordered-keys ordered-keys
                   rest-children children]
              (if (empty? rest-children)
                entry
                (let [{[child-content] :body} (first rest-children)]
                  (if (re-find #": " child-content)
                    (let [[k v] (cs/split child-content #": ")]
                      (recur (assoc entry
                                    k v)
                             rest-ordered-keys
                             (rest rest-children)))
                    (recur (assoc entry
                                  (first rest-ordered-keys)
                                  child-content)
                           (rest rest-ordered-keys)
                           (rest rest-children)))))))
          entries)))

(defn read-entries [path]
  (-> (p/parse (p/* (bullet-parser 0 2) 1)
               (slurp path))
      (dissoc :last-parse-data)
      :result
      analyze-table))

;; ----------------------------------------------------------------------

(defn mapify-entries
  [entries k]
  (reduce (fn [ret entry]
            (let [pair (find entry k)]
              (if pair
                (let [[_ entity-key] pair]
                  (assoc ret
                         entity-key
                         entry))
                (throw (ex-info "entity key not found in entry"
                                {:k k
                                 :entry entry})))))
          {}
          entries))

(defn join-by-k [left k & others]
  (reduce (fn [ret right]
            (let [index (mapify-entries right k)]
              (mapv (fn [entry]
                      (let [pair (find entry k)]
                        (if pair
                          (let [[_ entity-key] pair]
                            (merge entry (index entity-key)))
                          (throw (ex-info "entity key not found in entry"
                                {:k k
                                 :entry entry})))))
                    ret)))
          left
          others))

(defn display-table [entries {:keys [title ordered-keys width height] :or {title "a window"
                                                                           width 800
                                                                           height 600}}]
  (let [ordered-keys (or ordered-keys
                         (keys (first entries)))
        f (javax.swing.JFrame. ^String title)
        table (javax.swing.JTable. (java.util.Vector. ^java.util.Collection
                                                      (mapv (fn [entry]
                                                              (java.util.Vector. ^java.util.Collection (mapv (fn [k]
                                                                                                               (let [[the-k v :as e] (find entry k)]
                                                                                                                 (if e
                                                                                                                   v
                                                                                                                   (throw (ex-info "key not found"
                                                                                                                                   {:k the-k})))))
                                                                                                             ordered-keys)))
                                                            entries))
                                   (java.util.Vector. ^java.util.Collection ordered-keys))]

    (doto (-> f
              (.getContentPane))
      (.add (javax.swing.JScrollPane. table)))
    (doto f
      (.setSize (int width) (int height))
      #_ (.pack)
      (.setDefaultCloseOperation javax.swing.JFrame/DISPOSE_ON_CLOSE)
      (.setVisible true))))
