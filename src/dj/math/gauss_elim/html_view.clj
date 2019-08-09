(ns dj.math.gauss-elim.html-view
  (:require [dj.template.sgml :as sgml]
            [dj.template.css :as css]
            [clojure.set :as cs]))

(defn page
  "content: list of html data"
  [content]
  [[:!DOCTYPE "html"]
   [:html {:lang "en"}
    [:head nil
     [:meta {:charset "UTF-8"}]
     [:style {}
      (css/->css [css/reset
                  {"table, th, td" {:border "1px solid black"}}])]]
    (into [:body {}]
          content)]])

;;; Data structures to handle
;; 1. num-rows, easy
;; 2. [X] row-id-order, table (two columns), subset, row-id->original-id
;; 3. [X] col-id->row-ids, table (col-id per column, row-ids per row), subset
;; 4. [X] original-rows, table, any row you want
;; 5. [X] current-rows, rectangular subset of full sparse matrix, if you have a row or col, you get a col and row, (add '...' for inbetween?)

(defn out-row-id-order [row-id-order start end]
  [[:div {} "Row ID -> Original Row ID"]
   (-> [:table {}]
       (conj [:tr {}
              [:th {} "RID"]
              [:th {} "ORID"]])
       (into (map
              (fn [rid orid]
                [:tr {}
                 [:td {} rid]
                 [:td {} orid]])
              (range start end)
              (subvec row-id-order start end))))])

(defn out-col-id->row-ids [col-id->row-ids start end]
  (let [sv (subvec col-id->row-ids start end)
        cols-used (range start end)
        rows-used (sort (apply cs/union sv))]
    [[:div {} "Col ID -> Non-zero Row IDs"]
     (-> [:table {}]
         (conj (into [:tr {}]
                     (map (fn [n]
                            [:th {} n]))
                     (range start end)))
         (into (map (fn [rid]
                      (into [:tr {}]
                            (map (fn [cid]
                                   (let [entry (-> (sv (- cid start))
                                                   (get rid))]
                                     (if entry
                                       [:td {} entry]
                                       [:td {}]))))
                            cols-used)))
               rows-used))]))

(defn out-original-rows [original-rows orow-ids]
  (let [selected-rows (for [orid orow-ids]
                        (get original-rows orid))
        all-col-ids (sort
                     (apply cs/union (map (comp set
                                                keys) selected-rows)))]
    [[:div {} "Original Rows (Row-ids->map(col-ids->entries))"]
     (-> [:table {}]
         (conj (into [:tr {} [:th {}]]
                     (map (fn [cid]
                            [:th {} cid])
                          all-col-ids)))
         (into
          (map-indexed
           (fn [orid row]
             (into [:tr {} [:th {} orid]]
                   (map (fn [cid]
                          (let [entry (get row cid)]
                            (if entry
                              [:td {} entry]
                              [:td {}]))))
                   all-col-ids)))
          selected-rows))]))

(defn out-current-rows [original-rows row-id-order start-rid end-rid]
  (let [row-ids (range start-rid end-rid)
        selected-rows (for [rid row-ids]
                        (get original-rows (row-id-order rid)))
        all-col-ids (sort
                     (apply cs/union (map (comp set
                                                keys) selected-rows)))]
    [[:div {} "Current Rows (Row-ids->map(col-ids->entries))"]
     (-> [:table {}]
         (conj (into [:tr {} [:th {}]]
                     (map (fn [cid]
                            [:th {} cid])
                          all-col-ids)))
         (into
          (map
           (fn [rid row]
             (into [:tr {} [:th {} rid]]
                   (map (fn [cid]
                          (let [entry (get row cid)]
                            (if entry
                              [:td {} entry]
                              [:td {}]))))
                   all-col-ids))
           row-ids
           selected-rows)))]))

(comment

  (let [rio [1 3 5]
        cid->rids [#{0 1 2} #{0 2 3} #{1 2 4}]
        oor [{0 2, 1 1, 2 -1, 3 8} {0 -3, 1 -1, 2 2, 3 -11} {0 -2, 2 2, 3 -3}]]
    (-> ["Test GE Web Viewer"]
        (into (out-row-id-order rio 0 2))
        (into (out-col-id->row-ids cid->rids 0 3))
        (into (out-original-rows oor [0 1 2]))
        (into (out-current-rows oor [0 2 1]
                                0 3))
        page
        sgml/emit
        (->> (dj.io/poop (dj.io/file "/home/username/tmp/test.html")))))

  )
