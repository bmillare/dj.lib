(ns dj.org
  (:require [dj.algorithms.peg :as p]
            [clojure.pprint :as pp]
            [clojure.java.shell :as sh]))

;; for performance and simplicity, will need to separate typing and grouping
(defn parse-lines [txt path]
  (let [header:prefix (p/alt (p/t #"\*+ ")
                             (fn [s]
                               {:type :header
                                :indent (count s)}))
        bullet-or-body:prefix (p/alt (p/s (p/t #" *") (p/? (p/t #"- ")))
                                     (fn [[prefix:space bullet]]
                                       {:indent (count prefix:space)
                                        :type (if bullet
                                                :bullet
                                                :body)}))
        node:content (p/alt (p/t #"[^\n]*")
                            (let [num (p/alt (p/t #"\d+")
                                             #(Integer/parseInt %))
                                  dash (p/t #"-")
                                  space (p/t #" ")
                                  date-fn (fn [open close]
                                            (p/| (p/alt (p/s open
                                                             num
                                                             dash
                                                             num
                                                             dash
                                                             num
                                                             space
                                                             (p/t #"\w+")
                                                             space
                                                             num
                                                             (p/t #":")
                                                             num
                                                             close)
                                                        (fn [[_ year _ month _ day _ weekday _ hour _ minute]]
                                                          {:year year
                                                           :month month
                                                           :day day
                                                           :weekday weekday
                                                           :hour hour
                                                           :minute minute}))
                                                 (p/alt (p/s open
                                                             num
                                                             dash
                                                             num
                                                             dash
                                                             num
                                                             space
                                                             (p/t #"\w+")
                                                             close)
                                                        (fn [[_ year _ month _ day _ weekday]]
                                                          {:year year
                                                           :month month
                                                           :day day
                                                           :weekday weekday}))))]
                              (fn [s]
                                (let [tags (mapv (fn [tag-txt]
                                                   (:result (p/parse (p/?
                                                                      (p/alt (p/s (p/t #"[^ \t]+")
                                                                                  (p/t #"\s+")
                                                                                  (p/| (date-fn (p/t #"<")
                                                                                                (p/t #">"))
                                                                                       (date-fn (p/t #"\[")
                                                                                                (p/t #"\]"))
                                                                                       (p/alt (p/t #"\"(?:\\\"|[^\"])*?\"")
                                                                                              (fn [s]
                                                                                                (subs s 1 (dec (count s)))))
                                                                                       (p/alt (p/t #":\S+")
                                                                                              (fn [s]
                                                                                                (keyword (subs s 1))))
                                                                                       (p/t #"\S+")))
                                                                             (fn [[name _ v]]
                                                                               [name v])))
                                                                         tag-txt)))
                                                 (rest (clojure.string/split s #"#")))]
                                  (if (empty? tags)
                                    {:text s}
                                    {:text s
                                     :tags tags})))))
        node (p/alt (p/s (p/!? (p/t #"\z"))
                         (p/| header:prefix bullet-or-body:prefix) node:content (p/? (p/t #"\n")))
                    (fn [[_ prefix content _]]
                      (merge prefix
                             content
                             {:file path})))]
    (:result (p/parse (p/* node) txt))))

(defn hierarchy [lines]
  (let [max-indent (apply max (map :indent lines))]
    (loop [rem-lines lines
           free-id 1
           nodes {0 {:type :header
                     :indent -1}}
           children {0 []}
           ;; level->id
           active-nodes {-1 0}
           ;; active-levels
           active-level (sorted-set -1)]
      (let [line (first rem-lines)]
        (if line
          ;; add to most recent active elder
          (let [preindent (:indent line)
                indent (+ preindent (case (:type line)
                                      :header 0
                                      :bullet max-indent
                                      :body max-indent))
                recent-active-elder-id (active-nodes (first (filter #(< % indent) (rseq active-level))))
                inactive (drop-while #(< % indent) active-level)]
            (recur (rest rem-lines)
                   (inc free-id)
                   (assoc nodes
                          free-id
                          line)
                   (update children recent-active-elder-id (fn [existing]
                                                             (conj (or existing [])
                                                                   free-id)))
                   (-> (apply dissoc active-nodes inactive)
                       (assoc indent free-id))
                   (-> (apply disj active-level inactive)
                       (conj indent))))
          {:nodes nodes
           :children children})))))

(defn inline [{:keys [children nodes]}]
  (:children
   ((fn substitute [node]
      (if-let [children' (children node)]
        (assoc (nodes node)
               :children (mapv substitute (children node)))
        (nodes node)))
    0)))

(defn special-tags
  "
special tags are the first of a non-body

"
  [{:keys [children nodes]}]
  (let [body-type? #(= :body (:type %))]
    (reduce-kv (fn [ret k {:keys [type tags file]}]
                 (if (= :body type)
                   ret
                   (let [[tag description] (first tags)]
                     (if tag
                       (conj ret
                             [tag
                              description
                              (into (vec (rest tags))
                                    (comp
                                     (map nodes)
                                     (filter body-type?)
                                     (mapcat :tags))
                                    (children k))
                              file])
                       ret))))
               []
               ;; special tasks happen on non-bodies only
               nodes)))

(def file-ignore 34)

(defn display [tags-special filter-fn]
  (pp/print-table [:tag-type :description :status :date :file]
                  (sort-by (fn [{:keys [status date]}]
                             [status date])
                           (for [[tag-type description log file] tags-special
                                 :let [child-tags (reduce (fn [ret tag]
                                                            (let [[tag-name v] tag]
                                                              (update ret
                                                                      (case tag-name
                                                                        "create" :status
                                                                        "start" :status
                                                                        "cancel" :status
                                                                        "pause" :status
                                                                        "waiting" :status
                                                                        "resume" :status
                                                                        "mark" :status
                                                                        "finish" :status
                                                                        "begin" :event
                                                                        "hide-until" :hide
                                                                        "focus" :focus
                                                                        :unhandled)
                                                                      #(conj (or %
                                                                                 [])
                                                                             tag))))
                                                          {}
                                                          log)
                                       [latest-status {:keys [year month day weekday hour minute]}] (case tag-type
                                                                                                      "EVENT" (peek (:event child-tags))
                                                                                                      "TASK" (peek (:status child-tags))
                                                                                                      [])]
                                 :when (and (Character/isUpperCase (first tag-type))
                                            (filter-fn tag-type child-tags))]
                             {:tag-type tag-type
                              :description description
                              :status latest-status
                              :date (if (and year month day)
                                      (str year "-" (format "%02d" month) "-" (format "%02d" day) " " weekday
                                           (if (and hour minute)
                                             (format " %02d:%02d" hour minute)
                                             ""))
                                      "none")
                              :file (subs file
                                          file-ignore)}))))

;; ----------------------------------------------------------------------

(defn view [& [filter-type arg1]]
  (let [org-file-paths (-> (sh/sh "find" "/home/username/dj/usr/store/notes/" "-iname" "*.org")
                           :out
                           (clojure.string/split-lines))
        tags-special (-> (apply concat
                                (for [p org-file-paths]
                                  (-> (dj.io/file p)
                                      (dj.io/eat)
                                      (parse-lines p))))
                         hierarchy
                         special-tags)]
    
    (case filter-type
      :running (display tags-special
                        (fn [tag-type child-tags]
                          (let [now (java.util.GregorianCalendar.)]
                            (case tag-type
                              "TASK" (and (#{"create" "start" "resume"} (first (peek (:status child-tags))))
                                          (let [{:keys [year month day hour minute]} (second (peek (:hide child-tags)))]
                                            (if year
                                              (.after now
                                                      (if (and hour minute)
                                                        (java.util.GregorianCalendar. year (dec month) day hour minute)
                                                        (java.util.GregorianCalendar. year (dec month) day)))
                                              true)))
                              "EVENT" (and (let [{:keys [year month day hour minute]} (second (peek (:event child-tags)))]
                                             (if year
                                               (.before now
                                                        (if (and hour minute)
                                                          (java.util.GregorianCalendar. year (dec month) day hour minute)
                                                          (java.util.GregorianCalendar. year (dec month) day)))
                                               true))
                                           (let [{:keys [year month day hour minute]} (second (peek (:hide child-tags)))]
                                             (if year
                                               (.after now
                                                       (if (and hour minute)
                                                         (java.util.GregorianCalendar. year (dec month) day hour minute)
                                                         (java.util.GregorianCalendar. year (dec month) day)))
                                               true)))
                              false))))
      :focus (pp/print-table [:tag-type :focus :description :status :date :file]
                             (sort-by (fn [{:keys [status date]}]
                                        [status date])
                                      (for [[tag-type description log file] tags-special
                                            :let [child-tags (reduce (fn [ret tag]
                                                                       (let [[tag-name v] tag]
                                                                         (update ret
                                                                                 (case tag-name
                                                                                   "create" :status
                                                                                   "start" :status
                                                                                   "cancel" :status
                                                                                   "pause" :status
                                                                                   "waiting" :status
                                                                                   "resume" :status
                                                                                   "mark" :status
                                                                                   "finish" :status
                                                                                   "begin" :event
                                                                                   "hide-until" :hide
                                                                                   "focus" :focus
                                                                                   :unhandled)
                                                                                 #(conj (or %
                                                                                            [])
                                                                                        tag))))
                                                                     {}
                                                                     log)
                                                  [latest-status {:keys [year month day weekday hour minute]}] (case tag-type
                                                                                                                 "TASK" (peek (:status child-tags))
                                                                                                                 [])
                                                  [_ focus-status] (peek (:focus child-tags))]
                                            :when (and (Character/isUpperCase (first tag-type))
                                                       (let [now (java.util.GregorianCalendar.)]
                                                         (case tag-type
                                                           "TASK" (and (#{"create" "start" "resume"} (first (peek (:status child-tags))))
                                                                       focus-status)
                                                           false)))]
                                        {:tag-type tag-type
                                         :focus focus-status
                                         :description description
                                         :status latest-status
                                         :date (if (and year month day)
                                                 (str year "-" (format "%02d" month) "-" (format "%02d" day) " " weekday
                                                      (if (and hour minute)
                                                        (format " %02d:%02d" hour minute)
                                                        ""))
                                                 "none")
                                         :file (subs file
                                                     file-ignore)})))
      :deadline (pp/print-table [:tag-type :focus :description :status :deadline :file]
                                (sort-by (fn [{:keys [tag-type deadline]}]
                                           [deadline tag-type])
                                         (for [[tag-type description log file] tags-special
                                               :let [child-tags (reduce (fn [ret tag]
                                                                          (let [[tag-name v] tag]
                                                                            (update ret
                                                                                    (case tag-name
                                                                                      "create" :status
                                                                                      "start" :status
                                                                                      "cancel" :status
                                                                                      "pause" :status
                                                                                      "waiting" :status
                                                                                      "resume" :status
                                                                                      "mark" :status
                                                                                      "finish" :status
                                                                                      "begin" :event
                                                                                      "hide-until" :hide
                                                                                      "focus" :focus
                                                                                      "target" :target
                                                                                      :unhandled)
                                                                                    #(conj (or %
                                                                                               [])
                                                                                           tag))))
                                                                        {}
                                                                        log)
                                                     [latest-status _] (case tag-type
                                                                         "TASK" (peek (:status child-tags))
                                                                         [])
                                                     [_ focus-status] (peek (:focus child-tags))
                                                     [_ target] (peek (:target child-tags))
                                                     begin-time (second (peek (:event child-tags)))
                                                     {:keys [year month day weekday hour minute]} (or target begin-time nil)
                                                     check-date (if year
                                                                  (if (and hour minute)
                                                                    (java.util.GregorianCalendar. year (dec month) day hour minute)
                                                                    (java.util.GregorianCalendar. year (dec month) day 23 49))
                                                                  nil)]
                                               :when (and (Character/isUpperCase (first tag-type))
                                                          (let [now (java.util.GregorianCalendar.)]
                                                            (and check-date
                                                                 (case tag-type
                                                                   "TASK" (#{"create" "start" "resume"} (first (peek (:status child-tags))))
                                                                   "EVENT" (.before now
                                                                                    check-date)
                                                                   true
                                                                   false)
                                                                 (if arg1
                                                                   (.after (case arg1
                                                                             :day (doto now
                                                                                    (.add java.util.Calendar/DAY_OF_MONTH 2))
                                                                             :week (doto now
                                                                                     (.add java.util.Calendar/DAY_OF_MONTH 7))
                                                                             :month (doto now
                                                                                      (.add java.util.Calendar/MONTH 1))
                                                                             :year (doto now
                                                                                     (.add java.util.Calendar/YEAR 1)))
                                                                           check-date)
                                                                   true))))]
                                           {:tag-type tag-type
                                            :focus focus-status
                                            :description description
                                            :status latest-status
                                            :deadline (if (and year month day)
                                                        (str year "-" (format "%02d" month) "-" (format "%02d" day) " " weekday
                                                             (if (and hour minute)
                                                               (format " %02d:%02d" hour minute)
                                                               ""))
                                                        "none")
                                            :file (subs file
                                                        file-ignore)})))
      (display tags-special (constantly true)))))
