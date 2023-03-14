(ns dj.applications.timer
  (:require [dj.durable :as dd]
            [dj.template.css :as dtc])
  (:import [javax.swing JFrame JPanel JLabel JButton Timer JTextArea]
           [java.awt.event ActionListener]))
(set! *warn-on-reflection* true)

(defn to-datetime
  "java.util.Date -> java.time.datetime"
  [^java.util.Date date]
  (-> date
      (.toInstant)
      (.atZone (java.time.ZoneId/systemDefault))
      (.toLocalDateTime)))

(defn duration-breakdown
  "create data entry of breakdown from java.time.duration"
  [^java.time.Duration duration]
  {:seconds (mod (Math/abs (.getSeconds duration))
                 60)
   :minutes (mod (Math/abs (.toMinutes duration))
                 60)
   :hours (mod (Math/abs (.toHours duration))
               24)})

(defn duration-between
  "converts java.util.Date into datetimes and then creates entry with duration and breakdown"
  [d1 d2]
  (let [duration (java.time.Duration/between (to-datetime
                                              d1)
                                             (to-datetime
                                              d2))]
    (assoc (duration-breakdown duration)
           :duration duration)))

(defn duration-str
  "creates str time format from hours, minutes, seconds entry"
  [{:keys [hours minutes seconds]}]
  (str (format "%02d" hours) ":"
       (format "%02d" minutes) ":"
       (format "%02d" seconds)))

(defn update-time!
  "updates JLabel with current time"
  [^JLabel label start-datetime]
  (let [current-datetime (java.util.Date.)]
    (.setText label
              (-> (duration-between current-datetime
                                    start-datetime)
                  duration-str))))

(defn deduce-categories
  "given db as a map, collect set of user defined categories"
  [m]
  (reduce-kv (fn [ret k v]
               (let [{:keys [category]} v]
                 (conj ret
                       category)))
             #{}
             m))

(def default-categories
  #{"unlabeled"
    "break"
    "coding"
    "help guests"
    "cooking"
    "chores"
    "exercise"
    "weight training"
    "shopping"
    "eating"
    "sleeping"
    "organizing"
    "morning routine"
    "evening routine"
    "baby care"
    "baby playing"
    "learning"
    "cleaning"
    "driving"
    "pre-travel"
    "planning"})

(defn latest-time
  "given db as a map, O(n) determine most recent entry via java.util.Date"
  [m]
  (if (empty? m)
    (java.util.Date.)
    (reduce-kv (fn [best k v]
                 (if (.after ^java.util.Date best
                             k)
                   best
                   k))
               (first (keys m))
               m)))

(defn analyze-splits
  "returns entries with categories, durations, and h:m:s breakdown"
  [splits->data]
  (let [sorted-times (sort (keys splits->data))]
    (map (fn [[t0 t1]]
           (let [data (splits->data t0)]
             (merge data
                    (duration-between t0 t1)
                    {:start t0
                     :end t1})))
         (partition 2 1 sorted-times))))

(defn aggregate-analysis
  "aggregates analysis entries by category"
  [analysis]
  (reduce (fn [m {:keys [duration
                         category]}]
            (assoc m
                   category
                   (if-let [prev-duration (get m category)]
                     (.plus ^java.time.Duration prev-duration
                            ^java.time.Duration duration)
                     duration)))
          {}
          analysis))

(defn analysis->plot-data
  [ag-analysis]
  (sort-by second
           (map (fn [[category ^java.time.Duration duration]]
                  [category (.getSeconds duration)])
                ag-analysis)))

(defn random-color []
  (let [digits ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "a" "b" "c" "d" "e" "f"]]
    (str "#"
         (rand-nth digits)
         (rand-nth digits)
         (rand-nth digits))))

(defn plot-data->percentage-chart
  [plot-data]
  (let [total-seconds (reduce (fn [total [_ seconds]]
                                (+ total seconds))
                              0
                              plot-data)
        bar-height (fn bar-height [seconds]
                     (str (long
                           (* 1000
                              (float (/ seconds total-seconds))))
                          "px"))]
    `[:div {:style "width: 400px;background-color: #aaa"}
      ~@(map (fn [[category seconds]]
               (let [height (bar-height seconds)]
                 [:div {:style (dtc/->style {"text-align" "center"
                                             "vertical-align" "middle"
                                             "line-height" height
                                             "height" height
                                             "background-color" (random-color)
                                             "border-style" "solid"
                                             "border-width" "1px"})}
                  (str category
                       ": "
                       (-> seconds
                           (java.time.Duration/ofSeconds)
                           duration-breakdown
                           duration-str))]))
             plot-data)]))

(defn split-summary
  "given db, runs analysis and aggregate analysis, creates txt summary
  from this"
  [new-data]
  (let [split-analysis (analyze-splits new-data)]
    (str "partial log\n"
         (clojure.string/join "\n"
                              (take 10
                                    (map (fn [{:keys [category]
                                               :as entry}]
                                           (str category ": "
                                                (duration-str entry)))
                                         (reverse split-analysis))))
         "\n\nsummary\n"
         (clojure.string/join "\n"
                              (->> (aggregate-analysis split-analysis)
                                   (sort-by second)
                                   (map (fn [[category duration]]
                                          (str category ": "
                                               (-> duration
                                               duration-breakdown
                                               duration-str)))))))))

(defn in-days-predicate [start-localdatetime days]
  (let [^java.time.LocalDateTime start-localdatetime (if (string? start-localdatetime)
                                                       (java.time.LocalDateTime/parse start-localdatetime)
                                                       start-localdatetime)
        ^java.time.LocalDateTime end-localdatetime (.plusDays start-localdatetime
                                                              (long days))]
    (fn in-days? [date]
      (let [^java.time.LocalDateTime date (to-datetime date)]
        (and (.isAfter date
                     start-localdatetime)
             (.isBefore date
                        end-localdatetime))))))

(defn in-breakdown-predicate [start-localdatetime {:keys [days hours minutes seconds local->system-hour-shift]
                                                   :or {local->system-hour-shift 3}}]
  (let [^java.time.LocalDateTime start-localdatetime (.plusHours ^java.time.LocalDateTime
                                                                 (if (string? start-localdatetime)
                                                                   (java.time.LocalDateTime/parse start-localdatetime)
                                                                   start-localdatetime)
                                                                 local->system-hour-shift)
        ^java.time.LocalDateTime end-localdatetime (cond-> start-localdatetime
                                                     days
                                                     (.plusDays (long days))
                                                     hours
                                                     (.plusHours (long hours))
                                                     minutes
                                                     (.plusMinutes (long minutes))
                                                     seconds
                                                     (.plusSeconds (long seconds)))]
    (fn in-days? [date]
      (let [^java.time.LocalDateTime date (to-datetime date)]
        (and (.isAfter date
                     start-localdatetime)
             (.isBefore date
                      end-localdatetime))))))

(defn create-stopwatch [path]
  (let [store (dd/kv-agent (dd/read-kv-log-file path))
        store-writer (dd/kv-writer path)
        record-split (fn record-split [m]
                       (dd/send-off-kv! store
                                        store-writer
                                        (java.util.Date.)
                                        m))
        categories (-> (clojure.set/union default-categories
                                          (deduce-categories @store))
                       sort
                       vec)
        frame (JFrame. "Timer")
        panel (doto (JPanel.)
                (.setLayout (java.awt.GridLayout. (+ 2
                                                     (count categories))
                                                  1)))
        timer (Timer. 1000 nil)
        current-category-label (let [initial-store @store
                                     current-time (latest-time initial-store)
                                     v (@store
                                        current-time)]
                                 (JLabel. ^String (get v
                                                       :category
                                                       "unlabeled")
                                          javax.swing.SwingConstants/CENTER))
        split-buttons (map (fn [^String category]
                             (doto (JButton. category)
                               (.addActionListener
                                (proxy [ActionListener] []
                                  (actionPerformed [_]
                                    (.setText current-category-label
                                              category)
                                    (record-split {:category category})
                                    (.restart timer))))))
                           categories)
        time-label (doto (JLabel. "00:00:00"
                                  javax.swing.SwingConstants/CENTER)
                     (.setFont (java.awt.Font. "Monospaced" java.awt.Font/PLAIN 24)))
        image-url (-> (dj.io/file "/home/bmillare/Pictures/icons/timer.png")
                      (.toURI)
                      (.toURL))
        icon (javax.swing.ImageIcon. image-url)
        splits-frame (JFrame. "Splits")
        splits-text (JTextArea.)
        splits-panel (doto (JPanel.)
                       (.setLayout (java.awt.FlowLayout.))
                       (.add splits-text))]
    (doto timer
      (.addActionListener
        (proxy [ActionListener] []
          (actionPerformed [_]
            (update-time! time-label (latest-time @store))))))
    (update-time! time-label (latest-time @store))
    (.start timer)
    (doto panel
      (.add current-category-label)
      (.add time-label))
    (doseq [button split-buttons]
      (.add panel ^JButton button))
    (doto frame
      (.add panel)
      (.addWindowListener (proxy [java.awt.event.WindowAdapter] []
                            (windowClosing [e]
                              (println "closing timer")
                              (.close ^java.io.Writer store-writer))))
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setSize (int 400) (int 600))
      (.setIconImage (.getImage icon))
      (.setVisible true))
    (.setText splits-text
              ^String
              (split-summary @store))
    (add-watch store
               :splits-frame
               (fn [key ref old new-data]
                 (.setText splits-text
                           ^String
                           (split-summary new-data))))
    (doto splits-frame
      (.add splits-panel)
      (.addWindowListener (proxy [java.awt.event.WindowAdapter] []
                            (windowClosing [e]
                              (println "closing splits")
                              (remove-watch store :splits-frame))))
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setSize (int 400) (int 600))
      (.setIconImage (.getImage icon))
      (.setVisible true))))
