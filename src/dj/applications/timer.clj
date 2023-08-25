(ns dj.applications.timer
  (:require [dj.durable :as dd]
            [dj.template.sgml :as dts]
            [dj.template.css :as dtc]
            [clojure.string :as cstr])
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

(defn to-pst-datetime
  "java.util.Date -> java.time.datetime"
  [^java.util.Date date]
  (-> date
      (.toInstant)
      (.atZone (java.time.ZoneId/of "America/Los_Angeles"))
      (.toLocalDateTime)))

(let [thread-local-utc-date-format (proxy [ThreadLocal] []
                                     (initialValue []
                                       (doto (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSS-00:00")
                                         ;; RFC3339 says to use -00:00 when the timezone is unknown (+00:00 implies a known GMT)
                                         (.setTimeZone (java.util.TimeZone/getTimeZone "GMT")))))]
  (defn str-date [^java.util.Date date]
    (let [^java.text.DateFormat utc-format (.get thread-local-utc-date-format)]
      (.format utc-format date))))

(defn to-date
  [^java.time.LocalDateTime datetime]
  (-> datetime
      (.atZone (java.time.ZoneId/of "America/Los_Angeles"))
      (.toInstant)
      java.util.Date/from))
#_ (-> "2023-03-24T08:45:00"
    java.time.LocalDateTime/parse
    to-date
    to-datetime)

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
  #_ #{"baby care"
       "baby playing"
       "break"
       "chores"
       "cleaning"
       "coding"
       "cooking"
       "driving"
       "eating"
       "evening routine"
       "exercise"
       "help guests"
       "household work"
       "learning"
       "morning routine"
       "organizing"
       "planning"
       "pre-travel"
       "shopping"
       "sleeping"
       "unlabeled"
       "weight training"
       "work"}
  ;; Lan's categories
  #{"baby care"
    "baby playing"
    "break"
    "chores"
    "cooking"
    "eating"
    "evening routine"
    "exercise"
    "household work"
    "learning"
    "morning routine"
    "organizing"
    "physical therapy"
    "planning"
    ; "resting"
    "shopping"
    "sleeping"
    "travel"
    "unlabeled"
    "work"})

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
                                                   :or {local->system-hour-shift 1}}]
  (let [^java.time.LocalDateTime start-localdatetime 
        (if (string? start-localdatetime)
          (java.time.LocalDateTime/parse start-localdatetime)
          start-localdatetime)
        ^java.time.LocalDateTime start-localdatetime (.plusHours start-localdatetime
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
        ;splits-frame (JFrame. "Splits")
        ;splits-text (JTextArea.)
        #_ #_ splits-panel (doto (JPanel.)
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
    #_ (.setText splits-text
              ^String
              (split-summary @store))
    #_ (add-watch store
               :splits-frame
               (fn [key ref old new-data]
                 (.setText splits-text
                           ^String
                           (split-summary new-data))))
    #_ (doto splits-frame
      (.add splits-panel)
      (.addWindowListener (proxy [java.awt.event.WindowAdapter] []
                            (windowClosing [e]
                              (println "closing splits")
                              (remove-watch store :splits-frame))))
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setSize (int 400) (int 600))
      (.setIconImage (.getImage icon))
      (.setVisible true))))

(defn start-localdatetime [date]
  (let [dt ^java.time.LocalDateTime (to-datetime date)]
    (-> dt
        (.plusHours -1) ;; laptop -> actual local time
        (.toLocalDate)
        (.atTime java.time.LocalTime/MIDNIGHT))))

(defn add-now-unlabeled
  "for allowing analysis to take into account current running task"
  [store-value]
  (assoc store-value
         (java.util.Date.)
         {:category "unlabeled"}))

(defn read-post [txt]
  (reduce (fn [m entry]
            (let [[k v] (cstr/split entry #"=")]
              (assoc m k v)))
          {}
          (cstr/split txt #"&")))

(defn past-entries [db n]
  (let [sorted-times (take-last n (sort (keys db)))]
    (map (fn [time]
           [time
            (db time)])
         sorted-times)))

(defn filter-ignored [m]
  (reduce-kv (fn [ret k {i? :ignore? :as v}]
               (if i?
                 ret
                 (assoc ret
                       k
                       v)))
             {}
             m))

(defn make-request-handler [store
                            store-writer]
  (let [record-split (fn record-split [m]
                       (dd/send-off-kv! store
                                        store-writer
                                        (java.util.Date.)
                                        m))
        record-manual-split
        (fn record-manual-split [date m]
          (dd/send-off-kv! store
                           store-writer
                           date
                           m))]
    (fn request-handler [req]
      (let [{:keys [uri]} req
            [_ op] (cstr/split uri #"/" 4)
            now-store @store
            latest-date (latest-time now-store)
            latest-entry (now-store latest-date)
            latest-datetime (to-datetime latest-date)
            body-kv (when (:body req)
                      (-> req
                          :body
                          slurp
                          java.net.URLDecoder/decode
                          read-post))]
        (case op
          "ignore"
          (let [date (clojure.instant/read-instant-date (first (keys body-kv)))
                {i? :ignore? :as entry} (get now-store date)
                new-entry (assoc entry
                                 :ignore? (not i?))]
            (record-manual-split
             date
             new-entry)
            {:status 302
             :headers {"Location" "/"}})
          "manual"
          (let [{:strs [category datetime]} body-kv]
            (prn {:kv body-kv
                  :dt (java.time.LocalDateTime/parse datetime)
                  :submit (record-manual-split (to-date
                                                (java.time.LocalDateTime/parse datetime))
                                               {:category category})})
            {:status 302
             :headers {"Location" "/"}})
          "submit"
          (let [prev-category (when body-kv
                                (get body-kv "category"))]
            (when (and prev-category
                       (not= prev-category
                             (:category latest-entry)))
              (record-split {:category prev-category}))
            {:status 302
             :headers {"Location" "/"}})
          {:status 200
           :headers {"Content-Type" "text/html; charset=utf-8"}
           :body (dts/emit [[:!DOCTYPE "html"]
                            [:html {:lang "en"}
                             (into [:head {}
                                    [:meta {:charset "utf-8"}]
                                    [:title {} "Stopwatch"]
                                    [:link {:rel "icon"
                                            :href "data:;base64,iVBORw0KGgo="}]
                                    [:style {:type "text/css"}
                                     "img:hover {position: relative; top: -10px; cursor: pointer;}
.selected {width: 80px;}
button {height: 50px; margin: 5px 5px 5px 5px; padding: 5px 5px 5px 5px;}
button:hover {cursor: pointer;}
.public-log {width: 20%; border-style: dotted; margin: 5px 5px 5px 5px; padding: 5px 5px 5px 5px; float: right; overflow-y: scroll; height: 300px;}"]])
                             [:body {}
                              `[:div {:id "app" :style ~(dtc/->style {:float "left"})}
                                [:form {:action "/submit"
                                        :method "post"}
                                 ~@(for [x [(str "latest: " latest-datetime)
                                            (str "category: " (:category latest-entry))
                                            (str "duration: " (-> (duration-between (java.util.Date.)
                                                                                    latest-date)
                                                                  duration-str))]]
                                     [:div {}
                                      x])
                                 ~@(for [category (sort default-categories)]
                                     [:div {}
                                      [:input {:type "submit"
                                               :style (dtc/->style {:width "300px"
                                                                    :height "50px"})
                                               :name "category"
                                               :value category}]])]]
                              [:div {:style (dtc/->style {:float "left"})}
                               (let [p (in-breakdown-predicate (start-localdatetime
                                                                (java.util.Date.))
                                                               {:days 1})]
                                 (->> now-store
                                      filter-ignored
                                      add-now-unlabeled
                                      analyze-splits
                                      (filter (fn [{:keys [start end]}]
                                                (or (p start)
                                                    #_ (and end
                                                            (p end)))))
                                      aggregate-analysis
                                      analysis->plot-data
                                      plot-data->percentage-chart))]
                              [:div {:style (dtc/->style {:float "left"})}
                               [:form {:action "/manual"
                                       :method "post"}
                                [:label {:for "datetime"} "New datetime: "]
                                [:input {:type "datetime-local"
                                         :name "datetime"}]
                                (into [:select {:name "category"}]
                                      (map (fn [category]
                                             [:option {:value category}
                                              category])
                                           (sort default-categories)))
                                [:input {:type "submit"
                                         :name "submit"
                                         :value "submit manual"}]]
                               (-> [:form {:action "/ignore"
                                           :method "post"}
                                    [:label {} "Toggle ignore past 15"]]
                                   (into (map (fn [[date {c :category
                                                          i? :ignore?}]]
                                                [:div {}
                                                 [:label {} (str (to-pst-datetime date)
                                                                 " "
                                                                 c
                                                                 " ")]
                                                 [:input {:type "submit"
                                                          :style (dtc/->style {:height "50px"})
                                                          :name (str-date date)
                                                          :value (if i?
                                                                   "REVEAL"
                                                                   "IGNORE")}]])
                                              (past-entries now-store
                                                            15))))]]]])})))))

(defn logger [path]
  (fn log [content]
    (spit path
          (prn-str (into [(java.util.Date.)] content)) :append true)))

(defn make-server-handler [log request-handler]
  (fn server-handler [req]
    (try
      (let [resp (request-handler req)]
        (log [req resp])
        resp)
      (catch Exception e
        (log [req e])
        (throw e)))))
