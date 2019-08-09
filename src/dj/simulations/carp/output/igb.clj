(ns dj.simulations.carp.output.igb
  (:require [dj.io]
            [dj.algorithms.peg :as ap]
            [dj.dispatch.treefn]))

;; channel is passed, when does it get closed?
(defn get-igb-header [channel]
  (let [header-byte-size 1024
        buf (.map ^java.nio.channels.FileChannel channel
		  java.nio.channels.FileChannel$MapMode/READ_ONLY
		  0
		  header-byte-size)
	ba (byte-array header-byte-size)]
    (.get ^java.nio.MappedByteBuffer buf ba)
    (apply str
	   (map char ba))))

(defn parse-igb-header [txt]
  (let [word (ap/t #"\w+")
	k (ap/alt
	   (ap/s word (ap/t #":"))
	   (fn [[s _]]
	     (keyword s)))
        number-keys #{:x :y :z :t :dim_t :zero :facteur :org_t}
	kv (ap/alt
	    (ap/s k word (ap/t #"\s+"))
	    (fn [[k* v* _]]
	      (list k* (if (number-keys k*)
                         (Integer/parseInt v*)
                         v*))))
	kvs (ap/alt
	     (ap/+ kv)
	     (fn [pairs]
               (persistent!
                (reduce (fn [ret [k v]]
                          (assoc! ret k v))
                        (transient {})
                        pairs))))]
    (:result (ap/parse kvs txt))))

(defmacro with-file-channel [[channel file]
                             & body]
  (let [file-sym (vary-meta (gensym "file") assoc :tag 'java.io.File)]
    `(let [~file-sym ~file
           raf# (java.io.RandomAccessFile. ~file-sym
                                           "r")]
       (with-open [~channel (.getChannel raf#)]
         ~@body))))

(defn channel->mapped-byte-buffer-frame
  "doesn't close resources since assumes will be called as part of loop

  does not dispatch on realtype (double vs float), assumes float

  see igb-fns for newer version
  "
  ^java.nio.MappedByteBuffer [^java.nio.channels.FileChannel channel
   {:keys [header
           time]}]
  (let [header-byte-size 1024
        size (long (:x header)) ; the length of 1 frame is stored in :x
        image-mem-size (* 4 size)
        buf (.map channel
                  java.nio.channels.FileChannel$MapMode/READ_ONLY
                  (+ header-byte-size (* (long time) image-mem-size))
                  image-mem-size)]
    (.order buf java.nio.ByteOrder/LITTLE_ENDIAN)
    buf))

;; pts = point-index->[x y]
;; coordinate system, bottom left is origin
;; (int/ (+ x x-mesh-bottom-left->tex-bottom-left-offset) x-resolution)
;; (int/ (+ y y-mesh-bottom-left->tex-bottom-left-offset) y-resolution)

;; colorbar mapping code
;; -currently only need to worry about getting value from image
;; -dj.plot dj/plot
;; -dj.cuda dj/cuda/image

;;; igb tree function implementations
;; ----------------------------------------------------------------------

(def igb-fns
  {:igb/input-random-access-file
   (dj.dispatch.treefn/fm
    [:igb/input-file]
    (java.io.RandomAccessFile. ^java.io.File input-file "r"))
   :igb/file-channel
   (dj.dispatch.treefn/fm
    [:igb/input-random-access-file]
    (.getChannel ^java.io.RandomAccessFile input-random-access-file))
   :igb/header-str
   (dj.dispatch.treefn/fm
    [:igb/file-channel
     :igb/header-byte-size]
    (let [buf (.map ^java.nio.channels.FileChannel file-channel
                    java.nio.channels.FileChannel$MapMode/READ_ONLY
                    0
                    (long header-byte-size))
          ba (byte-array header-byte-size)]
      (.get ^java.nio.MappedByteBuffer buf ba)
      (apply str
             (map char ba))))
   :igb/header
   (dj.dispatch.treefn/fm
    [:igb/header-str]
    (dj.simulations.carp.output.igb/parse-igb-header header-str))
   :igb/frame-size
   (dj.dispatch.treefn/fm
    [:igb/header]
    ;; the length of 1 frame is stored in :x
    (:x header))
   :igb/realtype
   (dj.dispatch.treefn/fm
    [:igb/header]
    (keyword (:type header)))
   :igb/time->frame
   (dj.dispatch.treefn/fm
    [:igb/frame-size
     :igb/file-channel
     :igb/header-byte-size
     :igb/realtype
     :constants/realtype->bytes]
    ;; according to FileChannel spec, closing the file/channel has no
    ;; effect on the validity of the mapping of the buffer aka, safe
    ;; to close channel after done mapping, however in this case, we
    ;; probably still want to make more maps. Memory usage will
    ;; explode without manual calls to (System/gc)
    (let [bytes-per-realtype (realtype->bytes realtype)
          image-mem-size (* (long bytes-per-realtype) (long frame-size))]
      (case realtype
        :float
        (fn [time]
          (let [buf (.map ^java.nio.channels.FileChannel file-channel
                          java.nio.channels.FileChannel$MapMode/READ_ONLY
                          (+ header-byte-size (* (long time) image-mem-size))
                          image-mem-size)]
            (.order buf java.nio.ByteOrder/LITTLE_ENDIAN)
            (.asFloatBuffer buf)))

        :double
        (fn [time]
          (let [buf (.map ^java.nio.channels.FileChannel file-channel
                          java.nio.channels.FileChannel$MapMode/READ_ONLY
                          (+ header-byte-size (* (long time) image-mem-size))
                          image-mem-size)]
            (.order buf java.nio.ByteOrder/LITTLE_ENDIAN)
            (.asDoubleBuffer buf))))))})

(def igb-defaults
  {:igb/header-byte-size 1024
   :constants/realtype->bytes {:float 4
                               :double 8}})

(def igb-analyze-fns
  {:igb.analyze/t-dim
   (dj.dispatch.treefn/fm
    [:igb/header]
    (:t header))
   :igb.analyze/ts
   (dj.dispatch.treefn/fm
    [:igb.analyze/t-dim
     :simulations/record-dt]
    (range 0.0
           (* record-dt (inc t-dim))
           record-dt))
   ;; will want to build variants to handle different query
   ;; requirements
   :igb.analyze/single-node-points
   (dj.dispatch.treefn/fm
    [:igb/time->frame
     :input/node-id
     :igb.analyze/t-dim]
    (mapv (fn [i]
            (let [frame (time->frame i)]
              (.get ^java.nio.DoubleBuffer frame (int node-id))))
          (range t-dim)))
   :igb.analyze/multiple-node-points
   (dj.dispatch.treefn/fm
    [:igb/time->frame
     :input/node-ids
     :igb.analyze/t-dim]
    (loop [accumulate (mapv (fn [_]
                              (transient []))
                            node-ids)
           i 0]
      (if (< i t-dim)
        (let [frame (time->frame i)]
          (recur (mapv (fn [node-id l]
                         (conj! l (.get ^java.nio.DoubleBuffer frame (int node-id))))
                       (range)
                       accumulate)
                 (inc i)))
        (mapv persistent! accumulate))))})

(let [fid :grouped/add-sv-fn
      get-igb-data (dj.dispatch.treefn/treefm
                    (merge
                     igb-fns
                     igb-analyze-fns
                     {:input/node-ids
                      (dj.dispatch.treefn/fm
                       [:igb/frame-size]
                       (range frame-size))
                      fid
                      (dj.dispatch.treefn/fm
                       [:igb/time->frame
                        :input/node-ids
                        :igb.analyze/t-dim
                        :group/accumulate
                        :group/v]
                       (let [seed
                             (if (empty? accumulate)
                               (let [empty-maps (mapv (fn [_] {}) node-ids)]
                                 {:initial-conditions empty-maps
                                  :output empty-maps})
                               accumulate)
                             
                             seed2
                             (update seed
                                     :initial-conditions
                                     (fn [node-maps]
                                       (mapv (fn [node-id m]
                                               (assoc m
                                                      v
                                                      (.get ^java.nio.DoubleBuffer (time->frame 0) (int node-id))))
                                             (range)
                                             node-maps)))
                             full-lines
                             (loop [lines (mapv (fn [_]
                                                  (transient []))
                                                node-ids)
                                    i 1]
                               (if (< i t-dim)
                                 (let [frame (try
                                               (time->frame i)
                                               (catch Exception e
                                                 :error))]
                                   (if (= frame :error)
                                     (mapv persistent! lines)
                                     (recur (mapv (fn [node-id l]
                                                    (conj! l (.get ^java.nio.DoubleBuffer frame (int node-id))))
                                                  (range)
                                                  lines)
                                            (inc i))))
                                 (mapv persistent! lines)))]
                         (update seed2
                                 :output
                                 (fn [node-maps]
                                   (mapv (fn [m line]
                                           (assoc m
                                                  v
                                                  line))
                                         node-maps
                                         full-lines)))))})
                    fid)
      close-data (dj.dispatch.treefn/dismantlefm {} get-igb-data)]
  (defn grouped-igb-output
    "takes groups of igb files with consistent labeling scheme and
  produces data format similar to
  dj.simulations.cuda.run/simulation-data applied to output
  from :cuda.simulate/output-host-data

  output: {:initial-conditions [{\"sv1\" 1.0 ...} ...] :output [{\"sv1\" [1.0 2.0...] ...} ...]}
  (:initial-conditions | :output) -> node-id -> (state-var -> ( value | time-step -> value))

  takes: :simulations/labels and :simulations/output-dir
  "
    [input]
    (let [{:keys [:simulations/labels
                  :simulations/output-dir
                  :input/track-vars]} input]
      (reduce (fn [ret label]
                (let [guess-filenames (reduce (fn [ret v]
                                                (assoc ret
                                                       (str v "." label ".igb")
                                                       v))
                                              {}
                                              track-vars)
                      igb-files
                      (filter (fn [f]
                                (guess-filenames (dj.io/get-name f)))
                              (dj.io/ls output-dir))

                      next-acc (loop [fs igb-files
                                      accumulate {}]
                                 (System/gc)
                                 (let [f (first fs)]
                                   (if f
                                     (recur (rest fs)
                                            (let [igb-data
                                                  (-> (assoc igb-defaults
                                                             :igb/input-file f
                                                             :group/accumulate accumulate
                                                             :group/v (guess-filenames (dj.io/get-name f)))
                                                      get-igb-data)
                                                  ret
                                                  (get igb-data fid)]
                                              (close-data igb-data)
                                              ret)) 
                                     accumulate)))]
                  (-> ret
                      (update :initial-conditions
                              into
                              (:initial-conditions next-acc))
                      (update :output
                              into
                              (:output next-acc)))))
              {:initial-conditions []
               :output []}
              labels))))


(defn emit-igb-header-str
  "x:<length>
   t:<num time data points (simulation points + IC)>
   dim_t:<num simulation points> (opt)"
  [data-map]
  (let [default-data {:y 1
                      :z 1
                      :type "double"
                      :systeme "little_endian"
                      :org_t 0
                      :unites_x "um"
                      :unites_y "um"
                      :unites_z "um"
                      :unites_t "ms"
                      :unites "mV"
                      :facteur 1
                      :zero 0}
        dim_t (dec (:t data-map))
        key-order [:x :y :z :t :type :systeme :org_t :dim_t :unites_x :unites_y :unites_z :unites_t :unites :facteur :zero]
        combined-data (merge default-data
                             {:dim_t dim_t}
                             data-map)
        str:entries (reduce (fn [ret k]
                              (conj ret
                                    (str (name k)
                                         ":"
                                         (let [entry (combined-data k)]
                                           (if entry
                                             entry
                                             (throw (ex-info (str "missing key: " k)
                                                             {:k k :ret ret :combined-data combined-data})))))))
                            []
                            key-order)]
    ;; Header whitespace formatting looks like:
    ;; entries + CRLF + (70 CRLF)*p + rCRLFFF
    (let [header-size 1024]
      (loop [remaining str:entries
             header-data []
             line-length 0
             total-length 0]
        (if (> total-length (- header-size 3)) ; need room for \r\n\f
          (throw (throw (ex-info "max header length (1024) exceeded"
                                 {:str:entries str:entries :header-data header-data
                                  :line-length line-length :total-length total-length})))
          (let [current (first remaining)]
            (if current
              ;; append entry if fits, else start new line
              (let [new-entry-length (inc (count current))
                    new-line-length (+ line-length new-entry-length)]
                (if (> new-line-length 70)
                  (recur remaining
                         (conj header-data "\r\n")
                         0
                         (+ total-length 2))
                  (recur (rest remaining)
                         (conj header-data current " ")
                         new-line-length
                         (+ total-length new-entry-length))))
              ;; pad the rest of the string to 1024 and place suffix \r\n\f
              (let [space-left (- header-size total-length 2)
                    pads (quot space-left 72)
                    final-size (rem space-left 72)]
                (-> header-data
                    (conj "\r\n" ; +2
                          (apply str (repeat pads "                                                                      \r\n"))
                          (apply str (repeat (- final-size 3) " "))
                          "\r\n\f")
                    (->> (apply str)))))))))))

#_ (emit-igb-header-str {:x 63001 :t 1001 :type "float"})
