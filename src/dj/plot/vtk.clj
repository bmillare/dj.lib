(ns dj.plot.vtk)

(defn xyz-polydata [x y z]
  (let [num-points (count x)
        points (apply str (map (fn [xi yi zi]
                                 (str xi " " yi " " zi "\n"))
                               x
                               y
                               z))
        num-lines (dec num-points)
        lines (apply str (map (fn [i]
                                (str "2 " i " " (inc i) "\n"))
                              (range num-lines)))]
    (str "# vtk DataFile Version 2.0
VTK from Matlab
ASCII
DATASET POLYDATA
POINTS " num-points " float
"
         points
         "\nLINES " num-lines " " (* num-lines 3) "\n"
         lines)))

(defn write-binary-vtk-file
  "{:points [[0.0 0.0 0.0]
             [1.0 0.0 0.0]
             [2.0 1.0 0.0]
             [3.0 0.0 0.0]
             [4.0 4.0 0.0]]
    :lines [[0 1 2 3 4]]
    :title \"Hello world\"
    :scalars [{:name \"color\"
    :values [0.0 1.0 2.0 3.0 2.0]}]}

  I guess scalars need to be defined for all points?

  or

  :vertices [0 1 2 3 4]
  "
  [vtk-data
                             path]
  (let [fp (java.nio.file.Paths/get path (into-array String []))
        {:keys [title points lines scalars vertices]} vtk-data
        pcount (count points)
        lcount (count lines)
        vcount (count vertices)
        scount (apply + (map (fn [s]
                               (-> s
                                   :values
                                   count))
                             scalars))
        lsize (apply + (map (comp inc count) lines))]
    (with-open [channel
                (java.nio.channels.FileChannel/open
                 fp
                 (java.util.EnumSet/of java.nio.file.StandardOpenOption/CREATE
                                       java.nio.file.StandardOpenOption/WRITE
                                       java.nio.file.StandardOpenOption/TRUNCATE_EXISTING)
                 (into-array java.nio.file.attribute.FileAttribute []))]
      (let [buf (java.nio.ByteBuffer/allocate (+ 256
                                                 1024 ;; keywords
                                                 (* 4 3 pcount)
                                                 (* 4 lsize)
                                                 (* 4 2 vcount )
                                                 (* 4 scount)))]
        (doto buf
          (.order java.nio.ByteOrder/BIG_ENDIAN)
          (.put (.getBytes (str "# vtk DataFile Version 3.0\n"
                                title "\n"
                                "BINARY\n"
                                "DATASET POLYDATA\n"
                                "POINTS " pcount " float\n"))))
        (doseq [[x y z] points]
          (.putFloat buf (float x))
          (.putFloat buf (float y))
          (.putFloat buf (float z)))
        (when vertices
          (.put buf
                (.getBytes (str "VERTICES " vcount " " (* vcount 2) "\n")))
          (doseq [v vertices]
            (.putInt buf (int 1))
            (.putInt buf (int v))))
        (when lines
          (.put buf
                (.getBytes (str "LINES " lcount " " lsize "\n")))
          (doseq [line lines]
            (.putInt buf (int (count line)))
            (doseq [x line]
              (.putInt buf (int x)))))
        (when scalars
          (.put buf
                (.getBytes (str "POINT_DATA " pcount "\n")))
          (doseq [{:keys [name values]} scalars]
            (.put buf
                  (.getBytes (str "SCALARS " name " float 1\n"
                                  "LOOKUP_TABLE default\n")))
            (doseq [v values]
              (.putFloat buf (float v)))))
        (.flip buf)
        (.write channel buf)))))

(comment
  (write-binary-vtk-file {:points [[0.0 0.0 0.0]
                                   [1.0 0.0 0.0]
                                   [2.0 1.0 0.0]
                                   [3.0 0.0 0.0]
                                   [4.0 4.0 0.0]]
                          :lines [[0 1 2 3 4]]
                          :title "Hello world"
                          :scalars [{:name "color"
                                     :values [0.0 1.0 2.0 3.0 2.0]}]}
                         "/home/username/tmp/test.vtk")

  (write-binary-vtk-file {:points [[0.0 0.0 0.0]
                                   [1.0 0.0 0.0]
                                   [2.0 1.0 0.0]
                                   [3.0 0.0 0.0]
                                   [4.0 4.0 0.0]]
                          :vertices [0 1 2 3 4]
                          :title "Hello world"
                          :scalars [{:name "color"
                                     :values [0.0 1.0 2.0 3.0 2.0]}]}
                         "/home/username/tmp/test.vtk")


  (-> (java.nio.file.Paths/get "/home/username/tmp/test.vtk"
                               (into-array String []))
      (.getParent))

  
  )

