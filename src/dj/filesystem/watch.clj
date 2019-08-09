(ns dj.filesystem.watch
  (:require [dj.io])
  (:import (java.nio.file FileSystems WatchKey WatchService StandardWatchEventKinds Path)
           (java.io File)))

(defn make-watcher
  "returns watcher service, probably make one per jvm (or one per
  path), see register. Closeable"
  []
  (.newWatchService (FileSystems/getDefault)))

(defn make-registry
  "tracked paths->watchkey, WatchKeys are cancelable with .cancel"
  ([]
   (make-registry {}))
  ([init]
   (atom init)))

(defn register
  "registers file or dir f with watchservice. Use take-events to wait/block for new events"
  [^WatchService ws ^File f registry]
  (let [path (.toPath f)
        wk (.register path
                      ws
                      (into-array
                       (type StandardWatchEventKinds/ENTRY_CREATE)
                       [StandardWatchEventKinds/ENTRY_CREATE
                        StandardWatchEventKinds/ENTRY_DELETE
                        StandardWatchEventKinds/ENTRY_MODIFY]))]
    (swap! registry
           assoc
           path
           wk)
    wk))

(defn poll-events
  "extract available events from work service as vector"
  [^WatchKey wk]
  (let [dir (.watchable wk) ;; this is actually a Path
        ]
    (mapv (fn [e]
            (let [context (.context e)]
              {:kind (.name (.kind e))
               :file (if (instance? java.nio.file.Path context)
                       (.toFile (.resolve dir context)))
               :count (.count e)}))
          (.pollEvents wk))))

(defn take-events
  "block/wait on current new events and return them"
  [^WatchService ws]
  (let [wk (.take ws)
        ret (poll-events wk)]
    (.reset wk)
    ret))

(comment

  (def watcher (make-watcher))
  (.close watcher)
  (def registry (make-registry))

  (register watcher
            (dj.io/file "/home/username/tmp")
            registry)
  (.cancel (second (first @registry)))
  (def r (future
           (take-events watcher)))
  @r
  [{:kind "ENTRY_MODIFY", :file #java.io.File["/home/username/tmp/watchtest"], :count 2}]

  )
