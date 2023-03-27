(ns dj.applications.dir-browser
  (:require [dj.shell :as ds]))

(def ignore-list
  #{".m2"
    "clj/"
    ".cache"
    ".mozilla"
    ".local"
    ".java"
    ".media_backup"
    ".ssh"
    ".config"
    "Software/sources/clo"
    "Software/sources/mae"
    "Software/install"
    "Music/audio/music/Ri"
    "tmp"
    "Software/tmp"})

(def ignore-match
  [#"\.git"])

(def main-dir
  (-> (ds/sh ["bash" "-c" "echo $HOME"])
      :out
      clojure.string/trim-newline))

(defn get-files []
  (:out
   (ds/sh ["find" "."
           :dir main-dir])))

(let [ignore-list (map (fn [ignore]
                         (str "./" ignore))
                       ignore-list)]
  (defn filter-p [prefix line]
    (or (some (fn [ignore]
                (.startsWith ^String line
                             ^String ignore))
              ignore-list)
        (some (fn [re]
                (re-find re line))
              ignore-match))))

#_ (def prefixes-10
    (reduce (fn [agg line]
              (let [prefix (subs line 0 (min (count line)
                                             20))]
                (if (filter-p prefix line)
                  agg
                  (update agg
                          prefix
                          (fn [c]
                            (inc (or c 0)))))))
            {}
            (clojure.string/split txt #"\n")))

(defn filtered-lines [txt]
  (->> (clojure.string/split txt #"\n")
       (remove (fn [line]
                 (filter-p (subs line 0 (min (count line)
                                             20))
                           line)))))

#_ (->> prefixes-10
     (sort-by second)
     reverse
     vec)

(defn normalize [parents filename]
  (apply dj.io/file main-dir (conj (vec (drop 1 parents))
                                   filename)))
(defn symlink? [file]
  (zero?
   (:exit
    (clojure.java.shell/sh "test" "-h" (dj.io/get-path file)))))

(defn file->paths [lines]
  (reduce (fn [agg line]
            (let [breakdown (clojure.string/split line #"/")
                  filename (last breakdown)
                  path (drop-last breakdown)
                  ^java.io.File full-file (normalize path filename)
                  filesize (.length full-file)]
              (if #_ (or (.isDirectory full-file)
                         (symlink? full-file))
                  (.isDirectory full-file)
                agg
                (update agg
                        [filename filesize]
                        (fn [paths]
                          (conj (or paths #{})
                                path))))))
          {}
          lines))

#_ (let [ignore-re [#"\.pyi"
                 #"Thumbs\.db"
                 #"desktop.ini"
                 #"__init__"
                 #"\.js$"
                 #"\.au$"]]
  (->> (reduce-kv (fn [file->paths file paths]
                    (if (and (not= (count paths) 1)
                             (not (some (fn [re]
                                          (re-find re file))
                                        ignore-re)))
                      (assoc file->paths
                             file
                             (count paths))
                      file->paths))
                  {}
                  file->paths)
       (sort-by second)
       reverse
       vec))

(defn sorted-counts [file->paths]
  (let [ignore-re [#"\.pyi"
                   #"Thumbs\.db"
                   #"\.emacs"
                   #"desktop.ini"
                   #"__init__"
                   #"\.js$"
                   #"\.au$"
                   #"Folder\.jpg"
                   #"AlbumArtSmall\.jpg"
                   #"trustdb\.gpg"]]
    (->> (reduce-kv (fn [file->paths pair paths]
                      (let [[file size] pair]
                        (if (and (not= (count paths) 1)
                                 (not (some (fn [re]
                                              (re-find re file))
                                            ignore-re)))
                          (assoc file->paths
                                 pair
                                 paths)
                          file->paths)))
                    {}
                    file->paths)
         (sort-by (comp count second))
         reverse
         vec)))

(defn path-list->path [list]
  (apply dj.io/file main-dir (drop 1 list)))

(defn diff [paths]
  (let [x0 (first paths)
        xrest (rest paths)]
    (map (fn [path]
           [[x0 path]
            (:out
             (clojure.java.shell/sh "bash" "-c" (str "git diff "
                                                     (dj.io/get-path x0)
                                                     " "
                                                     (dj.io/get-path path))))])
         xrest)))

(defn analyze-entry [entry]
  (let [[[file file-size] path-lists] entry
        full-paths (doall
                    (map (fn [path-list]
                           (path-list->path (into (vec path-list) [file])))
                         path-lists))]
    (diff full-paths)))

(defn rm-files [files]
  (doseq [file files]
    (dj.io/rm file)))

#_ (def results
     (-> (get-files)
         filtered-lines
         file->paths
         sorted-counts))

(do
  results)

(let [p '("." "Music" "audio" "music3" "my-mix")]
  (->> results
       (filter (fn [[_ paths]]
                 (paths p)))
       #_ (map analyze-entry)
       (map (fn [[[file _] paths]]
              (normalize p file)))
                                        ;rm-files
       ))



(-> results
    (nth 0)
    analyze-entry
    )

(-> results
    (nth 0)
    analyze-entry
    first
    first
    first
                                        ;dj.io/rm
                                        ;rm-files
    )

