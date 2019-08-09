(ns dj.store.clj
  (:require [dj.io]
            [dj]
            [clojure.edn :as edn]))

(defn read-from-id
  "convenience function to read clojure files from store"
  [store-path store-id local-path]
  (-> (dj.io/file store-path store-id local-path)
      dj.io/eat
      edn/read-string))

(defn load-from-id
  "convenience function to load clojure files from store"
  [store-path store-id local-path]
  (load-file (dj/str-path store-path store-id local-path)))
