(ns dj.durable
  (:require [clojure.java.io :as cji]
            [clojure.edn :as edn]))

(def kv-log-format "an entry example:
11
:name \"Bob\"
7
:age 21
where 11 is the hash of the value
")

(defn read-kv-log-file
  "given file/path, returns kv state based on defined format
  - should throw error if hash doesn't match key's value
  - if error detected, throw error and require manual intervention to fix log

  assumes log defines a kv store
  where each entry consists of a hash of value, key, and value

  - should catch file corruption
    - from hash
    - hash must interleave entries
  - should catch failed partial write
    - since must be edn readable
    - and hash must match
  "
  [path]
  (with-open [reader (-> path
                         cji/reader
                         (java.io.PushbackReader.))]
    (loop [ret (transient {})]
      (let [recorded-v-hash (edn/read {:eof :end}
                                 reader)]
        (if (= :end recorded-v-hash)
          (persistent! ret)
          (if (number? recorded-v-hash)
            (recur (try
                     (let [k (edn/read reader)
                           v (edn/read reader)
                           v-hash (hash v)]
                       (when (not= recorded-v-hash v-hash)
                         (throw (ex-info (str "hash values do not match for key " k "'s value")
                                         {:k k
                                          :v v
                                          :recorded-v-hash recorded-v-hash
                                          :actual-v-hash v-hash})))
                       (assoc! ret
                               k
                               v))
                     (catch Exception e
                       (throw (ex-info (str "unexpected error while reading " path)
                                       {:current-kv-map (persistent! ret)
                                        :path path
                                        :recorded-v-hash recorded-v-hash}
                                       e)))))
            (throw (ex-info (str "incorrect input: expecting number as recorded-v-hash got " recorded-v-hash)
                            {:recorded-v-hash recorded-v-hash}))))))))

#_ (read-kv-log-file "/home/bmillare/tmp/test.edn")
#_ (read-kv-log-file "/home/bmillare/tmp/test2.edn")

(defn write-entry!
  "takes a :append writer to a file

  writes hash of v, k, and v as edn to file
  "
  [writer k v]
  (let [v-hash (hash v)]
    (.write writer (str v-hash "\n"))
    (.write writer (str (pr-str k) " " (pr-str v) "\n"))
    (.flush writer)))

(defn write-entry-no-flush!
  "takes a :append writer to a file

  writes hash of v, k, and v as edn to file
  "
  [writer k v]
  (let [v-hash (hash v)]
    (.write writer (str v-hash "\n"))
    (.write writer (str (pr-str k) " " (pr-str v) "\n"))))

(defn conj-entry-fn
  "takes a :append writer to a file"
  [^java.io.Writer writer]
  (fn conj-entry [m k v]
    (write-entry! writer k v)
    (assoc m k v)))

(defn update-entry-fn
  [^java.io.Writer writer]
  (fn update-entry [m f k args]
    (let [v (apply f (get m k) args)]
      (write-entry! writer k v)
      (assoc m k v))))

(defn assoc+save!-fn [^java.io.Writer writer]
  (fn assoc+save! [m k v]
    (write-entry! writer k v)
    (assoc m k v)))

(defn kv-writer [path]
  (cji/writer path :append true))

(defn kv-agent
  ([]
   (agent {}))
  ([v]
   (agent v)))

(comment
  
  (def test-writer (kv-writer "/home/bmillare/tmp/test2.edn"))

  (def my-conj-entry (conj-entry-fn test-writer))
  (def my-agent (kv-agent))
  (send-off my-agent (fn [_] {}))
  (send-off my-agent my-conj-entry :name "bob")
  (send-off my-agent my-conj-entry :height 15)
  @my-agent

  )

(defn map->storage-dumb
  "converts map to log style, no ordering defined"
  [m path]
  (with-open [w (cji/writer path)]
    (doseq [[k v] m]
      (write-entry-no-flush! w k v))))
