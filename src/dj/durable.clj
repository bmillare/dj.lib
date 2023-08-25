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
  [path & [{:keys [ignore-missing
                   ignore-hash
                   version] :or {ignore-missing true
                                 ignore-hash false}}]]
  (case version
    :no-hash
    (try
      (with-open [reader (-> path
                             cji/reader
                             (java.io.PushbackReader.))]
        (loop [ret (transient {})]
          (let [k (edn/read {:eof ::end
                             :default tagged-literal}
                            reader)]
            (if (= ::end k)
              (persistent! ret)
              (recur (try
                       (let [v (edn/read {:default tagged-literal}
                                         reader)]
                         (assoc! ret
                                 k
                                 v))
                       (catch Exception e
                         (throw (ex-info (str "unexpected error while reading " path)
                                         {:current-kv-map (persistent! ret)
                                          :path path
                                          :k k}
                                         e)))))))))
      (catch java.io.FileNotFoundException e
        (if ignore-missing
          {}
          (throw e))))
    (try
      (with-open [reader (-> path
                             cji/reader
                             (java.io.PushbackReader.))]
        (loop [ret (transient {})]
          (let [recorded-v-hash (edn/read {:eof ::end
                                           :default tagged-literal}
                                          reader)]
            (if (= ::end recorded-v-hash)
              (persistent! ret)
              (if (number? recorded-v-hash)
                (recur (try
                         (let [k (edn/read {:default tagged-literal}
                                           reader)
                               v (edn/read {:default tagged-literal}
                                           reader)
                               v-hash (hash v)]
                           (when (and (not ignore-hash)
                                      (not= recorded-v-hash v-hash))
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
                                {:recorded-v-hash recorded-v-hash})))))))
      (catch java.io.FileNotFoundException e
        (if ignore-missing
          {}
          (throw e))))))

(defn read-tx-log-as-history
  "given file/path that refers to history of transactions (transaction
  is just a list of kv updates) produce history of databases (vector
  of different db values)
  "
  [path & [{:keys [ignore-missing]
            :or {ignore-missing true}}]]
  (try
    (with-open [reader (-> path
                           cji/reader
                           (java.io.PushbackReader.))]
      (loop [ret (transient [{}])]
        (let [tx (edn/read {:eof ::end
                           :default tagged-literal}
                          reader)]
          (if (= ::end tx)
            (persistent! ret)
            (recur (try
                     (let [i (dec (count ret))
                           db (ret i)]
                       (conj! ret
                              (reduce (fn [db' [k v]]
                                        (assoc db'
                                               k
                                               v))
                                      db
                                      tx)))
                     (catch Exception e
                       (throw (ex-info (str "unexpected error while reading " path)
                                       {:current-kv-map (peek (persistent! ret))
                                        :path path
                                        :tx tx}
                                       e)))))))))
    (catch java.io.FileNotFoundException e
      (if ignore-missing
        [{}]
        (throw e)))))

#_ (read-kv-log-file "/home/bmillare/tmp/test.edn")
#_ (read-kv-log-file "/home/bmillare/tmp/test2.edn")

(defn write-entry!
  "takes an :append writer to a file

  writes hash of v, k, and v as edn to file
  "
  [writer k v]
  (let [v-hash (hash v)]
    (.write writer (str v-hash "\n"))
    (.write writer (str (pr-str k) " " (pr-str v) "\n"))
    (.flush writer)))

(defn write-entry-v2!
  "takes an :append writer to a file
  (doesn't write hash, unlike v1)
  writes k, and v as edn to file
  "
  [^java.io.Writer writer k v]
  (.write writer (str (pr-str k) " " (pr-str v) "\n"))
  (.flush writer))

(defn write-transaction!
  "takes an :append writer to a file
  (doesn't write hash, unlike v1)
  writes k, and v as edn to file
  "
  [^java.io.Writer writer tx]
  (.write writer (str (pr-str tx) "\n"))
  (.flush writer))

(defn write-entry-no-flush!
  "takes an :append writer to a file

  writes hash of v, k, and v as edn to file
  "
  [writer k v]
  (let [v-hash (hash v)]
    (.write writer (str v-hash "\n"))
    (.write writer (str (pr-str k) " " (pr-str v) "\n"))))

(defn conj-entry-fn
  "takes an :append writer to a file"
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

(defn update+save!-fn
  "takes an :append writer to a file
  alias for update-entry-fn"
  [^java.io.Writer writer]
  (fn update-entry [m f k args]
    (let [v (apply f (get m k) args)]
      (write-entry! writer k v)
      (assoc m k v))))

(defn assoc+save!-fn
  "takes an :append writer to a file
  alias for conj-entry-fn"
  [^java.io.Writer writer]
  (fn assoc+save! [m k v]
    (write-entry! writer k v)
    (assoc m k v)))

(defn kv-writer [path]
  (cji/writer path :append true))

(defn tx-writer [path]
  (cji/writer path :append true))

(defn tx-agent
  ([]
   (agent [{}]))
  ([v]
   (agent v)))

(defn kv-agent
  ([]
   (agent {}))
  ([v]
   (agent v)))

(defn write-kv!-fn
  "creates a send-off like fn that writes kv to writer"
  [the-agent ^java.io.Writer writer {:keys [write-entry-fn!]
                                     :or {write-entry-fn write-entry!}}]
  (fn write-kv!
    [k v]
    (send-off the-agent
              (fn +writer [state]
                (write-entry-fn! writer k v)
                (assoc state k v)))))

(defn txs-as-history!-fn
  "creates a send-off like fn that writes kvs to writer history style"
  [the-agent ^java.io.Writer writer]
  (fn transact!
    [tx]
    (send-off the-agent
              (fn +writer [state]
                (write-transaction! writer tx)
                (let [i (dec (count state))
                      db (state i)]
                  (conj state
                        (reduce (fn [db' [k v]]
                                  (assoc db' k v))
                                db
                                tx)))))))

(defn send-off-kv!
  "send-off like fn that writes kv to writer"
  [the-agent ^java.io.Writer writer k v]
  (send-off the-agent
            (fn +writer [state]
              (write-entry! writer k v)
              (assoc state k v))))

(comment
  
  (def test-writer (kv-writer "/home/bmillare/tmp/test2.edn"))

  (def my-conj-entry (conj-entry-fn test-writer))
  (def my-agent (kv-agent))
  (send-off my-agent (fn [_] {}))
  (send-off my-agent my-conj-entry :name "bob")
  (send-off my-agent my-conj-entry :height 15)
  @my-agent

  ;; 1. get file writer
  ;; 2. get durable assoc/update via assoc+save!-fn or update+save!-fn
  ;; 3. get state manager (agent)
  ;; 4. **wait, rethinking

  ;; v2
  ;; 1. create agent
  ;; 2. create write-kv! fn
  ;; 2.opt track the writer
  (def my-agent (kv-agent))
  (def my-agent2 (kv-agent (read-kv-log-file "/home/bmillare/tmp/test2.edn")))
  (def the-writer (kv-writer "/home/bmillare/tmp/test2.edn"))
  (def write-kv! (write-kv!-fn my-agent the-writer))

  (write-kv! assoc :name "bob")

  ;; v3
  ;; 1. create agent
  ;; 2. create writer
  ;; 3. call send-off-kv!
  (def my-agent2 (kv-agent (read-kv-log-file "/home/bmillare/tmp/test2.edn")))
  (def the-writer (kv-writer "/home/bmillare/tmp/test2.edn"))
  (send-off-kv! my-agent2 the-writer :name "bob")
  
  
  )

(defn map->storage-dumb
  "converts map to log style, no ordering defined"
  [m path]
  (with-open [w (cji/writer path)]
    (doseq [[k v] m]
      (write-entry-no-flush! w k v))))
