(ns dj.simulations.scripting
  (:require [dj.io]
            [dj.repl]
            [dj.shell :as sh]))

(defn ssh
  "args: ssh args
  sh-code: the code to be executed on remote"
  [args sh-code]
  (let [pass (into (into ["ssh"] args)
                   ["sh" :in sh-code])]
    (sh/sh pass)))

(defn sh-on [executor:handle sh-code]
  (let [{:keys [username address port]} executor:handle]
    ;; TODO:1 :unsure {:tags [:programming]} do a check, if handle refers to localhost, run as sh
    ;; -Currently, localhost must be explicit
    (if (= address
           "localhost")
      (assoc (sh/sh "sh"
                    :in sh-code)
             :input sh-code)
      (if (and username address port)
        (assoc (dj.io/ssh username address port sh-code)
               :input sh-code)
        (throw (ex-info "missing arguments"
                        {:status 400
                         :local-context (dj.repl/local-context)}))))))

;; Why have this? Isn't putting code in a try block and grabbing local-state better?
;; - I think this was because we didn't have a way to keep track of
;;   the cause, hence capturing the cause
(defn procedure-seq
  "

given a seq of functions that accept no args, returns the results of
the functions or a str-representation of the exception. Short circuits
on the first exception

"
  [procedure-seq]
  (loop [ps procedure-seq
         statuses []]
    (let [p (first ps)]
      (if p
        (let [[result success] (try
                                 [(p) true]
                                 (catch Exception e
                                   [e false]))]
          (if success
            (recur (rest ps)
                   (conj statuses
                         result))
            (throw (ex-info "procedure failed"
                            {:statuses statuses}
                            result))))
        statuses))))

(defn poop-map [destination-dir m]
  (reduce-kv (fn [_ filename data]
               (dj.io/poop (dj.io/file destination-dir filename)
                           data))
             nil
             m))

;; deprecate
#_ (defn build [call-args]
  (let [{:keys [caller-id
                store-partition
                env
                args]}
        call-args
        call-signature (dissoc call-args
                               :env)]
    (loop []
      (let [denv @env
            store-path (:research/store-path denv)]
        (when-not ((:research.simulations.scripting/called denv) call-signature)
          (if (compare-and-set! env
                                denv
                                (update-in denv
                                           [:research.simulations.scripting/called]
                                           conj
                                           call-signature))
            (let [store-id (dj.store/->store-entry env store-partition)]
              (dj.io/mkdir (dj.io/file store-path store-id))
              (dj.io/poop (dj.io/file store-path store-id "signature.clj")
                          (pr-str call-signature))
              ((dj.store.clj/load-clj-store-id store-path
                                               caller-id)
               env
               (assoc args
                 :this:id store-id)))
            (recur)))))))

;; deprecate
#_ (defn over-build [store-id call-args]
  (let [{:keys [caller-id
                store-partition
                env
                args]}
        call-args
        call-signature (dissoc call-args
                               :env)]
    (loop []
      (let [denv @env
            store-path (:research/store-path denv)]
        (when-not ((:research.simulations.scripting/called denv) call-signature)
          (if (compare-and-set! env
                                denv
                                (update-in denv
                                           [:research.simulations.scripting/called]
                                           conj
                                           call-signature))
            (do
              (dj.io/mkdir (dj.io/file store-path store-id))
              (dj.io/poop (dj.io/file store-path store-id "signature.clj")
                          (pr-str call-signature))
              ((dj.store.clj/load-clj-store-id store-path
                                               caller-id)
               env
               (assoc args
                 :this:id store-id)))
            (recur)))))))

;; deprecate
#_ (defn merge-id-args [env ids]
  (apply merge
         (let [store-path (:research/store-path @env)]
           (map (fn [id]
                  (dj.store.clj/get-data store-path id))
                ids))))

;; deprecate
#_ (defn get-data-fn [env]
  (let [store-path (:research/store-path @env)]
    (fn [id]
      (dj.store.clj/get-data store-path id))))
