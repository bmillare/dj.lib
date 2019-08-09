(ns dj.simulations.cuda.fressian
  (:require [v.legacy.dj.fressian :as f]))

(def poop (f/->poop-fn (merge f/clojure-write-handlers
                              {clojure.lang.AFunction
                               (f/->write-tag-handler "fn" 1 [w s] (.writeString w (str "function" s)))
                               java.io.File
                               (f/->write-tag-handler "file" 1 [w s] (.writeString w (.getPath s)))})))

(def eat (f/->eat-fn (merge f/clojure-read-handlers
                            {"fn"
                             (f/->read-tag-handler [rdr]
                                                   (.readObject rdr)
                                                   identity)
                             "file"
                             (f/->read-tag-handler [rdr] (java.io.File. (.readObject rdr)))})))

;; temporary solution, needs discussion on google groups
#_ (defmethod clojure.core/print-dup Object [o ^java.io.Writer w]
  (.write w "#")
  (.write w (.getName (class o)))
  (.write w "[\"")
  (.write w (str o))
  (.write w "\"]"))

#_ (defmethod clojure.core/print-method Object [o ^java.io.Writer w]
  (.write w "#")
  (.write w (.getName (class o)))
  (.write w "[\"")
  (.write w (str o))
  (.write w "\"]"))
