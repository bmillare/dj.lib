(ns dj.shell
  (:require [clojure.java.shell :as sh]))

(defn sh
  "wrap shell to fail with exceptions"
  [args]
  (let [{:keys [exit out err] :as m} (apply sh/sh args)]
    (if (zero? exit)
      m
      (throw (ex-info "sh command failed"
                      {:exit exit
                       :out out
                       :err err
                       :args args})))))

