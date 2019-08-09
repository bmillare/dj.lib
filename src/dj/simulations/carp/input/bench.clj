(ns dj.simulations.carp.input.bench
  (:require [dj.repl]
            [dj.dispatch.treefn :as tf]
            [dj]))

(def arg-alias
  {:simulation-duration ["duration" :double]
   :stimulation-count ["numstim" :int]
   :stimulation-start-time ["stim-start" :double]
   :read-init-path ["read-ini-file" :string]
   :save-init-path ["save-ini-file" :string]
   :save-init-time ["save-ini-time" :double]
   :ionic-model-name ["imp" :string]
   :ionic-model-shared-library ["load-module" :string]
   :output-state-variables ["validate" :exist]
   :ionic-model-parameters ["imp-par" :string]})

(let [emit-arg (fn [value alias]
                 (let [[arg-name type] (arg-alias alias)]
                   (if (nil? value)
                     nil
                     (case type
                       :double (if (number? value)
                                 (str "--" arg-name "=" value)
                                 (throw (ex-info "value should be a number"
                                                 (dj.repl/local-context))))
                       :int (if (number? value)
                              (str "--" arg-name "=" value)
                              (throw (ex-info "value should be a number"
                                              (dj.repl/local-context))))
                       :string (if (string? value)
                                 (str "--" arg-name "=" value)
                                 (throw (ex-info "value should be a string"
                                                 (dj.repl/local-context))))
                       :exist (if value
                                (str "--" arg-name)
                                nil)))))]
  (def emit
    (tf/fm [:research.simulations.carp/root-path
            :research.simulations.carp/version
            :bench/directory

            :bench/simulation-duration
            :bench/stimulation-count
            :bench/stimulation-start-time
            :bench/read-init-path
            :bench/save-init-path
            :bench/save-init-time
            :bench/ionic-model-name
            :bench/ionic-model-shared-library
            :bench/output-state-variables
            :bench/ionic-model-parameters]
           (when (or (and (not save-init-time) save-init-path)
                     (and save-init-time (not save-init-path)))
             (throw (ex-info "need both save-init-time and save-init-path"
                             (dj.repl/local-context))))
           (str "cd " directory " && "
                (dj/str-path root-path version "bin/bench")
                " "
                (apply str (for [[k v] [[:simulation-duration simulation-duration]
                                        [:stimulation-count stimulation-count]
                                        [:stimulation-start-time stimulation-start-time]
                                        [:read-init-path read-init-path]
                                        [:save-init-path save-init-path]
                                        [:save-init-time save-init-time]
                                        [:ionic-model-name ionic-model-name]
                                        [:ionic-model-shared-library ionic-model-shared-library]
                                        [:output-state-variables output-state-variables]
                                        [:ionic-model-parameters ionic-model-parameters]]
                                 :when v]
                             (str (emit-arg v k) " ")))
                " > vm.txt"))))

(let [emit-arg (fn [value alias]
                 (let [[arg-name type] (arg-alias alias)]
                   (if (nil? value)
                     nil
                     (case type
                       :double (if (number? value)
                                 (str "--" arg-name "=" value)
                                 (throw (ex-info "value should be a number"
                                                 (dj.repl/local-context))))
                       :int (if (number? value)
                              (str "--" arg-name "=" value)
                              (throw (ex-info "value should be a number"
                                              (dj.repl/local-context))))
                       :string (if (string? value)
                                 (str "--" arg-name "=" value)
                                 (throw (ex-info "value should be a string"
                                                 (dj.repl/local-context))))
                       :exist (if value
                                (str "--" arg-name)
                                nil)))))]
  (def emit2
    (tf/fm [:research.simulations.carp/root-path
            :research.simulations.carp/version
            :executor/run-directory-path

            :bench.run.option/simulation-duration
            :bench.run.option/stimulation-count
            :bench.run.option/stimulation-start-time
            :bench.run.option/read-init-path
            :bench.run.option/save-init-path
            :bench.run.option/save-init-time
            :bench.run.option/ionic-model-name
            :bench.run.option/ionic-model-shared-library
            :bench.run.option/output-state-variables
            :bench.run.option/ionic-model-parameters]
           (when (or (and (not save-init-time) save-init-path)
                     (and save-init-time (not save-init-path)))
             (throw (ex-info "need both save-init-time and save-init-path"
                             (dj.repl/local-context))))
           (str "cd " run-directory-path " && "
                (dj/str-path root-path version "bin/bench")
                " "
                (apply str (for [[k v] [[:simulation-duration simulation-duration]
                                        [:stimulation-count stimulation-count]
                                        [:stimulation-start-time stimulation-start-time]
                                        [:read-init-path read-init-path]
                                        [:save-init-path save-init-path]
                                        [:save-init-time save-init-time]
                                        [:ionic-model-name ionic-model-name]
                                        [:ionic-model-shared-library ionic-model-shared-library]
                                        [:output-state-variables output-state-variables]
                                        [:ionic-model-parameters ionic-model-parameters]]
                                 :when v]
                             (str (emit-arg v k) " ")))
                " > vm.txt"))))
