(ns dj.simulations.carp.input.poopers.run
  (:require [dj.simulations.scripting :as scripting]
            [dj.simulations.carp.input.par :as par]
            [dj.simulations.templating :as templating]
            [dj.store.clj :as clj]
            [dj.repl]))

;; carp simulation provider

(defn basic-run-script
  [sys
   this:store-id
   m]
  (let [{:keys [cell-model:id
                cell-model-binary:id
                mesh:id
                run-template:id
                par-template:id
                result-path
                virtual-cpus-per-job

                executor:store-path
                executor:mpirun-deps
                executor:carp-root-path
                executor:cpus-per-node
                executor:cpu-as-node-virtualization
                executor:max-walltime]}
        m
        store-path (:dj/store-path sys)
        read-data #(clj/read-from-id store-path % "data.clj")
        
        working-dir (dj.io/file store-path this:store-id)

        {:keys [cell-model:initial-state-filename
                cell-model:name]}
        (read-data cell-model:id)

        {:keys [cell-model:module-path]}
        (read-data cell-model-binary:id)

        ]
    (doseq [v [this:store-id
               cell-model:id
               cell-model-binary:id
               mesh:id
               run-template:id
               par-template:id
               result-path
               virtual-cpus-per-job

               executor:store-path
               executor:mpirun-deps
               executor:carp-root-path
               executor:max-walltime]]
      (when-not v
        (throw (ex-info "value cannot be nil"
                        {:local-context (dj.repl/local-context)}))))
    (dj.io/cp (dj.io/file store-path
                          cell-model:id
                          cell-model:initial-state-filename)
              working-dir)
    (scripting/poop-map
     working-dir
     {"mesh.par"
      (templating/apply-template store-path
                                 par-template:id
                                 (merge m
                                        {:ionic-model cell-model:name
                                         :ionic-model-path (dj/str-path executor:store-path
                                                                        cell-model-binary:id
                                                                        cell-model:module-path)
                                         :region0-parameters-emit (par/region-parameters
                                                                   (:region0-parameters m))
                                         :region1-parameters-emit (par/region-parameters
                                                                   (:region1-parameters m))
                                         :state-variable-dump (apply str (interpose "," (:sv-out m)))
                                         :state-variable-path cell-model:initial-state-filename
                                         :gvecs (par/gvec (:num-regions m)
                                                          (:sv-out m))
                                         :save-state-times-emit (par/tsav (:save-state-times m))
                                         :mesh-path (dj/str-path executor:store-path
                                                                 mesh:id
                                                                 (:mesh:name (read-data mesh:id)))}))
      "run.sh"
      (templating/apply-template store-path
                                 run-template:id
                                 (merge
                                  m
                                  (if executor:cpu-as-node-virtualization
                                    {:nodes-per-job virtual-cpus-per-job}
                                    {:cpus-per-node executor:cpus-per-node
                                     :nodes-per-job (quot virtual-cpus-per-job
                                                          executor:cpus-per-node)})
                                  {:job-name this:store-id
                                   :run-command
                                   (str
                                    (dj/str-path executor:carp-root-path
                                                 "bin/carp")
                                    " +F mesh.par >" (dj/str-path result-path
                                                                  "RUN.log")
                                    (when (:gunzip m)
                                      (str " && cd " result-path " && gunzip vm.igb.gz && cd ..")))
                                   :mpirun-deps executor:mpirun-deps
                                   :max-walltime executor:max-walltime}))})))
