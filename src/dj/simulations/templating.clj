(ns dj.simulations.templating
  (:require [dj.store.clj :as clj]))

(defn apply-template
  [store-path
   template-id
   template-args]
  (let [{:keys [template-filename substitute-map-fn]} (clj/load-from-id store-path
                                                                        template-id
                                                                        "load.clj")]
    (try
      (dj/replace-map (dj.io/eat (dj.io/file store-path
                                             template-id
                                             template-filename))
                      (substitute-map-fn template-args))
      (catch Exception e
        (throw (ex-info "error substituting"
                        {:local-context (dj.repl/local-context)}
                        e))))))
