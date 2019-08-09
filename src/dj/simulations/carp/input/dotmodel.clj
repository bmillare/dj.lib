(ns dj.simulations.carp.input.dotmodel
  (:require [dj.io]
            [dj.simulations.carp.translator :as rsct]
            [dj.classreloader :as dc]
            [dj.simulations.expression :as e]
            [dj.simulations.integrator :as i]
            [dj.dispatch.treefn :as tf]))

(defn add-ods-data
  "
adds to config parse output of model-file
"
  [config]
  (let [model-file (:model-file config)
        ods-data (e/dotmodel-ods-data
                  (:result
                   (rsct/parse-dotmodel
                    (if (dj.io/exists? model-file)
                      (dj.io/eat model-file)
                      (dc/resource-as-str
                       (dj.io/get-path model-file))))))]
    (merge config
           (assoc ods-data
             :state-vars
             ;; BUG: for current spec, not all state-vars have differential assignments
             (map first (:differential-assignment ods-data))))))

(defn config->assignment-maps [config]
  (-> (:model-file config)
      dj.io/get-path
      dc/resource-as-str
      rsct/parse-dotmodel
      :result
      e/dotmodel-ods-data
      (select-keys [:algebra-assignment
                    :differential-assignment])
      (dj/update-vals i/pairs->exp-map)))

;; input is dotmodel txt
;; output boundary:
;; - dotmodel clojure data representation groups
;;   - variables, track-vars, shared-vars etc
(def dotmodel-fns
  {:dotmodel/txt
   (tf/fm
    [:dotmodel/model-file]
    (dj.io/eat model-file))

   :dotmodel/ods-data
   ;; This entry returns a composite however the elements might be
   ;; accessed multiple times and thus might benefit from flattening
   ;; - Thus we lazily convert entries to flat for consistency of
   ;;   interface
   (tf/fm
    [:dotmodel/txt]
    (-> txt
        rsct/parse-dotmodel
        :result
        e/dotmodel-ods-data))

   :dotmodel/shared-algebra-assignment
   (tf/fm
    [:dotmodel/ods-data]
    (:shared-algebra-assignment ods-data))
   
   :dotmodel/algebra-assignment
   (tf/fm
    [:dotmodel/ods-data]
    (:algebra-assignment ods-data))
   
   :dotmodel/differential-assignment
   (tf/fm
    [:dotmodel/ods-data]
    (:differential-assignment ods-data))
   
   :dotmodel/initial-conditions-assignment
   (tf/fm
    [:dotmodel/ods-data]
    (:initial-conditions-assignment ods-data))
   
   :dotmodel/track-vars
   (tf/fm
    [:dotmodel/ods-data]
    (:track-vars ods-data))
   
   :dotmodel/shared-vars
   (tf/fm
    [:dotmodel/ods-data]
    (:shared-vars ods-data))

   ;; should supersede :state-vars from add-ods-data
   :dotmodel/all-state-vars
   (tf/fm
    [:dotmodel/ods-data]
    (:all-state-vars ods-data))
   
   :dotmodel/special-differential-dependencies
   (tf/fm
    [:dotmodel/ods-data]
    (:special-differential-dependencies ods-data))

   :dotmodel/trace
   (tf/fm
    [:dotmodel/ods-data]
    (:trace ods-data))})
