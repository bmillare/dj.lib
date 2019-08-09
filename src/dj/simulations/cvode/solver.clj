;; #section:1 {:tags [:natalia/project2 :programming/cvode :programming/tools] :created "2016-08-04"}
(ns dj.simulations.cvode.solver
  (:require [dj.dispatch.treefn :as tf]
            [dj.math.cemit :as ce]
            [dj.math.solver :as ms]
            [dj.math.parser :as p]
            [dj.classreloader :as dc]
            [dj.template.c :as tc]
            [dj.simulations.carp.output.igb :as i]))

(def emit-solver-code-fns
  {:cvode/c-code
   (tf/fm
    [:dotmodel/all-state-vars
     :input/shared-algebra-assignment
     :simulations/compute-initial-conditions
     :simulations/parameters
     :cvode/relative-tolerance
     :cvode/absolute-tolerance
     :integrator/algebra-bindings
     :integrator/differential-exp-map
     :integrator/jacobian-map
     :simulations/num-nodes
     :simulations/dt
     :simulations/num-time-steps
     :simulations/iterations-per-record
     :simulations/label
     :simulations/igb-header-string
     :simulations/debug-switch
     :simulations/debug-start
     :simulations/debug-node]
    (let [sv-order (sort all-state-vars)
          emit (ce/c-emitter {"ln" "log"})
          load-sv (vec
                   (map-indexed (fn [i sv]
                                  [:set
                                   'const 'double sv
                                   ['NV_Ith_S '___y i]])
                                sv-order))]
      (dj/replace-map
       (dc/resource-as-str "dj.simulations/templates/multiple_cvode.c")
       {"_TEMPLATE_num_vars"
        (str (count sv-order))

        "_TEMPLATE_num_nodes"
        (str num-nodes)

        "_TEMPLATE_dt"
        (str dt)

        "_TEMPLATE_num_time_steps"
        (str num-time-steps)

        "_TEMPLATE_iterations_per_record"
        (str iterations-per-record)

        #_"_TEMPLATE_num_algebra"
        #_(str (count algebra-bindings))

        "_TEMPLATE_relative_tolerance"
        (str relative-tolerance)

        "_TEMPLATE_igb_header_string"
        (pr-str igb-header-string)

        "_TEMPLATE_parameters;"
        (apply str
         (mapcat (fn [v_idx [vname vvalue]]
                   (if (number? vvalue)
                     (str (tc/emit [:declare "#define" (str vname "_parameter") vvalue]) "\n")
                     (let [array-name (str vname "_array")]
                       (str (tc/emit [:declare "#define" (str vname "_parameter") [:aget array-name 'n_idx]]) "\n"
                            (tc/emit `[:declare ~'double ~(str array-name "[]") "="
                                       "{" ~@(interpose "," (map vvalue 
                                                                 (range num-nodes))) "}"]) ";\n"))))
                 (range)
                 parameters))

        "_TEMPLATE_initial_conditions;"
        (tc/emit-body
         (for [[i sv] (map-indexed vector sv-order)
               n_idx (range num-nodes)]
           [:set
            ['NV_Ith_S [:aget '___ys n_idx] i]
            (compute-initial-conditions sv n_idx)]))

        "_TEMPLATE_absolute_tolerances;"
        (tc/emit-body
         (map-indexed (fn [i sv]
                        [:set
                         ['NV_Ith_S '___abstol i]
                         (absolute-tolerance sv)])
                      sv-order))

        "_TEMPLATE_load_sv;"
        (tc/emit-body load-sv)

        "_TEMPLATE_load_shared_sv;"
        (tc/emit-body
         (map-indexed (fn [i [vname vvalue]]
                        [:set
                         'const 'double vname
                         (vvalue
                          (fn sdata [sv n_idx]
                            (tc/emit ['NV_Ith_S
                                      [:aget '___ys n_idx]
                                      (.indexOf (vec sv-order) sv)])))])
                      shared-algebra-assignment))

        "_TEMPLATE_algebra_bindings;"
        (tc/emit-body
         (map (fn [[an av]]
                [:set 'const 'double (emit an) (emit av)])
              algebra-bindings))

        #_"_TEMPLATE_save_algebra;"
        #_(tc/emit-body
         (map-indexed
          (fn [i [an _]]
            [:set ['NV_Ith_S '___algebra_cache i]
             (emit an)])
          algebra-bindings))

        #_"_TEMPLATE_load_algebra;"
        #_(tc/emit-body
         (map-indexed
          (fn [i [an _]]
            [:set 'const 'double (emit an)
             ['NV_Ith_S '___algebra_cache i]])
          algebra-bindings))

        "_TEMPLATE_differential_bindings;"
        (tc/emit-body
         (map-indexed (fn [i sv]
                        [:set
                         ['NV_Ith_S '___ydot i]
                         (emit (differential-exp-map sv))])
                      sv-order))

        "_TEMPLATE_jacobian_bindings;"
        (let [alternated-jacobian-map
              (reduce-kv (fn [ret sv-of sv-by->e]
                           (reduce-kv (fn [ret' sv-by e]
                                        (update ret'
                                                sv-by
                                                assoc
                                                sv-of
                                                e))
                                      ret
                                      sv-by->e))
                         {}
                         jacobian-map)]
          (tc/emit-body
           (mapcat (fn [j diff-by-sv]
                     (let [col_j (str "_col_" j)
                           diff-of-sv->e (alternated-jacobian-map diff-by-sv)]
                       (into [[:set
                               'double (str "*" col_j)
                               ['DENSE_COL '___J j]]]
                             (for [[i diff-of-sv] (map-indexed vector sv-order)
                                   :let [e (diff-of-sv->e diff-of-sv)]
                                   :when (not (and (number? e)
                                                   (zero? e)))]
                               [:set
                                [:aget col_j i]
                                (emit e)]))))
                   (range)
                   sv-order)))

        "_TEMPLATE_igb_init;"
        (tc/emit-body
         (into [[:declare 'int '___elements_written]]
               (mapcat (fn [i sv]
                         (let [fptr (str "___" sv "_fptr")
                               filename (pr-str (str sv "." label ".igb"))]
                           [[:declare 'FILE '* fptr]
                            [:declare 'if "(!(" [:set fptr ['fopen filename "\"wb\""]] "))"
                             [:block [['printf (pr-str "Can't open file %s\n") filename]
                                      [:declare 'return 1]]]]
                            [:set '___elements_written ['fwrite '___header 1 1024 fptr]]]))
                       (range)
                       sv-order)))

        "_TEMPLATE_on_error;"
        (tc/emit-body
         (map-indexed (fn [i sv]
                        (let [fptr (str "___" sv "_fptr")]
                          ['fprintf
                           'stderr
                           (pr-str (str sv " %.17g\n"))
                           ['NV_Ith_S [:aget '___ys 'n_idx] i]]))
                      sv-order))

        "_TEMPLATE_debug_sv;"
        (tc/emit-body
         (into
          [['fprintf
            'stderr
            (pr-str (str "t %.17g\n"))
            '___t]
           ['fprintf
            'stderr
            (pr-str (str "n_idx %d\n"))
            'n_idx]]
          (map-indexed (fn [i sv]
                         (let [fptr (str "___" sv "_fptr")]
                           ['fprintf
                            'stderr
                            (pr-str (str sv " %.17g\n"))
                            ['NV_Ith_S [:aget '___ys 'n_idx] i]]))
                       sv-order)))

        "_TEMPLATE_debug_start"
        (str debug-start)
        
        "_TEMPLATE_debug_node"
        (str debug-node)

        "_TEMPLATE_debug_switch;"
        (if debug-switch
          "#define DEBUG_SV"
          "")

        "_TEMPLATE_write_data;"
        (tc/emit-body
         (map-indexed (fn [i sv]
                        (let [fptr (str "___" sv "_fptr")]
                          ['fwrite
                           ['&NV_Ith_S [:aget '___ys 'n_idx] i]
                           1 8 fptr]))
                      sv-order))
        "_TEMPLATE_close_igb;"
        (tc/emit-body
         (map (fn [sv]
                ['fclose (str "___" sv "_fptr")])
              sv-order))})))})

(def emit-banded-solver-code-fns
  {:cvode.emit/sv-name-order
   (tf/fm
    [:dotmodel/all-state-vars]
    (sort all-state-vars))
   :cvode.emit/sv-name->idx
   (tf/fm
    [:cvode.emit/sv-name-order]
    (-> (reduce (fn [ret [i sv]]
                  (assoc! ret sv i))
                (transient {})
                (map-indexed vector sv-name-order))
        persistent!))
   :cvode.emit/sv-name->msv
   (tf/fm
    [:integrator/mapped-sv]
    (reduce (fn [ret msv]
              (update ret
                      (-> msv
                          :children
                          first
                          :name)
                      (fn [s]
                        (conj (or s #{})
                              msv))))
            {}
            mapped-sv))
   :cvode/c-code
   (tf/fm
    [:cvode.emit/sv-name-order
     :cvode.emit/sv-name->idx
     :cvode.emit/sv-name->msv
     :simulations/compute-initial-conditions
     :simulations/parameters
     :cvode/relative-tolerance
     :cvode/absolute-tolerance
     :integrator/algebra-bindings
     :integrator/differential-exp-map
     :integrator/jacobian-map
     :simulations/num-nodes
     :simulations/dt
     :simulations/num-time-steps
     :simulations/iterations-per-record
     :simulations/label
     :simulations/igb-header-string
     :simulations/constrained-vars
     :simulations/save-algebra
     :simulations/debug-switch
     :simulations/debug-start
     :simulations/debug-node]
    (let [emit (ce/c-emitter
                {"map"
                 (fn [emit]
                   (fn [{:keys [children]}]
                     (let [[sv local-idx] children]
                       (tc/emit ['NV_Ith_S '___y (str (emit local-idx) "*___NEQ+" (sv-name->idx (:name sv)))]))))}
                {"ln" "log"})
          load-sv (vec
                   (map-indexed (fn [i sv]
                                  [:set
                                   'const 'double sv
                                   (if-let [z0 (constrained-vars sv)]
                                     (let [raw-sv (tc/emit ['NV_Ith_S '___y (str "n_idx*___NEQ+" i)])]
                                       (str "(" raw-sv "<" z0 ") ? " z0 " : " raw-sv))
                                     ['NV_Ith_S '___y (str "n_idx*___NEQ+" i)])])
                                sv-name-order))]
      (dj/replace-map
       (dc/resource-as-str "dj.simulations/templates/banded_cvode.c")
       {"_TEMPLATE_num_vars"
        (str (count sv-name-order))

        "_TEMPLATE_num_nodes"
        (str num-nodes)

        "_TEMPLATE_dt"
        (str dt)

        "_TEMPLATE_num_time_steps"
        (str num-time-steps)

        "_TEMPLATE_iterations_per_record"
        (str iterations-per-record)

        "_TEMPLATE_relative_tolerance"
        (str relative-tolerance)

        "_TEMPLATE_igb_header_string"
        (pr-str igb-header-string)

        "_TEMPLATE_parameters;"
        (apply str
         (mapcat (fn [v_idx [vname vvalue]]
                   (if (number? vvalue)
                     (str (tc/emit [:declare "#define" (str vname "_parameter") vvalue]) "\n")
                     (let [array-name (str vname "_array")]
                       (str (tc/emit [:declare "#define" (str vname "_parameter") [:aget array-name 'n_idx]]) "\n"
                            (tc/emit `[:declare ~'double ~(str array-name "[]") "="
                                       "{" ~@(interpose "," (map vvalue 
                                                                 (range num-nodes))) "}"]) ";\n"))))
                 (range)
                 parameters))

        "_TEMPLATE_initial_conditions;"
        (tc/emit-body
         (for [[i sv] (map-indexed vector sv-name-order)
               n_idx (range num-nodes)]
           [:set
            ['NV_Ith_S '___y (str n_idx "*___NEQ+" i)]
            (compute-initial-conditions sv n_idx)]))

        "_TEMPLATE_absolute_tolerance"
        (tc/emit-body
         (map-indexed (fn [i sv]
                        [:set
                         ['NV_Ith_S '___abstol (str "n_idx*___NEQ+" i)]
                         (absolute-tolerance sv)])
                      sv-name-order))

        "_TEMPLATE_load_sv;"
        (tc/emit-body load-sv)

        "_TEMPLATE_algebra_bindings;"
        (tc/emit-body
         (map (fn [[an av]]
                [:set 'const 'double (emit an) (emit av)])
              algebra-bindings))

        "_TEMPLATE_differential_bindings;"
        (tc/emit-body
         (map-indexed (fn [i sv]
                        [:set
                         ['NV_Ith_S '___ydot (str 'n_idx "*___NEQ+" i)]
                         (emit (differential-exp-map sv))])
                      sv-name-order))

        "_TEMPLATE_jacobian_bindings;"
        (let [alternated-jacobian-map
              (reduce-kv (fn [ret sv-of sv-by->e]
                           (reduce-kv (fn [ret' sv-by e]
                                        (update ret'
                                                sv-by
                                                assoc
                                                sv-of
                                                e))
                                      ret
                                      sv-by->e))
                         {}
                         jacobian-map)]
          (tc/emit-body
           (mapcat (fn [j diff-by-sv]
                     (let [col_j (str "___col_" j)
                           nid_sv_j (str "___nid_sv_" j)
                           diff-of-sv->e (alternated-jacobian-map diff-by-sv)]
                       (-> [[:set 'long nid_sv_j (str "___NEQ*n_idx+" j)]
                               [:set
                                'double (str "*" col_j)
                                ['BAND_COL '___J nid_sv_j]]]
                           (into 
                            (for [[i diff-of-sv] (map-indexed vector sv-name-order)
                                  :let [e (diff-of-sv->e diff-of-sv)]
                                  :when (not (and (number? e)
                                                  (zero? e)))]
                              [:set
                               ['BAND_COL_ELEM col_j (str "___nid_sv_0+" i) nid_sv_j]
                               (emit e)]))
                           (into
                            (for [msv (sv-name->msv diff-by-sv)
                                  diff-of-sv sv-name-order
                                  :let [e (-> (alternated-jacobian-map msv)
                                              (get diff-of-sv))]
                                  :when (not (and (number? e)
                                                  (zero? e)))]
                              (let [children (:children msv)
                                    msv-name (-> children
                                                 first
                                                 :name)
                                    of-n_idx (-> (ms/solve {:op "var"
                                                            :name "n_idx"}
                                                           (second children)
                                                           {:op "var"
                                                            :name "n_idx"})
                                                 first
                                                 emit)

                                    by-sv-i (sv-name->idx msv-name)]
                                [:set
                                 ['BAND_COL_ELEM col_j (str by-sv-i "+" of-n_idx "*___NEQ") nid_sv_j]
                                 (-> (ms/walk-replace e
                                                      msv {:op "var"
                                                           :name diff-of-sv})
                                     emit)]))))))
                   (range)
                   sv-name-order)))

        "_TEMPLATE_igb_init;"
        (tc/emit-body
         (into [[:declare 'int '___elements_written]]
               (mapcat (fn [i sv]
                         (let [fptr (str "___" sv "_fptr")
                               filename (pr-str (str sv "." label ".igb"))]
                           [[:declare 'FILE '* fptr]
                            [:declare 'if "(!(" [:set fptr ['fopen filename "\"wb\""]] "))"
                             [:block [['printf (pr-str "Can't open file %s\n") filename]
                                      [:declare 'return 1]]]]
                            [:set '___elements_written ['fwrite '___header 1 1024 fptr]]]))
                       (range)
                       (concat sv-name-order save-algebra))))
        
        "_TEMPLATE_on_error;"
        (tc/emit-body
         (map-indexed
          (fn [i sv]
            (let [fptr (str "___" sv "_fptr")]
              ['fprintf
               'stderr
               (pr-str (str sv " %.17g\n"))
               ['NV_Ith_S '___y (str "n_idx*___NEQ+" i)]]))
          sv-name-order))

        "_TEMPLATE_debug_sv;"
        (tc/emit-body
         (into
          [['fprintf
            'stderr
            (pr-str (str "t %.17g\n"))
            '___t]
           ['fprintf
            'stderr
            (pr-str (str "n_idx %d\n"))
            'n_idx]]
          (map-indexed (fn [i sv]
                         (let [fptr (str "___" sv "_fptr")]
                           ['fprintf
                            'stderr
                            (pr-str (str sv " %.17g\n"))
                            ['NV_Ith_S [:aget '___ys 'n_idx] i]]))
                       sv-name-order)))

        "_TEMPLATE_debug_start"
        (str debug-start)
        
        "_TEMPLATE_debug_node"
        (str debug-node)

        "_TEMPLATE_debug_switch;"
        (if debug-switch
          "#define DEBUG_SV"
          "")

        "_TEMPLATE_write_data;"
        (tc/emit-body
         (concat load-sv
                 (map (fn [[an av]]
                        [:set 'const 'double (emit an) (emit av)])
                      algebra-bindings)
                 (map-indexed
                  (fn [i av]
                    (let [fptr (str "___" av "_fptr")]
                      ['fwrite
                       (str "&" av)
                       1 8 fptr]))
                  save-algebra)
                 (map-indexed
                  (fn [i sv]
                    (let [fptr (str "___" sv "_fptr")]
                      ['fwrite
                       ['&NV_Ith_S '___y (str 'n_idx "*___NEQ+" i)]
                       1 8 fptr]))
                  sv-name-order)))
        "_TEMPLATE_close_igb;"
        (tc/emit-body
         (map (fn [sv]
                ['fclose (str "___" sv "_fptr")])
              sv-name-order))})))})

(def debug-defaults
  {:simulations/debug-switch false
   :simulations/debug-start 0
   :simulations/debug-node 0})

(def simulate-fns
  {;; iteration data
   :simulations/iterations-per-record
   (tf/fm
    [:simulations/record-dt :simulations/dt]
    (long (/ record-dt
             dt)))

   ;; iteration data
   :simulations/num-time-steps
   (tf/fm
    [:simulations/end-time :simulations/dt :simulations/iterations-per-record]
    (+ (long (/ end-time
                dt))
       iterations-per-record))

   ;; iteration data
   :simulations/num-record-time-steps
   (tf/fm
    [:simulations/num-time-steps :simulations/iterations-per-record]
    (/ num-time-steps
       iterations-per-record))

   :simulations/compute-initial-conditions
   (tf/fm
    [:simulations/parameters
     :dotmodel/initial-conditions-assignment]
    (let [parameters-map (reduce (fn [m [k v]]
                                   (assoc m
                                          k v))
                                 {}
                                 parameters)
          default-ic (reduce (fn [ret [id v]]
                               (assoc ret
                                      id (Double/parseDouble (dj/substring v
                                                                           0
                                                                           -1))))
                             {}
                             initial-conditions-assignment)]
      (fn [id node-id]
        (let [default-val (if-let [result (default-ic id)]
                            result
                            (double 0))]
          (if-let [setting (parameters-map id)]
            (if (number? setting) ;; Dispatch on value type, maps correspond to vary on node-id
              (double setting)
              (double (setting node-id)))
            default-val)))))

   :simulations/num-parameters
   (tf/fm
    [:simulations/parameters]
    (count parameters))

   :simulations/num-state-vars
   (tf/fm
    [:dotmodel/all-state-vars]
    (count all-state-vars))

   :simulations/shared-vars
   (tf/fm
    [:dotmodel/shared-vars]
    (or shared-vars [0.0]))

   :simulations/num-shared-vars
   (tf/fm
    [:simulations/shared-vars]
    (count shared-vars))

   :simulations/num-track-vars
   (tf/fm
    [:dotmodel/track-vars]
    (count track-vars))

   :simulations/igb-header-string
   (tf/fm
    [:simulations/num-nodes
     :simulations/num-record-time-steps]
    (i/emit-igb-header-str {:x num-nodes :type "double" :t num-record-time-steps}))})
