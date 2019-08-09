(ns dj.simulations.carp.input.poopers.ode2
  (:require [dj.simulations.carp.translator :as translator]
            [dj.dispatch.treefn :as tf]
            [dj]))

;; see v.studies.2018.7.plot-ap for example
(def limpet-build-fms
  {:carp/limpet-path
   (tf/fm
    [:executor/carp-root-path]
    (dj/str-path carp-root-path
                 "bin/limpet_fe"))
   :carp/make-dynamic-path
   (tf/fm
    [:executor/carp-root-path]
    (dj/str-path carp-root-path
                 "bin/make_dynamic_model"))
   :limpet/model-def-filename
   (tf/fm
    [:model/name]
    (str name ".model"))

   :limpet/model-c-filename
   (tf/fm
    [:model/name]
    (str name ".c"))

   :limpet/run-limpet-txt
   (tf/fm
    [:executor/build-directory-path!
     :carp/limpet-path
     :limpet/model-def-filename]
    (str "cd "
         build-directory-path!
         " && "
         limpet-path " " model-def-filename))

   :limpet/alter-CVODE-tolerance
   (tf/fm
    [:limpet/c-txt]
    (dj/replace-map
     c-txt
     {"#include \"ION_IF.h\"" "#include \"ION_IF.h\"\n#include \"parallel.h\""
      "#define LIMPET_CVODE_RTOL 1e-5" "#define LIMPET_CVODE_RTOL 1e-7"}))

   :limpet/run-make-dynamic-txt
   (tf/fm
    [:executor/build-directory-path!
     :carp/make-dynamic-path
     :limpet/model-c-filename]
    (str "cd "
         build-directory-path!
         " && "
         make-dynamic-path " " model-c-filename))
   })
