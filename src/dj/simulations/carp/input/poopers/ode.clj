(ns dj.simulations.carp.input.poopers.ode
  (:require [dj.simulations.carp.translator :as translator]
            [dj.simulations.scripting :as scripting]
            [dj.store.clj :as clj]
            [dj.io]
            [dj]))

(defn tolerance-var-dump-cell-model
  [sys
   store-id
   args]
  (let [{:keys [cell-model:id
                executor:carp-root-path
                executor:store-path
                executor:handle]}
        args

        store-path (:dj/store-path sys)
        working-dir (dj.io/file store-path store-id)
        
        limpet-path (dj/str-path executor:carp-root-path
                                 "bin/limpet_fe")
        dynamic-path (dj/str-path executor:carp-root-path
                                  "bin/make_dynamic_model")
        {:keys [cell-model:name cell-model:filename]} (clj/read-from-id store-path cell-model:id "data.clj") 
        model-filename-dest (str cell-model:name ".model")
        c-filename (str cell-model:name ".c")
        c-file (dj.io/file store-path
                           store-id
                           c-filename)
        statuses (scripting/procedure-seq
                  [(fn []
                     (dj.io/cp (dj.io/file store-path
                                           cell-model:id
                                           cell-model:filename)
                               (dj.io/file store-path
                                           store-id
                                           model-filename-dest)))
                   (fn []
                     (scripting/sh-on executor:handle
                                      (str "cd "
                                           (dj/str-path executor:store-path
                                                        store-id)
                                           " && "
                                           limpet-path " " model-filename-dest)))
                   (fn []
                     (let [c-txt (dj.io/eat c-file)
                           {:keys [compute internal cvode-vars]} (translator/parse-dotmodel-c-global-data c-txt)
                           dump-wrap (fn [vars]
                                       (str "{
char filename[20];
FILE *fp;
sprintf(filename, \"dump_%d.clj\", get_rank());
fp = fopen(filename, \"a\");
"
                                            (translator/cvode-dump-state "fp" vars)
                                            "
fprintf(fp,\"\\n\");
fclose(fp);}
"))]
                       (dj.io/poop c-file
                                   (dj/replace-map c-txt
                                                   {"#include \"ION_IF.h\"" "#include \"ION_IF.h\"\n#include \"parallel.h\""
                                                    "#define LIMPET_CVODE_RTOL 1e-5" "#define LIMPET_CVODE_RTOL 1e-7"
                                                    "assert(CVODE_flag == CV_SUCCESS);" ""
                                                    "//Finish the update" (str "if (CVODE_flag != CV_SUCCESS) " (dump-wrap compute)
                                                                               "\n //Finish the update\n"
                                                                               "assert(CVODE_flag == CV_SUCCESS);\n")
                                                    "return 0;" (str "if (t > 12192.4 && __i == 345) "
                                                                     (dump-wrap (concat (map #(str "NV_Ith_S(CVODE,CVODE_" % ")")
                                                                                             cvode-vars)
                                                                                        internal))
                                                                     "\n return 0;")}))))
                   (fn []
                     (scripting/sh-on executor:handle
                                      (str "cd "
                                           (dj/str-path executor:store-path
                                                        store-id)
                                           " && "
                                           dynamic-path " " c-filename)))])
        result {:cell-model:name cell-model:name
                :cell-model:c-filename c-filename
                :cell-model:module-path (str cell-model:name ".so")
                :cell-model:build-statuses statuses}]
    (dj.io/poop (dj.io/file working-dir "data.clj")
                (pr-str result))
    result))
