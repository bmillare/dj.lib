(ns dj.simulations.carp.output.sv)

(defn sv-txt [{:keys [header
                      model-name
                      label->value
                      label-order]}]
  (let [header' (or header
                    "-89.0061            # Vm
-                   # Lambda
-                   # delLambda
-                   # Tension
-                   # K_e
-                   # Na_e
-                   # Ca_e
-                   # Iion
-                   # tension_component
-                   # light
")]
    (str header'
         model-name "\n"
         (apply str
                (for [label label-order]
                  (let [value (get label->value label)
                        value-length (count value)]
                    (str value (apply str (repeat (- 20 value-length)
                                                  " "))
                         "# "
                         label "\n")))))))
