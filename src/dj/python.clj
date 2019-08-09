(ns dj.python)

(defprotocol EmitPython
  "convert Clojure data to roughly equivalent python code"
  (-emit [x]))

(extend-protocol EmitPython
  clojure.lang.IPersistentMap
  (-emit [x]
    (str "{" (apply str (into []
                              (comp
                               (map (fn [[k v]]
                                      (str (-emit k)
                                           ":"
                                           (-emit v))))
                               (interpose ","))
                              x)) "}"))
  clojure.lang.Sequential
  (-emit [x]
    (str "[" (apply str (into []
                              (comp
                               (map -emit)
                               (interpose ","))
                              x)) "]"))
  Number
  (-emit [x]
    (str x))
  clojure.lang.Symbol
  (-emit [x]
    (str "'" x "'"))
  String
  (-emit [x]
    (pr-str x)))

#_ (-emit '{save_path "/home/username/tmp/test.svg"
            color_list ("#ff0000" "#ffff00" "#0000ff" "0.6" "c" "m")
            xlabel "x (cm)"
            ylabel "y (cm)"
            title "imshow2"
            cat_2_num {"no" 4 "dep" 3 "osc" 1}
            cmap_bounds [1.0 3.0 3.5 4.0]
            cbar_ticks [2.0 3.25 3.75]
            cat_idxs [1 3 4]
            z_cat [["no" "dep" "no"]
                   ["dep" "osc" "dep"]
                   ["dep" "osc" "dep"]
                   ["no" "dep" "no"]]
            xlist [-3. 0. 3.]
            ylist [-3. -1. 1. 3.]})
