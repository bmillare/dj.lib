(ns dj.simulations.carp.input.par
  (:require [dj.algorithms.peg :as ap]))

(defn gvec
  "num-regions: number of regions
sv-out: vector of strings of state-variables

returns the mesh.par formatting of gvec and the svars
"
  [num-regions sv-out]
  (let [num-gvecs (count sv-out)]
    (apply
     str
     (interpose
      "\n"
      (apply
       concat
       [(str "num_gvecs = " num-gvecs)]
       (for [i (range num-gvecs)]
	 (concat
	  [(str "gvec[" i "].name = \"G_" (nth sv-out i) "\"")]
	  (for [j (range num-regions)]
	    (str "gvec[" i "].ID[" j "] = \""
		 (nth sv-out i) "\"")))))))))

(defn adjustments [v->f]
  (if (empty? v->f)
    ""
    (str "num_adjustments = " (count v->f) "\n"
         (apply str (map (fn [n [v f]]
                           (str "adjustment[" n "].variable = \"" v "\"\n"
                                "adjustment[" n "].file = \"" f "\"\n"))
                         (range (count v->f))
                         v->f)))))

(defn region-parameters
  "given a map of parameter names -> values, returns a string of the
  form <name>=<value>,..."
  [m]
  (apply str
	 (interpose ","
		    (map (fn [[k v]]
			   (str k "=" v))
			 m))))

(defn tsav
  "produces par data to include save states"
  [times]
  (if (empty? times)
    ""
    (let [times:count (count times)]
      (apply str
             (into [(str "num_tsav = " times:count "\n")]
                   (map (fn [t idx]
                          (str "tsav[" idx "] = " t "\n"))
                        times
                        (range times:count)))))))

(defn parse-par-content [txt]
  (let [whitespace (ap/t #"\s*")
        lhs (ap/t #"\S+")
        eq (ap/t #"=")
        rhs (ap/t #"[^\n]+")
        newline (ap/t #"\n")
        pair (ap/alt (ap/s whitespace lhs whitespace eq whitespace rhs newline)
                     (fn [[_ k _ _ _ v _]]
                       (list k (read-string v))))
        pairs (ap/alt (ap/+ pair)
                      (fn [ps]
                        (persistent!
                         (reduce (fn [ret [k v]]
                                   (assoc! ret k v))
                                 (transient {})
                                 ps))))]
    (:result (ap/parse pairs txt))))
