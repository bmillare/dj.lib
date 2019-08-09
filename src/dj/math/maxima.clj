(ns dj.math.maxima
  (:require [clojure.java.shell :as cjs]
            [dj.math.parser :as dmp]
            [dj.math.cemit]
            [dj.dispatch.plurality :as dp]
            [dj.algorithms.peg :as ap]
            [dj.dispatch.graphfn :as gf]))

(let [expr (-> dmp/parse-base
               (assoc
                :mult-expr
                (gf/fn #{mult-div infix-couple} #{exp-expr}
                  (ap/alt (ap/s exp-expr
                                (ap/*
                                 (ap/s mult-div
                                       exp-expr)))
                          (infix-couple "*" "/")))
                :id (gf/fn #{} #{}
                      (ap/t #"%?\p{Alpha}\w*"))
                :exp-expr (gf/fn #{carrot infix-node} #{atom}
                            (ap/alt (ap/s atom
                                          (ap/*
                                           (ap/s carrot
                                                 atom)))
                                    infix-node)))
               (gf/mutual-graphfn :expr
                                  {:expr :cond-expr})
               :expr
               deref)]
  (defn parse [txt]
    (ap/parse expr txt)))

(defn call-maxima* [commands]
  (cjs/sh "maxima"
          "--very-quiet"
          :in commands))

(defn call-maxima [commands]
  (let [{:keys [out err]} (call-maxima* (str "display2d:false;" commands))]
    (if (empty? err)
      (-> (.split #"\n" out)
          (->> (drop 2)
               (apply str)))
      (throw (ex-info "calling maxima failed"
                      (dj.repl/local-context))))))

(def default-settings "display2d:false$ratprint:false$")

;; $ terminates command but also silences, need to figure out way to remove comments

(def emit
  (dp/update-implementation (dj.math.cemit/c-emitter {"exp" "exp"})
                            assoc
                            "pow"
                            (fn [emit]
                              (fn [{:keys [op children]}]
                                (let [[base exponent] children]
                                  (str (emit base)
                                       "^"
                                       (emit exponent)))))
                            java.lang.Long
                            (fn [emit]
                              (fn [x]
                                (str x)))))

#_ (do
     (call-maxima "diff(x^2,x);")
     (let [ce (dj.math.cemit/c-emitter {"exp" "exp"})]
       ce)
     "(Vmuni_ktrans * Cai * (FRT2 * ( Dpsi - 91.0 )) * Cai_ktrans_plus1_p3 / ((Cai_ktrans_plus1_p3 * Cai_ktrans_plus1 + L / pow((float)(1.0 + Cai * inv_kact), na) ) * (1.0 - exp(-(FRT2 * ( Dpsi - 91.0 ))))));"
     
     
     "((Cai*Cai_ktrans_plus1_p3*pow(((Cai*inv_kact)+1.0),na)*Vmuni_ktrans)/(L+(Cai_ktrans_plus1*Cai_ktrans_plus1_p3*pow(((Cai*inv_kact)+1.0),na))))"
     "((Cai*Cai_ktrans_plus1_p3*pow(((Cai*inv_kact)+1.0),na)*Vmuni_ktrans)/(L+(Cai_ktrans_plus1*Cai_ktrans_plus1_p3*pow(((Cai*inv_kact)+1.0),na))))"
     "((Cai*Cai_ktrans_plus1_p3*pow(((Cai*inv_kact)+1.0),na)*Vmuni_ktrans*FRT2)/(2.0*(L+(Cai_ktrans_plus1*Cai_ktrans_plus1_p3*pow(((Cai*inv_kact)+1.0),na)))))"

     ;; Need to expand out algebra so that variable that approaches singular value is explicit.
     ;; Want to specify simply the variable in question, the singular value to take the limit, and the range we want to extrapolate over
     
     "((Vmuni_ktrans*Cai*FRT2_Dpsi*Cai_ktrans_plus1_p3)/(((Cai_ktrans_plus1_p3*Cai_ktrans_plus1)+(L/(1.0+(Cai*inv_kact))^na))*(1.0-exp((-FRT2_Dpsi)))))"

     )
