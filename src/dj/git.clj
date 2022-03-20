(ns dj.git
  (:require [dj.shell :as sh]))

(def ^:dynamic *pretend?* false)

(defn sh [args]
  (let [pretend? *pretend?*]
    (if pretend?
      (println args)
      (sh/sh args))))

(defn diff [{:keys [target-dir
                    src-branch
                    dest-branch]}]
  (sh ["git" "diff" (str src-branch ".." dest-branch)
       :dir target-dir]))

(defn checkout-branch
  "checkout branch with same name as remote"
  [{:keys [target-dir
           branch]}]
  (sh ["git" "checkout"
       "-b" branch
       (str "origin/" branch)
       :dir target-dir]))

(comment
  (def branches [#_ "0.90-1"
                 #_ "0.90-2"
                 #_ "0.90-3"
                 "1.0-1"
                 "1.2-1"
                 "1.2-2"])
  (doseq [branch branches]
    )
  (binding [*pretend?* true]
    (doseq [branch branches]
      (checkout-branch {:target-dir "/home/bmillare/Software/source/sagemaker-xgboost-container"
                        :branch branch})))

  (binding [*pretend?* false #_ true]
    (def results
      (doall
       (for [branch branches]
         (diff {:target-dir "/home/bmillare/Software/source/sagemaker-xgboost-container"
                :src-branch "master"
                :dest-branch branch})))))

  (doseq [[n branch] (map-indexed vector branches)]
    (let [fname (str "/home/bmillare/tmp/branch_" branch ".diff")]
      (println fname)
      (-> results
          (nth n)
          :out
          (->> (spit fname)))))
  
  )
