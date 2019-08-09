(ns dj.simulations.carp.input.poopers.mesh
  (:require [dj.simulations.carp.input.mesh :as mesh]
            [dj.simulations.scripting :as scripting]))

(defn regular-mesh-2d 
  [sys
   store-id
   m]
  (let [{:keys [width height resolution fiber-orientation sink-radius mesh:name]} m
        {:keys [points elements lon tris]}
        (mesh/regular-mesh-2d
         [width height]
         resolution
         "Tr"
         fiber-orientation
         (mesh/single-sink-classifier sink-radius
                                      [0 0]))]
    (scripting/poop-map
     (dj.io/file (:dj/store-path sys)
                 store-id)
     (let [mesh-name (or mesh:name
                         "mesh")]
       {"data.clj" m
        (str mesh-name ".pts") points
        (str mesh-name ".elem") elements
        (str mesh-name ".lon") lon
        (str mesh-name ".tris") tris}))))
