;; #section:1 {:tags [:natalia/project1] :created "2016-06-11"}
(ns dj.simulations.carp.output.igb.image
  (:require [dj.dispatch.treefn :as tf]
            [dj.image.png :as png]
            [dj.template.html :as html]
            [dj.template.sgml :as sgml]
            [dj.template.css :as css]))

(def igb-list-view-fms
  {:igb.image/image-info
   (tf/fm [:mesh/width
           :mesh/height]
          (png/image-info width height))
   :igb.image/palette
   (tf/fm
    [:igb.analysis.input/color-map
     :igb.image/image-info]
    (png/make-palette ^ar.com.hjg.pngj.ImageInfo image-info
                      color-map))
   :igb.analysis/show-times
   (tf/fm
    [:igb.analysis.input/paces
     :igb.analysis.input/points]
    (for [pace paces
          point points]
      (+ point (* pace 1000))))
   :igb.image/write-image
   (tf/fm
    [:igb.analysis.input/upper-limit
     :igb.analysis.input/lower-limit
     :igb.analysis.input/color-map

     :igb.analysis.input/image-folder

     :igb/time->frame
     :mesh/width
     :mesh/height
     :igb.analysis/show-times
     :igb.image/image-info
     :igb.image/palette]
    (let [upper-limit (double upper-limit)
          lower-limit (double lower-limit)
          span (- upper-limit lower-limit)
          largest-index (dec (count color-map))
          _ (when (> largest-index 255)
              (throw (ex-info "largest-index is greater than what byte can store (255)"
                              {:largest-index largest-index})))

          image-line (png/image-line image-info)
          image-line-raw (.getScanlineByte image-line)
          width (long width)
          height (long height)]
      (when-not (dj.io/exists? image-folder)
        (dj.io/mkdir image-folder))
      (loop [ts show-times]
        (let [t (first ts)]
          (when t
            (let [f (time->frame t)
                  os (java.io.FileOutputStream. (dj.io/file image-folder (str t ".png")))
                  w (png/indexed-png-writer os
                                            palette
                                            image-info)]
              (loop [x 0
                     y 0]
                (if (< x width)
                  (if (< y height)
                    (let [v (unchecked-double (.get ^java.nio.FloatBuffer f (int (+ x (* width (- (dec height) y))))))]
                      (aset image-line-raw
                            x
                            (unchecked-byte
                             (if (> v upper-limit)
                               largest-index
                               (if (< v lower-limit)
                                 0
                                 (* largest-index
                                    (/ (- v lower-limit)
                                       span))))))
                      (recur (inc x)
                             y))
                    (.end w))
                  (do
                    (.writeRow w image-line (int y))
                    (recur 0 (inc y))))))
            (recur (rest ts)))))))
   :igb.image/viewer
   (tf/fm
    [:igb.image/write-image
     :igb.analysis.input/paces
     :igb.analysis.input/points
     :igb.analysis.input/html-file
     :igb.analysis.input/image-folder]
    (-> (conj html/basic
              (into [:body {}]
                    (for [pace paces]
                      [:div {}
                       [:h3 {} (inc pace)]
                       (into [:div {}]
                             (for [point points]
                               [:span {:style (css/->style {:margin "10px"})}
                                (str point)]))
                       (into [:div {}]
                             (for [point points]
                               [:img {:src (str "file://" (dj.io/get-path image-folder) "/" (+ point (* pace 1000)) ".png")}]))])))
        sgml/emit
        (->> (dj.io/poop html-file))))})
