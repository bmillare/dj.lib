(ns dj.parsers.bibtek
  (:require [dj.algorithms.peg :as peg]
            [dj.dispatch.graphfn :as gf]))

(let [not-at (peg/t #"[^@]*")
      at (peg/t #"[@]")
      word (peg/t #"(?U)(?:\w|-)+")
      open-brace (peg/t #"\{")
      close-brace (peg/t #"\}")
      non-braces (peg/t #"[^\{\}]+")
      comma (peg/t #",?")
      ws (peg/t #"\s*")
      equals (peg/t #"=")
      gfs {:content (gf/fn #{} #{content}
                      (peg/alt (peg/s open-brace (peg/*
                                                  (peg/| non-braces
                                                         content))
                                      close-brace)
                               (fn [[_ c _]]
                                 c)))
           :pair (gf/fn #{} #{content}
                   (peg/alt (peg/s word ws equals ws content comma ws)
                            (fn [[k _ _ _ c _ _]]
                              {k c})))
           :entry (gf/fn #{} #{pair}
                    (peg/alt (peg/s not-at at word open-brace word comma ws
                                    (peg/+ pair)
                                    ws
                                    close-brace)
                             (fn [[_ _ item-type _ handle _ _ pairs _ _]]
                               {handle
                                (apply merge
                                       {:item-type item-type}
                                       pairs)})))}
      entries (peg/alt (peg/+ (:entry (gf/mutual-graphfn gfs :entry)))
                       (fn [es]
                         (apply merge es)))]
  (defn parse [txt]
    (peg/parse entries txt)))

(defn re-title->handle [data re]
  (reduce-kv (fn [ret k v]
               (let [title
                     (-> (get v "title")
                         first
                         first)]
                 (if (re-find re
                              title)
                   (conj ret [k title])
                   ret)))
             []
             data))

(defn scrape_references->title [txt]
  (clojure.string/split txt #"\ 19\d\d\.|\ 20\d\d\."))

(comment
  (try
    (->
     (dj.io/file "/home/username/Documents/library.bib")
     (dj.io/eat)
     (parse)
     :result
     (re-title->handle #"Glutathione"))
    (catch Exception e
      :error))

  (->
     (dj.io/file "/home/username/tmp/2017/9/ref-handles.txt")
     (dj.io/eat)
     (scrape_references->title)
     rest
     (->> (mapv (fn [s]
                  (subs s 1 30))))
     )

  (let [result (->
                (dj.io/file "/home/username/Documents/library.bib")
                (dj.io/eat)
                (parse)
                :result)]
    (doseq [[n name] (mapv (fn [n
                                search]
                             [n (re-title->handle result
                                                  (re-pattern (java.util.regex.Pattern/quote search)))
                              #_(first
                               (map first
                                    (re-title->handle result
                                                      (re-pattern (java.util.regex.Pattern/quote search)))))])
                           (range 1 100)
                           ["ATP consumption by uncoupled"
                            "Percolation and criticality"
                            "Linking flickering to waves"
                            "Myocardial Substrate Metabolism"
                            "Cardiac system bioenergetics"
                            "Model for cascading failures"
                            "Glutathione oxidation as a tr"
                            "Cardiac arrhythmias induced b"
                            "Optical imaging of mitochondr"
                            "Cardiac mitochondrial network"
                            "Singlet oxygen-induced arrhyt"
                            "Distinct roles of autophagy i"
                            "A mitochondrial oscillator de"
                            "Reactive oxygen species (ROS)"
                            "A reaction-diffusion model of"
                            "Mitochondrial oscillations an"
                            "Synchronized Whole Cell Oscil"
                            "Integrating mitochondrial ene"
                            "Hydroperoxide metabolism in m"
                            "Thiol chemistry and specifici"
                            "Mitochondrial criticality: A "
                            "Mitochondrial free radical ge"
                            "Mitochondrial network determi"
                            "Hydrogen peroxide as a paracr"
                            "Cardiac mitochondria exhibit "
                            "From Kuramoto to Crawford: ex"
                            "Modeling the mechanism of met"
                            "A wave of reactive oxygen spe"
                            "Fluctuations in mitochondrial"
                            "Mitochondria are sources of m"
                            "Effects of regional mitochond"])]
      (println n name)))






  )
