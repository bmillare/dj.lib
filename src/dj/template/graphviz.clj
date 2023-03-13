(ns dj.template.graphviz
  (:require [clojure.java.shell :as cjs]))

;; we prefer to work with clojure data not some DSL

;; each edge is represented as:
;; a pair vector of ids (keywords) from first to second
;; or for undirected, an pair set of ids

;; attributes

;; attribute map, hashmap of attribute-key -> attribute-value

;; node attributes
;; hashmap from id -> attribute-map

;; edge attributes
;; hashmap of vector/set pairs -> attribute-map

;; graph map

;; graph map has attributes, type (digraph or graph), and edges

#_ [{:type :graph
     :name :graphname
     :attributes {:graph {:size "1,1"}
                  :node {:a {:label "Foo"}
                         :b {:shape :box}}
                  :edge {#{:a :b} {:color :blue}
                         #{:b :c} {:color :blue}
                         #{:b :d} {:style :dotted}}}
     :edges #{#{:a :b}
              #{:b :c}
              #{:b :d}}}
    {:type :digraph
     :name :graphname
     :attributes {:graph {:size "1,1"}
                  :node {:a {:label "Foo"}
                         :b {:shape :box}}
                  :edge {[:a :b] {:color :blue}
                         [:b :c] {:color :blue}
                         [:b :d] {:style :dotted}}}
     :edges #{[:a :b]
              [:b :c]
              [:b :d]}}]

(defn get-label [id]
  (if (string? id)
    id
    (str (if (namespace id)
               (str (namespace id) "/")
               "")
             (clojure.core/name id))))

(defn emit-graph-attributes [attributes]
  (apply str
         (for [[k v] attributes]
           (str (get-label k)
                "="
                (if (keyword? v)
                  (get-label v)
                  (str "\"" v "\""))
                ";"))))

(defn emit-vector-attributes [attributes]
  (str (apply str
              "["
              (interpose " "
                         (for [[k v] attributes]
                           (str (get-label k)
                                "="
                                (if (keyword? v)
                                  (get-label v)
                                  (str "\"" v "\""))))))
       "]"))

(defn graphviz
  "
Convert clojure data representation of a graphviz file to the graphviz format
  
  {:type :graph
     :name :graphname
     :attributes {:graph {:size \"1,1\"}
                  :node {:a {:label \"Foo\"}
                         :b {:shape :box}}
                  :edge {#{:a :b} {:color :blue}
                         #{:b :c} {:color :blue}
                         #{:b :d} {:style :dotted}}}
     :edges #{#{:a :b}
              #{:b :c}
              #{:b :d}}}
    {:type :digraph
     :name :graphname
     :attributes {:graph {:size \"1,1\"}
                  :node {:a {:label \"Foo\"}
                         :b {:shape :box}}
                  :edge {[:a :b] {:color :blue}
                         [:b :c] {:color :blue}
                         [:b :d] {:style :dotted}}}
     :edges #{[:a :b]
              [:b :c]
              [:b :d]}}"
  [graph]
  (let [{:keys [type name attributes edges]} graph
        edge-seperator ({:digraph "->"
                         :graph "--"} type)]
    (str (clojure.core/name type) " " (clojure.core/name name) "{"
         (emit-graph-attributes (:graph attributes))
         (apply str (for [[nid attrs] (:node attributes)]
                      (str "\"" (get-label nid) "\"" (emit-vector-attributes attrs) ";")))
         (apply str (for [e edges]
                      (str "\"" (get-label (first e)) "\""
                           edge-seperator
                           "\""  (get-label (second e)) "\""
                           (if-let [edge (:edge attributes)]
                             (emit-vector-attributes (edge e)))
                           ";")))
         "}")))

(defn emit
  "calls graphviz to produce a svg string"
  ([graphviz-format]
     (emit graphviz-format "neato"))
  ([graphviz-format processor]
     (:out (cjs/sh processor "-Tsvg" :in graphviz-format))))

(defn display!
  "like emit but displays the svg immediately

  types:
  dot -> hierarchical
  neato, fdp, sfdp (large) -> spring model
  twopi -> radial
  circo -> circular
  "
  [graphviz-format & [type]]
  (let [type' (or type "fdp")]
    (->> graphviz-format
         (clojure.java.shell/sh type' "-Tsvg" :in)
         :out
         (clojure.java.shell/sh "display" :in))))

(comment
  (-> {:type :digraph
       :name :graphname
       :attributes {:graph {:size "1,1"}
                    :node {:a {:label "Foo"}
                           :b {:shape :box}}
                    :edge {[:a :b] {:color :blue}
                           [:b :c] {:color :blue}
                           [:b :d] {:style :dotted}}}
       :edges #{[:a :b]
                [:b :c]
                [:b :d]}}
      graphviz
      emit
      (->> (spit "some-file.svg"))))
