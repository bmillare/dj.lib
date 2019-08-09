(ns dj.code.analyzer
  (:require [dj.dispatch.plurality]
            [dj.template.graphviz]
            [clojure.java.shell :as sh]))

(defn file->forms [f]
  (-> (str "["
           (-> f
               dj.io/eat)
           "]")
      read-string))

(def dispatch-fn
  (fn [accum node]
    (if (map? node)
      :hash-map
      (if (coll? node)
        :sequential
        (if (symbol? node)
          :symbol
          :default)))))

(def default-get-ns-methods
  {:hash-map (fn [get-namespaces]
               (fn [accum children]
                 (reduce-kv (fn [ret k v]
                              (get-namespaces (get-namespaces ret k) v))
                            accum
                            children)))
   :sequential (fn [get-namespaces]
                 (fn [accum children]
                   (reduce (fn [ret v]
                             (get-namespaces ret v))
                           accum
                           children)))
   :symbol (fn [get-namespaces]
             (fn [accum node]
               (if-let [n (namespace node)]
                 (conj accum
                       n)
                 accum)))
   :default (fn [get-namespaces]
              (fn [accum _]
                accum))})

(let [get-namespaces
      (dj.dispatch.plurality/->recursive-simple-multi-fn
       default-get-ns-methods
       dispatch-fn)]

  (defn forms->ns-used
    "given vector of forms, returns mapping of top-level vars to what
  namespaces they use"
    [forms]
    (into []
          (comp
           (filter (fn [args] ;TODO: use walk.
                     (#{'defn 'def 'extend-type 'defonce} (first args))))
           (map (fn [[_ identifier & args]]
                  [identifier (let [d (get-namespaces #{} args)]
                                (if (empty? d)
                                  #{'none}
                                  d))])))
          forms)))

(defn forms->current-ns-vars-used
  "returns mapping of top-level vars to other vars in the current
  namespace"
  [forms deps]
  (let [all-ids (set (map first deps))
        get-namespaces (dj.dispatch.plurality/->recursive-simple-multi-fn
                        (assoc default-get-ns-methods
                               :symbol (fn [get-namespaces]
                                         (fn [accum node]
                                           (if-let [n (all-ids (symbol (name node)))]
                                             (conj accum
                                                   n)
                                             accum))))
                        dispatch-fn)]
    (into []
          (comp
           (filter (fn [args]
                     (#{'defn 'def 'extend-type} (first args))))
           (map (fn [[_ identifier & args]]
                  ;; this part is different from above, basically if its empty, put a place holder to group them together
                  [identifier (get-namespaces #{} args)])))
          forms)))

(defn graph!
  "make and display fdp graphviz graph from map of node-id -> neighbor-ids"
  [deps]
  (-> {:type :digraph
       :name :codegraph
       :attributes {:graph {:size "15,15"
                            :splines :true}}
       :edges (reduce (fn [ret [i ds]]
                        (clojure.set/union ret
                                           (set (doall
                                                 (for [d ds]
                                                   [(keyword i)
                                                    (keyword d)])))))
                      #{}
                      deps)}
      (dj.template.graphviz/graphviz)
      (->> (sh/sh "fdp" "-Tsvg" :in)
           :out
           (sh/sh "display" :in))))

(defn graphviz-vars-use-these-ns
  "graphviz for current namespace what local vars use what vars and
  namespaces"
  [deps var-deps]
  (let [namespaces (apply clojure.set/union (map second deps))]
    (-> {:type :digraph
         :name :codegraph
         :attributes {:graph {:size "15,15"
                              :splines :true}
                      :node (reduce (fn [ret n]
                                      (assoc ret
                                             n
                                             {:color "blue"
                                              :style "bold"}))
                                    {}
                                    namespaces)}
         :edges (clojure.set/union
                 (reduce (fn [ret [i ds]]
                           (clojure.set/union ret
                                              (set (doall
                                                    (for [d ds]
                                                      [(keyword i)
                                                       (keyword d)])))))
                         #{}
                         var-deps)
                 (reduce (fn [ret [i ds]]
                           (clojure.set/union ret
                                              (set (doall
                                                    (for [d ds]
                                                      [(keyword i)
                                                       (keyword d)])))))
                         #{}
                         deps))}
        (dj.template.graphviz/graphviz)
      
        )))

(defn call-graphviz [g type]
  (->> g
       (sh/sh type "-Tsvg" :in)
       :out))

(defn display!
  "immediately display graphviz template with form

  types: dot:hiearchical, neato:spring, fdp:spring, sfdp:large-spring,
  twopi:radial, circo:circular"
  [g type]
  (->> g
       (sh/sh type "-Tsvg" :in)
       :out
       (sh/sh "display" :in)))

(defn vars-use-these-ns! [f type]
  (let [fs (file->forms f)
        d (forms->ns-used fs)
        vd (forms->current-ns-vars-used fs d)
        g (graphviz-vars-use-these-ns d vd)]
    (display! g type)))

(defn display2!
  "immediately display graphviz template with form

  types: dot:hiearchical, neato:spring, fdp:spring, sfdp:large-spring,
  twopi:radial, circo:circular"
  [g type]
  (->> g
       (sh/sh type "-Tplain" :in)
       :out))

;; see /home/username/store/code-countries/scratch/xml_example.clj for
;; sample clojure.xml/parse output

;; eventually write to:
;; - /home/username/store/code-countries/1/code/analyzer/navigation.clj

;; see for batik reference
;; /home/username/store/code-countries/1/gui/svg.clj

;; How to add onclick
;; http://batik.2283329.n4.nabble.com/Handling-SVG-element-onclick-event-td2975205.html

#_ (do

     (require 'clojure.xml)
     
     (let [f (dj.io/file "/home/username/store/code-countries/1/code/analyzer.clj")
           fs (file->forms f)
           d (forms->ns-used fs)
           vd (forms->current-ns-vars-used fs d)
           g (graphviz-vars-use-these-ns d vd)]
       #_ (->> (with-out-str
              (-> (display2! g "dot")
                  (.getBytes)
                  (java.io.ByteArrayInputStream.)
                  clojure.xml/parse
                  clojure.xml/emit))
            (sh/sh "display" :in))

       (display! g "dot")))

(defonce debug (atom {}))

(defmacro T [code]
  `(let [result# ~code]
     (reset! debug result#)
     result#))

#_ (do
     
     
     (let [f (dj.io/file "/home/username/store/code-countries/1/code/analyzer.clj")
           fs (file->forms f) ;reads-string
           d (T (forms->ns-used fs))
           vd (forms->current-ns-vars-used fs d)
           g (graphviz-vars-use-these-ns d vd)]
       #_ (->> (with-out-str
              (-> (display2! g "dot")))
            (sh/sh "display" :in))

       (display! g "dot")))

#_ @debug
