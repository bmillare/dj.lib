(ns dj.code.analyzer.navigation-swing
  (:require [dj.code.analyzer :as anal]
            [clojure.string :as str]))

(def debug (atom nil))

(defn color [color-string]
  (java.awt.Color. (Integer/parseInt (subs color-string
                                           0 2)
                                     16)
                   (Integer/parseInt (subs color-string
                                           2 4)
                                     16)
                   (Integer/parseInt (subs color-string
                                           4 6)
                                     16)))

(defn read-graphviz-plain [txt]
  (reduce (fn [ret l]
            (let [[ltype & args] (read-string (str "[" l "]"))
                  [_ x y width height node-id style shape color fillcolor] args]
              (if (= ltype 'node)
                (assoc ret
                       node-id
                       {:x x
                        :y y
                        :width width
                        :height height
                        :node-id node-id
                        :style style
                        :shape shape
                        :color color
                        :fillcolor fillcolor})
                ret)))
          {}
          (str/split-lines txt)))

(defn start-frame! [app-state]
  (let [{:keys [:app.swing/title
                :app.swing/width
                :app.swing/height
                :app.swing/x
                :app.swing/y
                :app.data/nodes]} @app-state
        f (javax.swing.JFrame. title)]
    (javax.swing.SwingUtilities/invokeLater
     (fn []
       (let [cp (.getContentPane f)
             g (.getGraphics cp)]
         (.setLayout f nil)
         (.setSize f width height)
         (.setLocation f x y)
         (.setAlwaysOnTop f true)
         (.setBackground cp
                         java.awt.Color/BLACK)
         (.setVisible f true)
         #_ (-> (java.awt.KeyboardFocusManager/getCurrentKeyboardFocusManager)
                (.addKeyEventDispatcher KeyEventDispatcher))

         #_ (doto g
           (.setColor (color "ff0000"))
           (.drawLine 0 0 width height))
         (doseq [n (vals nodes)]
           (let [scale 100
                 {:keys [x
                         y
                         node-id]} n
                 x (* scale x)
                 y (* scale y)
                 text (str node-id)
                 foreground-color "aaaaaa"
                 background-color "111111"
                 l (javax.swing.JLabel. text)
                 _ (do
                     (.setForeground l (color foreground-color))
                     (.setBackground l (color background-color))
                     (.setFont l (java.awt.Font. "Monospaced" java.awt.Font/PLAIN 14)))
                 preferred-size (.getPreferredSize l)
                 width (.width preferred-size) 
                 height (.height preferred-size)]
             (.setOpaque l true)
             (.setBounds l
                         x
                         y
                         width
                         height)
             (.add cp l))))))))


#_ (let [nodes (let [f (dj.io/file "/dj/code/analyzer.clj")
                     fs (anal/file->forms f)
                     d (anal/forms->ns-used fs)
                     vd (anal/forms->current-ns-vars-used fs d)
                     g (anal/graphviz-vars-use-these-ns d vd)]
                 (-> (anal/display2! g "dot")
                     (read-graphviz-plain)))
         app (atom {:app.swing/title "Navigation 1"
                    :app.swing/x (+ 1920 250)
                    :app.swing/y 100
                    :app.swing/width 250
                    :app.swing/height 250
                    :app.data/nodes nodes})]
     (start-frame! app))

#_ @debug

#_ (let [f (dj.io/file "/dj/code/analyzer.clj")
                     fs (anal/file->forms f)
                     d (anal/forms->ns-used fs)
                     vd (anal/forms->current-ns-vars-used fs d)
                     g (anal/graphviz-vars-use-these-ns d vd)]
                 (-> (anal/display2! g "dot")
                     (read-graphviz-plain)))
