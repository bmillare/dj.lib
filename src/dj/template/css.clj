(ns dj.template.css
  (:require [dj.template.sgml]))

(defprotocol IEmitCSSForm
  (-emit-form [form]))

(extend-protocol IEmitCSSForm
  String
  (-emit-form [form]
    form)
  nil
  (-emit-form [form]
    nil)
  Number
  (-emit-form [form]
    (str form))
  Object
  (-emit-form [form]
    (throw (ex-info (str "No implementation for " (type form))
                    {:form form}))))

(defn emit-props [m]
  (apply str (interpose ";" (for [[k v] m]
			      (str (name k)
				   ":"
				   (-emit-form v))))))

(defrecord style [s]
  dj.template.sgml/IEmitForm
  (-emit-form [_]
    (emit-props s)))

(defn emit-group [selectors properties]
  (str selectors
       "{" (if (vector? properties)
	     (apply str (for [[k v] (partition 2 properties)]
			  (emit-group k v)))
	     (emit-props properties)) "}"))

(def reset {"html,body,div,span,applet,object,iframe,h1,h2,h3,h4,h5,h6,p,blockquote,pre,a,abbr,acronym,address,big,cite,code,del,dfn,em,img,ins,kbd,q,s,samp,small,strike,strong,sub,sup,tt,var,b,u,i,center,dl,dt,dd,ol,ul,li,fieldset,form,label,legend,table,caption,tbody,tfoot,thead,tr,th,td,article,aside,canvas,details,figcaption,figure,footer,header,hgroup,menu,nav,section,summary,time,mark,audio,video"
	    {:margin "0"
	     :padding "0"
	     :border "0"
	     :outline "0"
	     :font-size "100%"
	     :font "normal"
	     :vertical-align "baseline"}

	    "article,aside,details,figcaption,figure,footer,header,hgroup,menu,nav,section"
	    {:display "block"}

	    ".hidden"
	    {:display "none"}})

(defn emit [group-map]
  (apply str (map (fn [[k v]]
		    (emit-group k v))
		  group-map)))

(defn emit-multiple [group-maps]
  (apply str (map emit group-maps)))

(defrecord css [group-maps]
  dj.template.sgml/IEmitForm
  (-emit-form [_]
    (emit-multiple group-maps)))

(comment
  (defn page
    "content: list of html data"
    [content]
    [[:!DOCTYPE "html"]
     (into [:html {:lang "en"}
            [:head nil
             [:meta {:charset "UTF-8"}]
             [:style {}
              (css/->css [css/reset])]]]
           content)])
  )
