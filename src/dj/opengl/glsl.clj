(ns dj.opengl.glsl)

#_ [[:#version "130"]
    [:#if [:>= "__VERSION__" 130]
     [:#define "attribute" "in"]
     [:#define "varying" "out"]]
    [:#ifdef "GL_ES"
     [:precision "mediump" "float"]
     [:precision "mediump" "int"]]
    [:uniform "mat4" "uniform_Projection"]
    [:attribute "vec4" "attribute_Position"]
    [:attribute "vec4" "attribute_Color"]
    [:varying "vec4" "varying_Color"]
    [:defmain
     [:set "varying_Color" "attribute_Color"]
     [:set "gl_Position" [:* "uniform_Projection" "attribute_Position"]]]]

#_ (print (emit [[:#version "130"]
           [:#ifdef "GL_ES"
            [:precision "mediump" "float"]
            [:precision "mediump" "int"]]
           [:uniform "mat4" "uniform_Projection"]
           [:in "vec4" "attribute_Position"]
           [:in "vec4" "attribute_Color"]
           [:out "vec4" "varying_Color"]
           [:defmain
            [:set "varying_Color" "attribute_Color"]
            [:set "gl_Position" [:* "uniform_Projection" "attribute_Position"]]]]))

#_ [[:#version "130"]
    [:#ifdef "GL_ES"
     [:precision "mediump" "float"]
     [:precision "mediump" "int"]]
    [:out "vec4" "varying_Color"]
    [:defmain
     [:set "mgl_FragColor" "varying_Color"]]]

(declare emit)

(defprotocol IEmitForm
  (-emit-form [form]))

(extend-type String
  IEmitForm
  (-emit-form [form]
    form))

(extend-type clojure.lang.Keyword
  IEmitForm
  (-emit-form [form]
    (name form)))

(extend-type nil
  IEmitForm
  (-emit-form [form]
    nil))

(extend-type Number
  IEmitForm
  (-emit-form [form]
    (str form)))

(defn emit-cond-exp [open-tag
                     close-tag
                     [cond-exp & body]]
  (str open-tag " " (-emit-form cond-exp) "\n"
       (emit body)
       close-tag "\n"))

(defn emit-block-exp [open-tag
                      close-tag
                      body]
  (str open-tag "\n"
       (emit body)
       close-tag "\n"))

(defn emit-declaration [elements]
  (str (apply str (interpose " "
                             (map -emit-form elements))) "\n"))

(defn emit-statement [elements]
  (str (apply str (interpose " "
                             (map -emit-form elements))) ";\n"))

(defn emit-infix-exp [op args]
  (str "(" (apply str (interpose (str " " op " ")
                                 (map -emit-form args)))
       ")"))

(defn emit-ternary-exp [[conditional whentrue whenfalse]]
  (str "(" (-emit-form conditional) " ? " (-emit-form whentrue) " : " (-emit-form whenfalse)
       ")"))

(defn emit-tag [[tag & args :as data]]
  (case tag
    :#if (emit-cond-exp "#if" "#endif" args)
    :#ifdef (emit-cond-exp "#ifdef" "#endif" args)
    :defmain (emit-block-exp "void main(void)\n{"
                             "}"
                             args)
    :#version (emit-declaration data)
    :#define (emit-declaration data)
    
    :in (emit-statement data)
    :out (emit-statement data)
    :precision (emit-statement data)
    :uniform (emit-statement data)

    :set (let [[larg rarg] args]
           (str larg " = " (-emit-form rarg) ";\n"))
    :>= (emit-infix-exp ">=" args)
    :== (emit-infix-exp "==" args)
    :<= (emit-infix-exp "<=" args)
    :> (emit-infix-exp ">" args)
    :< (emit-infix-exp "<" args)
    :* (emit-infix-exp "*" args)
    :+ (emit-infix-exp "+" args)
    :- (emit-infix-exp "-" args)
    :/ (emit-infix-exp "/" args)
    :if (emit-ternary-exp args)
    (throw (ex-info (str tag " form not supported")
                    {:tag tag
                     :args args}))))

(extend-type Object
  IEmitForm
  (-emit-form [form]
    (emit-tag form)))

(defn emit [forms]
  (apply str (map -emit-form forms)))

