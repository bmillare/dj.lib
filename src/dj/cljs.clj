(ns dj.cljs
  "code/instructions how to get clojurescript working dj style, which typically means as dynamic as possible and maybe via source instead of via deps"
  (:require [cemerick.pomegranate :as pom]
            [dj.dependencies2 :as dd]))

;; clojurescript/deps.edn
;; - main deps
;; - paths: src/main/clojure and src/main/cljs

(defn setup-source-classpaths
  ([cljs-dir]
   (pom/add-classpath (clojure.java.io/file cljs-dir
                                            "clojure"))
   (pom/add-classpath (clojure.java.io/file cljs-dir
                                            "cljs")))
  ([]
   (setup-source-classpaths
    (clojure.java.io/file (System/getProperty "user.home")
                          "clj/git/clojurescript/src/main"))))

(defn load-deps []
  ;; check "clojurescript/deps.edn" for more possible needed deps
  (dd/add-dependencies '[[com.google.javascript/closure-compiler-unshaded "v20220502"]
                         [org.clojure/google-closure-library "0.0-20211011-0726fdeb"]])
  (require '[cljs.repl])
  (require '[cljs.repl.browser]))

(defn load-all []
  (setup-source-classpaths)
  (load-deps))

#_ (do
     (def env (cljs.repl.browser/repl-env :port 1080 :launch-browser false))
     (cljs.repl/repl env)
     (def rows (drop 1 (.-rows (first (.getElementsByClassName js/document "companies-table")))))
     (js/console.log (str
                (vec (for [r rows]
                       (.-href (first (.getElementsByClassName r "company-name")))))))
     (.-value)
     (.-downloads js/browser)

     (def p (.getElementById js/document "anal"))
     (def p2 (.getElementsByTagName p "label"))
     (js/console.log (str
                      (mapv (fn [entry]
                              (vec (drop 1 (map (fn [x] (.-innerHTML x)) (.-children entry)))))
                            p2)))

     )

(defn private-field [^Object obj ^String field-name]
  (when-let [^java.lang.reflect.Field f (some
                        (fn [^Class c]
                          (try (.getDeclaredField c field-name)
                               (catch NoSuchFieldException _ nil)))
                        (take-while some? (iterate (fn [^Class c] (.getSuperclass c)) (.getClass obj))))]
    (. f (setAccessible true))
    (. f (get obj))))
