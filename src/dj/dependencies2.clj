(ns dj.dependencies2
  (:require [cemerick.pomegranate :as pom]
            [cemerick.pomegranate.aether :as a]
            [dj.io]))

(-> (Thread/currentThread)
    (.setContextClassLoader (clojure.lang.RT/baseLoader)))

(defn add-dependencies
  "(add-dependencies '[[incanter \"1.7.0\" :exclusions [org.clojure/clojure]])

  stores jars in local m2 directory"
  [coordinates]
  (pom/add-dependencies
   :coordinates coordinates
   :local-repo (dj.io/file (or (get (System/getenv) "DJ_SOLO_DIRECTORY")
                               (System/getProperty "user.dir"))
                           "m2")
   :repositories (merge a/maven-central
                        {"clojars" "https://clojars.org/repo"})))
