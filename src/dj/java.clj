(ns dj.java)

(defn reload-class-file
  "reload a class file, you need to specify the fully qualified class
  name, which is dependent on the classname and the package name."
  ^Class [^java.io.File file classname]
  (.defineClass (clojure.lang.DynamicClassLoader.)
                classname
                (dj.io/to-byte-array file)
                nil))
