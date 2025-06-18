(ns dj.string)

(defn search-left
  ([^String txt ^String match]
   (search-left txt match (dec (count txt))))
  ([^String txt ^String match index]
   (let [size (long (count txt))]
     (loop [i (long index)]
       (if (and (>= i 0)
                (< i size))
         (if (.startsWith txt match i)
           i
           (recur (dec i)))
         nil)))))

(defn search-right
  ([^String txt ^String match]
   (search-right txt match 0))
  ([^String txt ^String match index]
   (let [size (long (count txt))
         i (long index)]
     (if (and (>= i 0)
              (< i size))
       (let [result (.indexOf txt match i)]
         (if (= -1 result)
           nil
           result))
       nil))))
