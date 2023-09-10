(ns uniform.util.string)

(defn width
  [s]
  (reduce
    (fn [accm c]
      (+ accm
         (if (= 1 (count (.getBytes (str c)))) 1 2)))
    0 s))
