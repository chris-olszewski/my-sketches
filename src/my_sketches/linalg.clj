(ns my-sketches.linalg)


(defn norm
  [xs]
  (->>
   xs
   (map #(* % %))
   (reduce +)
   (Math/sqrt)))

(defn distance
  "Returns Euclidean distance of two vectors"
  [x y]
  (norm (map - x y)))

(defn avg
  [& xss]
  (let [s (count xss)]
    (->>
     xss
     (apply map +)
     (map #(/ % s)))))

(defn normalize
  "Will scale vector so it has norm of a"
  [a x]
   (map #(* % (/ a (norm x))) x))

