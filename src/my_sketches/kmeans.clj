(ns my-sketches.kmeans
  (:gen-class)
  (:require [my-sketches.linalg :as l]
            [quil.core :as qc]
            [quil.middleware :as m]))

(def interval 800)
(def k-fixed 5)
(def n-fixed 2000)
(def dot-size 9)

(defn rand-data
  "Makes n points of random data in d dimensions"
  [n]
  (->>
   (repeatedly #(rand interval))
   (partition 2)
   (take n)))

;; make more general i.e. be able to pass in a seed data point set
(defn stoc-data
  [n]
  (defn- gen-new [xs]
    (let [[x y] (rand-nth xs)
          size (apply min (map (fn [[a b]] first (l/distance [a] [b]))
                         [[0 x] [0 y] [interval x] [interval y]]))]
      (list (+ (- x size) (rand (* 2 size)))
            (+ (- y size) (rand (* 2 size))))))
  (loop [xs (rand-data 50)]
    (if (<= n (count xs))
      xs
      (recur (cons (gen-new xs) xs)))))

(defn find-nearest
  [ks p]
  (apply min-key (partial l/distance p) ks))


(defn update-state
  [ks]
  (->>
   ks
   (vals)
   (apply concat)
   (group-by (partial find-nearest (keys ks)))
   (map (fn [[k v]] [(apply l/avg v) v]))
   (into {})))


(defn setup
  []
  (qc/background 255)
  (qc/frame-rate 1)
  (group-by (partial find-nearest
                     (rand-data k-fixed))
            (stoc-data n-fixed)))

(defn k-color
  [[x y]]
  (let [fixed (/ 255 interval)]
    [(* fixed x) (* fixed y) 45]))

(defn draw-dot
  [c [x y]]
  (apply qc/fill c)
  (qc/no-stroke)
  (qc/ellipse x y dot-size dot-size))

(defn draw-cluster
  [[k v]]
  (dorun
   (map (partial draw-dot (k-color k)) v)))

(defn draw-state
 [state]
  (dorun (map draw-cluster state)))

(defn gen-k-means
  []
  (qc/defsketch k-means-animated
    :title "Stable Graph"
    :middleware [m/fun-mode]
    :setup setup
    :update update-state
    :draw draw-state
    :size [interval interval]
    ))

(defn -main [& args]
  (gen-k-means))
