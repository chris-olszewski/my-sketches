(ns my-sketches.kmeans
  (:gen-class)
  (:require [my-sketches.linalg :as l]
            [quil.core :as qc]
            [quil.middleware :as m]))

(def interval 800)
(def k-fixed 8)
(def n-fixed 1200)
(def dot-size 9)

(defn rand-data
  "Makes n points of random data in d dimensions"
  [n]
  (->>
   (repeatedly #(rand interval))
   (partition 2)
   (take n)))

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
            (rand-data n-fixed)))

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
