(ns my-sketches.tri2
  (:gen-class)
  (:require [my-sketches.linalg :as l]
            [quil.core :as qc]))

;; wants:
;; chooses 3 points from d,e,f ab, bc, ac resp
;; returns def, adf, bde, cef

;; could generalize to be
;; given set of edges
;; -> picks a pt on the edge and draws a triangle

(defn tri-recur [[a b c] r]
    "Takes a triangle, returns seq of triangles which subdivide given triangle"
    (defn- r-midpoint [x y]
      (into []
            (map +
                 x (l/normalize (* r (l/distance x y)) (map - y x)))))
  (let [[e f] (max-key #(apply l/distance %) [a b] [b c] [c a])
        g (first (filter (comp not #{e f}) [a b c]))
        d (r-midpoint e f)]
    (list
     [d e g]
     [d g f])))

;; expects xs to be string of x in [0, 1)
;; idea extend this so if (1/n) < x
(defn triangles
  [init k xs p]
  (defn p-recur
    [d]
    (fn [t x]
      (if (< (Math/pow p d) (rand))
        (tri-recur t x)
        '())))
  (loop [r init
         ts '()
         d k]
    (if (< d 1)
      r
      (recur (mapcat (p-recur d) r xs) (cons r ts) (dec d)))))

(def of1 272)

(defn draw
  []
  (defn- gen-gauss []
    (repeatedly (fn [] (->
                        (qc/random-gaussian)
                        (* 0.5)
                        (qc/constrain-float -2.7 2.7)
                        (+ 2.7)
                        (/ 5.4)))))
  (qc/color-mode :hsb 360 100 100 1.0)
  (qc/background 220 25 95)

  (qc/stroke-join :round)
  ;; background triangles
  (qc/no-fill)
  (qc/stroke 0 0 100 0.44)
  (doseq [t (triangles '([[0 0] [1600 0] [0 1600]]) 16 (gen-gauss) 0.25)]
    (apply qc/triangle (apply concat t)))
  (qc/display-filter :blur 2)

  ;; draw circ
  (qc/fill 5 60 80)
  (qc/no-stroke)
  (qc/ellipse (+ of1 (/ (- 650 of1) 2)) (+ of1 (/ (- 650 of1) 2)) (l/distance [of1 of1] [650 650]) (l/distance [of1 of1] [650 650]))

  (qc/no-fill)
  (qc/stroke 0 0 0 0.15)
  (doseq [t (triangles '([[650 650] [550 300] [300 550]]) 16 (gen-gauss) 0.01)]
    (apply qc/triangle (apply concat t)))
  (qc/stroke 0 0 0 0.25)
  (doseq [t (triangles (list [[of1 of1] [(- 800 of1) of1] [of1 (- 800 of1)]]) 14 (gen-gauss) 0.05)]
    (apply qc/triangle (apply concat t)))
  (qc/display-filter :blur 1))

(defn -main [& args]
  (qc/defsketch graph
    :title "Graph"
    :setup draw
    :settings (fn []
                (qc/smooth 8)
                (qc/pixel-density 2))
    :size [800 800]))
