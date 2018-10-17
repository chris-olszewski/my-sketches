;; Inspired by https://twitter.com/anvaka/status/1051610248010555392
(ns my-sketches.stable-graph
  (:gen-class)
  (:require [quil.core :as qc]
	        [clojure.pprint :refer :all]
            [quil.middleware :as m]))

(def speed 4)
(def dot-size 16)
(def dist 48)
(def width 1200)
(def height 800)

(def cardinals
  '([1 0]
    [-1 0]
    [0 1]
    [0 -1]))

(defn gen-graph
  [w h]
  (letfn [(in-range? [m n]
            (and (< n m)
                 (<= 0 n)))
          (valid? [[x y]]
            (and (in-range? w x)
                 (in-range? h y)))
          (neighbor [[x y]]
            (filter valid? (list [(dec x) y]
                             [(inc x) y]
                             [x (dec y)]
                             [x (inc y)])))
          (gen-node [c]
            {:pos c
             :ne (neighbor c)})]
    (->>
     (for [x (range w)
           y (range h)]
       [x y])
     (reduce (fn [acc k] (assoc acc k (gen-node k)))
             {}))))

(defn scramble-graph
  "Takes a graph in format of gen-graph and mixes position"
  [w h g]
  (letfn [(rand-c []
            [(rand-int w) (rand-int h)])]
    (into {} (for [[k v] g]
               [k (assoc v :pos (rand-c))]))))

(defn sum-vect
  [& xs]
  (into [] (apply map + xs)))

(defn sc-map
  [f xs]
  (into [] (map f xs)))

(defn avg-vect
  "Takes list of points and finds average"
  [xxs]
  (let [len (count xxs)]
    (sc-map #(/ % len) (apply sum-vect xxs))))

(defn norm-vect
  [xs]
  (->>
   xs
   (sc-map #(* % %))
   (reduce +)
   (Math/sqrt)))

(defn mv-towards
  "Move pos x towards y by distance len"
  [x y len]
  (let [distance (norm-vect (into [] (map - x y)))
        diff-vect (into [] (map - y x))
        scaled-diff (sc-map #(* % (/ (min len distance) (norm-vect diff-vect))) diff-vect)]
    (sum-vect x scaled-diff)))


(defn wanted-pos
  [x v len]
  (->>
   v
   (sc-map #(* len %))
   (sum-vect x)))

(defn update-node-pos
  "Given a map and a key it will update given node"
  [xs x]
  (let [pos (:pos (xs x))
        ne (:ne (xs x))
        avg (avg-vect (map
                       (fn [y]
                         (wanted-pos
                          (:pos (xs y))
                          (into [] (map - x y))
                          dist))
                       ne))]
    {:pos (mv-towards pos avg speed) :ne ne}))

(defn update-state
  "Takes a state and returns a new state"
  [state]
  (into
   {}
   (map (fn [k] [k (update-node-pos state k)]) (keys state))))

(defn setup
  "Does initial setup; returns initial state"
  []
  (qc/background 0)
  ;;(qc/smooth)
  (->>
   (gen-graph 12 12)
   (scramble-graph
    (- width dot-size)
    (- height dot-size))))

(defn draw-state
  "Draw graph"
  [state]
  (letfn [(draw-circ [[x y]]
            (qc/fill (* x (/ 255 width)) (* y (/ 255 width)) 255)
            (qc/no-stroke)
            (qc/ellipse x y dot-size dot-size))
          (draw-ne [{:keys [pos ne]}]
            (qc/stroke 255)
            (dorun (map #(qc/line pos (:pos (state %))) ne)))
          (draw-node [{:keys [pos]}]
            (draw-circ pos))]
    (qc/background 0)
    (dorun (map draw-ne (vals state)))
    (dorun (map draw-node (vals state)))))

(defn gen-stable-graph
  [params]
  (qc/defsketch stable-graph
    :title "Stable Graph"
    :middleware [m/fun-mode]
    :setup setup
    :update update-state
    :draw draw-state
    :size [width height]
    ))

(defn -main [& args]
  (gen-stable-graph {:w 100 :h 100}))
