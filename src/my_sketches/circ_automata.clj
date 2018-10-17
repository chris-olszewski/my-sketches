;; Based off of github.com/quil/quil-examples/blob/master/src/quil_sketches/automata.clj
(ns my-sketches.circ-automata
  (:gen-class)
  (:require [quil.core :as qc]
	    [clojure.pprint :refer :all]))

(defn state-color
  "Gets color for state, "
  [n st]
  (let [val (mod (* st (quot 256 n)) 256)
        val2 (mod (* val val) 256)
        val3 (mod (* val val val) 256)
        ]
    [val val3 val2 ]))

(defn int->ndigits
  "Gets string representation of number using n base"
  [n number]
  (let [powers (iterate #(* n %) 1)
        l-powers (reverse (take-while #(>= number %) powers))]
    (loop [xs '()
           cur number
           ys l-powers]
      (if (empty? ys)
        (reverse xs)
        (let [c-power (first ys)
              b (quot cur c-power)]
          (recur (cons b xs)
                 (- cur (* b c-power))
                 (rest ys)))))))

(defn zero-pad
  "Forward pads a seq with 0s to match a given length. Used for making sure int->bdigits hits byte boundaries"
  [x len]
  (let [shortage (- len (count x))]
    (if (< shortage 1)
      x
      (concat (repeat shortage 0) x))))

(defn repli-pad
  "Forward pads a seq with a repetition until it matches given length"
  [x len]
  (->> x
       (reverse)
       (cycle)
       (take len)
       (reverse)))

(defn n-input-patterns
  "Generalized input patterns"
  [n]
  (map #(zero-pad (int->ndigits n %1) 3) (range (* n n n))))

(defn n-rule-mappings
  "Returns a mapping of patterns to new states. Returns a structure like:
   {(0 1 1) 1
    ...}"
  [n number]
  (zipmap (n-input-patterns n)
          (reverse (repli-pad (int->ndigits n number) (* n n n)))))

(defn n-rule
  "Returns a function that will process a triad of input values according to a given rule #.
   Since rules are simple lookup tables, this maps to nothing more than a get really.
   We use a function here only to be able to close over the rule-mappings and only evaluate those once."
  [n number]
  ;; Applying a rule is really simple, since we've reduced the problem to pattern matching, and
  ;; clojrue can match lists well (e.g. (= [1 2 3] [1 2 3]) => true even though they're separate
  ;; objects), can simply see which pattern the 3 given values match with a map lookup via get.
  (let [mappings (n-rule-mappings n number)]
    (fn [triad]
      ;;(println "Big Poop Energy")
      ;;(println triad)
      (get mappings triad))))

;; Since we assume cells that are off the grid are zeroes, and the far left and far
;; right calculatoins both require these cells, we make our calculation a bit easier by
;; simply pretending the previous row has two extra 0s on either side
(defn bookend
  "Pads a seq with a given value on both sides."
  [x v]
  (concat v x v))

(defn simulate
  "Runs a single iteration of a given rule-fn on a given-state"
  [rule-fn state]
  ;; We bookend the value below to add a 0 on both sides of the previous state
  ;; as it makes calculations simpler
  (for [triad (partition 3 1 (bookend state [0 0]))]
    (rule-fn triad)))

(defn simulation
  "Returns a lazy-seq of future states for a given rule-fn and state"
  [rule-fn state]
  (let [new-state (simulate rule-fn state)]
    ;; This is an infinitely recursive lazy sequence! Notice how we start
    ;; by considing (prepending) to a new-state onto the head of a not-yet extant
    ;; lazy sequence. 
    ;; You'll notice that the lazy sequence is declared with a
    ;; body that will recurse from the present state, passing the current state into
    ;; itself. Lazy sequences such as this are inherently tail-recursive, so they won't
    ;; blow the stack.
    (cons new-state (lazy-seq (simulation rule-fn new-state)))))

(defn construct-map
  "Takes a grid and constructs a map"
  [grid]
  (->>
   (for [[y row] (map-indexed vector grid)
             [x val] (map-indexed vector row)]
         [[x y] val])
   (reduce
    (fn [acc [k v]] (assoc acc k v)) {})))

(defn big-circles
  "Takes a map of x,y and converts to list of circles"
  [grid]
  (letfn [(cartesian [xs ys]
            (for [x xs y ys] [x y]))
          (cons-sq [x y s]
            (cartesian
             (range x (+ x s))
             (range y (+ y s))))
          (check-circ [g x y s]
            (apply = (map g (cons-sq x y s))))
          (max-circ [g x y]
            (last (take-while
                   (fn [s] (check-circ g x y s))
                   (rest (range)))))]
    (loop [xs grid
           [x y] [0 0]
           circs '()]
      (if (empty? xs)
        circs
        (let [diam (max-circ xs x y)
              state (xs [x y])
              coords (cons-sq x y diam)
              n-xs (apply dissoc xs coords)]
          (recur n-xs
                 (first (sort (keys n-xs)))
                 (cons [x y state diam] circs)))))))



(defn draw-circs
  "Draw a collection of circles"
  [n buffer scale]
  (letfn [(draw-circ [x y st d]
            (apply qc/fill (state-color n st))
            (qc/no-stroke)
            (qc/ellipse-mode :corner)
            (qc/ellipse (* scale x) (* scale y) (* scale d) (* scale d)))]
    (qc/smooth)
    (->> buffer
         (construct-map)
         (big-circles)
         (map #(apply draw-circ %))
         (dorun))))
  

(defn setup
  "Setup the UI"
  []
  ;;(qc/smooth) ;; Enable AA
  (qc/frame-rate 1))

;; idea pass through specific generation to skip to
(defn run-rule [rule-num base {:keys [width height scale]}]
  (let [width (or width 100)
        height (or height 100)
        scale (or scale 5) ; Scale factor for rendering
        ;; Our initial state is a single row of random 0s and 1s
        initial (repeatedly width #(rand-int base))
        sim (simulation (n-rule base rule-num) initial)]
    (println "Rule " rule-num " mappings:")
    ;;(pprint (n-rule-mappings base rule-num))
    ;; Initialize the graphics
    (qc/defsketch automata
      :title (str "Rule " rule-num)
      :setup (fn drawfn []
              (draw-circs base (take height sim) scale))
      ;;:draw (fn drawfn []
      ;;        (draw-circs base (take height sim) scale))
      :size [(* scale width) (* scale height)])))


(defn -main [base rule-num & args]
  (run-rule (Integer/valueOf rule-num)
            (Integer/valueOf base)
            {:width 50 :height 20 :scale 24}))
