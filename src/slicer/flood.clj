;after sliced the model, we need to check for watertight and discretized the slice shape
(ns slicer.flood)


(defn line-box-inc
  "check if a line start and end by two points is intersected with an AABB box. Imprative since performance is important"
  [[[x1 y1 :as p1] [x2 y2 :as p2] :as line]
   [min-x min-y max-x max-y :as box]]
  (let [m (atom 0.0)
        x (atom 0.0)
        y (atom 0.0)]
    (cond
     (or
       (and (<= (x1 min-x)) (<= (x2 min-x)))
       (and (<= (y1 min-y)) (<= (y2 min-y)))
       (and (>= (x1 max-x)) (<= (x2 max-x)))
       (and (>= (y1 max-y)) (<= (y2 max-y))))
     false
     (do
       (reset! m (/ (- y2 y1) (- x2 x1)))
       (reset! y (+ (* @m (- min-x x1)) y1))
       (and (> @y min-y) (< @y max-y)))
     true
     (do
       (reset! y (+ (* @m (- max-x x1)) y1))
       (and (> @y min-y) (< @y max-y)))
     true
     (do
       (reset! x (/ (- min-y y1) (+ @m x1)))
       (and (> @x min-x) (< @x max-x)))
     true
     (do
       (reset! x (/ (- max-y y1) (+ @m x1)))
       (and (> @x min-x) (< @x max-x)))
     true
     :else false
     )))
