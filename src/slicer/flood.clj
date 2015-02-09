;after sliced the model, we need to check for watertight and discretized the slice shape
(ns slicer.flood)


(defn line-box-inc
  "check if a line start and end by two points is intersected with an AABB box. Imprative since performance is important"
  [[x1 y1 :as line-start] [x2 y2 :as line-end]
   [min-x min-y :as box-min] [max-x max-y :as box-max]]
  (let [m (atom 0.0)
        x (atom 0.0)
        y (atom 0.0)]
    (cond
     (or
       (and (< x1 min-x) (< x2 min-x))
       (and (< y1 min-y) (< y2 min-y))
       (and (> x1 max-x) (> x2 max-x))
       (and (> y1 max-y) (> y2 max-y))) false
     (= x2 x1) true
     (do
       (reset! m (/ (- y2 y1) (- x2 x1)))
       (reset! y (+ (* @m (- min-x x1)) y1))
       (and (>= @y min-y) (<= @y max-y))) true
     (do
       (reset! y (+ (* @m (- max-x x1)) y1))
       (and (>= @y min-y) (<= @y max-y))) true
     (do
       (reset! x (+ (/ (- min-y y1) @m) x1))
       (and (>= @x min-x) (<= @x max-x))) true
     (do
       (reset! x (+ (/ (- max-y y1) @m) x1))
       (and (>= @x min-x) (<= @x max-x))) true
     :else false
     )))

(defn tri-box-inc
  "check if a triangle intersects with an AABB box."
  [[x1 y1 :as tri-1] [x2 y2 :as tri-2] [x3 y3 :as tri-3]
   [min-x min-y :as box-min] [max-x max-y :as box-max]]
  (let []
    (cond
     (or
       (and (< x1 min-x) (< x2 min-x) (< x3 min-x))
       (and (< y1 min-y) (< y2 min-y) (< y3 min-y))
       (and (> x1 max-x) (> x2 max-x) (> x3 max-x))
       (and (> y1 max-y) (> y2 max-y) (> y3 max-y))) false ;tri completely outside
     (and
      (and (< x1 min-x) (> x1 max-x) (< y1 min-y) (> y1 max-y))
      (and (< x2 min-x) (> x2 max-x) (< y2 min-y) (> y2 max-y))
      (and (< x3 min-x) (> x3 max-x) (< y3 min-y) (> y3 max-y))) true ;tri completely inside box
     (or
      (line-box-inc [x1 y1] [x2 y2] [min-x min-y] [max-x max-y])
      (line-box-inc [x2 y2] [x3 y3] [min-x min-y] [max-x max-y])
      (line-box-inc [x3 y3] [x1 y1] [min-x min-y] [max-x max-y])) true ; edge intersecting
     (and
      (< (min x1 x2 x3) min-x)
      (> (max x1 x2 x3) max-x)
      (< (min y1 y2 y3) min-y)
      (> (max y1 y2 y3) max-y)) true ;box completely inside tri
     :else false
     )))
