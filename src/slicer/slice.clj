;; # slicing algorithms
;; This is the core of the algorithm.
;; Mainly to get the intersection between triangles and a sequence of Z planes.
(ns slicer.slice)

;; ## dot product
(defn dot*
  [[x1 y1 z1 :as p1]
   [x2 y2 z2 :as p2]]
  (+
   (* x1 x2)
   (* y1 y2)
   (* z1 z2)))

;; ## Ray/Segment and Plane intersection
(defn plane-line-inc
  "
  line is represented by start-point and end-point
  plane is represented by normal direction vector (from original) and the position point
  returns the intersection point, or nil when not having any.
  "
  [[[x1 y1 z1 :as start-point]  [x2 y2 z2 :as end-point] :as line]
   [[x3 y3 z3 :as normal] [x4 y4 z4 :as position] :as plane]]
  (let [d (dot* normal (map - end-point start-point))]
    (if (= 0 d)
      nil
      (/ (dot* normal (map - position start-point)) d)
      )))

;(plane-line-inc [[1 2 3] [2 2 3]] [[1 0 0] [0 0 0]])
