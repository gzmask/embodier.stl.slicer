;; # slicing algorithms
;; This is the core of the algorithm.
;; Mainly to get the intersection between triangles and a sequence of Z planes.
(ns slicer.slice)

(defn dot*
  "
  Dot Product

  This is a lazy version of dot product just for 3-tuples"
  [[x1 y1 z1 :as p1]
   [x2 y2 z2 :as p2]]
  (+
   (* x1 x2)
   (* y1 y2)
   (* z1 z2)))

(defn norm
  "
  Norm of a vector
  "
  [[x y z :as v]]
  (Math/sqrt (dot* v v)))

(defn point-plane 
  "
  Distance from point to plane

  Returns the distance.
  If result is positive, point is at the side of the normal,
  if the result is negative, point is at the other side of the normal.

  i.e

    (point-plane [0 -1 0] [[0 1 0] [0 0 0]]) => -1
    (point-plane [0 1 0] [[0 1 0] [0 0 0]]) => 1
    (point-plane [0 0 0] [[0 1 0] [0 0 0]]) => 0
  "
  [[x0 y0 z0 :as point]
   [[x2 y2 z2 :as normal] [x1 y1 z1 :as position] :as plane]]
  (/
    (dot* normal (map - point position))
    (norm normal)))


(defn plane-line-inc
  "
  Ray/Segment and Plane intersection

  line is represented by start-point and end-point
  plane is represented by normal direction vector (from original) and the position point
  returns the intersection point if there is one, 
  returns nil when not having any, 
  returns the line when the line is on the plane.

  i.e

    (plane-line-inc [[5 1.1 0] [-2 1.1 0]] [[1 0 0] [0 0 0]]) => [0 1 0]
    (plane-line-inc [[5 1.1 0] [-2 1.1 0]] [[0 1 0] [0 0 0]]) => nil
    (plane-line-inc [[0 1 0] [0 2 0]] [[0 1 0] [0 0 0]]) => nil
    (plane-line-inc [[0 0.1 0] [0 2 0]] [[0 1 0] [0 0 0]]) => nil
    (plane-line-inc [[5 1.1 0] [-2 1.1 0]] [[0 1 0] [0 1.1 0]]) => [[5 1.1 0] [-2 1.1 0]]
  "
  [[[x1 y1 z1 :as start-point]  [x2 y2 z2 :as end-point] :as line]
   [[x3 y3 z3 :as normal] [x4 y4 z4 :as position] :as plane]]
  (let [d (dot* normal (map - end-point start-point))]
    (cond (and (zero? (point-plane start-point plane)) (zero? (point-plane end-point plane))) line
          (zero? d) nil 
          :else (let [r (/ (dot* normal (map - position start-point)) d)] 
                  (if (and (<= r 1) (>= r 0)) 
                    (vec (map + start-point (map * [r r r] (map - end-point start-point))))
                    nil)))))



(defn triangle-plane-inc
  "
  Triangle and Plane intersection

    (triangle-plane-inc [[0.0 1 0] [-1 -1 0] [1 -1 0]] [[0 1 0] [0 0 0]]) => ([-0.5 0 0] [0.5 0 0])
    (triangle-plane-inc [[0 0 0] [-1 -1 0] [1 -1 0]] [[0 1 0] [0 0 0]]) => [0 0 0]
    (triangle-plane-inc [[0 -1 0] [-1 -2 0] [1 -2 0]] [[0 1 0] [0 0 0]]) => nil 
  "
  [[[x1 y1 z1 :as p1]
    [x2 y2 z2 :as p2]
    [x3 y3 z3 :as p3] :as triangle]
   [[x4 y4 z4 :as normal] 
    [x5 y5 z5 :as position] :as plane]]
  (let [ds (map #(point-plane % plane) triangle)]
    (cond 
      (= 3 (count (filter zero? ds))) 
      triangle
      (= 2 (count (filter zero? ds))) 
      (filter #(zero? (point-plane % plane)) triangle)
      (= 1 (count (filter zero? ds))) 
      (if (neg? (apply * (filter (complement zero?) ds)))
        [(first (filter #(zero? (point-plane % plane)) triangle))
         (plane-line-inc 
           (vec (filter #(not (zero? (point-plane % plane))) triangle)) 
           plane)] 
        (first (filter #(zero? (point-plane % plane)) triangle)))
      :else 
      (if (or (empty? (filter neg? ds)) (empty? (filter pos? ds)))
        nil
        (filter (complement nil?)
                [(plane-line-inc [p1 p2] plane) 
                 (plane-line-inc [p2 p3] plane) 
                 (plane-line-inc [p3 p1] plane)])
        ))))

