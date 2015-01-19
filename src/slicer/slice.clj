;; # slicing algorithms
;; This is the core of the algorithm.
;; Mainly to get the intersection between triangles and a sequence of Z planes.
(ns slicer.slice)

(defn dot*
  "
  Dot Product

  This is a simple version of dot product just for 3-tuples"
  [[x1 y1 z1 :as p1]
   [x2 y2 z2 :as p2]]
  {:pre [(float? x1) (float? y1) (float? z1)
         (float? x2) (float? y2) (float? z2)]}
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
  "
  [[x0 y0 z0 :as point]
   [[x2 y2 z2 :as normal] [x1 y1 z1 :as position] :as plane]]
  {:pre [(float? x1) (float? y1) (float? z1)
         (float? x2) (float? y2) (float? z2)
         (float? x0) (float? y0) (float? z0)]}
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
  "
  [[[x1 y1 z1 :as start-point]  [x2 y2 z2 :as end-point] :as line]
   [[x3 y3 z3 :as normal] [x4 y4 z4 :as position] :as plane]]
  {:pre [(float? x1) (float? y1) (float? z1)
         (float? x2) (float? y2) (float? z2)
         (float? x3) (float? y3) (float? z3)
         (float? x4) (float? y4) (float? z4)]}
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
  If the entire triangle sits on the plane, returns the whole triangle;
  If one segment of the triangle sits on the plane, that's what you got;
  If a point of the triangle sits there, you have it;
  If not intersecting, nil;
  Otherwise, you have the intersection
  "
  [[[x1 y1 z1 :as p1]
    [x2 y2 z2 :as p2]
    [x3 y3 z3 :as p3] :as triangle]
   [[x4 y4 z4 :as normal]
    [x5 y5 z5 :as position] :as plane]]
  {:pre [(float? x1) (float? y1) (float? z1)
         (float? x2) (float? y2) (float? z2)
         (float? x3) (float? y3) (float? z3)
         (float? x5) (float? y5) (float? z5)
         (float? x4) (float? y4) (float? z4)]}
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

(defn slicing-plane
  "
  Given a x/y/z axis value and the axis keyword :x/:y/:z, returns the plane [[x y z :as normal] [x y z :as plane]]
  "
  [a b]
  {:pre [(float? a)
         (keyword? b)]}
  (cond
   (= b :x) [[1.0 0.0 0.0] [a 0.0 0.0]]
   (= b :y) [[0.0 1.0 0.0] [0.0 a 0.0]]
   (= b :z) [[0.0 0.0 1.0] [0.0 0.0 a]]))

(defn gen-planes
  "
  Generate a series of slicing-planes from 'start' to 'end' each 'step' along the provided 'axis'
  "
  [start end step axis]
  {:pre [(number? start)
         (number? end)
         (number? step)
         (keyword? axis)]}
  (vec
   (for [i (range (bigdec start) (bigdec (+ end step)) (bigdec step))]
     (slicing-plane (double i) axis))))

(defn find-min-max
  "finds the highest and lowest point in axis of a collection of triangles along provided axis"
  [axis 
   [{[x1 y1 z1 :as p1] :vertex-1 [x2 y2 z2 :as p2] :vertex-2 [x3 y3 z3 :as p3] :vertex-3} & tris :as triangles]]
  {:pre [(number? x1) (number? x2) (number? x3) (number? y1) (number? y2) (number? y3) (number? z1) (number? z2) (number? z3) (keyword? axis)]}
  {:min 
   (reduce (fn [min-num tri] 
            (cond 
              (= axis :x) 
              (min
                min-num
                (min (first (:vertex-1 tri))
                     (first (:vertex-2 tri)) 
                     (first (:vertex-3 tri))))
              (= axis :y) 
              (min
                min-num
                (min (second (:vertex-1 tri))
                     (second (:vertex-2 tri)) 
                     (second (:vertex-3 tri))))
              (= axis :z)  
              (min 
                min-num
                (min (last (:vertex-1 tri))
                     (last (:vertex-2 tri)) 
                     (last (:vertex-3 tri))))))
          (cond (= axis :x) (min x1 x2 x3)
                (= axis :y) (min y1 y2 y3)
                (= axis :z) (min z1 z2 z3))
          tris)
   :max
   (reduce (fn [max-num tri] 
            (cond 
              (= axis :x) 
              (max
                max-num
                (max (first (:vertex-1 tri))
                     (first (:vertex-2 tri)) 
                     (first (:vertex-3 tri))))
              (= axis :y) 
              (max
                max-num
                (max (second (:vertex-1 tri))
                     (second (:vertex-2 tri)) 
                     (second (:vertex-3 tri))))
              (= axis :z)  
              (max 
                max-num
                (max (last (:vertex-1 tri))
                     (last (:vertex-2 tri)) 
                     (last (:vertex-3 tri))))))
          (cond (= axis :x) (max x1 x2 x3)
                (= axis :y) (max y1 y2 y3)
                (= axis :z) (max z1 z2 z3))
          tris)}
  )

(defn triangle-map2vector
  "convert triangle map to vector"
  [{v3 :vertex-3, v2 :vertex-2, v1 :vertex-1, :as triangle}]
  [v1 v2 v3])

(defn slice
  "slice every triangle of the model with every plane along the axis"
  [triangles planes axis]
  (for [triangle triangles
        plane planes] 
    (let [result (triangle-plane-inc (triangle-map2vector triangle) plane)] 
      {:axis axis 
       :cut-point (cond (= axis :x) (first (second plane)) 
                        (= axis :y) (second (second plane)) 
                        (= axis :z) (last (second plane))) 
       :plane plane
       :triangle triangle 
       :result result})))
