;after sliced the model, we need to check for watertight and discretized the slice shape
(ns slicer.flood
  (:require [clojure.core.match :refer [match]]
            [slicer.tree :as tree])
  (:use slicer.util))


(defn flooding-aabb-gen
  [[min-x min-y max-x max-y :as aabb]]
  (let [w (- max-x min-x)
        h (- max-y min-y)]
  [[(- min-x w) min-y min-x max-y] ;left box
   [min-x max-y max-x (+ max-y h)] ;upper box
   [max-x min-y (+ max-x w) max-y] ;right box
   [min-x (- min-y h) max-x min-y]])) ;lower box

(defn slow-flood
  "given a tree and aabb generated from the slice,
  flood the tree from outside and return the nodes that are flooded.
  Perfect flood compare to fast-flood, and no border is required."
  [t aabb]
  (let [flooding-aabbs (atom (flooding-aabb-gen aabb))
        leafs (tree/leafs t)
        flooded-nodes (atom #{})]
    (loop [flooded-count (count @flooded-nodes)]
      (doseq [[leaf collided] leafs
              flooding-aabb @flooding-aabbs]
        (let [leaf-aabb (tree/index-to-aabb aabb tree/tree-arity leaf)]
          (match [(tree/aabb-inc leaf-aabb flooding-aabb)
                  (contains? @flooded-nodes leaf)
                  collided]
                 [true false false] ;when the leaf is intersecting with the flooding nodes; and not flooded before; and is not part of the slice matters; flood it now
                 (swap! flooded-nodes conj leaf)
                 :else :do-not-care)))
      (reset! flooding-aabbs
              (->> @flooded-nodes
                   vec
                   (map #(tree/index-to-aabb aabb tree/tree-arity %))
                   vec))
      (if (<= (count @flooded-nodes) flooded-count) ;when flooded nodes are not increasing. water has reach all parts
        (vec @flooded-nodes)
        (recur (count @flooded-nodes))))))


(defn aabb-flood-points
  "generate flooding points for interseciton check from aabb"
  [[min-x min-y max-x max-y :as aabb] nozzle-diameter]
  (let [[mx my :as mid-point] [(-> (- max-x min-x) (/ 2) (+ min-x)) (-> (- max-y min-y) (/ 2) (+ min-y))]
        half-nozzle (/ nozzle-diameter 2)
        third-nozzle (/ nozzle-diameter 3)
        ]
    (match [(s<= (- max-x min-y) nozzle-diameter 0.000001)]
           [true] [
                   [(- min-x half-nozzle) my] ;left point
                   [(+ max-x half-nozzle) my] ;right point
                   [mx (+ max-y half-nozzle)] ;upper point
                   [mx (- min-y half-nozzle)] ;lower point
                   ;[(- min-x third-nozzle) my] ;left point
                   ;[(+ max-x third-nozzle) my] ;right point
                   ;[mx (+ max-y third-nozzle)] ;upper point
                   ;[mx (- min-y third-nozzle)] ;lower point
                   ]
           [false]
           [;left column
             [(- min-x third-nozzle) (- my third-nozzle)]
             [(- min-x third-nozzle) (+ my third-nozzle)]
             ;right column
             [(+ max-x third-nozzle) (- my third-nozzle)]
             [(+ max-x third-nozzle) (+ my third-nozzle)]
             ;upper row
             [(- mx third-nozzle) (+ max-y third-nozzle)]
             [(+ mx third-nozzle) (+ max-y third-nozzle)]
             ;lower row
             [(- mx third-nozzle) (- min-y third-nozzle)]
             [(+ mx third-nozzle) (- min-y third-nozzle)]]
           :else nil)))


;(count (range -10 10 0.1))
;(aabb-flood-points [-10 -10 10 10] 0.1)
;(aabb-flood-points [-0.5 -0.5 0.5 0.5] 1)
;(into #{} [1 2 3])

;(def debugging (atom []))
;(def tree-debug (atom nil))
;(def aabb-debug (atom nil))
;(count @debugging)
;(nth @debugging 1)
;(doseq [ind (range (count @debugging))]
;  (slicer.draw/gui-main (nth @debugging ind) @tree-debug @aabb-debug (str "resources/pic/flood-debug-" ind ".png")))


(defn flood-node
  "geometrically flood the nodes that are intersecting with the given aabbs, given collision of either true or false.
  the second aabb is the root aabb box for the tree t.
  returns:
  #{leaf-index ...}"
  [aabbs-or-points aabb t nozzle-diameter collision]
  (let [init-flood-points (match [(count (first aabbs-or-points))]
                                 [2] aabbs-or-points
                                 [4] (->> aabbs-or-points
                                          (map (fn [ab] (aabb-flood-points ab nozzle-diameter)))
                                          (reduce into #{})
                                          vec))
        init-flooded-leafs (->> init-flood-points
                                (map (fn [p] (tree/point-leaf p t aabb)))
                                (filter (complement nil?))
                                (filter (fn [i] (= collision (nth t i)))))
        flooded-set (atom (set init-flooded-leafs))]
    (loop [flooding-leafs init-flooded-leafs]
      (let [flood-points (->> flooding-leafs
                              (map (fn [i] (tree/index-to-aabb aabb tree/tree-arity i)))
                              (map (fn [ab] (aabb-flood-points ab nozzle-diameter)))
                              (reduce into #{})
                              (filter (complement nil?)))
            flooded-leafs (->> flood-points
                               (map (fn [p] (tree/point-leaf p t aabb))) ;;get leafs for the points
                               (filter (complement nil?))
                               (filter (fn [node] (not (contains? @flooded-set node)))) ;; remove the flooded ones
                               (filter (fn [i] (= collision (nth t i)))) ;;filtered according to collision boolean
                               )]
        (swap! flooded-set into flooded-leafs)
        (if (empty? flooded-leafs) ;; if the new flooded set is not grew, flooding is done
          @flooded-set
          (recur flooded-leafs))))))

;(keys (zipmap (map (comp keyword str) [1 2 3]) (repeat nil)))
;(vec (reduce into #{} '([1 2 3] [4 5 6])))
;(map inc #{1 2 3})


;(contains? #{1 2 3} 4)
;(conj #{} 1)
;(set nil)
;(set [0 1 2 3 4])
;(trampoline (flood [true false true true true])) => 1
;(trampoline (flood [true true true true false])) => 4
;(tree/leafs [true false true true true])

;(let [a [1 2 3]
;      b (atom a)
;      _ (swap! b rest)]
;  [a @b]
;  )

; Now to look for contained space, I use line-slice intersection.
; Shoot a list of parallel lines along the longer axis where the slice sits.
; If the distance between first and second intersection points is larger than the smallest node (nozzle diameter)
; then we have a flooding point to start.
;
; But what if the the model has no contained spaced after made as quad-tree,
; then a simple outer flood with all the collided nodes will left nothing.
;
; Out most flood should be done first to see if there is contained space.
; then line intersection checks
; then the inner flooding.
;
; In case of line intersection checks finds no flooding point,
; while out most flood + collided nodes = all leafs,
; the lines are not generated good enough.

;(map vector
;  (map vector [1 2 3] [3 4 5])
;  (map vector [1 2 3] [3 4 5]))
;
;(into [1 2 3] [4 5 6])

(defn move-point-towards-point
  "giveing two points, return the point distant d away from p1
  ---*-------------x-d-*----"
  [[x1 y1 :as p1] [x2 y2 :as p2] d]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (match [(pos? dx) (pos? dy) (neg? dx) (neg? dy) (zero? dx) (zero? dy)]
           [true true _ _ _ _] [(+ x1 d) (+ y1 d)]
           [true _ _ true _ _] [(+ x1 d) (- y1 d)]
           [_ true true _ _ _] [(- x1 d) (+ y1 d)]
           [_ _ true true _ _] [(- x1 d) (- y1 d)]
           [_ true _ _ true _] [x1 (+ y1 d)]
           [true _ _ _ _ true] [(+ x1 d) y1]
           [_ _ true _ _ true] [(- x1 d) y1]
           [_ _ _ true true _] [x1 (- y1 d)]
           )))

;(move-point-towards-point [0 0] [0 1] 0.1)

(defn mid-point
  "given two points, return middle point"
  [[x1 y1 :as p1] [x2 y2 :as p2]]
  (let [dx (/ (- x2 x1) 2)
        dy (/ (- y2 y1) 2)]
    [(double (+ x1 dx)) (double (+ y1 dy))]))

;(mid-point [0 0] [2 2])
;(mid-point [0 0] [-2 -2])
;(mid-point [1 0] [-2 -2])

(defn line-slice-flood-point
  "give a line segment, a slice, returns the flood point or nil if none exist.
  --*x*------------*--x*---"
  [line a-slice nozzle-diameter]
  (let [intersections (tree/line-slice-inc line a-slice)]
    (cond
      (empty? intersections)
      nil
      (even? (count intersections)) ;there are some missing intersecitons, even? will filter those out.
      (let [[x1 y1 :as p1] (first intersections)
            [x2 y2 :as p2] (second intersections)]
        (debugger intersections "intersections: ")
        (cond
          (or ;if a min-node aabb can fit in first two points
            (> (Math/abs (- x2 x1)) nozzle-diameter)
            (> (Math/abs (- y2 y1)) nozzle-diameter))
          (move-point-towards-point p1 p2 (* 1.1 nozzle-diameter)) ;return the middle point
          :else nil ))
      :else
      nil)))

(defn find-contained-flooding-point
  "generate a list of parallel lines from AABB of a slice,
  find the point that is contained in the slice"
  [a-slice nozzle-diameter [min-x min-y max-x max-y :as aabb]]
  (let [;[min-x min-y max-x max-y :as aabb] (tree/aabb-slice a-slice (* 2 nozzle-diameter))
        x-points (range (+ min-x (* nozzle-diameter 1.5)) (- max-x (* nozzle-diameter 1.5)) nozzle-diameter)
        x-start-points (map vector x-points (repeat max-y))
        x-end-points (map vector x-points (repeat min-y))
        x-lines (map vector x-start-points x-end-points)
        y-points (range (+ min-y (* nozzle-diameter 1.5)) (- max-y (* nozzle-diameter 1.5)) nozzle-diameter)
        y-start-points (map vector (repeat max-x) y-points)
        y-end-points (map vector (repeat min-x) y-points)
        y-lines (map vector y-start-points y-end-points)
        lines (into x-lines y-lines)
        ;_ (debugger lines "lines:")
        ]
    (loop [ind 0
           results []]
      (if (>= ind (count lines))
        results
        (let [flood-point (line-slice-flood-point (nth lines ind) a-slice nozzle-diameter)]
          (debugger (nth lines ind) "line: ")
          (debugger flood-point "intersection mid-point: ")
          (if (not (nil? flood-point))
            (recur (inc ind) (conj results flood-point))
            (recur (inc ind) results)
            ))))))

;(find-contained-flooding-point [[[10 10 1] [10 -10 1]]
;                                [[-10 -10 1] [10 10 1] [10 -10 1]]
;                                [[1 1]]]
;                                1)
;
;(find-contained-flooding-point [[[1 1 1] [1 -1 1]]
;                                [[-1 -1 1] [1 1 1] [1 -1 1]]
;                                [[1 1]]]
;                               1)

(defn fast-flood
  "above flood is so slow, why not a new one.
  faster, but sacrificed some accuracy.
  This will not flood a slice correctly if tree is not generated with a border.
  border needs to be at least two times of the nozzle size"
  [t aabb nozzle-diameter a-slice]
  (let [
        outer-aabbs (flooding-aabb-gen aabb)
        _ (debugger outer-aabbs "outer aabbs:")
        outer-nodes (flood-node outer-aabbs aabb t nozzle-diameter false)
        contained-points (find-contained-flooding-point a-slice nozzle-diameter aabb)
        debug-nodes (->> contained-points
                         (map (fn [p] (tree/point-leaf p t aabb)))
                         (filter (complement nil?))
                         )
        contained-nodes (if (empty? contained-points)
                          nil
                          (flood-node contained-points aabb t nozzle-diameter false))
        ]
    ;outer-nodes
    contained-nodes
    ;debug-nodes
    ))

