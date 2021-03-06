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

;since aabb can be smaller than nozzle-diameter, this functin needs extra care

;(range 0.5 1 0.5)
(defn aabb-flood-points
  "generate flooding points for interseciton check from aabb"
  [[min-x min-y max-x max-y :as aabb] leaf-size]
  (let [hl (/ leaf-size 2)
        x-points (range (+ min-x hl)  max-x hl)
        y-points (range (+ min-y hl)  max-y hl)
        left-points (map #(identity [(- min-x hl) %]) y-points)
        right-points (map #(identity [(+ max-x hl) %]) y-points)
        up-points (map #(identity [% (+ max-y hl)]) x-points)
        low-points (map #(identity [% (- min-y hl)]) x-points)]
    (-> left-points
        (into right-points)
        (into up-points)
        (into low-points))))

;(aabb-flood-points [0 0 1 1] 1) => ([1/2 -1/2] [1/2 3/2] [3/2 1/2] [-1/2 1/2])
;(aabb-flood-points [0 0 2 2] 1)

;(defn aabb-flood-points
;  "generate flooding points for interseciton check from aabb"
;  [[min-x min-y max-x max-y :as aabb] leaf-size]
;  (let [[mx my :as mid-point] [(-> (- max-x min-x)
;                                   (/ 2)
;                                   (+ min-x))
;                               (-> (- max-y min-y)
;                                   (/ 2)
;                                   (+ min-y))]
;        dx (min leaf-size (- max-x min-x))
;        dy (min leaf-size (- max-y min-y))]
;         [;left column
;           [(- min-x (/ dx 2)) (- my (/ dy 2))]
;           [(- min-x (/ dx 2)) (+ my (/ dy 2))]
;           ;right column
;           [(+ max-x (/ dx 2)) (- my (/ dy 2))]
;           [(+ max-x (/ dx 2)) (+ my (/ dy 2))]
;           ;upper row
;           [(- mx (/ dx 2)) (+ max-y (/ dy 2))]
;           [(+ mx (/ dx 2)) (+ max-y (/ dy 2))]
;           ;lower row
;           [(- mx (/ dx 2)) (- min-y (/ dy 2))]
;           [(+ mx (/ dx 2)) (- min-y (/ dy 2))]]
;           ))


;(count (range -10 10 0.1))
;(aabb-flood-points [-10 -10 10 10] 0.1)
;(aabb-flood-points [-0.5 -0.5 0.5 0.5] 1)
;(into #{} [1 2 3])

;(def debugging (atom []))
;(def tree-debug (atom nil))
;(def aabb-debug (atom nil))
;(count @debugging)
;(nth @debugging 1)
;(slicer.draw/gui-main @debugging @tree-debug @aabb-debug (str "resources/pic/fdd-1.png"))

(defn flood-node
  "geometrically flood the nodes that are intersecting with the given aabbs,
  given collision of either true or false.
  the second aabb is the root aabb box for the tree t.
  returns:
  #{leaf-index ...}"
  [aabbs-or-points aabb t leaf-size collision]
  (let [init-flood-points (match [(count (first aabbs-or-points))]
                                 [2] aabbs-or-points
                                 [4] (->> aabbs-or-points
                                          (map (fn [ab] (aabb-flood-points ab leaf-size)))
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
                              (map (fn [ab] (aabb-flood-points ab leaf-size)))
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
  [line a-slice leaf-size]
  (let [intersections (tree/line-slice-inc line a-slice)]
    (cond
      (empty? intersections)
      nil
      (even? (count intersections)) ;there are some missing intersecitons, even? will filter those out.
      (let [[x1 y1 :as p1] (first intersections)
            [x2 y2 :as p2] (second intersections)]
        (cond
          (or ;if a min-node aabb can fit in first two points
            (> (Math/abs (- x2 x1)) leaf-size)
            (> (Math/abs (- y2 y1)) leaf-size))
          (mid-point p1 p2) ;(move-point-towards-point p1 p2 (* 1.1 leaf-size)) ;return the middle point
          :else nil ))
      :else
      nil)))

(defn find-contained-flooding-point
  "generate a list of parallel lines from AABB of a slice,
  find the point that is contained in the slice"
  [a-slice leaf-size [min-x min-y max-x max-y :as aabb]]
  (let [x-points (range (+ min-x (* leaf-size 1.5)) (- max-x (* leaf-size 1.5)) (/ leaf-size 2))
        x-start-points (map vector x-points (repeat max-y))
        x-end-points (map vector x-points (repeat min-y))
        x-lines (map vector x-start-points x-end-points)
        y-points (range (+ min-y (* leaf-size 1.5)) (- max-y (* leaf-size 1.5)) (/ leaf-size 2))
        y-start-points (map vector (repeat max-x) y-points)
        y-end-points (map vector (repeat min-x) y-points)
        y-lines (map vector y-start-points y-end-points)
        lines (into x-lines y-lines)]
    (loop [ind 0
           results []]
      (if (>= ind (count lines))
        results
        (let [flood-point (line-slice-flood-point (nth lines ind) a-slice leaf-size)]
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
  [t aabb a-slice]
  (let [
        ;outer-aabbs (flooding-aabb-gen aabb)
        ;_ (debugger outer-aabbs "outer aabbs:")
        ;outer-nodes (flood-node outer-aabbs aabb t leaf-size false)
        ;debug-nodes (->> contained-points (map (fn [p] (tree/point-leaf p t aabb))) (filter (complement nil?)))
        leaf-size (tree/tree-leaf-size t aabb)
        edges (filter (fn [i] (nth t i)) (map first (tree/leafs t)))
        contained-points (find-contained-flooding-point a-slice leaf-size aabb)
        contained-nodes (if (empty? contained-points)
                          nil
                          (flood-node contained-points aabb t leaf-size false))]
    (into edges contained-nodes)
    ))

