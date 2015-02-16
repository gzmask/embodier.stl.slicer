;after sliced the model, we need to check for watertight and discretized the slice shape
(ns slicer.flood
  (:require [clojure.core.match :refer [match]]))


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
   ))

(defn point-box-inc
  "check if a point is inside an AABB"
  [[x1 y1 :as point]
   [min-x min-y :as box-min]
   [max-x max-y :as box-max]]
  (and (>= x1 min-x) (<= x1 max-x) (>= y1 min-y) (<= y1 max-y)))

(defmacro debugger [expr & [msg]]
  `(do
     (when ~msg (println ~msg))
     (println ~expr)
     ~expr
     )
  )

(defn slice-box-inc
  "check if a slice is intersecting an AABB"
  [[min-x min-y max-x max-y :as aabb-box] a-slice]
  (reduce #(or %1 %2) false
    (for [geo a-slice]
      (match [geo]
             [[[x1 y1 z1][x2 y2 z2][x3 y3 z3]]]
               (tri-box-inc [x1 y1] [x2 y2] [x3 y3] [min-x min-y] [max-x max-y])
             [[[x1 y1 z1][x2 y2 z2]]]
               (line-box-inc [x1 y1] [x2 y2] [min-x min-y] [max-x max-y])
             [[x1 y1 z1]]
               (point-box-inc [x1 y1] [min-x min-y] [max-x max-y])
             :else false))))

(defn aabb-tri
  "get aabb from triangle"
  [[[x1 y1 _] [x2 y2 _] [x3 y3 _] :as tri]]
  [(min x1 x2 x3) (min y1 y2 y3) (max x1 x2 x3) (max y1 y2 y3)])

(defn aabb-line
  "get aabb from line"
  [[[x1 y1 _] [x2 y2 _] :as line]]
  [(min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)])

(defn aabb-slice
  "get aabb box from a list of geometries"
  [a-slice]
  (loop [geos a-slice
         min-x 0
         min-y 0
         max-x 0
         max-y 0]
    (if (<= (count geos) 0)
      [min-x min-y max-x max-y]
      (match [(first geos)]
             [[[x1 y1 _][x2 y2 _][x3 y3 _]]] ;triangle
               (let [[mx my maxx mayy] (aabb-tri (first geos))]
                 (recur (rest geos) (min min-x mx) (min min-y my) (max max-x maxx) (max max-y mayy)))
             [[[x1 y1 _][x2 y2 _]]] ;line
               (let [[mx my maxx mayy] (aabb-line (first geos))]
                 (recur (rest geos) (min min-x mx) (min min-y my) (max max-x maxx) (max max-y mayy)))
             [[x1 y1 _]] ;point
               (recur (rest geos) (min min-x x1) (min min-y y1) (max max-x x1) (max max-y y1))
             :else
               [min-x min-y max-x max-y]))))

(defn smaller-than-nozzle?
  "is the current aabb is smaller than the nozzle"
  [[min-x min-y max-x max-y :as aabb] nozzle-diameter]
  (cond
   (<= (- max-x min-x) nozzle-diameter) true
   (<= (- max-y min-y) nozzle-diameter) true
   :else false
   )
  )

(defn make-square
  "make an aabb BOX square / width = height"
  [[min-x min-y max-x max-y :as aabb]]
  (let [delta-x (Math/abs (- max-x min-x))
        delta-y (Math/abs (- max-y min-y))]
    (if (> delta-x delta-y)
      [min-x min-y max-x (+ delta-x min-y)]
      [min-x min-y (+ delta-y min-x) max-y])))

(defn split-aabb
  "split aabb box to four smaller boxes"
  ([aabb pos]
  {:pre [(keyword? pos)]}
   (let [aabbs (split-aabb aabb)]
     (case pos
       :upper-left (first aabbs)
       :upper-right (second aabbs)
       :lower-left (nth aabbs 2)
       :lower-right (nth aabbs 3)
       :else nil)))
  ([[min-x min-y max-x max-y :as aabb]]
   (let [delta-x (/ (Math/abs (- max-x min-x)) 2)
         delta-y (/ (Math/abs (- max-y min-y)) 2)]
     [[min-x (+ min-y delta-y) (- max-x delta-x) max-y]
      [(+ min-x delta-x) (+ min-y delta-y) max-x  max-y]
      [min-x min-y (- max-x delta-x) (- max-y delta-y)]
      [(+ min-x delta-x) min-y max-x (- max-y delta-y)]])))

;;follwing three functions are deprecated due to potential StackOverflowErrors. commented out as referencing matters.
(comment
(declare make-node)

(defn make-leaf
  [aabb a-slice pos nozzle-diameter]
  (let [aabb-node (split-aabb aabb pos)
        toosmall? (smaller-than-nozzle? aabb-node nozzle-diameter)
        intersects? (slice-box-inc aabb-node a-slice)]
    (cond (and toosmall? intersects?) [:leaf aabb-node intersects?]
          (and toosmall? (not intersects?)) [:emptyleaf aabb-node intersects?]
          (and (not toosmall?) (not intersects?)) [:emptyleaf aabb-node intersects?]
          (and (not toosmall?) intersects?) (make-node [(case pos
                                                          :upper-left :floodingleafA
                                                          :upper-right :floodingleafB
                                                          :lower-left :floodingleafC
                                                          :lower-right :floodingleafD)]
                                                       aabb-node a-slice nozzle-diameter)
          :else [:error aabb-node]
          )))

;;this is a non-tail call recusive function. Need core.async optimization later
;;a better way is to use a set of BFS nodes of the whole tree down to the smallest node
;;then use pmap to check AABB collision and assigned the true/false value
;;way easier and fasters, no conrecursion too.
(defn make-node
  [tree aabb a-slice nozzle-diameter]
  (let [m-leaf #(identity
                 [:node [(make-leaf (split-aabb aabb %) a-slice :upper-left nozzle-diameter)
                         (make-leaf (split-aabb aabb %) a-slice :upper-right nozzle-diameter)
                         (make-leaf (split-aabb aabb %) a-slice :lower-left nozzle-diameter)
                         (make-leaf (split-aabb aabb %) a-slice :lower-right nozzle-diameter)]
                  aabb (slice-box-inc aabb a-slice)])]
    (match [(first tree)]
           [:floodingleafA]
            (m-leaf :upper-left)
           [:floodingleafB]
            (m-leaf :upper-right)
           [:floodingleafC]
            (m-leaf :lower-left)
           [:floodingleafD]
            (m-leaf :lower-right))
  ))

(defn make-tree
  "tree construction from a layer of slice"
  [a-slice nozzle-diameter]
  {:pre [(seq? a-slice)
         (number? nozzle-diameter)]}
  (let [aabb (-> (aabb-slice a-slice)
                 make-square)
        aabbs (split-aabb aabb)
        tree [:node
              (make-node [:floodingleafA] (first aabbs) a-slice nozzle-diameter)
              (make-node [:floodingleafB] (second aabbs) a-slice nozzle-diameter)
              (make-node [:floodingleafC] (nth aabbs 2) a-slice nozzle-diameter)
              (make-node [:floodingleafD] (nth aabbs 3) a-slice nozzle-diameter)
              aabb
              (slice-box-inc aabb a-slice)]]
    tree))
)

(defn tree-height
  "the least tree height require for containing certain number of leaf nodes"
  [leaf-num base]
  (loop [power 0]
    (if (>= (Math/pow base power) leaf-num)
      (inc power)
      (recur (inc power)))))

(defn tree-nodes-count
  "the totoal number of nodes from height for K-arity based tree"
  [height base]
  (/ (dec (Math/pow base height)) (dec base)))

(defn index-to-hrp
  "given tree arity base and the index to one of its node, return height and level position across the same level"
  [ind base]
  (loop [h 1]
    (if (>= (tree-nodes-count h base) (inc ind))
      (let [i (int (- ind (tree-nodes-count (dec h) base)))
            p (case (mod i base) 0 :upper-left 1 :upper-right 2 :lower-left 3 :lower-right)
            ]
      {:height h
       :row-index i
       :position p})
      (recur (inc h))
      )
    )
)

;repl tests
;(clojure.repl/doc case)
;(case 2
;  1 :1
;  2 :2)
;(mod 4 4)
;(index-to-hrp 4 4)

(defn aabb-walk
  "given base, height and row-index, returns a walk from the root to the node"
  [b h r]
  (let [div #(quot % b)
        divs #(iterate div %)
        pos #(mod % 4)
        aabb-walk (map #(identity {:position
                                   (case (mod %1 4) 0 :upper-left 1 :upper-right 2 :lower-left 3 :lower-right)
                                   :height %2})
                       (take h (divs r)) ;reverse walking in row indexes
                       (reverse (range 1 (inc h))))
        ]
    aabb-walk
    ))

(aabb-walk 4 4 16)
;(mod 15 4)

(defn hr-to-aabb
  "given aabb, base, height and row index, return AABB box"
  [aabb b h r]
  (loop [walkings (reverse (drop-last (aabb-walk b h r)))
         p (:position (first walkings))
         current-aabb (split-aabb aabb p)]
    (let [next-walkings (rest walkings)]
    (if (empty? next-walkings)
      current-aabb
      (recur next-walkings (:position (first next-walkings)) (split-aabb current-aabb (:position (first next-walkings))))))))

;(hr-to-aabb [-10 -10 10 10] 4 2 0)
;(hr-to-aabb [-10 -10 10 10] 4 3 15)

;(quot (quot 63 4) 4)
;(def tt1 #(quot % 4))
;(def tt2 #(iterate tt1 %))
;(def tt3 #(nth (tt2 %1) (- %2 2)))
;(tt3 16 4)
;(take 3 (tt2 30))
;(iterate #(quot % 4) 63)
;(iterate #(quot % 4) 15)
;(iterate #(quot % 4) 3)

(defn generate-BFS
  "generate an empty tree down to the lowest level in BFS order"
  [a-slice nozzle-diameter]
  (let [[min-x min-y max-x max-y:as aabb]  (-> (aabb-slice a-slice)
                                               make-square)
        diff-x (- max-x min-x)
        leaf-num (let [round-up (/ diff-x nozzle-diameter)]
                   (if (> round-up (int round-up))
                     (inc (int round-up))
                     (int round-up)))
        tree-height (tree-height leaf-num 4)
        node-count (tree-nodes-count tree-height 4)]
    (vec (repeat node-count false))
    )
  )
