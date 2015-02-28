;this is tree generation according to the sliced data
(ns slicer.tree
  (:require [clojure.core.match :refer [match]])
  (:use slicer.util))

(def tree-arity 4);changing this will affect the performance of this algorithm. aware that some functions (split-aabb ...) are not arity changable

(defn line-box-inc
  "check if a line start and end by two points is intersected with an AABB box. Imprative since performance is important"
  ([[x1 y1 :as line-start] [x2 y2 :as line-end]
    [min-x min-y max-x max-y :as aabb]]
   (line-box-inc [x1 y1] [x2 y2] [min-x min-y] [max-x max-y])
    )
  ([[x1 y1 :as line-start] [x2 y2 :as line-end]
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
       ))))

(defn tri-box-inc
  "check if a triangle intersects with an AABB box."
  ([[x1 y1 :as tri-1] [x2 y2 :as tri-2] [x3 y3 :as tri-3]
    [min-x min-y max-x max-y :as aabb]]
   (tri-box-inc [x1 y1] [x2 y2] [x3 y3] [min-x min-y] [max-x max-y])
    )
  ([[x1 y1 :as tri-1] [x2 y2 :as tri-2] [x3 y3 :as tri-3]
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
     )))

(defn point-box-inc
  "check if a point is inside an AABB"
  ([[x1 y1 :as point]
    [min-x min-y :as box-min]
    [max-x max-y :as box-max]]
   (and (>= x1 min-x) (<= x1 max-x) (>= y1 min-y) (<= y1 max-y)))
  ([point [min-x min-y max-x max-y :as aabb]]
   (point-box-inc point [min-x min-y] [max-x max-y])))

(defn line-line-inc
  "check if two lines intersects"
  [[x1 y1 :as start-1] [x2 y2 :as end-1] [x3 y3 :as start-2] [x4 y4 :as end-2]]
  (let [aabb1 [(min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)]
        aabb2 [(min x3 x4) (min y3 y4) (max x3 x4) (max y3 y4)]
        a1 (- y2 y1)
        b1 (- x1 x2)
        c1 (+ (* a1 x1) (* b1 y1))
        a2 (- y4 y3)
        b2 (- x3 x4)
        c2 (+ (* a2 x3) (* b2 y3))
        det (- (* a1 b2) (* a2 b1))]
    (if (zero? det)
      nil
      (let [x (/ (- (* b2 c1) (* b1 c2)) det)
            y (/ (- (* a1 c2) (* a2 c1)) det)
            intersect? (and (point-box-inc [x y] aabb1)
                            (point-box-inc [x y] aabb2))]
        (if intersect?
          [(double x) (double y)]
          nil)))))

;(line-line-inc [-1 0] [1 0] [0 -1] [0 1])
;(line-line-inc [-1 0] [1 0] [-1 1] [1 1])
;(line-line-inc [-1 0] [1 0] [2 -1] [2 1])
;(line-line-inc [-1 0] [2 0] [2 -1] [2 1])
;(line-line-inc [-1 0] [2 0] [2 3] [2 1])


(defn distant-closer-to-point [[x1 y1 :as p1]]
  "give a point, returns a function taht takes two points,
  and returns if the distance between two points are closer to the first point"
  (fn [[x2 y2 :as p2] [x3 y3 :as p3]]
    (match [p2 p3]
           [[x2 y2 ] [x3 y3]]
           (let [delta-x1 (Math/abs (- x2 x1))
                 delta-y1 (Math/abs (- y2 y1))
                 delta-x2 (Math/abs (- x3 x1))
                 delta-y2 (Math/abs (- y3 y1))]
             (< (+ delta-x1 delta-y1) (+ delta-x2 delta-y2)))
           [p2 nil] false
           [nil p3] true
           :else false)))

;(reduce into (sorted-set-by (distant-point [0 0]))
;        [[[3 4] [4 4] nil nil [1 1] nil [2 2]]
;         nil
;         [[4 3] [3 3]]])

(defn line-slice-inc
  "check segment of line and slice intersection.
  returns first intersections in order of their distance to start point"
  [[[sx1 sy1 :as start] [ex2 ey2 :as end] :as line] a-slice]
  (vec (reduce into (sorted-set-by (distant-closer-to-point start))
    (for [geo a-slice]
      (match [geo]
             [[[x1 y1 z1][x2 y2 z2][x3 y3 z3]]] ;triangle
             [(line-line-inc start end [x1 y1] [x2 y2])
              (line-line-inc start end [x2 y2] [x3 y3])
              (line-line-inc start end [x3 y3] [x1 y1])]
             [[[x1 y1 z1][x2 y2 z2]]] ;line
             [(line-line-inc start end [x1 y1] [x2 y2])]
             :else nil )))))

;(line-slice-inc [[0 0] [10 0]]
;                [[[1 1 1] [1 -1 1]]
;                 [[-1 -1 1] [1 1 1] [1 -1 1]]
;                 [[1 1]]])

;(line-line-inc [0 0] [10 0] [1 1] [1 -1])
;(line-line-inc [-1 0] [10 0] [-1 1] [1 -1])
;(line-line-inc [-1 0] [10 0] [-1 -1] [1 1])


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
  [a-slice & [border]]
  (loop [geos a-slice
         min-x 0
         min-y 0
         max-x 0
         max-y 0]
    (if (<= (count geos) 0)
      (if (nil? border)
                 [min-x min-y max-x max-y]
                 [(- min-x border) (- min-y border) (+ max-x border) (+ max-y border)])
      (match [(first geos)]
             [[[x1 y1 _] [x2 y2 _] [x3 y3 _]]] ;triangle
               (let [[mx my maxx mayy] (aabb-tri (first geos))]
                 (recur (rest geos) (min min-x mx) (min min-y my) (max max-x maxx) (max max-y mayy)))
             [[[x1 y1 _] [x2 y2 _]]] ;line
               (let [[mx my maxx mayy] (aabb-line (first geos))]
                 (recur (rest geos) (min min-x mx) (min min-y my) (max max-x maxx) (max max-y mayy)))
             [[x1 y1 _]] ;point
               (recur (rest geos) (min min-x x1) (min min-y y1) (max max-x x1) (max max-y y1))
             :else
               (if (nil? border)
                 [min-x min-y max-x max-y]
                 [(- min-x border) (- min-y border) (+ max-x border) (+ max-y border)])
                     ))))

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

(defn tree-nodes-count
  "the totoal number of nodes from height for K-arity based tree"
  [height base]
  (/ (dec (Math/pow base height)) (dec base)))

(defn height
  "given tree or its leafs count, return its height"
  [t b]
  (cond
   (or (seq? t) (vector? t)); if given a tree, return its height
   (let [d (dec b)
         c (count t)
         a (Math/log (inc (* c d)))]
     (int (Math/ceil (/ a (Math/log b)))))
   (number? t); if given a leaf count, return its height
   (let [d (Math/log 4)
        c (Math/log t)]
    (int (inc (Math/ceil (/ c d)))))
   :else nil))

;(height [1] 4)
;(height (vec (range 5)) 4)
;(height (vec (range 21)) 4)
;(height (vec (range 85)) 4)
;(height 4 4)
;(height 16 4)
;(height 65 4)
;(Math/log 4)

(defn index-to-hrp
  "given tree arity base and the index to one of its node, return height and level position across the same level. OLogN time."
  [ind base]
  {:pre [(pos? base) (not (neg? ind))]
   }
  (loop [h 1]
    (if (>= (tree-nodes-count h base) (inc ind))
      (let [i (int (- ind (tree-nodes-count (dec h) base)))]
      {:height h
       :row-index i})
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
  "given base, height and row-index, returns a walk from the root to the node. OLogN time."
  [b h r]
  {:pre [(< r (Math/pow b h))]}
  (let [div #(quot % b)
        divs #(iterate div %)
        pos #(mod % tree-arity)
        aabb-walk (map #(identity {:position
                                   (case (mod %1 tree-arity) 0 :upper-left 1 :upper-right 2 :lower-left 3 :lower-right)
                                   :height %2})
                       (take h (divs r)) ;reverse walking in row indexes
                       (reverse (range 1 (inc h))))
        ]
    aabb-walk
    ))

;(aabb-walk 4 1 0)
;(aabb-walk 4 2 0)
;(aabb-walk 4 4 16)
;(mod 15 4)

(defn hr-to-aabb
  "given aabb, base, height and row index, return AABB box in OLogN time."
  [aabb b h r]
  (if (= 1 h)
    aabb
    (loop [walkings (reverse (drop-last (aabb-walk b h r)))
           p (:position (first walkings))
           current-aabb (split-aabb aabb p)]
      (let [next-walkings (rest walkings)]
      (if (empty? next-walkings)
        current-aabb
        (recur next-walkings (:position (first next-walkings)) (split-aabb current-aabb (:position (first next-walkings)))))))))

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

(defn index-to-aabb
  "given aabb, base and index, returns aabb. OLogN time."
  [aabb b i]
  (let [hrp (index-to-hrp i b)]
    (hr-to-aabb aabb b (:height hrp) (:row-index hrp))))

(defn parent
  "given a node index, returns parent index"
  [i]
  {:pre [(integer? i)]}
  (cond
   (= i 0) nil
   (< i 5) 0
   :else (let [hr (index-to-hrp i tree-arity)
               grandparent-node-count (tree-nodes-count (- (:height hr) 2) tree-arity)
               parent-row-index (Math/floor (/ (:row-index hr) tree-arity))]
           (int (+ grandparent-node-count parent-row-index)))))

;(parent 1)
;(parent 5)
;(parent 21)
;(parent 85)
;(parent 341)

(defn children
  "given a node index, returns children indexes:
  [child-upper-left child upper-right child-lower-left child-lower-right]"
  [i]
  {:pre [(integer? i)]}
  (let [hr (index-to-hrp i tree-arity)
        a (-> (->> hr
                   :height)
              (tree-nodes-count tree-arity))
        e (-> hr
              :row-index
              inc
              (* 4)
              (+ a)
              dec
              int)]
    [(- e 3) (- e 2) (dec e) e]))

;(children 0)
;(children 1)
;(children 2)
;(children 3)
;(children 4)
;(children 5)

(defn generate-tree
  "generate tree down to the lowest level in BFS order. O(NLogN) time"
  [a-slice nozzle-diameter & [border]]
  {:pre [(seq? a-slice)
         (number? nozzle-diameter)]}
  (let [[min-x min-y max-x max-y :as aabb] (if (nil? border)
                                             (-> a-slice aabb-slice make-square)
                                             (-> a-slice (aabb-slice border) make-square))
        diff-x (- max-x min-x)
        leaf-num (let [round-up (/ diff-x nozzle-diameter)]
                   (int (Math/pow round-up 2)))
        tree-height (height leaf-num tree-arity)
        node-count (tree-nodes-count tree-height tree-arity)
        result (atom (vec (repeat node-count nil)))]
    (doseq [ind (range node-count)]
      (cond
       (= ind 0);get collsion test for root node
       (swap! result assoc ind
              (-> (index-to-aabb aabb tree-arity ind)
                  (slice-box-inc a-slice)))
       (true? (nth @result (parent ind)));only get collision test for nodes which parent is collided
       (swap! result assoc ind
              (-> (index-to-aabb aabb tree-arity ind)
                  (slice-box-inc a-slice)))))
    @result
    ))

(defn adjacent
  "given indexes of two nodes, returns if their AABB boxes are adjacent to each other"
  [n1 n2 aabb]
  {:pre [(integer? n1) (integer? n2)]}
  (let [[min-x1 min-y1 max-x1 max-y1 :as n1-aabb] (index-to-aabb aabb tree-arity n1)
        [min-x2 min-y2 max-x2 max-y2 :as n2-aabb] (index-to-aabb aabb tree-arity n2)]
    (cond
     (= min-x1 max-x2) true
     (= min-x2 max-x1) true
     (= min-y1 max-y2) true
     (= min-y2 max-y1) true
     :else false)))

(defn aabb-inc
  "given two aabb returns if they are intersecting with each other"
  [[min-x1 min-y1 max-x1 max-y1 :as aabb1] [min-x2 min-y2 max-x2 max-y2 :as aabb2]]
  (cond
       (and
        (or (and (>= min-x2 min-x1) (<= min-x2 max-x1))
            (and (>= max-x2 min-x1) (<= max-x2 max-x1)))
        (or (and (>= min-y2 min-y1) (<= min-y2 max-y1))
            (and (>= max-y2 min-y1) (<= max-y2 max-y1)))) true
     :else false))

(defn node-inc
  "given indexes of two nodes, returns if their AABB boxes are intersecting with each other"
  [n1 n2 aabb]
  {:pre [(integer? n1) (integer? n2)]}
  (let [n1-aabb (index-to-aabb aabb tree-arity n1)
        n2-aabb (index-to-aabb aabb tree-arity n2)]
    (aabb-inc n1-aabb n2-aabb)))

(defn false-ancestor
  "given a tree and an index of an nil node. returns its ancestor which is a false node"
  [t i]
  {:pre [(-> t (nth i) nil?)]}
  (loop [parent-node (parent i)]
    (cond
      (false? (nth t parent-node)) parent-node
      :else (recur (parent parent-node))
      )
    )
  )

;(false-ancestor [false nil nil nil false nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] 17)

(defn leafs
  "given a tree, returns its leafs"
  [t]
  (let [last-index (-> t count dec)
        hr (index-to-hrp last-index tree-arity)
        last-row-start-index (-> hr :height dec (tree-nodes-count tree-arity) int)]
    (->
     (for [i (range last-row-start-index (inc last-index))]
       (if (nil? (nth t i))
         (let [anc (false-ancestor t i)] [anc (nth t anc)])
         [i (nth t i)]))
     set
     vec)))

;(leafs [true false false false true nil nil nil nil nil nil nil nil nil nil nil nil true false false false])
;(leafs (range 5))
;(leafs (range 21))
;(leafs (range 85))

(defn center-aabb
  "give aabb, put it to the center so that [0 0 20 20] becomes [-10 -10 10 10]"
  [[min-x min-y max-x max-y :as aabb]]
  (let [delta-x (-> (- max-x min-x) (/ 2) (+ min-x))
        delta-y (-> (- max-y min-y) (/ 2) (+ min-y))]
    [(- min-x delta-x)  (- min-y delta-y)  (- max-x delta-x)  (- max-y delta-y)]))

;(center-aabb [-1 -1 5 5])
;(center-aabb [-11 -11 -5 -5])
;(center-aabb [11 11 15 15])

(defn point-leaf
  "given a point, returns the leaf node where it sits on"
  [p t aabb]
  {:pre [
         ;(do (debugger p "P:") true)
         (number? (first p)) (number? (second p)) (vector? t) (number? (first aabb))
         ]}
  (if (point-box-inc p aabb)
    (let [child-nodes (children 0)
          child-aabbs (map (fn [i] (index-to-aabb aabb tree-arity i)) child-nodes)
          sits-in-node (match [(point-box-inc p (first child-aabbs))
                               (point-box-inc p (nth child-aabbs 1))
                               (point-box-inc p (nth child-aabbs 2))
                               (point-box-inc p (nth child-aabbs 3))]
                              [true _ _ _] (first child-nodes)
                              [_ true _ _] (nth child-nodes 1)
                              [_ _ true _] (nth child-nodes 2)
                              [_ _ _ true] (nth child-nodes 3))]
      (loop [node sits-in-node]
        (cond
          ;out of bounce
          (-> (children node)
              last
              (>= (count t)))
          node
          ;false node is a leaf node
          (false? (nth t node))
          node
          ;true node is needs check deeper level
          (nth t node)
          (let [c-nodes (children node)
                c-aabbs (map (fn [i] (index-to-aabb aabb tree-arity i)) c-nodes)]
            (recur (match [(point-box-inc p (first c-aabbs))
                           (point-box-inc p (nth c-aabbs 1))
                           (point-box-inc p (nth c-aabbs 2))
                           (point-box-inc p (nth c-aabbs 3))]
                          [true _ _ _] (first c-nodes)
                          [_ true _ _] (nth c-nodes 1)
                          [_ _ true _] (nth c-nodes 2)
                          [_ _ _ true] (nth c-nodes 3))))
          ;you shouldn't be here
          :else :warning-intruder-alert! )))
    nil)
)

;(point-leaf [1 1] [true false true false false nil nil nil nil false true false false nil nil nil nil nil nil nil nil] [-10 -10 10 10])
;(point-leaf [-1 1] [true false true false false nil nil nil nil false true false false nil nil nil nil nil nil nil nil] [-10 -10 10 10])
;(point-leaf [-1 -1] [true false true false false nil nil nil nil false true false false nil nil nil nil nil nil nil nil] [-10 -10 10 10])
;(point-leaf [1 -1] [true false true false false nil nil nil nil false true false false nil nil nil nil nil nil nil nil] [-10 -10 10 10])