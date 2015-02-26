;after sliced the model, we need to check for watertight and discretized the slice shape
(ns slicer.flood
  (:require [clojure.core.match :refer [match]]
            [slicer.tree :as tree])
  (:use slicer.util))


(defn flooding-aabb-gen
  [[min-x min-y max-x max-y :as aabb]]
  (let [w (- max-x min-x)
        h (- max-y min-y)]
  [[(- min-x w) min-y max-x max-y]
   [max-x max-y max-x (+ max-y h)]
   [max-x min-y (+ max-x w) max-y]
   [min-x (- min-y h) max-x min-y]]))

(defn flood
  "given a tree and aabb generated from the slice, flood the tree from outside and return the nodes that are flooded"
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
  (match [(< (- max-x min-x) (* 2 nozzle-diameter))]
         [true]
         (let [[mx my :as mid-point] [(-> (- max-x min-x) (/ 2) (+ min-x)) (-> (- max-y min-y) (/ 2) (+ min-y))]]
           [[(- mx nozzle-diameter) my]
            [(+ mx nozzle-diameter) my]
            [mx (- my nozzle-diameter)]
            [mx (+ my nozzle-diameter)]])
         [false]
         (let [x-points (range (+ min-x (/ nozzle-diameter 2)) max-x nozzle-diameter)
               y-points (range (+ min-y (/ nozzle-diameter 2)) max-y nozzle-diameter)]
           (-> (into [] (map (fn [p] [(double (- (first x-points) (/ nozzle-diameter 2))) (double p)]) y-points)) ;left column
               (into (map (fn [p] [(double (+ (last x-points) (/ nozzle-diameter 2))) (double p)]) y-points)) ;right column
               (into (map (fn [p] [(double p) (double (- (first y-points) (/ nozzle-diameter 2)))]) x-points)) ;lower row
               (into (map (fn [p] [(double p) (double (+ (last y-points) (/ nozzle-diameter)))]) x-points)) ;upper row
               ))
         :else :Schrodinger-cat))

;(count (range -10 10 0.1))
;(count (aabb-flood-points [-10 -10 10 10] 0.1))
;(into #{} [1 2 3])

(defn flood-node
  "geometrically flood the nodes that are intersecting with the given aabbs, given collision of either true or false.
  the second aabb is the root aabb box for the tree t.
  returns:
  #{leaf-index ...}"
  [aabbs aabb t nozzle-diameter collision]
  (let [init-flood-points (->> aabbs
                              (map (fn [ab] (aabb-flood-points ab nozzle-diameter)))
                              (reduce into #{})
                              vec)
        init-flooded-leafs (->> init-flood-points
                                (map (fn [p] (tree/point-leaf p t aabb)))
                                (filter (complement nil?)))
        flooded-set (atom (set init-flooded-leafs))]
    (loop [flood-count (count @flooded-set)]
      (let [flood-points (->> @flooded-set
                              (map (fn [i] (tree/index-to-aabb aabb tree/tree-arity i)))
                              (map (fn [ab] (aabb-flood-points ab nozzle-diameter)))
                              (reduce into #{})
                              (filter (complement nil?))
                              vec)
            flooded-leafs (->> flood-points
                               (map (fn [p] (tree/point-leaf p t aabb))) ;;get leafs for the points
                               (filter (complement nil?))
                               (filter (fn [i] (= collision (nth t i)))) ;;filtered according to collision boolean
                               )]
      (doseq [leaf flooded-leafs]
        (swap! flooded-set conj leaf))
      (if (<= (count @flooded-set) flood-count) ;; if the new flooded set is not grew, flooding is done
        @flooded-set
        (recur (count @flooded-set)))))))

;(keys (zipmap (map (comp keyword str) [1 2 3]) (repeat nil)))
;(vec (reduce into #{} '([1 2 3] [4 5 6])))
;(map inc #{1 2 3})

(defn better-flood
  "above flood is so slow, why not a new one"
  [t aabb nozzle-diameter]
  (let [flooding-aabbs (flooding-aabb-gen aabb)
        flooded-nodes (flood-node flooding-aabbs aabb t nozzle-diameter false)]
    flooded-nodes
    ))

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
