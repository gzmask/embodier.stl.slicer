; according to the fact that:
; sum of all the degrees of all the vertices of a graph is equal to twice the total number of its edges,
; a.k.a Handshake Lemma
; we know that we can convert every graph to one has eulerian circuit.
; generate eulerian circuit for flooded nodes
(ns slicer.eulerian
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as s]
            [slicer.tree :as tree]
            [slicer.flood :as flood])
  (:use slicer.util))

;exmple of an neighbour-set:
;{:neg {:1 #{2 3 4}
;       :2 #{4 1}
;       :3 #{4 1}
;       :4 #{1 2 3}}
; :pos {:1 #{2 3 4}
;       :2 #{4 1}
;       :3 #{4 1}
;       :4 #{1 2 3}}}

(defn neighbours
  "give the a list of nodes and a leaf node, returns its neighbours within the list of nodes"
  [node nodes t aabb & [neighbour-set]]
  (let [node-aabb (tree/index-to-aabb aabb tree/tree-arity node)
        leaf-size (tree/tree-leaf-size t aabb)
        neighbour-points (flood/aabb-flood-points node-aabb leaf-size)
        results (->> neighbour-points
                     ;find leaf of each poine
                     (map #(tree/point-leaf % t aabb))
                     ;remove ones that are not in the flooded nodes
                     (filter #(contains? (set nodes) %))
                     set
                     vec
                     )]
    (if (nil? neighbour-set)
      results
      (->> results
           (filter (fn [n] ;remove ones that are in the neg neighbour-set
                (let [non-neighbours ((keyword (str node)) (:neg neighbour-set))]
                  (not (contains? non-neighbours n))))
              )
           (into ((keyword (str node)) (:pos neighbour-set))) ;add ones in pos neighbour-set
           vec
           ))))

;(contains? (set [1 2 3]) 4)
;(contains? (set [1 2 3]) 3)
;((keyword (str 3)) {:3 [1 2 3]})

(defn bodd? [a]
  (if (> a 2)
    (odd? a)
    false))

;this is too slow and not correct
;(defn remove-odd-deg-nodes
;  "remove adjacent nodes edge that connects nodes of both odd degree."
;  [nodes-with-odd-degrees t aabb pre-set]
;  (let [result-set (atom pre-set)]
;    (doseq [node-from nodes-with-odd-degrees]
;      (doseq [node-to (neighbours node-from nodes-with-odd-degrees t aabb @result-set)] ;neighbours of odd degrees
;          (swap! result-set update-in
;                 [:neg (keyword (str node-from))]
;                 conj node-to)
;          (swap! result-set update-in
;                 [:neg (keyword (str node-to))]
;                 conj node-from)))
;    @result-set))

(defn first-odd-node
  "find the first node of the searching-nodes that has odd degrees within nodes."
  [searching-nodes nodes t aabb pre-set]
  (loop [ind 0]
    (cond (>= ind (count searching-nodes))
          nil
          (bodd? (count (neighbours (nth searching-nodes ind) nodes t aabb pre-set)))
          (nth searching-nodes ind)
          :else (recur (inc ind)))))

(defn remove-odd-deg-nodes
  "remove adjacent odd-nodes edge that connects odd-nodes of both odd degree."
  [odd-nodes nodes t aabb pre-set searched-nodes]
  (let [node-odd-deg (first (s/difference odd-nodes searched-nodes))
        neighbour-node-odd-deg (if (not (nil? node-odd-deg))
                                  (first-odd-node (neighbours node-odd-deg nodes t aabb pre-set) nodes t aabb pre-set)
                                  nil)]
    (debugger (count odd-nodes) "counting odd-nodes")
    (debugger (count searched-nodes) "counting searched-odd-nodes")
    (match [(nil? node-odd-deg) (nil? neighbour-node-odd-deg)]
           [false false]; two adjacent odd-nodes with odd degrees are found
           (recur odd-nodes
                  nodes t aabb
                  (update-in pre-set [:neg (keyword (str node-odd-deg))] conj neighbour-node-odd-deg)
                  (conj searched-nodes node-odd-deg neighbour-node-odd-deg))
           [false true]; one node of odd degree without neighbour of odd degrees are found, and not all odd-nodes are searched.
           (recur odd-nodes nodes t aabb pre-set (conj searched-nodes node-odd-deg))
           :else
           pre-set
           )))

;(assoc-in {:neg {:1 #{2}}}
;          [:neg :1]
;          (conj (:1 (:neg {:neg {:1 #{2}}})) 3))

(defn min-index [v]
  (first (apply min-key second (map-indexed vector v))))

(defn connect-odd-deg-nodes
  "connect the nodes of odd degrees in a heruistic TSP fashion:
  links each node to its closest non-neighbour"
  [odd-nodes nodes current-node t aabb pre-set]
  (let [neighbour-nodes (neighbours current-node nodes t aabb pre-set)
        searching-odd-nodes (vec (s/difference odd-nodes neighbour-nodes))
        searching-points (map #(tree/index-to-center aabb tree/tree-arity %) searching-odd-nodes)
        searching-distances (map #(tree/point-point-distant
                                   (tree/index-to-aabb aabb tree/tree-arity current-node) %)
                                 searching-points)
        _ (debugger (count searching-distances) "searching distances:")
        min-node (if (not (empty? searching-distances))
                   (nth searching-odd-nodes (min-index searching-distances))
                   nil)]
    (if (number? min-node)
      (recur (disj odd-nodes current-node)
             nodes
             min-node
             t aabb
             (update-in pre-set [:pos (keyword (str current-node))] conj min-node))
      pre-set)))

;(disj #{1 2 3} 3 2 1)
;(min-key (range 4) [4 4 4 1])
;(min-index [1 2 3 0 4])

(defn convert-to-eulerian
  "given a flooded leaf nodes,
  returns a neighbour-set that will convert the graph that has eularian path"
  [nodes t aabb]
  (let [init-set (zipmap
                      (map (fn [n] (keyword (str n))) nodes)
                      (repeat #{}))
        pre-set {:neg init-set :pos init-set}
        odd-nodes (set (filter (fn [n] (bodd? (count (neighbours n nodes t aabb)))) nodes))
        neg-set (remove-odd-deg-nodes odd-nodes nodes t aabb pre-set #{})
        odd-nodes2 (set (filter (fn [n] (bodd? (count (neighbours n nodes t aabb neg-set)))) nodes))
        pos-set (connect-odd-deg-nodes odd-nodes2 nodes (first odd-nodes2) t aabb neg-set)
        ]
    pos-set
    )
  )


;(zipmap [:1 :2 :3] (repeat #{}))
;(assoc {:neg {} :pos {}} :neg {:1 [2 3]})
;(assoc {:neg {:1 [4]} :pos {}} :neg {:1 [2 3]})
