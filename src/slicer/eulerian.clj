; according to the fact that:
; sum of all the degrees of all the vertices of a graph is equal to twice the total number of its edges,
; a.k.a Handshake Lemma
; we know that we can convert every graph to one has eulerian circuit.
; generate eulerian circuit for flooded nodes
(ns slicer.eulerian
  (:require [clojure.core.match :refer [match]]
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

(defn remove-odd-deg-nodes
  "remove adjacent nodes edge that connects nodes of both odd degree."
  [nodes-with-odd-degrees t aabb pre-set]
  (let [result-set (atom pre-set)]
    (doseq [node-from nodes-with-odd-degrees]
      (doseq [node-to (neighbours node-from nodes-with-odd-degrees t aabb @result-set)] ;neighbours of odd degrees
          (swap! result-set update-in
                 [:neg (keyword (str node-from))]
                 conj node-to)
          (swap! result-set update-in
                 [:neg (keyword (str node-to))]
                 conj node-from)))
    @result-set))

;(assoc-in {:neg {:1 #{2}}}
;          [:neg :1]
;          (conj (:1 (:neg {:neg {:1 #{2}}})) 3))

(defn convert-to-eulerian
  "given a flooded leaf nodes,
  returns a neighbour-set that will convert the graph that has eularian path"
  [nodes t aabb]
  (let [nodes-with-odd-degrees (->> nodes
                                    (filter (fn [n] (odd? (count (neighbours n nodes t aabb))))))
        init-set (zipmap
                      (map (fn [n] (keyword (str n))) nodes-with-odd-degrees)
                      (repeat #{}))
        pre-set {:neg init-set :pos init-set}
        neg-set (remove-odd-deg-nodes nodes-with-odd-degrees t aabb pre-set)
        ;pos-set (connect-odd-deg-nodes nodes-with-odd-degrees nodes t aabb (assoc pre-set :neg neg-set))
        ]
    neg-set
    )
  )


;(zipmap [:1 :2 :3] (repeat #{}))
;(assoc {:neg {} :pos {}} :neg {:1 [2 3]})
;(assoc {:neg {:1 [4]} :pos {}} :neg {:1 [2 3]})

