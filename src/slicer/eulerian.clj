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
  "give the a list of nodes and a leaf node and a neighbour set (neg and pos), returns its neighbours within the list of nodes"
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
                  (not (contains? non-neighbours n)))))
           (into ((keyword (str node)) (:pos neighbour-set))) ;add ones in pos neighbour-set
           vec
           ))))

;(contains? (set [1 2 3]) 4)
;(contains? (set [1 2 3]) 3)
;((keyword (str 3)) {:3 #{1 2 3}})

(defn bodd?
  "bigger odd"
  [a]
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
    ;(debugger (count odd-nodes) "counting odd-nodes")
    ;(debugger (count searched-nodes) "counting searched-odd-nodes")
    (match [(nil? node-odd-deg) (nil? neighbour-node-odd-deg)]
           [false false]; two adjacent odd-nodes with odd degrees are found
           (recur odd-nodes
                  nodes t aabb
                  (-> pre-set
                    (update-in [:neg (keyword (str node-odd-deg))] conj neighbour-node-odd-deg)
                    (update-in [:neg (keyword (str neighbour-node-odd-deg))] conj node-odd-deg))
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
  "connect the pair of nodes of odd degrees in a heruistic fashion:
  links each node to its closest non-neighbour"
  [odd-nodes nodes current-node t aabb pre-set]
  (let [neighbour-nodes (neighbours current-node nodes t aabb pre-set)
        searching-odd-nodes (vec (disj (s/difference odd-nodes neighbour-nodes) current-node))
        searching-points (map #(tree/index-to-center aabb tree/tree-arity %) searching-odd-nodes)
        searching-distances (map #(tree/point-point-distant
                                   (tree/index-to-aabb aabb tree/tree-arity current-node) %)
                                 searching-points)
        min-node (if (not (empty? searching-distances))
                   (nth searching-odd-nodes (min-index searching-distances))
                   nil)
        next-node (if (>= (count searching-odd-nodes) 2)
                    (first (disj (set searching-odd-nodes) min-node))
                    nil)]
    (cond
      (number? next-node)
      (recur (disj odd-nodes current-node min-node)
             nodes
             next-node
             t aabb
             (-> pre-set
               (update-in [:pos (keyword (str current-node))] conj min-node)
               (update-in [:pos (keyword (str min-node))] conj current-node)))
      (number? min-node)
      (-> pre-set
               (update-in [:pos (keyword (str current-node))] conj min-node)
               (update-in [:pos (keyword (str min-node))] conj current-node))
      :else
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
        odd-nodes2 (set (filter (fn [n] (odd? (count (neighbours n nodes t aabb neg-set)))) nodes))
        final-set (connect-odd-deg-nodes odd-nodes2 nodes (first odd-nodes2) t aabb neg-set)
        ;odd-nodes3 (filter (fn [n] (odd? (count (neighbours n nodes t aabb final-set)))) nodes)
        ]
    final-set
    ;(vec odd-nodes3)
    )
  )

(defn all-edges
  "returns all edges of the flooded node"
  [nodes t aabb & [fix-set]]
  (let [result (atom #{})]
    (doseq [node nodes]
      (doseq [neighbour (if (nil? fix-set)
                          (neighbours node nodes t aabb)
                          (neighbours node nodes t aabb fix-set))]
        (swap! result conj #{node neighbour})))
    @result))


;(= #{1 2} #{2 1})
;(conj #{} #{1 2} #{2 1})


;(zipmap [:1 :2 :3] (repeat #{}))
;(assoc {:neg {} :pos {}} :neg {:1 [2 3]})
;(assoc {:neg {:1 [4]} :pos {}} :neg {:1 [2 3]})

(defn random-loop-walk
  [start-node unwalked-edges & [init-node walked-edges]]
  (let [the-walked-edges (if (nil? walked-edges) [] walked-edges)
        the-init-node (if (nil? init-node) start-node init-node)
        step-edge (first (s/select #(contains? % start-node) unwalked-edges))
        ]
    (cond
      ;walked to an end without anymore step-edge, gives an error
      (nil? step-edge) (throw (Exception. "loop walked failed, graph is not eulerian."))
      ;walked to original position with a loop, walked is finished.
      (= (first (disj step-edge start-node)) the-init-node) (conj the-walked-edges step-edge)
      :else
      (recur (first (disj step-edge start-node))
             (disj unwalked-edges step-edge)
             [the-init-node (conj the-walked-edges step-edge)]))))

;(first (disj #{1 2} 1))

;(into
;  (random-loop-walk 1 #{#{1 2} #{6 7} #{5 7} #{7 1} #{2 3} #{4 3} #{4 5} #{6 5}})
;  (random-loop-walk 1 #{#{1 2} #{6 7} #{5 7} #{7 1} #{2 3} #{4 3} #{4 5} #{6 5}}))

;(disj #{#{1 2} #{2 3}} #{2 3})

(defn get-start-node
  [walked-edges unwalked-edges]
  (if (empty? walked-edges)
    (first (first unwalked-edges))
    (first (first
      (for [x walked-edges
            y unwalked-edges
            :when (not (empty? (s/intersection x y)))]
        (s/intersection x y))))))

;(get-start-node #{#{1 2} #{2 3}} #{#{3 4} #{4 1} #{4 5}})
;(get-start-node #{#{6 2} #{2 3}} #{#{3 4} #{4 1} #{4 5}})
;(get-start-node #{#{6 2} #{2 3}} #{#{7 4} #{4 1} #{4 5}})
;(get-start-node #{} #{#{7 4} #{4 1} #{4 5}})

;(s/select #(contains? % 3) #{#{1 2} #{3 4}} )

;(for [x #{#{1 2} #{2 3}}
;      y #{#{3 4} #{4 1} #{4 5}}
;      :when (not (empty? (s/intersection x y)))]
;  (s/intersection x y))

(defn hierholzer
  "recursively randomly walk the flooded nodes until all edges are walked,
  returns the walking path"
  [all-edges nodes walked-edges]
  (if (= (count walked-edges) (count all-edges));if all edges are walked
    walked-edges
    (let [
          unwalked-edges (s/difference all-edges (set walked-edges))
          start-node (get-start-node walked-edges unwalked-edges)
          ]
      (recur
        all-edges
        nodes
        (into walked-edges (random-loop-walk start-node unwalked-edges))
        ))))

(defn edge-to-node-path [edge-path]
  (let [edge-path-s (conj (vec (rest edge-path)) (first edge-path)) ;shift the path
        intersections (map (fn [a b] (first (s/intersection a b))) edge-path edge-path-s)
        result (filter (complement nil?) intersections)
        first-node (first (s/difference (first edge-path) (sorted-set (first result))))
        last-node (first (s/difference (last edge-path) (sorted-set (last result))))
        ]
    (cond
      (= 1 (count edge-path)) (vec (first edge-path)) ;single edge
      (= (last result) first-node) (into [first-node] result) ;if a loop
      (not= (last result) first-node) (into (into [first-node] result) [last-node]) ;if a path
      :else (throw (Exception. "node path failed"))
      )
    ))

;(conj [1 2] 3)
;(map #(identity [%1 %2]) [1 2 3] [2 3])
;(edge-to-node-path [#{1 2} #{3 2} #{3 4} #{4 5} #{5 1} ])
;(edge-to-node-path [#{1 2} #{3 2} #{3 4}])
;(edge-to-node-path [#{1 2} #{3 2}])
;(edge-to-node-path [#{1 2} #{1 2}])
;(edge-to-node-path [#{1 2}])
;(s/intersection #{1 2} #{2 3})
;(conj (vec (rest [1 2])) 1)

