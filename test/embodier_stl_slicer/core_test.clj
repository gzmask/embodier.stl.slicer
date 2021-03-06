(ns embodier-stl-slicer.core-test
  (:require [clojure.repl :refer :all]
            [clojure.test :refer :all]
            [slicer.slice :refer :all]
            [slicer.util :refer :all]
            [slicer.file :refer :all]
            [slicer.tree :refer :all]
            [slicer.flood :refer :all]
            [slicer.draw :refer :all]
            [slicer.gcode :refer :all]
            [slicer.core :refer :all]
            [slicer.eulerian :refer :all]
            ))

(deftest test-point-plane
  (testing "the point-plane distance."
    (is (= (point-plane [0.0 -1.0 0.0] [[0.0 1.0 0.0] [0.0 0.0 0.0]]) -1.0))
    (is (= (point-plane [0.0 1.0 0.0] [[0.0 1.0 0.0] [0.0 0.0 0.0]]) 1.0))
    (is (= (point-plane [0.0 0.0 0.0] [[0.0 1.0 0.0] [0.0 0.0 0.0]]) 0.0))))

(deftest test-plane-line-inc
  (testing "plane and line intersection."
    (is (= [0.0 1.1 0.0] (plane-line-inc [[5.0 1.1 0.0] [-2.0 1.1 0.0]] [[1.0 0.0 0.0] [0.0 0.0 0.0]])))
    (is (= nil (plane-line-inc [[5.0 1.1 0.0] [-2.0 1.1 0.0]] [[0.0 1.0 0.0] [0.0 0.0 0.0]])))
    (is (= nil (plane-line-inc [[0.0 1.0 0.0] [0.0 2.0 0.0]] [[0.0 1.0 0.0] [0.0 0.0 0.0]])))
    (is (= nil (plane-line-inc [[0.0 0.1 0.0] [0.0 2.0 0.0]] [[0.0 1.0 0.0] [0.0 0.0 0.0]])))
    (is (= [[5.0 1.1 0.0] [-2.0 1.1 0.0]] (plane-line-inc [[5.0 1.1 0.0] [-2.0 1.1 0.0]] [[0.0 1.0 0.0] [0.0 1.1 0.0]])))))

(deftest test-triangle-plane-inc
  (testing "triangle plane intersection."
    (is (= '([-0.5 0.0 0.0] [0.5 0.0 0.0]) (triangle-plane-inc [[0.0 1.0 0.0] [-1.0 -1.0 0.0] [1.0 -1.0 0.0]] [[0.0 1.0 0.0] [0.0 0.0 0.0]])))
    (is (= [0.0 0.0 0.0] (triangle-plane-inc [[0.0 0.0 0.0] [-1.0 -1.0 0.0] [1.0 -1.0 0.0]] [[0.0 1.0 0.0] [0.0 0.0 0.0]])))
    (is (= nil (triangle-plane-inc [[0.0 -1.0 0.0] [-1.0 -2.0 0.0] [1.0 -2.0 0.0]] [[0.0 1.0 0.0] [0.0 0.0 0.0]])))))

(deftest test-slicing-plane
  (testing "slicing plane from y"
    (is (= [[0.0 1.0 0.0] [0.0 1.0 0.0]] (slicing-plane 1.0 :y)))
    (is (= [[1.0 0.0 0.0] [-1.0 0.0 0.0]] (slicing-plane -1.0 :x)))
    (is (= [[0.0 0.0 1.0] [0.0 0.0 0.0]] (slicing-plane 0.0 :z)))
    ))

(def asc (parse-stl "resources/stl/asc.stl"))
(def bin (parse-stl "resources/stl/bin.stl"))


(deftest test-parse-stl
  (testing "parsing stl files"
    (is (= [10.0 10.0 0.0] (:vertex-1 (first (:triangles asc)))))
    (is (= [-10.0 -10.0 0.0] (:vertex-2 (first (:triangles asc)))))
    (is (= [-10.0 10.0 0.0] (:vertex-3 (first (:triangles asc)))))
    (is (= [0.0 0.0 -1.0] (:normal (first (:triangles asc)))))
    ;(is (s= [2.029 1.628 0.9109999] (:vertex-1 (first (:triangles bin))) 0.0001))
    ;(is (s= [2.229 1.628 0.9109999] (:vertex-2 (first (:triangles bin))) 0.0001))
    ;(is (s= [2.229 1.672 0.9109999] (:vertex-3 (first (:triangles bin))) 0.0001))
    ;(is (= [0.0 0.0 1.0] (:normal (first (:triangles bin)))))
    ))

(deftest test-planes-generator
  (testing "generates planes from an interval"
    (is (= [[[0.0 1.0 0.0] [0.0 0.0 0.0]]
            [[0.0 1.0 0.0] [0.0 0.3 0.0]]
            [[0.0 1.0 0.0] [0.0 0.6 0.0]]
            [[0.0 1.0 0.0] [0.0 0.9 0.0]]
            [[0.0 1.0 0.0] [0.0 1.2 0.0]]
            [[0.0 1.0 0.0] [0.0 1.5 0.0]]
            [[0.0 1.0 0.0] [0.0 1.8 0.0]]
            [[0.0 1.0 0.0] [0.0 2.1 0.0]]
            [[0.0 1.0 0.0] [0.0 2.4 0.0]]
            [[0.0 1.0 0.0] [0.0 2.7 0.0]]
            [[0.0 1.0 0.0] [0.0 3.0 0.0]]] (gen-planes 0.0 3.0 0.3 :y)))
    (is (= [[[1.0 0.0 0.0] [0.0 0.0 0.0]]
            [[1.0 0.0 0.0] [0.3 0.0 0.0]]
            [[1.0 0.0 0.0] [0.6 0.0 0.0]]
            [[1.0 0.0 0.0] [0.9 0.0 0.0]]
            [[1.0 0.0 0.0] [1.2 0.0 0.0]]
            [[1.0 0.0 0.0] [1.5 0.0 0.0]]
            [[1.0 0.0 0.0] [1.8 0.0 0.0]]
            [[1.0 0.0 0.0] [2.1 0.0 0.0]]
            [[1.0 0.0 0.0] [2.4 0.0 0.0]]
            [[1.0 0.0 0.0] [2.7 0.0 0.0]]
            [[1.0 0.0 0.0] [3.0 0.0 0.0]]] (gen-planes 0.0 3.0 0.3 :x)))
    ))

(deftest test-min-max-finder
  (testing "finds the highest and lowest point in axis of a collection of triangles along provided axis"
    (is (= {:min -10.0 :max 10.0}
           (find-min-max :x
             [{:vertex-3 [-10.0 10.0 0.0],
               :vertex-2 [-10.0 -10.0 0.0],
               :vertex-1 [10.0 10.0 0.0],
               :normal [0.0 0.0 -1.0],
               :_ ""}
              {:vertex-3 [10.0 -10.0 0.0],
               :vertex-2 [10.0 10.0 0.0],
               :vertex-1 [-10.0 -10.0 0.0],
               :normal [0.0 0.0 -1.0],
               :_ ""}])))
    (is (= {:min -100.0 :max 100.0}
           (find-min-max :z
             [{:vertex-3 [-10.0 10.0 0.0],
               :vertex-2 [-10.0 -10.0 0.0],
               :vertex-1 [10.0 10.0 0.0],
               :normal [0.0 0.0 -1.0],
               :_ ""}
              {:vertex-3 [-10.0 10.0 -100.0],
               :vertex-2 [-10.0 -10.0 0.0],
               :vertex-1 [10.0 10.0 0.0],
               :normal [0.0 0.0 -1.0],
               :_ ""}
              {:vertex-3 [10.0 -10.0 0.0],
               :vertex-2 [10.0 10.0 0.0],
               :vertex-1 [-10.0 -10.0 100.0],
               :normal [0.0 0.0 -1.0],
               :_ ""}])))
    (is (= {:min -10.0 :max 10.0}
           (find-min-max :y
             [{:vertex-3 [-10.0 10.0 0.0],
               :vertex-2 [-10.0 -10.0 0.0],
               :vertex-1 [10.0 10.0 0.0],
               :normal [0.0 0.0 -1.0],
               :_ ""}
              {:vertex-3 [10.0 -10.0 0.0],
               :vertex-2 [10.0 10.0 0.0],
               :vertex-1 [-10.0 -10.0 0.0],
               :normal [0.0 0.0 -1.0],
               :_ ""}])))
    (is (= {:min -10.0, :max 10.0}
           (find-min-max :y (:triangles asc))))
    (is (= {:min -10.0, :max 10.0}
           (find-min-max :x (:triangles asc))))
    (is (= {:min 0.0, :max 10.0}
           (find-min-max :z (:triangles asc))))
    ))

;(clojure.pprint/pprint (slice (:triangles asc) (gen-planes 0.0 3.0 0.3 :y) :y))
;(print "triangles")
;(clojure.pprint/pprint  (:triangles asc))
;(print "min-max asc triangles")
;(clojure.pprint/pprint  (find-min-max :y (:triangles asc)))
;(print "planes")
;(clojure.pprint/pprint  (count (gen-planes 0.0 3.0 0.3 :y)))
;(print "lines")
;(clojure.pprint/pprint  (count (slice (:triangles asc) (gen-planes 0.0 3.0 0.3 :y) :y)))

(def slicings
  (-> (slice (:triangles asc) (gen-planes (:min (find-min-max :z (:triangles asc))) (:max (find-min-max :z (:triangles asc))) 0.3 :z) :z)
      rm-nil
      tri-compressor))

;slicings
;(clojure.pprint/pprint slicings)

(deftest test-slice-function
  (testing "slices triangles with planes according to x/y/z axis"
    (is (= (*
             (count (:triangles asc))
             (count (gen-planes (:min (find-min-max :z (:triangles asc))) (:max (find-min-max :z (:triangles asc))) 0.3 :z)))
           (count (slice (:triangles asc) (gen-planes (:min (find-min-max :z (:triangles asc))) (:max (find-min-max :z (:triangles asc))) 0.3 :z) :z))))
    ))

(deftest test-line-box-intersection
  (testing "segment of line and AABB box intersection"
    (is (line-box-inc [0.5 1] [0.5 -1] [0 0] [1 1]))
    (is (line-box-inc [0 2] [0 -1] [0 0] [1 1]))
    (is (line-box-inc [1 2] [1 -1] [0 0] [1 1]))
    (is (line-box-inc [0.0001 2] [0.0001 -1] [0 0] [1 1]))
    (is (line-box-inc [-1 1] [1 -1] [0 0] [1 1]))
    (is (line-box-inc [-1 1.1] [1 -1] [0 0] [1 1]))
    (is (not (line-box-inc [-1 0.9] [1 -1.1] [0 0] [1 1])))
    ))

(deftest test-tri-box-intersection
  (testing "triangle and AABB box intersection"
    (is (tri-box-inc [0 0] [10 0] [5 5] [4 1] [6 3]))
    (is (not (tri-box-inc [0 0] [4 3] [4 4] [3 0] [5 2])))
    (is (not (tri-box-inc [0 0] [8 6] [7 7] [3 0] [5 2])))
    (is (tri-box-inc [0 0] [8 5] [7 7] [3 0] [5 2]))
    (is (tri-box-inc [1 1] [3 1] [2 2] [0 0] [4 3]))
    (is (tri-box-inc [10 10] [-10 -10] [-10 10] [-9 8] [-8 9]))
    ))

(deftest test-point-box-intersection
  (testing "point box intersect"
    (is (point-box-inc [1 1] [0 0] [2 2]))
    (is (point-box-inc [2 2] [0 0] [2 2]))
    (is (not (point-box-inc [-1 1] [0 0] [2 2])))
    ))

(deftest test-aabb-slice
  (testing "the aabb of a slice"
    (is (= (aabb-slice (:result (first slicings))) [-10.0 -10.0 10.0 10.0]))
    (is (= (aabb-slice (:result (second slicings))) [-10.0 -10.0 10.0 10.0]))
    ))

(deftest test-slice-box-intersection
  (testing "slice and box intersection test"
    (is (slice-box-inc [-10.0 -10.0 10.0 10.0] (:result (first slicings))))
    (is (slice-box-inc [-9.0 -9.0 -8.0 -8.0] (:result (first slicings))))
    (is (slice-box-inc [-9.0 8.0 -8.0 9.0] (:result (first slicings))))
    (is (not (slice-box-inc [-9.0 8.0 -8.0 9.0] (:result (second slicings)))))
    )
  )

(deftest test-aabb-handling
  (testing "aabb spliting and made square"
    (is (= (make-square [-10 -10 10 15]) [-10 -10 15 15]))
    (is (= (make-square [-5 -5 10 5]) [-5 -5 10 10]))
    (is (= (split-aabb [0 0 10 10]) [[0 5 5 10] [5 5 10 10] [0 0 5 5] [5 0 10 5]]))
    (is (= (split-aabb [0 0 10 10] :upper-left) [0 5 5 10]))
    ;(clojure.pprint/pprint (make-tree (:result (second slicings)) 0.3))
    ))

(def tree (generate-tree (:result (second slicings)) 1 1.6))
(def aabb (-> (:result (second slicings)) (aabb-slice 1.6) make-square))


(deftest test-tree-init
  (testing "testing initializing the tree based on the size of the aabb from a slice"
    (is (= (height 2 4) 2))
    (is (= (height 1000 4) 6))
    (is (= (tree-nodes-count 2 4) 5.0))
    ;(clojure.pprint/pprint tree)
    (is (adjacent 1 2 aabb))
    (is (adjacent 5 6 aabb))
    (is (adjacent 1 17 aabb))
    (is (adjacent 8 17 aabb))
    (is (not (adjacent 5 9 aabb)))
    (is (not (adjacent 5 20 aabb)))
    (is (node-inc 1 2 aabb))
    (is (node-inc 5 6 aabb))
    (is (node-inc 1 5 aabb))
    (is (node-inc 6 9 aabb))
    (is (node-inc 8 9 aabb))
    (is (node-inc 1 9 aabb))
    (is (node-inc 1 17 aabb))
    (is (node-inc 8 17 aabb))
    (is (not (node-inc 1 10 aabb)))
    (is (not (node-inc 1 18 aabb)))
    (is (not (node-inc 3 18 aabb)))
    (is (not (node-inc 4 7 aabb)))
    (is (not (node-inc 5 20 aabb)))
    (is (not (node-inc 3 20 aabb)))
    (is (not (node-inc 2 19 aabb)))
    )
  )

;(clojure.pprint/pprint (generate-BFS (:result (second slicings)) 0.3))

;(deftest test-flooding
;  (testing "testing flooding on the slice"
;    (debugger (flood tree aabb))
;    (debugger aabb "root aabb:")
;    (debugger (leafs tree) "leafs: ")
;    )
;  )
;
;(let [f
;      ;(parse-stl "resources/stl/asc.stl")
;      ;(parse-stl "resources/stl/20130520_PLA_LM8UU_8p2pi.stl")
;      (parse-stl "resources/stl/hotend_v2.stl")
;      ts (:triangles f)
;      planes (gen-planes (:min (find-min-max :z ts)) (:max (find-min-max :z ts)) 0.3 :z)
;      slices (-> (slice ts planes :z) rm-nil tri-compressor)
;      slice (:result (nth slices 1))
;      ;_ (debugger slice "slice:")
;      tree (generate-tree slice 1 2)
;      aabb (-> slice (aabb-slice 2) make-square) ;center-aabb)
;      ;_ (debugger aabb "aabb:")
;      ]
;  (-> (->> (leafs tree)
;       (map (fn [n] (first n))))
;       (gui-main tree aabb "resources/pic/d1.png")))

(let [f
      ;(parse-stl "resources/stl/asc.stl")
      (parse-stl "resources/stl/hotend_v2.stl")
      ts (:triangles f)
      planes (gen-planes (:min (find-min-max :z ts)) (:max (find-min-max :z ts)) 0.3 :z)
      slices (-> (slice ts planes :z) rm-nil tri-compressor)
      slice (:result (nth slices 1))
      ;_ (debugger slice "slice:")
      tree (generate-tree slice 1 2)
      aabb (-> slice (aabb-slice 2) make-square)
      _ (debugger aabb "aabb:")
      ]
  (-> (fast-flood tree aabb slice) (gui-main tree aabb "resources/pic/d1.png"))
  )

;(let [f
;      (parse-stl "resources/stl/asc.stl")
;      ;(parse-stl "resources/stl/hotend_v2.stl")
;      nozzle-diameter 1
;      ts (:triangles f)
;      planes (gen-planes (:min (find-min-max :z ts)) (:max (find-min-max :z ts)) 0.3 :z)
;      slices (-> (slice ts planes :z) rm-nil tri-compressor)
;      slice (:result (nth slices 1))
;      ;_ (debugger slice "slice:")
;      tree (generate-tree slice nozzle-diameter 2)
;      [min-x min-y max-x max-y :as aabb] (-> slice (aabb-slice 2) make-square)
;      x-points (range (+ min-x (* nozzle-diameter 1.5)) (- max-x (* nozzle-diameter 1.5)) nozzle-diameter)
;      x-start-points (map vector x-points (repeat max-y))
;      x-end-points (map vector x-points (repeat min-y))
;      x-lines (map vector x-start-points x-end-points)
;      y-points (range (+ min-y (* nozzle-diameter 1.5)) (- max-y (* nozzle-diameter 1.5)) nozzle-diameter)
;      ;y-start-points (map vector (repeat max-x) y-points)
;      ;y-end-points (map vector (repeat min-x) y-points)
;      ;y-lines (map vector y-start-points y-end-points)
;      ;lines (into x-lines y-lines)
;      intersections (filter (complement nil?) (reduce into [] (map #(line-slice-inc % slice) x-lines)))
;      lines-incs (into x-lines intersections)
;      ;_ (debugger aabb "aabb:")
;      ;_ (debugger lines "lines:")
;      ;_ (debugger intersections "intersections:")
;      ]
;  (gui-main lines-incs tree aabb "resources/pic/d3.png")
;  )

;all edges
(let [f
      ;(parse-stl "resources/stl/asc.stl")
      (parse-stl "resources/stl/hotend_v2.stl")
      ts (:triangles f)
      planes (gen-planes (:min (find-min-max :z ts)) (:max (find-min-max :z ts)) 0.3 :z)
      slices (-> (slice ts planes :z) rm-nil tri-compressor)
      slice (:result (nth slices 1))
      tree (generate-tree slice 1 2)
      aabb (-> slice (aabb-slice 2) make-square)
      _ (debugger aabb "aabb:")
      flooded-leafs (fast-flood tree aabb slice)
      fixing-set (convert-to-eulerian flooded-leafs tree aabb)
      edges (all-edges flooded-leafs tree aabb fixing-set)
      drawable-edges (for [edge edges] [(index-to-center aabb tree-arity (first edge))
                                        (index-to-center aabb tree-arity (second edge))])
      ]
  (gui-main drawable-edges tree aabb "resources/pic/d1.png")
  ;(gui-main fixing-set tree aabb "resources/pic/d3.png")
  ;fixing-set
  ;(:pos fixing-set)
  )
;(gui-main tree aabb [8])
;(index-to-aabb aabb tree-arity 8)

;(reduce into [1 2] [[3 4] [5 6]])

;(for [a (keys {:1 #{} :2 #{}})]
;  (Integer. (name a)))

;;eulerian path fixing set
(let [f
      ;(parse-stl "resources/stl/asc.stl")
      (parse-stl "resources/stl/hotend_v2.stl")
      ts (:triangles f)
      planes (gen-planes (:min (find-min-max :z ts)) (:max (find-min-max :z ts)) 0.3 :z)
      slices (-> (slice ts planes :z) rm-nil tri-compressor)
      slice (:result (nth slices 1))
      tree (generate-tree slice 1 2)
      aabb (-> slice (aabb-slice 2) make-square)
      _ (debugger aabb "aabb:")
      flooded-leafs (fast-flood tree aabb slice)
      fixing-set (convert-to-eulerian flooded-leafs tree aabb)
      neg-set (reduce into (for [node-from (keys (:neg fixing-set))]
                               (for [node-to (node-from (:neg fixing-set))]
                                 [(index-to-center aabb tree-arity (Integer. (name node-from)))
                                  (index-to-center aabb tree-arity node-to)])))
      pos-set (reduce into (for [node-from (keys (:pos fixing-set))]
                (for [node-to (node-from (:pos fixing-set))]
                  [(index-to-center aabb tree-arity (Integer. (name node-from)))
                   (index-to-center aabb tree-arity node-to)])))
      final-set (into neg-set pos-set)
      ]
  (gui-main final-set tree aabb "resources/pic/pd1.png")
  )

;;hierholzer algorithmn test
(let [f
      (parse-stl "resources/stl/asc.stl")
      ;(parse-stl "resources/stl/hotend_v2.stl")
      ts (:triangles f)
      planes (gen-planes (:min (find-min-max :z ts)) (:max (find-min-max :z ts)) 0.3 :z)
      slices (-> (slice ts planes :z) rm-nil tri-compressor)
      slice (:result (nth slices 0))
      _ (debugger slice "slice:")
      tree (generate-tree slice 1 2)
      aabb (-> slice (aabb-slice 2) make-square)
      _ (debugger aabb "aabb:")
      flooded-leafs (fast-flood tree aabb slice)
      fixing-set (convert-to-eulerian flooded-leafs tree aabb)
      edges (all-edges flooded-leafs tree aabb fixing-set)
      edge-path (hierholzer edges flooded-leafs [])
      drawable-edges (edge-to-lines edge-path aabb)
      drawable-points (edge-to-points edge-path aabb)
      node-path (edge-to-node-path edge-path)
      ]
  ;draw out edge-path
  ;edge-path
  ;(gui-main drawable-edges tree aabb "resources/pic/d1.png")
  (gui-main drawable-points tree aabb "resources/pic/d1.png")
  ;(gui-main flooded-leafs tree aabb "resources/pic/d1.png")
  ;(gui-main fixing-set tree aabb "resources/pic/d3.png")
  ;fixing-set
  ;(:pos fixing-set)
  )

;(run-all-tests)
