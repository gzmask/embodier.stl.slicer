(ns embodier-stl-slicer.core-test
  (:require [clojure.repl :refer :all]
            [clojure.test :refer :all]
            [slicer.slice :refer :all]
            [slicer.file :refer :all]
            [slicer.flood :refer :all]
            [slicer.gcode :refer :all]
            [slicer.core :refer :all]))

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

(def asc (parse-stl "asc.stl"))
(def bin (parse-stl "bin.stl"))
(defn s=
  [a b d]
  "let a and b be vectors of floating numbers. similarly equal: the difference of each element in a and b are less than d"
  (reduce #(and %1 %2) (map #(< (Math/abs (- %1 %2)) d) a b)))

(deftest test-parse-stl
  (testing "parsing stl files"
    (is (= [10.0 10.0 0.0] (:vertex-1 (first (:triangles asc)))))
    (is (= [-10.0 -10.0 0.0] (:vertex-2 (first (:triangles asc)))))
    (is (= [-10.0 10.0 0.0] (:vertex-3 (first (:triangles asc)))))
    (is (= [0.0 0.0 -1.0] (:normal (first (:triangles asc)))))
    (is (s= [2.029 1.628 0.9109999] (:vertex-1 (first (:triangles bin))) 0.0001))
    (is (s= [2.229 1.628 0.9109999] (:vertex-2 (first (:triangles bin))) 0.0001))
    (is (s= [2.229 1.672 0.9109999] (:vertex-3 (first (:triangles bin))) 0.0001))
    (is (= [0.0 0.0 1.0] (:normal (first (:triangles bin)))))
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

(deftest test-tree-init
  (testing "testing initializing the tree based on the size of the aabb from a slice"
    (is (= (tree-height 2 4) 2))
    (is (= (tree-height 1000 4) 6))
    (is (= (tree-nodes-count 2 4) 5.0))
    (clojure.pprint/pprint (generate-BFS (:result (second slicings)) 0.3))
    )
  )

;(clojure.pprint/pprint (generate-BFS (:result (second slicings)) 0.3))

;(run-all-tests)
