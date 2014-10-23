(ns embodier-stl-slicer.core-test
  (:require [clojure.repl :refer :all]
            [clojure.test :refer :all]
            [slicer.slice :refer :all]
            [slicer.file :refer :all]
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

(def asc (parse-stl "asc.stl"))
(deftest test-parse-stl
  (testing "parsing stl files"
    (is (= [10.0 10.0 0.0] (:vertex-1 (first (:triangles asc)))))
    (is (= [-10.0 -10.0 0.0] (:vertex-2 (first (:triangles asc)))))
    (is (= [-10.0 10.0 0.0] (:vertex-3 (first (:triangles asc)))))
    (is (= [0.0 0.0 -1.0] (:normal (first (:triangles asc)))))
    ))

(deftest test-sort-triangles
  (testing ""
    (is (= true true))
    ))
