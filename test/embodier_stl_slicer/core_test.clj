(ns embodier-stl-slicer.core-test
  (:require [clojure.test :refer :all]
            [slicer.slice :refer :all]
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
    (is (= [[0.0 1.0 0.0] [0.0 1.0 0.0]] (slicing-plane 1.0)))
    (is (= [[0.0 1.0 0.0] [0.0 -1.0 0.0]] (slicing-plane -1.0)))
    ))
