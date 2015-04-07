(ns slicer.gcode
  (:require [slicer.slice :as slice]
            [slicer.util :refer :all]
            [slicer.file :as file]
            [slicer.tree :as tree]
            [slicer.flood :as flood]
            [slicer.draw :as draw]
            [slicer.eulerian :as eulerian])
  (:use clojure.java.io))

(def header-str
  (str "; generated by embodier 0.0.1" \newline))

(defn point-str [p e]
  (str "G1 X" (first p) " Y" (second p) " E" e \newline))

(defn sum-lst
  "[1 2 3] => [1 3 6]"
  [a]
  (vec (reverse (take (count a) (map #(reduce + %) (iterate drop-last a))))))
;(reduce + [1 2 3 4])
;(def a [10 1 1 1 1 5])
;(sum-lst a)

(defn slice-str
  [cuts last-cmd last-e-height]
  (if (empty? cuts)
    last-cmd
    (let [cut (first cuts)
          init-cmd (str "G1 Z" (:cut-point cut) \newline)
          slice (:result cut)
          _ (debugger slice "slice:")
          tree (tree/generate-tree slice 1 2)
          aabb (-> slice (tree/aabb-slice 2) tree/make-square)
          flooded-leafs (flood/fast-flood tree aabb slice)
          fixing-set (eulerian/convert-to-eulerian flooded-leafs tree aabb)
          edges (eulerian/all-edges flooded-leafs tree aabb fixing-set)
          edge-path (eulerian/hierholzer edges flooded-leafs [])
          points (eulerian/edge-to-points edge-path aabb)
          point-distants (into [last-e-height]
                               (map tree/point-point-distant (drop-last points) (rest points)))
          extrusions (sum-lst point-distants)
          current-e-height (last extrusions)]
      #(slice-str (rest cuts)
                  (str last-cmd
                       init-cmd
                       (apply str (map point-str points extrusions)))
                  current-e-height))))

;this funciton is just a placepo for watertight, outline, infill, traversal and extruding accumulation
(defn gcode
  [cuts]
  (trampoline slice-str cuts header-str 0))

(defn write-gcode
  [gcode-file gcode-str]
  (with-open [g (writer (file gcode-file))]
    (.write g gcode-str)))
