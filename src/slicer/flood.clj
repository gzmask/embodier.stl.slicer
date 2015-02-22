;after sliced the model, we need to check for watertight and discretized the slice shape
(ns slicer.flood
  (:require [clojure.core.match :refer [match]]
            [slicer.tree])
  (:use slicer.util))


(defn flood
  "given a tree generated from the slice, flood the tree from outside and return the nodes that are flooded"
  [t]
  )
