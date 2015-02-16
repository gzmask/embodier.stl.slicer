(ns slicer.util
  (:require [clojure.core.match :refer [match]]))

(defmacro debugger [expr & [msg]]
  `(do
     (when ~msg (println ~msg))
     (println ~expr)
     ~expr))
