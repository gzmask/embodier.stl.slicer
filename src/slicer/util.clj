(ns slicer.util
  (:require [clojure.core.match :refer [match]]))

(defmacro debugger [expr & [msg]]
  `(do
     (when ~msg (println ~msg))
     (println ~expr)
     ~expr))

(defn s=
  [a b d]
  "let a and b be vectors of floating numbers. similarly equal: the difference of each element in a and b are less than d"
  (cond (or (and (vector? a) (vector? b))
            (and (seq? a) (seq? b)))
        (reduce #(and %1 %2) (map #(< (Math/abs (- %1 %2)) d) a b))
        (and (number? a) (number? b))
        (< (Math/abs (- a b)) d)))
