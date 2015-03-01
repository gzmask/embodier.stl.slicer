(ns slicer.util
  (:require [clojure.core.match :refer [match]]))

(defmacro debugger [expr & [msg]]
  `(do
     (when ~msg (println ~msg))
     (println ~expr)
     ~expr))

(defmacro s=
  [a b d]
  "let a and b be vectors of floating numbers. similarly equal: the difference of each element in a and b are less than d"
    `(< (Math/abs (- ~a ~b)) ~d))

;(s= 1.1 1.2 0.1)

(defmacro s>=
  [a b d]
  `(>= ~a (- ~b ~d)))

(defmacro s<=
  [a b d]
  `(<= ~a (+ ~b ~d)))
