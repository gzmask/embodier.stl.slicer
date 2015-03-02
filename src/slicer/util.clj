(ns slicer.util
  (:require [clojure.core.match :refer [match]]))

(defmacro debugger [expr & [msg]]
  `(do
     (when ~msg (println ~msg))
     (println ~expr)
     ~expr))

(defmacro s=
  [a b d]
    `(< (Math/abs (- ~a ~b)) ~d))

;(s= 1.1 1.2 0.1)

(defmacro s>=
  [a b d]
  `(>= ~a (- ~b ~d)))

;(s>= 1 1.1 0.1)

(defmacro s<=
  [a b d]
  `(<= ~a (+ ~b ~d)))

;(s<= 1 0.9 0.1)
