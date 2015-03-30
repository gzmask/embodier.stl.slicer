; draw slices and boxes onto screen
(ns slicer.draw
  (:require [quil.core :as q]
            [clojure.core.match :refer [match]]
            [slicer.tree :as tree]))

;(q/show-cats)
;(q/show-fns "sketch")
(def screen-width 1800)
(def screen-height 1600)

(defn aabb-points
  "given AABB, returns the four points"
  [[min-x min-y max-x max-y :as aabb]]
  [[min-x min-y]
   [min-x max-y]
   [max-x max-y]
   [max-x min-y]])

(defn line-center
  [[x1 y1] [x2 y2]]
  [(/ (+ x1 x2) 2) (/ (+ y1 y2) 2)]
  )

;(line-center [0 0] [1 1])
;(line-center [-3 -3] [-1 -1])

(defn draw-node
  "draws the node"
  [node collided aabb wpx hpx s]
  (let [node-aabb (tree/index-to-aabb aabb tree/tree-arity node)
        [[x1 y1] [x2 y2] [x3 y3] [x4 y4]] (aabb-points node-aabb)
        [text-x text-y] (tree/index-to-center aabb tree/tree-arity node)]
    (q/text-size 12)
    (q/text (str node) (* text-x wpx) (* text-y hpx))
    (->> [x1 y1 x2 y2] (map * [wpx hpx wpx hpx]) (apply q/line))
    (->> [x2 y2 x3 y3] (map * [wpx hpx wpx hpx]) (apply q/line))
    (->> [x3 y3 x4 y4] (map * [wpx hpx wpx hpx]) (apply q/line))
    (->> [x4 y4 x1 y1] (map * [wpx hpx wpx hpx]) (apply q/line))
    (when collided
      (->> [x3 y3 x1 y1] (map * [wpx hpx wpx hpx]) (apply q/line))
      (->> [x4 y4 x2 y2] (map * [wpx hpx wpx hpx]) (apply q/line)))
    ))

(defn draw-geo
  "draw a geo from a slice, can be a triangle, a line or a point"
  [geo wpx hpx s]
  (match [geo]
         [[[x1 y1 & z1][x2 y2 & z2][x3 y3 & z3]]] ;triangle
         (do (q/line (* x1 wpx) (* y1 hpx) (* x2 wpx) (* y2 hpx))
             (q/line (* x2 wpx) (* y2 hpx) (* x3 wpx) (* y3 hpx))
             (q/line (* x3 wpx) (* y3 hpx) (* x1 wpx) (* y1 hpx)))
         [[[x1 y1 & z1][x2 y2 & z2]]] ;line
         (do
           (q/text-size 12)
           (let [[x3 y3] (line-center [x1 y1] [x2 y2])]
             (q/text s (* x3 wpx) (* y3 hpx)))
           (q/line (* x1 wpx) (* y1 hpx) (* x2 wpx) (* y2 hpx)))
         [[x1 y1 & z1]] ;point
         (do (q/line (- (* x1 wpx) (/ wpx 10)) (- (* y1 hpx) (/ hpx 10)) (+ (* x1 wpx) (/ wpx 10)) (+ (* y1 hpx) (/ hpx 10)))
             (q/line (- (* x1 wpx) (/ wpx 10)) (+ (* y1 hpx) (/ hpx 10)) (+ (* x1 wpx) (/ wpx 10)) (- (* y1 hpx) (/ hpx 10))))))


(defn gui-main
  "the gui engine"
  [nodes-or-geos t [min-x min-y max-x max-y :as aabb] & [filename]]
  (let [count (atom 0)
        h (- max-y min-y)
        hpx (/ screen-height h)
        w (- max-x min-x)
        wpx (/ screen-width w)
        setup (fn []
                (q/smooth)                          ;; Turn on anti-aliasing
                (q/frame-rate 1)                    ;; Set framerate to 1 FPS
                (q/background 200))
        draw (fn []
               (q/translate (/ screen-width 2) (/ screen-height 2))
               (q/scale 1 -1) ;; matching the 3D printer coordinate
               ;(q/translate 178 470)
               (doseq [node-or-geo nodes-or-geos]
                 (match [(number? node-or-geo)]
                        [true] (draw-node node-or-geo (nth t node-or-geo) aabb wpx hpx (str @count))
                        [false] (draw-geo node-or-geo wpx hpx (str @count)))
                 (swap! count inc))
               (q/save (if (nil? filename)
                         "debug.png"
                         filename))
               (q/exit)
               )]
    (q/sketch  :title "Oh so many grey squares"    ;; Set the title of the sketch
               :setup setup                        ;; Specify the setup fn
               :draw draw                          ;; Specify the draw fn
               :size [screen-width screen-height])
    )
  )

;repls
;(def asc (slicer.file/parse-stl "asc.stl"))
;(def bin (slicer.file/parse-stl "bin.stl"))
;(def slicings
;  (-> (slicer.slice/slice (:triangles asc) (slicer.slice/gen-planes (:min (slicer.slice/find-min-max :z (:triangles asc))) (:max (slicer.slice/find-min-max :z (:triangles asc))) 0.3 :z) :z)
;      slicer.slice/rm-nil
;      slicer.slice/tri-compressor))
;(def tree (slicer.tree/generate-tree (:result (second slicings)) 0.3))
;(def aabb (-> (:result (second slicings)) (slicer.tree/aabb-slice 0.6) slicer.tree/make-square))
;(gui-main tree aabb [1 5 4 20])
;(gui-main [true false false false true nil nil nil nil nil nil nil nil nil nil nil nil false false false nil]
;          [-10 -10 10 10]
;          [0 4 20])
;
;(q/sketch  :title "Oh so many grey squares"
;           :setup (fn [] (q/smooth)
;                         (q/frame-rate 1)
;                         (q/background 200))
;           :draw (fn []
;                   (q/translate 320 240)
;                   (q/scale 1 -1)
;                   (q/line 0 0 20 20))
;           :size [640 480])
