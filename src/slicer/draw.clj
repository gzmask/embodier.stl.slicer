; draw slices and boxes onto screen
(ns slicer.draw
  (:require [quil.core :as q]
            [slicer.tree :as tree]))

;(q/show-cats)
;(q/show-fns "sketch")
(def screen-width 800)
(def screen-height 600)

(defn aabb-points
  "given AABB, returns the four points"
  [[min-x min-y max-x max-y :as aabb]]
  [[min-x min-y]
   [min-x max-y]
   [max-x max-y]
   [max-x min-y]])

(defn draw-node
  "draws the node"
  [node collided aabb wpx hpx]
  (let [node-aabb (tree/index-to-aabb aabb tree/tree-arity node)
        [[x1 y1] [x2 y2] [x3 y3] [x4 y4]] (aabb-points node-aabb)]
    (->> [x1 y1 x2 y2] (map * [wpx hpx wpx hpx]) (apply q/line))
    (->> [x2 y2 x3 y3] (map * [wpx hpx wpx hpx]) (apply q/line))
    (->> [x3 y3 x4 y4] (map * [wpx hpx wpx hpx]) (apply q/line))
    (->> [x4 y4 x1 y1] (map * [wpx hpx wpx hpx]) (apply q/line))
    (when collided
      (->> [x3 y3 x1 y1] (map * [wpx hpx wpx hpx]) (apply q/line))
      (->> [x4 y4 x2 y2] (map * [wpx hpx wpx hpx]) (apply q/line)))
    ))


(defn gui-main
  "the gui engine"
  [nodes t [min-x min-y max-x max-y :as aabb] & [filename]]
  (let [h (- max-y min-y)
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
               (doseq [node nodes]
                 (draw-node node (nth t node) aabb wpx hpx))
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
