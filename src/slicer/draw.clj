; draw slices and boxes onto screen
(ns slicer.draw
  (:require [quil.core :as q]
            [slicer.tree :as tree]))

;(q/show-cats)
;(q/show-fns "sketch")

(defn aabb-points
  "given AABB, returns the four points"
  [[min-x min-y max-x max-y :as aabb]]
  [[min-x min-y]
   [min-x max-y]
   [max-x max-y]
   [max-x min-y]])

(defn draw-node
  "draws the node"
  [node aabb wpx hpx]
  (let [node-aabb (tree/index-to-aabb aabb tree/tree-arity node)
        [[x1 y1] [x2 y2] [x3 y3] [x4 y4]] (aabb-points node-aabb)]
    (->> [x1 y1 x2 y2] (map * [wpx hpx wpx hpx]) (apply q/line))
    (->> [x2 y2 x3 y3] (map * [wpx hpx wpx hpx]) (apply q/line))
    (->> [x3 y3 x4 y4] (map * [wpx hpx wpx hpx]) (apply q/line))
    (->> [x4 y4 x1 y1] (map * [wpx hpx wpx hpx]) (apply q/line))
    ))


(defn gui-main
  "the gui engine"
  [t [min-x min-y max-x max-y :as aabb] nodes]
  (let [h (- max-y min-y)
        hpx (/ 480 h)
        w (- max-x min-x)
        wpx (/ 640 w)
        setup (fn []
                (q/smooth)                          ;; Turn on anti-aliasing
                (q/frame-rate 1)                    ;; Set framerate to 1 FPS
                (q/background 200))
        draw (fn []
               (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
                                   (doseq [node nodes]
                                     (draw-node node aabb wpx hpx)))
               (q/save "debug.png")
               )]
    (q/sketch  :title "Oh so many grey circles"    ;; Set the title of the sketch
               :setup setup                        ;; Specify the setup fn
               :draw draw                          ;; Specify the draw fn
               :size [640 480])
    )
  )

;(gui-main [true false false false true nil nil nil nil nil nil nil nil nil nil nil nil false false false nil]
;          [-10 -10 10 10]
;          [0 4 20])
