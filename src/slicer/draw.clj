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

(defn gui-main
  "the gui engine"
  [t aabb nodes]
  (let [[[x1 y1] [x2 y2] [x3 y3] [x4 y4]] (aabb-points aabb)
        setup #((q/smooth)                          ;; Turn on anti-aliasing
                (q/frame-rate 1)                    ;; Set framerate to 1 FPS
                (q/background 200))
        draw #(q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
                                   (q/line x1 y1 x2 y2)
                                   (q/line x2 y2 x3 y3)
                                   (q/line x3 y3 x4 y4)
                                   (q/line x4 y4 x1 y1))
        ]
    (q/sketch  :title "Oh so many grey circles"    ;; Set the title of the sketch
               :setup setup                        ;; Specify the setup fn
               :draw draw                          ;; Specify the draw fn
               :size [323 200])
    )
  )

;(gui-main [true false false false true nil nil nil nil nil nil nil nil nil nil nil nil false false false nil]
;          [-10 -10 10 10]
;          [0 4 20])
