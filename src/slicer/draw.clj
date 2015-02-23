; draw slices and boxes onto screen
(ns slicer.draw
  (:require [quil.core :as q]
            [slicer.tree :as tree]))

;(q/show-cats)
;(q/show-fns "sketch")

(defn draw-node
  "given node index and root AABB, draws it one the screen"
  [node [min-x min-y max-x max-y :as aabb]]
  )

(defn gui-main
  "the gui engine"
  [t aabb nodes]
  (let [setup #((q/smooth)                          ;; Turn on anti-aliasing
                (q/frame-rate 1)                    ;; Set framerate to 1 FPS
                (q/background 200))
        draw #((q/stroke (q/random 255))             ;; Set the stroke colour to a random grey
               (q/stroke-weight (q/random 10))       ;; Set the stroke thickness randomly
               (q/fill (q/random 0))               ;; Set the fill colour to a random grey
                 (let [diam (q/random 100)             ;; Set the diameter to a value between 0 and 100
                       x    (q/random (q/width))       ;; Set the x coord randomly within the sketch
                       y    (q/random (q/height))]     ;; Set the y coord randomly within the sketch
                   (q/ellipse x y diam diam)))
        _ (q/defsketch example                  ;; Define a new sketch named example
                       :title "Oh so many grey circles"    ;; Set the title of the sketch
                       :setup setup                        ;; Specify the setup fn
                       :draw draw                          ;; Specify the draw fn
                       :size [323 200])
        ]
    )
  )

;(gui-main nil nil nil)
