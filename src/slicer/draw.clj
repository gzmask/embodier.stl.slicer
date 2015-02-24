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
  [t [min-x min-y max-x max-y :as aabb] nodes]
  (let [[[x1 y1] [x2 y2] [x3 y3] [x4 y4]] (aabb-points aabb)
        h (- max-y min-y)
        hpx (/ 480 h)
        w (- max-x min-x)
        wpx (/ 640 w)
        setup (fn []
                (q/smooth)                          ;; Turn on anti-aliasing
                (q/frame-rate 1)                    ;; Set framerate to 1 FPS
                (q/background 200))
        draw #(q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
                                  (->> [x1 y1 x2 y2] (map + [1 1 1 -1]) (map * [wpx hpx wpx hpx]) (apply q/line))
                                  (->> [x2 y2 x3 y3] (map + [1 -1 -1 -1]) (map * [wpx hpx wpx hpx]) (apply q/line))
                                  (->> [x3 y3 x4 y4] (map + [-1 -1 -1 1]) (map * [wpx hpx wpx hpx]) (apply q/line))
                                  (->> [x4 y4 x1 y1] (map + [-1 1 1 1]) (map * [wpx hpx wpx hpx]) (apply q/line)))
        ]
    (q/sketch  :title "Oh so many grey circles"    ;; Set the title of the sketch
               :setup setup                        ;; Specify the setup fn
               :draw draw                          ;; Specify the draw fn
               :size [640 480])
    )
  )

(gui-main [true false false false true nil nil nil nil nil nil nil nil nil nil nil nil false false false nil]
          [-10 -10 10 10]
          [0 4 20])
