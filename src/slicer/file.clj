;; # File processing
;; Reading the binary STL is done with the gloss library.
;; Serialization order might not be a problem when encoding/decoding through gloss codec.
;; But if I am decoding an externally defined STL format into a map I will need to use Gloss' ordered-map which encodes/decodes map values in the order they are defined.
(ns slicer.file
  (:use [gloss.core
         gloss.io
         clojure.java.io]))

;; ## triangle frame
(defcodec triangle
  (ordered-map
   :normal [:float32 :float32 :float32]
   :vertex-1 [:float32 :float32 :float32]
   :vertex-2 [:float32 :float32 :float32]
   :vertex-3 [:float32 :float32 :float32]
   :attribute :int16))

;; ## binary stl frame
;; [header triangles]
;; another way to interpret header is (string :utf-8 :length 80)
(defcodec b-stl
  [(vec (repeat 80 :byte))
   (repeated triangle :prefix :int32)
   ])

;; ## parse file
(defn [stl-file]
  (let [buffer (byte-array 16)]
    (.read (input-stream stl-file) buffer)
    (decode b-stl buffer)))
