;; # File processing
;; Reading the binary STL is done with the gloss library.
;; Serialization order might not be a problem when encoding/decoding through gloss codec.
;; But if I am decoding an externally defined STL format into a map I will need to use Gloss' ordered-map which encodes/decodes map values in the order they are defined.
(ns slicer.file
  (:use gloss.core
        gloss.io
        clojure.java.io))

(def delimiters ["\r" "\r\n" "\n" \newline])

;; ## binary triangle frame
(defcodec b-triangle
  (ordered-map
   :normal (ordered-map
            :x :float32-le :y :float32-le :z :float32-le)
   :vertex-1 (ordered-map
              :x :float32-le :y :float32-le :z :float32-le)
   :vertex-2 (ordered-map
              :x :float32-le :y :float32-le :z :float32-le)
   :vertex-3 (ordered-map
              :x :float32-le :y :float32-le :z :float32-le)
   :attribute :uint16))

;; ## ascii triangle frame
(defcodec a-triangle
  (ordered-map
   :_ (string :utf-8 :delimiters ["normal "])
   :normal (ordered-map
            :x (string-float :utf-8 :delimiters [\space])
            :y (string-float :utf-8 :delimiters [\space])
            :z (string-float :utf-8 :delimiters delimiters))
   :_ (string :utf-8 :delimiters ["vertex "])
   :vertex-1 (ordered-map
              :x (string-float :utf-8 :delimiters [\space])
              :y (string-float :utf-8 :delimiters [\space])
              :z (string-float :utf-8 :delimiters delimiters))
   :_ (string :utf-8 :delimiters ["vertex "])
   :vertex-2 (ordered-map
              :x (string-float :utf-8 :delimiters [\space])
              :y (string-float :utf-8 :delimiters [\space])
              :z (string-float :utf-8 :delimiters delimiters))
   :_ (string :utf-8 :delimiters ["vertex "])
   :vertex-3 (ordered-map
              :x (string-float :utf-8 :delimiters [\space])
              :y (string-float :utf-8 :delimiters [\space])
              :z (string-float :utf-8 :delimiters delimiters))
   :_ (string :utf-8 :delimiters ["endfacet"])
   :_ (string :utf-8 :delimiters delimiters)
   )
  )

;; ## binary stl frame
;; [header triangles]
;; another way to interpret header is (vec (repeat 80 :byte))
(defcodec b-stl
  (ordered-map
   :header (string :utf-8 :length 80)
   :triangles (repeated b-triangle :prefix :uint32-le)))


;; ## Ascii stl frame decode
;; The number of appearance of "normal" will be exactly how many triangles
;; Gloss documentation gives the following method:
;;:triangles (repeated a-triangle :delmiters ["endsolid"]) .
;; But this does not work.
;; Thus I have to count the words manually
(defn adecode [buffer]
  (let [n (count (re-seq #"normal" (String. buffer)))
        a-stl (compile-frame
               (ordered-map
                :header (string :utf-8 :delimiters delimiters)
                :triangles (vec (repeat n a-triangle))
                :_ (string :utf-8 :delimiters delimiters)))]
    (decode a-stl buffer false)))


;; ## parse file
;; 115 111 108 105 100 are the magic numbers for "solid"
(defn parse-stl [stl-file]
  (let [length (.length (file stl-file))
        buffer (byte-array length)]
    (.read (input-stream stl-file) buffer)
    (if (= '(115 111 108 105 100) (first (split-at 5 buffer)))
      (adecode buffer)
      (decode b-stl buffer))))

;; At first it keeps giving incifient bytes errors.
;; Then I tested the codec and finds out that it's the endianess that is messing with me.
;(vec (.array (contiguous
;(encode b-stl
;        ["012345678911234567892123456789312345678941234567895123456789612345678971234567898123456789"
;         [{:normal [1.0 1.0 1.0]
;           :vertex-1 [1.0 2.0 3.0]
;           :vertex-2 [2.0 1.0 1.0]
;           :vertex-3 [3.0 1.0 1.0]
;           :attribute 0}
;          {:normal [1.0 1.0 1.0]
;           :vertex-1 [1.0 2.0 3.0]
;           :vertex-2 [2.0 1.0 1.0]
;           :vertex-3 [3.0 1.0 1.0]
;           :attribute 0}]
;         ]))))
