(set-env!
  :source-paths #{"src"}
  :dependencies '[[org.clojure/clojure "1.5.1"]
                  [org.clojure/tools.cli "0.3.1"]
                  [gloss "0.2.3"]])

;(task-options!
;  pom {:project 'embodier
;       :version "1.0.0"}
;  jar {:main 'slicer.core}
;  aot {:all true})
;
;(deftask build []
;  (comp (aot) (pom) (uber) (jar)))

(deftask build
  "Build embodier slicer."
  []
  (comp 
    (aot :all true)
    (pom :project 'slicer 
         :version "0.1.0-SNAPSHOT") 
    (uber)
    (jar :main 'slicer.core) 
    ))
