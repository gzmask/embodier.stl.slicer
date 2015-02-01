(set-env!
  :source-paths #{"src"}
  :dependencies '[[org.clojure/clojure "1.5.1"]
                  [org.clojure/tools.cli "0.3.1"]
                  [gloss "0.2.3"]])

(task-options!
  pom {:project 'slicer
       :version "0.1.0-SNAPSHOT"}
  jar {:manifest {"Foo" "bar"}})


(deftask build
  "Build my project."
  []
  (comp (pom) (jar) (install)))
