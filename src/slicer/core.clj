;; # The Core 
;; First of all, we need to take care of the cmd line parameters
;; I heard that tools.clj is pretty good
;; Also lein bin makes cmd easier
;; lein bin will generate target/embodier as the final executable
(ns slicer.core
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

;; ##cli specs 
;; declare the specs for the cmd args
(def cli-options
  [["-h" "--help"]
   ]
  )

;; ## Main function, entry point of the command
;; first destructed the map from tools.cli, making it more readable.
(defn -main [& args] 
  (let [{opts :options args :arguments summary :summary errs :errors} 
        (parse-opts args cli-options) ]
    (prn (prn-str opts))
    ))
