;; # The Core
;; First of all, we need to take care of the cmd line parameters.
;; I heard that tools.clj is pretty good.
;; Also lein bin makes cmd easier.
;; lein bin will generate target/embodier as the final executable.
;; Literature programming with margonilia for help doc.
;; Lein marg will generate this html help doc in docs folder.
(ns slicer.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [slicer.file :as f])
  (:gen-class))

;; ##cli specs
;; declare the specs for the cmd args
(def cli-options
  [["-h" "--help"]
   ["-s" "--stl STL-file" "embodier -s [stl-file-name]." :id :stl :validate [#(re-find #".+\.stl" %) "Must be a binary STL file."]]
   ["-g" "--gcode Gcode-file" "embodier -g [gcode-file-name]." :id :gcode :default "out.gcode"]
   ]
  )

(def help-txt "To slice STL, simply run: \"embodier -s [stl-file-name] -g [gcode-file-name]\" ")

;; ## the exit
(defn exit [status & msg]
  (when msg (println msg))
  (System/exit status))

;; ## Main function, entry point of the command
;; first destructed the map from tools.cli, making it more readable.
;; If there are no errors and it's not a help request, process with file IO
(defn -main [& args]
  (let [{opts :options args :arguments summary :summary errs :errors}
        (parse-opts args cli-options) ]
    (when (not (empty? errs))
      (doseq [err errs]
        (println err))
      (exit 1))
    (when (:help opts)
      (println help-txt)
      (exit 0 summary))
    (when (:stl opts)
      (println (prn-str opts)))
    ))
