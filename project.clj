(defproject slicer "0.1.0-SNAPSHOT"
  :description "The Embodier Slicer"
  :url "http://embodier.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [quil "2.2.5"]
                 [com.jakemccrary/lein-test-refresh "0.7.0"]
                 [gloss "0.2.3"]]
  :main slicer.core
  :aot :all
  :uberjar-name "embodier.jar"
  :plugins [[lein-bin "0.3.4"]
            [com.jakemccrary/lein-test-refresh "0.7.0"]
            [lein-marginalia "0.8.0"]]
  :bin { :name "embodier" }
  )
