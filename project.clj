(defproject slicer "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.cli "0.3.1"]
                 [gloss "0.2.3"]]
  :main slicer.core
  :aot :all
  :uberjar-name "embodier.jar"
  :plugins [[lein-bin "0.3.4"]
            [lein-marginalia "0.8.0"]]
  :bin { :name "embodier" }
  )
