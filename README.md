# embodier-stl-slicer

A clojure command line stl slicer.

Features:

  1.Single extrusion whenever possible.

  2.Infill pattern that promotes single extrusions.

## Compilation

* using leiningen
Lein bin

* using boot
boot build

## Documentation Generating

lein marg

## Run test

Lein test

or hotrunning:

Lein test-refresh

## Usage

* most of the time:
./target/embodier -s stl-file -g gcode-file

* if doesn't work, try:
java -jar .\target\embodier.jar -h

## License

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
