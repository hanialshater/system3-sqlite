#!/bin/sh
# Runs the engine: ./run.sh script.sql  |  ./run.sh --batch list.txt --out dir  |  ./run.sh (interactive)
# Classpath: Clojure 1.12.6 + spec.alpha 0.5.238 + core.specs.alpha 0.4.74 (the referee's toolchain), then ./src.
# CLJ_LIB is the folder holding those three jars (default ./lib; `./run.sh --fetch-jars` downloads them from Maven Central).
HERE="$(dirname "$(readlink -f "$0")")"; LIB="${CLJ_LIB:-$HERE/lib}"
if [ "$1" = "--fetch-jars" ]; then
  mkdir -p "$LIB" && cd "$LIB" || exit 1
  for j in org/clojure/clojure/1.12.6/clojure-1.12.6.jar org/clojure/spec.alpha/0.5.238/spec.alpha-0.5.238.jar org/clojure/core.specs.alpha/0.4.74/core.specs.alpha-0.4.74.jar; do
    curl -fsSLO "https://repo1.maven.org/maven2/$j" || exit 1; done; exit 0
fi
exec java -XX:+UseSerialGC -XX:TieredStopAtLevel=1 -Xss64m \
  -cp "$LIB/clojure-1.12.6.jar:$LIB/spec.alpha-0.5.238.jar:$LIB/core.specs.alpha-0.4.74.jar:$HERE/src" clojure.main -m sqlclj.main "$@"
