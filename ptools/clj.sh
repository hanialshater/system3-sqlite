#!/bin/sh
# a Clojure script with the engine's classpath (dev helper; CLJ_LIB as in ../run.sh)
H="$(dirname "$(readlink -f "$0")")/.."; L="${CLJ_LIB:-$H/lib}"
exec java -Xss64m -cp "$L/clojure-1.12.6.jar:$L/spec.alpha-0.5.238.jar:$L/core.specs.alpha-0.4.74.jar:$H/src" clojure.main "$@"
