#!/bin/sh
# Runs a Clojure script with the benchmark classpath (dev helper).
D=/tmp/claude-0/-home-user/aa2da695-733d-5610-822b-d0f420a6080f/scratchpad/b11-runs/arms/V/toolchain/lib
R=/tmp/claude-0/-home-user/aa2da695-733d-5610-822b-d0f420a6080f/scratchpad/b11-runs/arms/V/V
java -Xss64m -cp $D/clojure-1.12.6.jar:$D/spec.alpha-0.5.238.jar:$D/core.specs.alpha-0.4.74.jar:$R/src clojure.main "$@" 2>&1 | grep -v JAVA_TOOL
