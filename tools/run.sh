#!/bin/sh
# Local runner (clj-run equivalent): tools/run.sh script.sql | tools/run.sh --batch list --out dir
D=/tmp/claude-0/-home-user/aa2da695-733d-5610-822b-d0f420a6080f/scratchpad/b11-runs/arms/V/toolchain/lib
R=/tmp/claude-0/-home-user/aa2da695-733d-5610-822b-d0f420a6080f/scratchpad/b11-runs/arms/V/V
exec java -XX:+UseSerialGC -XX:TieredStopAtLevel=1 -Xss64m -cp $D/clojure-1.12.6.jar:$D/spec.alpha-0.5.238.jar:$D/core.specs.alpha-0.4.74.jar:$R/src clojure.main -m sqlclj.main "$@"
