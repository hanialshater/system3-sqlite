#!/usr/bin/env bash
# Setup for an agent's environment (Codex cloud's setup script, or a fresh clone): run from the repository root,
# with network on. It fetches the engine's three jars, writes referee/judge.json from the visible-only
# referee/judge.visible.json (no hidden path is ever written), and checks the visible score is 0.9666.
set -euo pipefail
cd "$(dirname "$(readlink -f "$0")")"
command -v java >/dev/null || { echo "codex-setup: java (JDK 17+) is missing" >&2; exit 1; }
command -v node >/dev/null || { echo "codex-setup: node (18+) is missing" >&2; exit 1; }
# the three jars (run.sh --fetch-jars; if Maven Central's repo1 host refuses, its other host), pinned by SHA-256
JARS="org/clojure/clojure/1.12.6/clojure-1.12.6.jar 21b578db389a8e74b286ce79c4a7f86ec641e26d88b12f18d69030916b0e18ca
org/clojure/spec.alpha/0.5.238/spec.alpha-0.5.238.jar 94cd99b6ea639641f37af4860a643b6ed399ee5a8be5d717cff0b663c8d75077
org/clojure/core.specs.alpha/0.4.74/core.specs.alpha-0.4.74.jar eb73ac08cf49ba840c88ba67beef11336ca554333d9408808d78946e0feb9ddb"
sha() { if command -v sha256sum >/dev/null; then sha256sum "$1"; else shasum -a 256 "$1"; fi | cut -d' ' -f1; }
jars_ok() { echo "$JARS" | while read -r j h; do f="lib/$(basename "$j")"; [ -f "$f" ] && [ "$(sha "$f")" = "$h" ] || exit 1; done; }
if ! jars_ok; then
  ./run.sh --fetch-jars || true
  if ! jars_ok; then
    mkdir -p lib
    echo "$JARS" | while read -r j h; do
      curl -fsSL --retry 3 --retry-delay 5 -o "lib/$(basename "$j")" "https://repo.maven.apache.org/maven2/$j" || exit 1; done
  fi
  jars_ok || { echo "codex-setup: the jars are missing or their checksums differ" >&2; exit 1; }
fi
# refuse if the visible config has gained a hidden key or a path that looks like a hidden set
node -e 'const c=require("./referee/judge.visible.json"); const bad=Object.entries(c).filter(([k,v])=>k!=="note"&&(/^hid/i.test(k)||/hidden|held-out|referees/i.test(String(v)))); if(bad.length){console.error("codex-setup: judge.visible.json names a hidden path ("+bad.map(b=>b[0]).join(", ")+"); refusing");process.exit(1)}'
cp referee/judge.visible.json referee/judge.json
mkdir -p .w/referee-tmp
out="$(node referee/judge.mjs fast --dir=. 2>&1)"; echo "$out" | grep -E 'mean over|fast score' || true
score="$(echo "$out" | sed -n 's/^B12 fast score: \([0-9.]*\).*/\1/p')"
# 0.9666 at the base commit d8676fd; a later commit may only be higher (main never gets worse)
node -e 'process.exit(Number(process.argv[1]) >= 0.9666 ? 0 : 1)' "${score:-0}" \
  || { echo "codex-setup: the visible score is ${score:-missing}, below 0.9666 (see above)" >&2; exit 1; }
echo "codex-setup: ready (visible score $score; 0.9666 expected at d8676fd)"
