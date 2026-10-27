#!/usr/bin/env bash
# Setup for an agent's environment (Codex cloud's setup script, or a fresh clone): run from the repository root,
# with network on. It fetches the engine's three jars, writes referee/judge.json from the visible-only
# referee/judge.visible.json (no hidden path is ever written), and checks the visible score is 0.9666.
set -euo pipefail
cd "$(dirname "$(readlink -f "$0")")"
command -v java >/dev/null || { echo "codex-setup: java (JDK 17+) is missing" >&2; exit 1; }
command -v node >/dev/null || { echo "codex-setup: node (18+) is missing" >&2; exit 1; }
[ -f lib/clojure-1.12.6.jar ] && [ -f lib/spec.alpha-0.5.238.jar ] && [ -f lib/core.specs.alpha-0.4.74.jar ] || ./run.sh --fetch-jars
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
