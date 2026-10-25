# SQLite in Clojure

A SQLite-compatible database engine written in Clojure by a company of AI agents in the
[System 3 city](https://github.com/hanialshater/system3-city). Its referee is SQLite's own test suite: the engine
passes when every statement in SQLite's test files gives the output SQLite gives.

| | |
|---|---|
| **Hidden test files (480), mean pass rate per file** | **91.85%** (B13, 2026-10-25) |
| All hidden statements | 93.59% |
| Visible test files (221), mean per file | 96.66% |
| Built by | an eight-member crew: an architect and lead, six area engineers, one deliberately fallible fast lander |
| Cost so far | $1,282.87 across three runs (B11, B12, B13; `company/company.json`) |

## Run it

Java 21 or later.

```sh
./run.sh --fetch-jars                 # Clojure 1.12.6 and its two spec jars into ./lib (or set CLJ_LIB)
echo "CREATE TABLE t(a); INSERT INTO t VALUES(1),(2); SELECT sum(a) FROM t;" > /tmp/t.sql
./run.sh /tmp/t.sql
./run.sh --batch list.txt --out out/  # many scripts, one output file each
```

With the Clojure CLI: `clojure -M:run script.sql`.

## The referee

`referee/judge.mjs` scores a tree:
- `fast` runs the visible corpus (`spec/b12/corpus`, 1,250 scripts from SQLite's test files).
- `full` runs the hidden corpus on a clean copy of the tree's HEAD, with no SQLite reachable.

The hidden set is held by the referee's owner, outside this repository. See `referee/README.md`.

## The company

`company/` is the company as data, read by the System 3 HQ through Meta's tools (system3-meta,
`docs/product/STARTER.md` §3):

| File | What |
|---|---|
| `company.json`, `milestones.json` | mission, measure, budget lines and milestones |
| `org.json` | the crew and its reporting lines (the org contract) |
| `events.jsonl` | B13's event log (v1): every land, refusal, note, claim and spend |
| `knowledge/entries.jsonl` | 245 lessons the members recorded when they left, with evidence and belief, by team (run) |

`docs/` holds the run designs (predictions committed before each run) and the results: `B11.md`/`BENCH11.md`,
`B12.md`/`BENCH12.md` and `B13.md`/`BENCH13.md`. `spec/` holds the specs the members worked from, and its
`KNOWLEDGE.md`.

## Publish to the city HQ

From a checkout of `hanialshater/system3-meta` beside this one:

```sh
node ../system3-meta/hq/build.mjs --city=hq/city.json --snapshot=sqlite --out=<snapshot dir>
```

## History

The git history is the crews' own: members commit under their role names (`m-architect`, `m-engine`, …), and
operators commit at the start of each run.

## Licence

Not chosen yet (the owner's decision). Until one is, all rights are reserved.
SQLite's test files, from which `spec/b12/corpus` is derived, are in the public domain.
