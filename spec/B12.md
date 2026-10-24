# B12: SQLite compatibility, measured by SQLite's own test suite

This repository holds a SQLite-compatible database written in Clojure (`src/sqlclj/`), built in an earlier benchmark
against a generated corpus of core SQL, where it gives exactly SQLite 3.53.4's output. **Your job: make it compatible
with much more of SQLite, as measured by the tests SQLite's own developers wrote.** The interface, output format,
driver and rules are those of `spec/B11.md` (read it): one statement per input line, a `~~~` line after each line's
output, `clj-run -m sqlclj.main --batch <list> --out <dir>`.

## The tests

The SQL of SQLite 3.53.4's own Tcl test suite (`test/*.test`), replayed in order through the driver against an
in-memory database (a reopened database starts a fresh script), with real SQLite's output as the expected result:
affinity, collation, window functions, CTEs, upsert, triggers, views, ALTER TABLE, JSON, date and time, printf,
savepoints, foreign keys, error messages, and much more. Test files of subsystems an in-memory build without
extensions cannot run (I/O and malloc faults, WAL, journals, FTS, R-tree, sessions, virtual tables, threads, the shell)
are left out.

- The visible part, about 30% of the test files: `spec/b12/corpus/` (`<testfile>-<k>.sql` with SQLite's `.ref`).
  `spec/b12/sqlrun` runs real SQLite on any script you write.
- The hidden part, the other test files (the same kinds of tests, different files), decides the score.

## The score

**The mean over test files of each file's statement pass rate** (a statement passes when its output up to the `~~~`
equals SQLite's byte for byte), so breadth counts. Also reported: the pass rate over all statements. The fast judge,
as often as you like: `node <toolchain>/judge/judge.mjs fast --dir=<your tree>` (`--only=<testfile>` for one file,
`--show=30` for more failures).

## What earlier teams learned

`spec/KNOWLEDGE.md`: the knowledge base, what the agents of earlier runs recorded when they left (practices, findings,
dead ends and pitfalls, each with its evidence and how sure they were). Use what you judge useful.
