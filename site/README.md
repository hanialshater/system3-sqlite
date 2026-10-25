# The demo site

`node site/build.mjs` writes `site/dist/index.html`, a static page built from real runs:
- the examples in `site/examples/`, each run statement by statement through the engine and through SQLite's
  reference build (`spec/b12/sqlrun`, used here only for comparison);
- the referee's visible score for each test file, and the first statements where the engine still differs;
- the score over B13's run (`company/events.jsonl`).

It needs Java (for the engine) and `referee/judge.json`. `site/dist/` is committed because a static host cannot run
the engine: rebuild and commit it when the engine changes. To add an example, add `site/examples/NN-name.sql`, one
statement per line.
