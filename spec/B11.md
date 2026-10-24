# B11: build a SQLite-compatible database in Clojure

Write, in this repository, a program that runs SQL scripts against an in-memory database and prints **exactly what
SQLite 3.53.4 prints** through the benchmark's driver. Clojure 1.12 only (the classpath is Clojure, spec.alpha and
core.specs.alpha, then `./src`; nothing else). `src/sqlclj/main.clj` is a stub to replace.

## The interface

```
clj-run -m sqlclj.main script.sql                      # output on stdout
clj-run -m sqlclj.main --batch <list> --out <dir>      # one fresh database per script; writes <dir>/<name>.out
```

A script has one statement per line (a line may hold several statements separated by `;`). After each non-blank
input line the output has that line's output and then a line `~~~`. A line's output, for each statement in it:

- if the statement returns columns: a line `# ` and the column names joined by `|` (SQLite's names: the alias, or
  the expression's text as written), then one line per row, the values joined by `|`: `NULL`; integers in decimal;
  reals as SQLite's text for them (`sqlite3_column_text`: for example `1.5`, `3.0e+100`, `0.1`); text as is; blobs as
  `X'` + uppercase hex + `'`;
- if the statement writes and returns no columns: `changes: N` (SQLite's `sqlite3_changes`);
- if it fails (at prepare or while running): `error: ` and SQLite's message, and the rest of that line is skipped.
  Rows already printed before a failure stay printed.

The exact reference is `spec/b11/sqlrun.c` (the driver, compiled against real SQLite by gcc as `spec/b11/sqlrun`,
which you may run to see what SQLite does with any SQL you write).

## The scope: the core families

Tables and constraints (`INTEGER PRIMARY KEY`, the type names and their affinities, `NOT NULL`, `DEFAULT`, `UNIQUE`,
`CHECK`, `COLLATE NOCASE`, `STRICT`, `REFERENCES` (not enforced: foreign keys are off), `INSERT OR IGNORE`),
insert/update/delete with `ON CONFLICT … DO UPDATE`, `REPLACE` and `RETURNING`, expressions and type affinity (casts,
`CASE`, `coalesce`, `nullif`, `iif`, `typeof`, `abs`, `BETWEEN`, `IN`, `IS`, comparisons across types, integer and
real arithmetic and their overflow, bit operators, `||`), string functions (`length`, `upper`, `lower`, `substr`,
`instr`, `replace`, `trim`, `ltrim`, `hex`, `quote`, `unicode`, `char`, `like`, `glob`), aggregates (`count`, `sum`,
`total`, `avg`, `min`, `max`, `group_concat`, `DISTINCT`) with `GROUP BY`/`HAVING`, joins (inner, left, cross,
`USING`), subqueries (scalar, `IN`, `EXISTS`, correlated, compound `UNION`/`UNION ALL`/`INTERSECT`/`EXCEPT`,
`ORDER BY`, `LIMIT`/`OFFSET`), and indexes (`CREATE [UNIQUE] INDEX … [WHERE …]`, `INDEXED BY`, `REINDEX`, `DROP
INDEX`). Row order without `ORDER BY` must match SQLite's too (SQLite scans a table in rowid order, and an index in
index order).

## The score

The hidden corpus is 3,000 scripts of these families, generated like the 600 visible ones in `spec/b11/corpus/`
(each `.sql` with SQLite's output in `.ref`). **The score is the fraction of statements (input lines) whose output,
up to the `~~~`, equals SQLite's byte for byte, counted after each script's setup prologue** (its leading
`CREATE TABLE` and plain `INSERT … VALUES` lines, which must still be right for what follows to be; their pass rate is
reported beside the score). A script that runs longer than 20 s fails entirely. The fast judge,
as often as you like: `node <toolchain>/judge/judge.mjs fast --dir=<your tree>` (the visible corpus, per family).

## Rules

- No SQLite, sqlrun or any other database at run time, in any form; the judge runs your program with no such program
  reachable. No expected outputs, or hashes or fingerprints of scripts, in the program: it must compute SQLite's
  behaviour.
- The same input gives the same output.
- Commit your work in this repository; only committed work is judged.
