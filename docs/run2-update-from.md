# Run 2: UPDATE FROM implementation checks

Implementation started 2026-10-27 at 09:09:30 UTC, following a readiness check at
09:04:50 UTC. The setup script's visible baseline is 0.9666.
The three target files reproduce the handover's failures. A separate visible-only
batch snapshot in ignored `work/run2/` will allow a statement and file comparison.

Before changing the engine, the proposed fixes and predictions are:

- Use the already resolved target table for the synthetic scan, bypassing same-named CTEs;
  keep those CTEs available to FROM and expressions.
- Evaluate each row-valued SET subquery once per joined row and distribute its
  columns to the assignment targets, with normal arity checks and NULLs for an
  empty result.
- Keep the FROM join group separate from the target when right/full or natural/USING
  joins require that boundary. Unmatched RIGHT/FULL rows must retain the target
  scan, and NATURAL must compare only the sources named in FROM.
- Preserve every joined row for an INSTEAD OF view update; ordinary table updates
  still collapse duplicate matches by target rowid.

Prediction: upfrom1, upfrom2 and upfrom4 reach 100%, the whole visible score reaches
about 0.9691, and no other visible file loses passing statements. Compare additional
small probes against the reference executable for tuple correlation, duplicate
matches, CTE shadowing, natural joins and outer joins before finishing.

This continues the existing chat, so the predictions exposure recorded in Run 1
still applies. Setup network access was explicitly authorized by the user; it
required enabling network for the setup command. Coding and tests use local files.

## Results

Verified at 09:21:55 UTC: all three target files pass at 100%; whole-visible fast
score is 0.9691, with 8,856/9,028 statements passing. Comparing the entire visible
batch against its pre-change snapshot found only the 12 intended output changes
and no regressions in any of the 221 files.

The 14 cases in `tools/update-from-regression.sql` match their SQLite reference
outputs in `tools/update-from-regression.ref`. Reproduce the comparison without
running SQLite:

```sh
mkdir -p work/update-from-check
./run.sh tools/update-from-regression.sql > work/update-from-check/actual.out
diff -u tools/update-from-regression.ref work/update-from-check/actual.out
```

Two lessons from the probes: repeated INSTEAD OF view firings need the original
materialized OLD row even after an earlier trigger writes the underlying table;
and target resolution must reuse the executor's table rather than introduce a
new schema qualifier, which can break already qualified attached-database keys.
Tuple values must be projected together to avoid evaluating a volatile RHS twice.

The hidden score is left to the steward. Evaluation limitations and the setup and
intervention record are in HANDOVER.md's Run 2 section.
