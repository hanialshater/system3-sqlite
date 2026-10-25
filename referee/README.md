# The referee

The referee is SQLite's own test suite, converted into SQL scripts with the outputs SQLite gives. B12 made the
conversion (`docs/B12.md`).

- **Score**: the mean, over test files, of each file's statement pass rate (`macro: true`), with no setup prologue.
  Each script gets 20 seconds; a script over that is cut and fails.
- **Visible corpus** (`spec/b12/corpus`): 221 test files, 1,250 scripts. Members score their work against it:
  `node referee/judge.mjs fast --dir=.`.
- **Hidden corpus**: 480 test files, 1,520 scripts. It is held by the referee's owner, never in this repository.
  `full` mode runs it on a clean copy of the tree's HEAD, with `spec/` removed and no SQLite binary reachable (shims
  refuse `sqlite3` and the rest). A land counts only if the hidden score does not fall.
- **Setup**: copy `judge.example.json` to `judge.json` (ignored by git) and fill in the paths.

Reproduced on 2026-10-25 in this repository: `fast` gives a 96.66% mean over the 221 visible files, the same as B13's
final scoring.
