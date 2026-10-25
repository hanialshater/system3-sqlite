# SQLite in Clojure: rules for every session

A company of the System 3 city. It builds a SQLite-compatible engine in Clojure (`src/sqlclj/`). It is judged only by
its referee (`referee/`), SQLite's own test files. **The disk is the memory**: the next session knows only what is here.

## Start of session

1. `node referee/judge.mjs fast --dir=.` (the visible score; it needs `referee/judge.json`).
2. Read `company/milestones.json` and the newest results in `docs/`.
3. Read `company/knowledge/entries.jsonl` for your area: the lessons earlier members left, with evidence and belief.

## Hard rules

- **Main never gets worse.** Work lands on main only past the referee: the hidden score must not fall.
- **No SQLite at run time.** The engine never calls or embeds SQLite. `spec/b12/sqlrun` (the reference) is for
  members' own comparisons, never for the engine.
- **Predictions before runs.** A new run's design and predictions go in `docs/` before it starts.
- **Spending needs the owner's approval** (`company/company.json` budget lines).
- **This repository is public.** No secrets, no personal data, no session transcripts.

## Before you finish

- Run the referee, and record the score.
- Add a lesson to `company/knowledge/entries.jsonl` if you learned something the next member should know.
