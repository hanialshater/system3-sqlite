# Rules for any coding agent in this repository

This file is for agents that read `AGENTS.md` (Codex and others). It restates the rules of `CLAUDE.md` in words true for
any agent, plus the rules of the pilot on this branch. If the two ever disagree, the stricter rule wins. The task is in
`HANDOVER.md`.

## The repository's rules (from CLAUDE.md)

- This is a SQLite-compatible database engine in Clojure (`src/sqlclj/`), judged only by its referee (`referee/`).
- **Main never gets worse.** The visible score must not fall: run `node referee/judge.mjs fast --dir=.` before you
  change anything and again when you finish, and record both scores in `HANDOVER.md`.
- **No SQLite at run time.** The engine never calls or embeds SQLite, starts a process, or opens a JDBC connection.
  `spec/b12/sqlrun` is a reference for your own comparisons only, never for the engine.
- **Never add a deploy marker.** Never put `[deploy]` in a commit message. Never run a deploy command.
- **No new dependency.** Do not add a library, a jar, an npm or a Python package.
- **This repository is public.** No secrets, no personal data, no session transcripts in any file or commit.

## The pilot's rules (this branch)

- **Work only inside this repository's folder.** Do not read, list or write anything outside it, your home folder
  included.
- **Never read hidden or held-out data.** Do not open, list or search `referee/judge.json`'s `hidCorpus` or `hidWork`
  paths if they exist, any folder named `hidden`, `held-out` or `referees`, or a `system3-referees` checkout. The
  visible corpus (`spec/b12/corpus`) is the only test data you use.
- **Do not read `docs/pilot-codex-1.md`** (the predictions about this run) or any other branch.
- **No network.** Do not try to fetch, install, push or call any service. If a step seems to need the network, stop and
  write why in your report.
- **Commit on the branch `codex/pilot-1` only.** Never switch to `main`, never commit to it, never push (the founder
  pushes the branch). Never rewrite history (`reset --hard`, `rebase`, `push --force`, `commit --amend` of an earlier
  commit).
- **Stop at 90 minutes** from your first command, or when your usage limit is reached. Commit what you have as it
  stands and write your report. Do not remove, weaken or special-case a test to make it pass.
- **When done, write a short report** in `HANDOVER.md`'s section "Codex's report" and commit it.
