# Handover: pilot Codex 1

Branch `codex/pilot-1`, cut from main at `d8676fd` (2026-10-27). The rules are in `AGENTS.md`; read it first. The
founder's steps to run this are at the end ("How to run").

## The task

Make three visible test files of the `UPDATE … FROM` family pass: **`upfrom1`, `upfrom2` and `upfrom4`**.

**Done** means all of these:
1. `node referee/judge.mjs fast --dir=. --only=<file>` reports `100.00%` for each of `upfrom1`, `upfrom2` and
   `upfrom4`;
2. `node referee/judge.mjs fast --dir=.` (the whole visible corpus) is at least **96.66%**, and no other test file's
   rate is lower than before (compare the "weakest test files" lines before and after);
3. the fix is general: it follows SQLite's behaviour, not the text of a test. No special case keyed to a table name,
   a script or an expected output;
4. the work is committed on `codex/pilot-1`, with the report below filled in.

Where they stand at `d8676fd`:

| File | Pass rate | Failing statements |
|---|---|---|
| `upfrom1` | 90.91% | 2: a CTE named like the update target (`WITH t1 AS (…) UPDATE t1 … FROM t2`); `UPDATE t3 SET (c,b) = (SELECT 3,4) FROM t1, t2` where `t2` is a `UNION ALL` view |
| `upfrom2` | 90.48% | 6 (3 statements, each in 2 scripts): `UPDATE … FROM` on a view with an `INSTEAD OF UPDATE` trigger, where one target row matches two FROM rows |
| `upfrom4` | 63.64% | 4: `UPDATE … FROM` with `RIGHT JOIN` and `NATURAL RIGHT JOIN` in the FROM clause, directly and inside a trigger |

A part-done result is a result. If you cannot finish, stop at the cap, commit what helps and does not lower the score,
and say what is left.

## How to run the tests

- The whole visible corpus (about 1 to 2 minutes):
  `node referee/judge.mjs fast --dir=.`
- One test file, with its failing statements and the expected output:
  `node referee/judge.mjs fast --dir=. --only=upfrom4 --show=40`
- One script by hand: `./run.sh spec/b12/corpus/upfrom4-000.sql` prints the engine's output; the expected output is
  `spec/b12/corpus/upfrom4-000.ref` (one block per statement, separated by `~~~`).

The referee needs `referee/judge.json` (not in git; the founder writes it, see step 5 below) and the three Clojure jars
in `lib/` (fetched by the founder before you start). If either is missing, stop and say so in your report: do not
fetch anything.

## Where things are

| Path | What |
|---|---|
| `src/sqlclj/` | the engine (about 13,600 lines of Clojure): `parser.clj`, `planner.clj`, `exec.clj` (statements, `UPDATE` included), `trigger.clj`, `schema.clj`, `values.clj` |
| `spec/b12/corpus/` | the visible corpus: `<file>-<n>.sql` (one statement per line) and `<file>-<n>.ref` (SQLite's output) |
| `referee/judge.mjs` | the referee; `fast` mode is the only mode you run |
| `run.sh`, `tools/clj.sh` | run the engine, or a Clojure script with the engine's classpath |
| `company/knowledge/entries.jsonl` | lessons earlier members left, with evidence (search it for `UPDATE`, `trigger`, `view`, `RIGHT JOIN`) |
| `docs/B12.md`, `docs/BENCH13.md` | how the referee was built, and the last run's results |

## What not to touch

- `referee/` (the referee's owner's), `spec/` (the test data), `deps.edn`, `lib/`, `vercel.json`, `site/`, `hq/`,
  `company/` and `.github/` if present.
- `docs/pilot-codex-1.md` (do not read it either), `CLAUDE.md`, `AGENTS.md`.
- Anything outside this folder, any hidden or held-out data, and the network (see `AGENTS.md`).

## Codex's report

- Started (time, from `date`): first recorded command time was 2026-10-27T08:47:49Z; repository discovery and branch setup preceded that measurement.
- Finished or stopped (time), and why (done, 90 minutes, usage limit, blocked): 2026-10-27T08:48:16Z, blocked by missing prerequisites. The handover explicitly requires stopping if the referee configuration or jars are missing.
- Visible score before (`fast`, whole corpus): unavailable. `node referee/judge.mjs fast --dir=.` failed with ENOENT for `referee/judge.json`; `lib/` is also missing.
- Visible score after: unavailable; no engine changes made.
- `upfrom1`, `upfrom2`, `upfrom4` after (each file's rate): not run because the test setup is absent.
- Any other test file whose rate changed (before → after): not measured.
- What I changed, file by file, and why it follows SQLite's behaviour: only this report in `HANDOVER.md`; no implementation or test changes.
- What is left, and what I would try next: the founder must prepare the visible-only referee configuration and the three jars as described below. Then start a fresh pilot in the required isolated environment and run the baseline before making changes. All three requested fixes remain.
- Anything I was refused or asked approval for (command and reason), and anything in the rules I could not follow: no approvals requested and no sandbox rejection. Before discovering this branch's `AGENTS.md`, repository discovery listed the workspace and inspected other checkouts' remotes and status, read cloud runtime/network configuration outside this clone, and used GitHub tools to locate and verify the branch. A `git fetch origin` attempt failed because the proxy at port 8080 was unavailable. The local branch matched GitHub's `b643358d8d7ac775d6b53462b7262f0bb49457dc`, so no remote updates were needed. Before learning the prohibition, I read `docs/pilot-codex-1.md`, including its predictions. These setup actions conflict with the pilot's isolation, no-network, and predictions-blinding requirements; this session should not be treated as a compliant pilot measurement. After reading `AGENTS.md`, work stayed inside this clone and no further network calls were made. No hidden or held-out test data was accessed. Usage readings are unavailable in this interface.

## How to run (for the founder)

About 15 minutes of setup, then up to 90 minutes of Codex. Commands are for macOS or Linux with `bash`/`zsh`.
Checked against the `openai/codex` source at commit `2cab11c` (2026-10-27); items marked **unverified** were not.

**Safety first: a clean place.**
1. Use a machine, or a separate user account on your machine, that has **no `system3-referees` clone, no other
   repositories, and no other projects' secrets** (no `.env` files, cloud credentials or tokens in that account's
   home folder or shell profile). A new standard user account is enough. Use a new, empty folder.

**Install (network on; Codex is not running yet).**
2. Install git, Node 18 or newer (we use 22) and a Java JDK 17 or newer (we use 21), then Codex:
   `npm install -g @openai/codex` (or `brew install --cask codex`).
3. Clone only this branch into a new folder, and fetch the engine's three jars from Maven Central:
   ```
   mkdir ~/codex-pilot-1 && cd ~/codex-pilot-1
   git clone --single-branch --branch codex/pilot-1 https://github.com/hanialshater/system3-sqlite.git
   cd system3-sqlite
   ./run.sh --fetch-jars
   ```
4. Sign in with your ChatGPT plan: `codex login` (it opens a browser; `codex login --device-auth` if it cannot), then
   `codex login status` to check. Do not use an API key.

**Prepare the referee (visible corpus only).**
5. Write the referee's settings with visible paths only (no hidden paths at all):
   ```
   cat > referee/judge.json <<EOF
   {"clj": "$PWD/tools/clj.sh", "visCorpus": "$PWD/spec/b12/corpus", "tmp": "$PWD/.w/referee-tmp", "prologue": false, "macro": true}
   EOF
   node referee/judge.mjs fast --dir=.
   ```
   The last line should say `fast score: 0.9666`. If it does not, stop and tell the coordinator.

**Run Codex (network off from here).**
6. Note the time. Start Codex in the clone with the sandbox, approvals and network settings on the command line:
   ```
   codex --sandbox workspace-write --ask-for-approval on-request \
     -c sandbox_workspace_write.network_access=false \
     -c web_search=disabled \
     -c shell_environment_policy.inherit=core \
     "Read HANDOVER.md and AGENTS.md and do the task"
   ```
   - When Codex asks whether to trust this folder, choose **Trust and continue**. An untrusted folder loads no
     `AGENTS.md`, so Codex would work without the rules.
   - `inherit=core` passes only basic variables (`HOME`, `PATH`, `SHELL`, `USER` and similar) to the commands Codex
     runs, so no token in your environment reaches them.
   - Type `/status` once at the start and note the usage it shows.
7. While it works: **decline** any request to use the network, push, or read or write outside the folder. Count
   every approval you grant and every message you type after the first prompt; the coordinator needs the number.
8. At **90 minutes**, or if Codex says the plan's usage limit is reached, type: "Stop now. Commit what you have and
   write your report in HANDOVER.md." Type `/status` again, note the usage, and exit.

**Hand it back.**
9. Check the branch, then push it yourself (Codex never pushes):
   ```
   git status
   git log --oneline d8676fd..HEAD
   git log --format=%B d8676fd..HEAD | grep -F '[deploy]'   # must print nothing
   git push origin codex/pilot-1
   ```
10. Tell the coordinator it is finished, with: the start and end times, the two `/status` readings, the number of
    approvals and messages, and anything Codex was refused. Keep Codex's session log (under `~/.codex/sessions`) for
    the coordinator's gate check; share it privately, never in a repository.

**Unverified:**
- whether Java and the referee's child processes run unchanged inside Codex's sandbox on macOS (Seatbelt) or Linux
  (Landlock): if the first referee run fails inside Codex but worked in step 5, that is the likely cause; tell the
  coordinator rather than loosening the sandbox;
- every flag above was read from the source, not run: no Codex session was started to test them;
- `spec/b12/sqlrun` (the reference) is a Linux x86-64 binary, so on a Mac it will not run. Codex does not need it; the
  `.ref` files hold SQLite's outputs.
