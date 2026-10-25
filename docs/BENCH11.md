# Benchmark 11 results: a SQLite-compatible database in Clojure (the autonomous org O, against one agent V)

owner: vp-meta · date: 2026-10-24 · status: **final** · protocol and predictions: [B11.md](B11.md) (committed before
the run; the score's prologue rule set before it) · off-boarding and the knowledge base: [../../knowledge/](../../knowledge/)

## Result (the full judge on each arm's final head, the hidden corpus: 3,000 core-family scripts)

| Arm | Hidden family statements | Scripts entirely right | All statements | Batch time (3,000 scripts) | Cost | Time to the ceiling |
|---|---|---|---|---|---|---|
| **V**, one agent | **1.0000** | **2,999 / 3,000** | 1.0000 | **34 s** | $145.69 | about 0.99 at 52 min; 1.000 visible within about 2 h |
| **O**, the org | **0.9998** | 2,987 / 3,000 | 0.9999 | 205 s | $117.91 | 0.9998 at 78 min (26 min after landing was unblocked) |
| start (stub) | 0 | 0 | 0 | 3 s | | |

Both arms passed the rules (deterministic; no process or database access). **V won by 0.02 points: a tie at the
ceiling.** The task was too easy to separate them (the founder asked "is the task difficult enough?": it was not);
B12 is grounded in SQLite's own test suite to fix that.

## How the org worked (no rounds, no resets, self-landing, a shared budget, roles not limits)

- **They set the architecture themselves in the first ten minutes.** Three members proposed different splits within
  two minutes; m-architect landed ARCHITECTURE.md (values, AST as maps with source spans, file ownership, the engine
  API) at 19:19 and ruled; all four modules fit together on the first integration.
- **Built in parallel, integrated at once**: parser, values layer and engine landed 20:09-20:10; the architect's
  wiring took main 0 → 0.84 at 20:15; two API mismatches found at wiring (aggregate state returned or mutated; a
  `Character` passed to a `^long` function) took it to 0.976; 0.9998 at 20:35.
- **23 lands** (architect 10, engine 6, values 4, language 3), none refused; **45 notes**; 37 board entries.
- **They managed the budget themselves**: once the visible score saturated, members went idle on their own (19 idle
  events), and by 20:54 all four had stopped, leaving $32 unspent. V, told "go on", ran 37 more tiny turns (about $26)
  after it had nothing left to do.

## The harness's failure (fixed during the run)

**`org land` left its lock held after a refused land** (it exited the process inside the `try`, skipping the
`finally`): from 19:20 to 20:12 **no member could land**, about 50 of the org's first 90 minutes. m-engine diagnosed it
from the code (notes, 19:54); the operator fixed it. Without it O would likely have reached the ceiling at about the
same time as V. All four members recorded it at off-boarding as a pitfall, with the evidence.

## Predictions

| # | Prediction | Verdict |
|---|---|---|
| B11-1 | O beats V by ≥ 5 points | **wrong** (V 1.0000, O 0.9998: the ceiling) |
| B11-2 | both ≥ 60% | **held** (both ≈ 100%) |
| B11-3 | O lands ≥ 15 times, from all four | **held** (23, all four) |
| B11-4 | a member widens its role on its own for an hour or more | **not shown**: members stayed near their areas; the visible saturation came too early to test it |
| B11-5 | ≥ 10 notes, and a note leads to a landed change by its receiver | **held** (45; m-language's INDEXED BY rule to m-engine, landed; bug reports to m-values, fixed within a round) |
| B11-6 | a land refused at least once | **wrong** (none refused: every land kept or raised main) |
| B11-7 | V's context compacts at least once | **wrong** (none; peak 918k tokens) |
| B11-8 | O spends within 10% of V | **wrong** (O $117.91, V $145.69: O stopped itself) |
| B11-9 | hidden within 3 points of visible | **held** (V 1.0 / 1.0; O 0.9998 / 0.9998) |

## Off-boarding and the persistent knowledge base (new)

After the run every session (V and the four members) got one last turn to record what it learned, with evidence and a
belief: **54 entries in 22 topics**, $17.43, in `knowledge/entries.jsonl` and `knowledge/KB.md`. The strongest:
differential fuzzing against the reference binary (V, belief 0.95; m-values 0.9); fixing a written contract in the
first ten minutes (architect 0.85, three members agreeing); "landing is the bottleneck, not coding" (m-values 0.85);
run the judge on the merge of everyone's code as soon as the first wiring lands. The next run's members read it at
the start.

## What this says

1. **On work this size, one strong agent and an autonomous org both reach the ceiling.** V got there sooner and wrote a
   six times faster engine; O got there with less money and stopped itself when done. Neither result discriminates.
2. **The autonomous org behaved like a good small team**: it chose its own architecture and contract, built in
   parallel, integrated within minutes, helped each other by note, found the harness's bug, and judged when to stop.
   That is the founder's design working, and it is what the knowledge base now records.
3. **The next test must not saturate**: SQLite's own test suite (B12), where the work is several times larger than
   one context and the corner cases are real ones.
