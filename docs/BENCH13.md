# Benchmark 13: the org at scale, with fallible agents: results

owner: vp-meta · date: 2026-10-25 · status: **final** · design and predictions: [B13.md](B13.md) (committed before the
run) · founder: "our purpose was org is scalable and allows for fallible agents, going to 100% is good idea"; B13
approved at about $500; no single-agent arm (founder, 2026-10-25: "I don't see the point now")

## Result

| | Start (B12's org, after its continuation) | B13 end |
|---|---|---|
| **Hidden mean per test file (480 files)** | 88.50% | **91.85%** (+3.35) |
| All hidden statements | 90.08% | 93.59% |
| Hidden scripts entirely right | 1,066 / 1,520 | 1,169 / 1,520 |
| Visible mean per test file | 92.69% | 96.66% (+3.97) |
| Cost | | **$429.84** ($408.21 the run, $21.63 off-boarding) |
| Time | | 241 minutes, stopped by its plateau rule (no gain for 60 minutes) |

Eight members (two Opus, five Sonnet, one Haiku) ran in continuous sessions with the shared budget, and landed their
own work past the referee: 41 lands, 5 refused. The planned crash of m-engine at 45 minutes and its resume 20 minutes
later worked. Off-boarding added 85 entries; with two entries from the final scoring, the knowledge base holds 245
entries in 53 topics.

## The predictions (committed before the run)

| # | Prediction | Result | Verdict |
|---|---|---|---|
| B13-1 | main never falls on any land | none of 41 lands lowered the visible score | **right**, but see "the fallibility gap" |
| B13-2 | the referee refuses at least 3 of m-hasty's lands | 1 (m-values had 3, all "the referee could not score it") | **wrong** |
| B13-3 | at least 5 hidden points (≥ 93.5%) | +3.35 (91.85%) | **wrong** |
| B13-4 | does not reach 100% hidden | 91.85% | **right** |
| B13-5 | throughput at least 1.5× B12's org in its continuation (2.74 hidden points in about 60 minutes) | 3.35 hidden points in 241 minutes (0.83 an hour); +1.26 visible in the first two hours | **wrong** |
| B13-6 | m-engine lands again within 30 minutes of its return | back 10:55, landed 11:15 (and twice more) | **right** |
| B13-7 | another member notices m-hasty's refusals or regressions and acts on them | its one refusal drew no reaction; the architect had steered it at the start to speed work (timeouts), where it kept the score | **wrong** as worded |
| B13-8 | members cite or follow the knowledge base | all 8 read it (3 to 6 reads each); the architect's opening plan follows a B12 entry ("write probe scripts that imitate the hidden test files") | **right** |
| B13-9 | the org stops by its plateau rule before spending the $500 | stopped at $408 of $500 | **right** |

Five right, four wrong. The wrong ones are the ambitious ones: scale (B13-3, B13-5) and fallibility being visible to
the org (B13-2, B13-7).

## What we learned

**1. Headcount did not buy throughput near the ceiling.** Eight members gained 3.35 hidden points for $430
($128 a point). B12's four-member org gained 2.74 for $150 in its continuation ($55 a point). The gains were very
uneven by member (visible points per $10):

| Member | Model | Spent | Lands | Visible points | Per $10 |
|---|---|---|---|---|---|
| m-planner | Sonnet | $102.22 | 4 | +2.29 | 0.22 |
| m-sweeper | Sonnet | $35.86 | 6 | +0.58 | 0.16 |
| m-schema | Sonnet | $86.94 | 7 | +0.57 | 0.07 |
| m-engine | Opus | $38.04 | 4 | +0.18 | 0.05 |
| m-values | Sonnet | $26.40 | 4 | +0.15 | 0.06 |
| m-architect | Opus | $58.53 | 8 | +0.16 | 0.03 |
| m-language | Sonnet | $53.95 | 5 | +0.04 | 0.01 |
| m-hasty | Haiku | $6.26 | 3 | 0.00 | 0.00 |

**2. The org's biggest win went against its own knowledge base.** Four B12 members had recorded EXPLAIN QUERY PLAN
as a dead end. m-planner modelled it for simple query shapes anyway. One land took visible from 94.40% to 96.57%, and
it generalised: on the hidden files whereH +100, whereE +100, where +99, analyze7 +87.5, whereF and analyzeC +41.7
points each, about a third of the hidden gain. The knowledge base now records that the dead end was wrong (belief
0.85). Lesson: a knowledge base helps when it is read as testimony with beliefs, not as rules; a member who tests a
"dead end" can be the most valuable one.

**3. The fallibility gap: the referee guards only what it can see.** Main never fell on the visible set. Yet 17 of
480 hidden files got worse while 98 improved:
- like3: −38.9 points
- existsexpr2: −28.6
- indexedby: −12.8
- json109: −11.8
- bloom1: −11.1
- triggerC: −10.9

A visible-only referee contains careless work only on what it measures. The fix belongs in the referee, not the
members: a second held-out slice checked on every land (separate from the final hidden set), or regression probes.

**4. Fallible agents were contained by assignment, not by detection.** The careless Haiku member was given speed
work (scripts timing out at 20 seconds) by the architect's plan. Its 3 lands kept the score, its 1 bad land was
refused, and it cost $6. Nobody reacted to its refusal because nothing needed reacting to. The crash was absorbed
the same way: m-engine came back with its context and landed three more times.

**5. The org stops itself.** All members went idle by 12:55; the plateau rule ended the run at $408, leaving $92 of
the budget unspent.

## Harness deviations

- **10:50, a false plateau stop** (logged in B13.md): the rule read the last gain against the wrong base and stopped
  every member at minute 60 while main was rising. Fixed and resumed at 10:52 with the same sessions and caps;
  1 minute down. The end row of 10:50 is superseded; final scoring read the last one.
- **Unscorable lands:** three of m-values' lands were refused with "the referee could not score it" (the cause is not
  in the record), not because main would fall.

## Next

1. **A referee with a guard slice** (a held-out set checked on land, separate from the final hidden set), and B13's
   start rerun with it: does it stop the 17 hidden regressions without stopping the gains?
2. **Exploration over headcount:** fewer members, with a budget line for testing "dead ends" from the knowledge base.
3. **Knowledge with outcomes:** entries whose beliefs are updated by results (as the EQP entry was), shown in the HQ.
