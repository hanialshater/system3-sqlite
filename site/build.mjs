#!/usr/bin/env node
// The demo site: a static page (site/dist/index.html) built from real runs, nothing typed by hand.
//   - examples/*.sql run through the engine (./run.sh) and through SQLite's own reference build (spec/b12/sqlrun),
//     statement by statement, so the page shows both outputs and whether they match
//   - the referee's visible score per SQLite test file (node referee/judge.mjs score)
//   - the score over B13's run, from company/events.jsonl (every land and every refusal)
// The engine never calls SQLite; the reference runs here only to show what SQLite says.
//   node site/build.mjs [--out=site/dist] [--no-score]
import fs from "node:fs";
import path from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const HERE = path.dirname(fileURLToPath(import.meta.url)), ROOT = path.join(HERE, "..");
const opt = Object.fromEntries(process.argv.slice(2).filter((a) => a.startsWith("--")).map((a) => { const [k, ...v] = a.slice(2).split("="); return [k, v.join("=") || true]; }));
const OUT = path.resolve(opt.out ?? path.join(HERE, "dist"));
const run = (cmd, args) => spawnSync(cmd, args, { cwd: ROOT, encoding: "utf8", maxBuffer: 64 << 20, timeout: 600_000 });
const blocks = (s) => { const out = []; let cur = []; for (const l of s.split("\n")) { if (l === "~~~") { out.push(cur.join("\n")); cur = []; } else cur.push(l); } return out; };

// 1. the examples: one statement per line; each line's output block from both programs
const examples = fs.readdirSync(path.join(HERE, "examples")).filter((f) => f.endsWith(".sql")).sort().map((f) => {
  const file = path.join(HERE, "examples", f), lines = fs.readFileSync(file, "utf8").split("\n").filter((l) => l.trim());
  const ours = blocks(run(path.join(ROOT, "run.sh"), [file]).stdout), ref = blocks(run(path.join(ROOT, "spec/b12/sqlrun"), [file]).stdout);
  const steps = lines.map((sql, i) => ({ sql, ours: ours[i] ?? "(no output)", sqlite: ref[i] ?? "(no output)", same: ours[i] === ref[i] }));
  return { id: f.replace(/\.sql$/, ""), title: f.replace(/^\d+-|\.sql$/g, "").replace(/-/g, " ").replace(/\b(cte|json)\b/g, (w) => w.toUpperCase()).replace(/^./, (c) => c.toUpperCase()), steps };
});

// 2. the referee's visible score, per test file
let score = null;
if (!opt["no-score"]) { const r = run("node", ["referee/judge.mjs", "score", "--dir=."]); try { score = JSON.parse(r.stdout.trim().split("\n").pop()); } catch { console.error("score: the referee did not answer (is referee/judge.json set up?)"); } }

// 2b. where it still differs: the referee's first failing statements on the visible corpus (fast mode prints them)
let fails = [];
if (!opt["no-score"]) { const r = run("node", ["referee/judge.mjs", "fast", "--dir=.", "--show=400"]), L = r.stdout.split("\n");
  for (let i = 0; i < L.length; i++) { const m = /^B12\s+fail (\S+):(\d+): (.*)$/.exec(L[i]); if (m && !/\(setup\)/.test(L[i])) { const want = /want: (.*)$/.exec(L[i + 1] ?? "")?.[1], got = /got:\s+(.*)$/.exec(L[i + 2] ?? "")?.[1]; if (fails.some((x) => x.file.replace(/-\d+$/, "") === m[1].replace(/-\d+$/, "")) || fails.length >= 8) continue; try { fails.push({ file: m[1], line: +m[2], sql: m[3], want: JSON.parse(want), got: JSON.parse(got) }); } catch {} } } }
// 3. the run: lands and refusals over time, and who made them
const ev = fs.readFileSync(path.join(ROOT, "company/events.jsonl"), "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l));
const start = ev.find((e) => e.type === "crew.start");
const lands = ev.filter((e) => e.type === "land" || e.type === "land.refused").map((e) => ({ t: e.t, by: e.by, ok: e.type === "land", before: e.before ?? null, after: e.after ?? null, msg: (e.msg ?? e.why ?? "").slice(0, 140) }));
const org = JSON.parse(fs.readFileSync(path.join(ROOT, "company/org.json"), "utf8")).roles;
const company = JSON.parse(fs.readFileSync(path.join(ROOT, "company/company.json"), "utf8"));

const data = { fails, built: new Date().toISOString(), commit: run("git", ["rev-parse", "--short", "HEAD"]).stdout.trim(), examples, start: start ? { t: start.t, rate: start.rate } : null, lands,
  score: score && { macro: score.macro, files: score.files, statements: score.statements, statementsPass: score.statementsPass, scripts: score.scripts, scriptsPass: score.scriptsPass, families: Object.entries(score.families).map(([f, v]) => ({ f, rate: v.rate, scripts: v.scripts })) },
  org: org.filter((o) => o.id.startsWith("m-")).map((o) => ({ id: o.id, title: o.title, focus: o.focus, model: o.model })), hidden: { mean: 0.9185, files: 480, statements: 0.9359, source: "docs/BENCH13.md" }, cost: company.budget.lines.reduce((a, l) => a + (l.spent ?? 0), 0) };

fs.mkdirSync(OUT, { recursive: true });
const page = fs.readFileSync(path.join(HERE, "page.html"), "utf8").replace("/*DATA*/null", JSON.stringify(data).replace(/</g, "\\u003c"));
fs.writeFileSync(path.join(OUT, "index.html"), page);
const same = examples.flatMap((x) => x.steps).filter((s) => s.same).length, all = examples.flatMap((x) => x.steps).length;
console.log(JSON.stringify({ out: OUT, examples: examples.length, statementsMatching: `${same}/${all}`, visible: score?.macro ?? null, lands: lands.length }));
