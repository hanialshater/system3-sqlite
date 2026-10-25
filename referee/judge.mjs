#!/usr/bin/env node
// Benchmark 12: the judge (B12.md "The score"): B11's, with families = SQLite test files, no setup prologue, and the
// primary score the mean over test files of each file's statement pass rate (cfg.macro).
//   node judge.mjs fast --dir=<tree> [--only=<family>]   the visible corpus on the tree as it is (committed or not):
//        the statement pass rate in total and per family, and the first failing statements. Lines only.
//   node judge.mjs full --dir=<tree> [--out=<json>]      the operator's: the hidden corpus on a clean copy of the
//        tree's HEAD with spec/ removed (no reference binary reachable), a determinism run, the rule checks.
// Paths come from judge.json beside this script: clj, visCorpus, hidCorpus, tmp.
import fs from "node:fs";
import path from "node:path";
import { spawn, spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const HERE = path.dirname(fileURLToPath(import.meta.url));
const mode = process.argv[2];
const opt = Object.fromEntries(process.argv.slice(3).filter((a) => a.startsWith("--")).map((a) => { const [k, ...v] = a.slice(2).split("="); return [k, v.join("=") || true]; }));
const cfg = JSON.parse(fs.readFileSync(path.join(HERE, "judge.json"), "utf8"));
const DIR = path.resolve(opt.dir ?? ".");
if (!["fast", "full", "score"].includes(mode)) { console.error("usage: judge.mjs fast|full --dir=<tree>"); process.exit(2); }
export const SCRIPT_SEC = 20, START_SEC = 60;
const say = (s) => console.log(`B12 ${s}`);
const r4 = (x) => Math.round(x * 10000) / 10000;
fs.mkdirSync(cfg.tmp, { recursive: true });
const tmp = fs.mkdtempSync(path.join(cfg.tmp, "judge-"));
process.on("exit", () => fs.rmSync(tmp, { recursive: true, force: true }));
// the program runs with no SQLite or sqlrun reachable: a PATH of shims that refuse them
const NODB = path.join(tmp, "nodb"); fs.mkdirSync(NODB);
for (const n of ["sqlite3", "sqlite", "sqlrun", "duckdb", "psql", "mysql"]) fs.writeFileSync(path.join(NODB, n), `#!/bin/sh\necho "B11 judge: ${n} is not available" >&2\nexit 1\n`, { mode: 0o755 });
const env = { ...process.env, PATH: `${NODB}:${process.env.PATH}` };

/** run the program in batch over scripts (absolute paths) into outDir, with a watchdog: a script that takes longer
 *  than SCRIPT_SEC is cut (it fails) and the batch resumes after it. Returns { cut: [names], restarts } */
async function batch(dir, scripts, outDir) {
  fs.mkdirSync(outDir, { recursive: true });
  const nameOf = (f) => path.basename(f).replace(/\.sql$/, ""), cut = []; let restarts = 0, rest = scripts.slice();
  while (rest.length) {
    const list = path.join(tmp, `list-${restarts}.txt`); fs.writeFileSync(list, rest.join("\n") + "\n");
    const ch = spawn(cfg.clj, ["-m", "sqlclj.main", "--batch", list, "--out", outDir], { cwd: dir, env, stdio: ["ignore", "ignore", "pipe"] });
    let err = ""; ch.stderr.on("data", (d) => { if (err.length < 8000) err += d; });
    const exited = new Promise((r) => ch.on("close", (code) => r(code)));
    let last = Date.now(), seen = 0, first = true, code = null, done = false;
    exited.then((c) => { code = c; done = true; });
    while (!done) {
      await new Promise((r) => setTimeout(r, 250));
      const n = rest.filter((f) => fs.existsSync(path.join(outDir, `${nameOf(f)}.out`))).length;
      if (n > seen) { seen = n; last = Date.now(); first = false; }
      if (Date.now() - last > (first ? START_SEC : SCRIPT_SEC) * 1000) { ch.kill("SIGKILL"); await exited; break; }
    }
    const pending = rest.filter((f) => !fs.existsSync(path.join(outDir, `${nameOf(f)}.out`)));
    if (!pending.length) break;
    // the first unfinished script is the one that hung or crashed the batch: it fails; go on after it
    cut.push(nameOf(pending[0])); rest = pending.slice(1); restarts++;
    if (restarts > 200) { cut.push(...rest.map(nameOf)); break; }
    if (restarts === 1 && err) say(`the batch stopped (exit ${code}); stderr: ${err.trim().split("\n").slice(-3).join(" | ").slice(0, 400)}`);
  }
  return { cut, restarts };
}
/** a script's setup prologue: its leading CREATE TABLE and plain INSERT … VALUES lines (63% of all statements); the
 *  primary score counts only the statements after it (the families' own), the prologue is reported beside it */
const PROLOGUE = /^(CREATE TABLE t\d\(|INSERT (OR IGNORE )?INTO t\d\([^)]*\) VALUES\([^;]*\);$)/;
export const prologueLen = (lines) => { if (cfg.prologue === false) return 0; let i = 0; while (i < lines.length && PROLOGUE.test(lines[i]) && !/RETURNING|ON CONFLICT/.test(lines[i])) i++; return i; };
const blocks = (buf) => { const s = buf.toString("utf8"), out = []; let cur = []; for (const l of s.split("\n")) { if (l === "~~~") { out.push(cur.join("\n")); cur = []; } else cur.push(l); } return out; };
/** score a corpus dir: statements and scripts passing, per family */
async function score(dir, corpusDir, { only = null } = {}) {
  const names = fs.readdirSync(corpusDir).filter((f) => f.endsWith(".sql") && (!only || f.startsWith(`${only}-`))).sort();
  const outDir = path.join(tmp, `out-${path.basename(corpusDir)}-${Date.now()}`);
  const t0 = Date.now(), b = await batch(dir, names.map((n) => path.join(corpusDir, n)), outDir), sec = (Date.now() - t0) / 1000;
  const fam = {}, fails = []; let st = 0, sp = 0, sc = 0, scp = 0, ft = 0, fp = 0;
  for (const n of names) {
    const base = n.replace(/\.sql$/, ""), f = base.replace(/-\d+$/, ""); fam[f] ??= { st: 0, sp: 0, sc: 0, scp: 0 };
    const ref = blocks(fs.readFileSync(path.join(corpusDir, `${base}.ref`))), outF = path.join(outDir, `${base}.out`);
    const got = !b.cut.includes(base) && fs.existsSync(outF) ? blocks(fs.readFileSync(outF)) : [];
    const lines = fs.readFileSync(path.join(corpusDir, n), "utf8").split("\n").filter((l) => l.trim());
    const pro = prologueLen(lines); let ok = 0, fok = 0;
    for (let i = 0; i < ref.length; i++) { if (got[i] === ref[i]) { ok++; if (i >= pro) fok++; } else if (fails.length < 400) fails.push({ script: base, line: i + 1, prologue: i < pro, sql: (lines[i] ?? "").slice(0, 160), want: ref[i].slice(0, 200), got: got[i] === undefined ? "(nothing)" : got[i].slice(0, 200) }); }
    st += ref.length; sp += ok; sc++; if (ok === ref.length) scp++; ft += ref.length - pro; fp += fok;
    fam[f].st += ref.length - pro; fam[f].sp += fok; fam[f].sc++; if (ok === ref.length) fam[f].scp++;
  }
  const famRates = Object.values(fam).filter((v) => v.st > 0).map((v) => v.sp / v.st), macro = famRates.length ? r4(famRates.reduce((a, x) => a + x, 0) / famRates.length) : 0;
  return { statements: ft, statementsPass: fp, rate: cfg.macro ? macro : ft ? r4(fp / ft) : 0, micro: ft ? r4(fp / ft) : 0, macro, files: famRates.length, all: { statements: st, pass: sp, rate: st ? r4(sp / st) : 0 }, scripts: sc, scriptsPass: scp, families: Object.fromEntries(Object.entries(fam).map(([k, v]) => [k, { rate: r4(v.sp / v.st), scripts: `${v.scp}/${v.sc}` }])), cut: b.cut, restarts: b.restarts, sec: Math.round(sec), fails, outDir };
}

if (mode === "fast" || mode === "score") {
  const s = await score(DIR, cfg.visCorpus, { only: opt.only ?? null });
  if (mode === "score") { const { fails, outDir, ...x } = s; console.log(JSON.stringify(x)); process.exit(0); }
  say(`visible corpus: ${s.statementsPass} of ${s.statements} family statements give SQLite's output (${(s.rate * 100).toFixed(2)}%; all statements including the setup prologues: ${(s.all.rate * 100).toFixed(2)}%); ${s.scriptsPass} of ${s.scripts} scripts entirely; ${s.sec} s${s.cut.length ? `; cut at ${SCRIPT_SEC} s: ${s.cut.slice(0, 10).join(", ")}${s.cut.length > 10 ? " …" : ""}` : ""}`);
  const fams = Object.entries(s.families).sort((a, b) => a[1].rate - b[1].rate);
  if (cfg.macro) { say(`mean over ${s.files} test files: ${(s.macro * 100).toFixed(2)}%; all statements: ${(s.micro * 100).toFixed(2)}%`); say(`the ${Math.min(25, fams.length)} weakest test files: ${fams.slice(0, 25).map(([f, v]) => `${f} ${(v.rate * 100).toFixed(0)}%`).join(", ")}`); }
  else for (const [f, v] of fams) say(`  ${f}: ${(v.rate * 100).toFixed(1)}% of statements, ${v.scripts} scripts`);
  for (const x of s.fails.filter((x) => !x.prologue).slice(0, Number(opt.show ?? 8)).concat(s.fails.filter((x) => x.prologue).slice(0, 4))) say(`  fail ${x.script}:${x.line}${x.prologue ? " (setup)" : ""}: ${x.sql}\n      want: ${JSON.stringify(x.want)}\n      got:  ${JSON.stringify(x.got)}`);
  say(`fast score: ${s.rate} (${cfg.macro ? "the mean over test files of each file's statement pass rate" : "the family statements' pass rate"}; higher is better)`);
  process.exit(0);
}

// ---- full: a clean copy of HEAD, without spec/
const snap = path.join(tmp, "snap"); spawnSync("git", ["-c", "safe.directory=*", "clone", "-q", "--no-hardlinks", DIR, snap]); fs.rmSync(path.join(snap, "spec"), { recursive: true, force: true });
const rules = {};
const s = await score(snap, cfg.hidCorpus);
// determinism: the first 150 hidden scripts again
const names = fs.readdirSync(cfg.hidCorpus).filter((f) => f.endsWith(".sql")).sort().slice(0, 150), out2 = path.join(tmp, "det");
await batch(snap, names.map((n) => path.join(cfg.hidCorpus, n)), out2);
let same = true; for (const n of names) { const b = n.replace(/\.sql$/, ""), a1 = path.join(s.outDir, `${b}.out`), a2 = path.join(out2, `${b}.out`); if (fs.existsSync(a1) !== fs.existsSync(a2) || (fs.existsSync(a1) && !fs.readFileSync(a1).equals(fs.readFileSync(a2)))) { same = false; break; } }
rules.deterministic = same;
const flagged = spawnSync("grep", ["-rlE", "ProcessBuilder|Runtime/getRuntime|clojure\\.java\\.shell|jdbc", path.join(snap, "src")], { encoding: "utf8" }).stdout.trim();
rules.noProcess = !flagged;
const ok = rules.noProcess && rules.deterministic;
if (!cfg.macro) for (const [f, v] of Object.entries(s.families)) say(`hidden ${f}: ${(v.rate * 100).toFixed(1)}% of statements, ${v.scripts} scripts`); else say(`hidden: mean over ${s.files} test files ${s.macro}; all statements ${s.micro}`);
say(`rules: ${JSON.stringify(rules)}${flagged ? ` (flagged files: ${flagged.split("\n").map((f) => path.relative(snap, f)).join(", ")})` : ""}`);
const summary = { dir: DIR, rate: s.rate, macro: s.macro, micro: s.micro, files: s.files, statements: s.statements, statementsPass: s.statementsPass, all: s.all, scripts: s.scripts, scriptsPass: s.scriptsPass, families: s.families, cut: s.cut.length, sec: s.sec, rules, ok, score: ok ? s.rate : 0 };
say(`score: ${summary.score}`);
if (opt.out) fs.writeFileSync(path.resolve(opt.out), JSON.stringify(summary, null, 1));
console.log(JSON.stringify(summary));
