// copied from system3-city pages/goals @ c6ef9bf; this is the company's copy: change it freely
// Goals and OKRs (a city page template): objectives with key results from the dashboard contract's `goals`. Each KR shows
// its metric, baseline, target, current value and owner, a progress bar only when it is measured, and its milestones
// with their predictions (from the dashboard's milestones). Unmeasured is shown as unmeasured, never as 0. Derived goals
// (no OKR file yet) say so. Reads only ctx.dashboard.
const STATE = { done: "✓", active: "●", partly: "◐", next: "○", blocked: "■" };
export function render({ dashboard, esc }) {
  const g = dashboard?.goals, ms = dashboard?.milestones ?? [];
  if (!g || !(g.objectives ?? []).length) return `<h1>Goals</h1><div class="empty">No goals recorded yet. A company keeps its OKRs in goals.json beside company.json; without one, the dashboard derives them from its mission and milestones.</div>`;
  const fmt = (x, unit) => (x == null ? "—" : `${typeof x === "number" ? x.toLocaleString(undefined, { maximumFractionDigits: 2 }) : esc(x)}${unit && unit !== "milestones" ? ` ${esc(unit)}` : ""}`);
  const krs = g.objectives.flatMap((o) => o.keyResults), measured = krs.filter((k) => k.measured);
  const mean = measured.length ? measured.reduce((a, k) => a + (k.progress ?? 0), 0) / measured.length : null;
  const msLine = (id) => { const m = ms.find((x) => x.id === id); if (!m) return `<li><code>${esc(id)}</code></li>`; const ps = Array.isArray(m.predictions) ? m.predictions : [];
    return `<li><span title="${esc(m.state)}">${STATE[m.state] ?? "·"}</span> <b>${esc(m.id)}</b> ${esc(m.title)}${ps.length ? `<ul class="gl-pred">${ps.map((p) => `<li>predicted: ${esc(p.text ?? p)}${p.wrongIf ? ` <span class="k">· wrong if ${esc(p.wrongIf)}</span>` : ""}${p.verdict ? ` <span class="chip">${esc(p.verdict)}</span>` : ""}</li>`).join("")}</ul>` : ""}</li>`; };
  const kr = (k) => `<div class="gl-kr"><div class="gl-head"><b>${esc(k.id)}</b> ${esc(k.title)}${k.measured ? "" : ` <span class="chip gl-un">unmeasured</span>`}<span class="k" style="margin-left:auto">${esc(k.owner ?? "")}</span></div>
    <div class="k">${esc(k.metric ?? "")}${k.direction === "at most" ? " · at most the target" : ""}</div>
    <div class="gl-nums"><span><span class="k">baseline</span> ${fmt(k.baseline, k.unit)}</span><span><span class="k">target</span> ${fmt(k.target, k.unit)}</span><span><span class="k">current</span> <b>${k.measured ? fmt(k.current, k.unit) : "not measured"}</b></span>${k.progress != null ? `<span><span class="k">progress</span> ${Math.round(k.progress * 100)}%</span>` : ""}</div>
    ${k.progress != null ? `<div class="bar"><i style="width:${Math.round(k.progress * 100)}%"></i></div>` : `<div class="gl-hatch" title="no current value recorded"></div>`}
    ${k.note ? `<div class="k" style="margin-top:4px">${esc(k.note)}</div>` : ""}
    ${(k.milestones ?? []).length ? `<details><summary class="k">${k.milestones.length} milestone${k.milestones.length > 1 ? "s" : ""} · ${k.predictions ?? 0} prediction${k.predictions === 1 ? "" : "s"}</summary><ul class="gl-ms">${k.milestones.map(msLine).join("")}</ul></details>` : ""}</div>`;
  return `${STYLE}<h1>Goals</h1><div class="sub1">${g.derived ? `<b>derived</b>: ${esc(g.source)}` : `from ${esc(g.source)}`}${g.period ? ` · ${esc(g.period)}` : ""}</div>
  <div class="row" style="margin-bottom:14px"><div class="card stat"><div class="k">objectives</div><div class="v">${g.objectives.length}</div></div><div class="card stat"><div class="k">key results</div><div class="v">${krs.length}</div><div class="k">${measured.length} measured · ${krs.length - measured.length} unmeasured</div></div><div class="card stat"><div class="k">mean progress (measured only)</div><div class="v">${mean == null ? "—" : `${Math.round(mean * 100)}%`}</div></div></div>
  ${g.objectives.map((o) => `<div class="card gl-o"><h3>${esc(o.id)} · ${esc(o.title)}${o.derived ? ` <span class="chip">derived</span>` : ""}</h3><div class="k">owner ${esc(o.owner ?? "")}</div>${o.keyResults.length ? o.keyResults.map(kr).join("") : `<div class="empty">No key results.</div>`}</div>`).join("")}`;
}
const STYLE = `<style>.gl-o{margin-bottom:14px}.gl-kr{border-top:1px solid var(--line);padding:10px 0 8px}.gl-head{display:flex;gap:6px;align-items:baseline;flex-wrap:wrap}.gl-nums{display:flex;flex-wrap:wrap;gap:6px 16px;margin:6px 0}.gl-un{border-color:var(--warn);color:var(--warn)}.gl-hatch{height:8px;border-radius:4px;background:repeating-linear-gradient(45deg,var(--line) 0 6px,transparent 6px 12px)}.gl-ms,.gl-pred{margin:6px 0;padding-left:18px}.gl-pred{color:var(--muted)}.gl-o h3{overflow-wrap:anywhere}</style>`;
