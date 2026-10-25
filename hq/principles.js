// copied from system3-city pages/principles @ baee396; this is the company's copy: change it freely
// The principles page (a city page template): the company against PRINCIPLES.md, from the dashboard's principles audit.
export function render({ dashboard, esc }) {
  const a = dashboard?.principles;
  if (!a?.checks?.length) return `<h1>Principles</h1><div class="empty">No principles audit (the dashboard tool prints principles; Meta's principles.mjs).</div>`;
  const mark = { ok: "✓ followed", gap: "✗ gap", manual: "◐ by hand" };
  const row = (c) => `<tr><td><b>${esc(c.id)}</b></td><td>${esc(c.title)}</td><td><span class="chip">${mark[c.state] ?? esc(c.state)}</span></td><td class="k">${esc(c.note ?? "")}</td></tr>`;
  return `<h1>Principles</h1><div class="sub1">What made an org of agents as good as one strong agent: the city's defaults. ${a.followed} followed · ${a.gaps} gaps · ${a.manual} checked by hand, of ${a.total}</div>
    ${a.gaps ? `<div class="card" style="margin:8px 0"><b>${a.gaps} gap${a.gaps > 1 ? "s" : ""}.</b> Close each one, or record why the company breaks it (a decision record the owner approves).</div>` : ""}
    <table><thead><tr><th></th><th>Principle</th><th>State</th><th>Evidence</th></tr></thead><tbody>${a.checks.map(row).join("")}</tbody></table>`;
}
