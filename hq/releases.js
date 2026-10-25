// copied from system3-city pages/releases @ baee396; this is the company's copy: change it freely
// The release log (a city page template): every release, newest first, with who approved it (PRINCIPLES.md P13).
export function render({ dashboard, esc }) {
  const rs = dashboard?.releases ?? [];
  if (!rs.length) return `<h1>Release log</h1><div class="empty">No releases recorded yet (releases.json beside company.json).</div>`;
  const row = (r) => `<div class="card" style="margin-bottom:8px"><div><span class="k">${esc(r.date ?? "")}</span> <b>${esc(r.title ?? r.id)}</b>${r.commit ? ` <code>${esc(r.commit)}</code>` : ""}</div>
    ${(r.notes ?? []).length ? `<ul>${(Array.isArray(r.notes) ? r.notes : [r.notes]).map((n) => `<li>${esc(n)}</li>`).join("")}</ul>` : ""}
    <div class="k">${r.approvedBy ? `approved by ${esc(r.approvedBy)}` : `<b>not approved</b>`}</div></div>`;
  return `<h1>Release log</h1><div class="sub1">${rs.length} releases · last ${esc(rs[0].date ?? "")}</div>${rs.map(row).join("")}`;
}
