// copied from system3-city pages/features @ baee396; this is the company's copy: change it freely
// The features list (a city page template): what the product does, by status, from the dashboard contract's features.
export function render({ dashboard, esc }) {
  const fs = dashboard?.features ?? [], order = ["live", "building", "planned", "retired"];
  if (!fs.length) return `<h1>Features</h1><div class="empty">No features recorded yet (features.json beside company.json).</div>`;
  const by = {}; for (const f of fs) (by[f.status ?? "planned"] ??= []).push(f);
  const keys = [...order.filter((k) => by[k]), ...Object.keys(by).filter((k) => !order.includes(k))];
  const card = (f) => `<div class="card" style="margin-bottom:8px"><div><b>${esc(f.title ?? f.id)}</b> <span class="chip">${esc(f.status ?? "planned")}</span>${f.since ? ` <span class="k">since ${esc(f.since)}</span>` : ""}${f.owner ? ` <span class="k">· ${esc(f.owner)}</span>` : ""}</div>
    ${f.summary ? `<div style="margin-top:4px">${esc(f.summary)}</div>` : ""}${(f.links ?? []).length ? `<div class="k" style="margin-top:4px">${f.links.map((l) => `<a href="${esc(l)}" target="_blank" rel="noopener">${esc(l)}</a>`).join(" · ")}</div>` : ""}</div>`;
  return `<h1>Features</h1><div class="sub1">${fs.length} features · ${keys.map((k) => `${by[k].length} ${esc(k)}`).join(" · ")}</div>${keys.map((k) => `<h2>${esc(k)}</h2>${by[k].map(card).join("")}`).join("")}`;
}
