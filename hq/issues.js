// copied from system3-city pages/issues @ c6ef9bf; this is the company's copy: change it freely
// Issues (a city page template), Linear-like, from the dashboard contract's `issues` ({ source, total, states, items:
// [{ id, title, state, priority, owner, project, labels, created, updated, url? }] }): a list with filters (state,
// owner, project, priority, words) grouped by state, owner or project; a board by state; an issue's detail. Fast at
// thousands: everything is paginated (50 rows a page, 20 cards a column), never one flat list. Keyboard: j/k move,
// Enter opens, Esc goes back, / searches, l list, b board. Reads only ctx.dashboard; view state in globalThis.__hqIssues.
const STATES = ["backlog", "todo", "in progress", "review", "done", "canceled"];
const PRIO = ["No priority", "Urgent", "High", "Medium", "Low"], PICON = ["–", "‼", "▲", "■", "▽"];
const PAGE = 50, COL = 20;
export function render(ctx) {
  const { dashboard, esc, company } = ctx, is = dashboard?.issues, all = is?.items ?? [];
  if (!all.length) return `<h1>Issues</h1><div class="empty">No issue tracker read for this company${is?.source ? ` (${esc(is.source)} is empty)` : ""}. Issues come from the company's own tracker through its tools: an issues.json beside company.json (Meta's <code>issues.mjs github &lt;owner/repo&gt;</code> writes one from GitHub issues), or an adapter for its board.</div>`;
  const G = (globalThis.__hqIssues ??= {}), key = company?.id ?? "hq";
  const ui = (G[key] ??= { view: "list", group: "state", states: new Set(["backlog", "todo", "in progress", "review"]), owner: "", project: "", prio: "", q: "", page: {}, col: {}, sel: 0, open: null, back: "list" });
  const owners = [...new Set(all.map((i) => i.owner).filter(Boolean))].sort(), projects = [...new Set(all.map((i) => i.project).filter(Boolean))].sort();
  const hits = () => { const ws = ui.q.toLowerCase().split(/\s+/).filter(Boolean); return all.filter((i) => ui.states.has(i.state) && (!ui.owner || i.owner === ui.owner) && (!ui.project || i.project === ui.project) && (ui.prio === "" || String(i.priority) === ui.prio) && (!ws.length || ws.every((w) => `${i.id} ${i.title} ${i.owner ?? ""} ${i.project ?? ""} ${(i.labels ?? []).join(" ")}`.toLowerCase().includes(w)))); };
  const $ = (id) => globalThis.document?.getElementById(id);
  const redraw = (all2 = true) => { if (all2) { const el = $("is-root"); if (el) el.innerHTML = draw(); } else { const el = $("is-results"); if (el) el.innerHTML = results(); } };
  G.set = (k, v) => { ui[k] = v; ui.page = {}; ui.col = {}; ui.sel = 0; redraw(); };
  G.toggleState = (s) => { ui.states.has(s) ? ui.states.delete(s) : ui.states.add(s); ui.page = {}; ui.sel = 0; redraw(); };
  G.q = (v) => { ui.q = v; ui.page = {}; ui.sel = 0; redraw(false); };
  G.more = (g, d) => { ui.page[g] = Math.max(0, (ui.page[g] ?? 0) + d); redraw(false); };
  G.colMore = (s) => { ui.col[s] = (ui.col[s] ?? 1) + 1; redraw(false); };
  G.open = (id) => { ui.back = ui.view === "detail" ? ui.back : ui.view; ui.open = id; ui.view = "detail"; redraw(); globalThis.scrollTo?.(0, 0); };
  G.close = () => { ui.view = ui.back ?? "list"; ui.open = null; redraw(); };
  if (!G.keys && globalThis.document) { G.keys = true; globalThis.document.addEventListener("keydown", (e) => {
    const k = G[G.cur], root = $("is-root"); if (!root || !k) return; const tag = (e.target?.tagName ?? "").toLowerCase();
    if (tag === "input" || tag === "select") { if (e.key === "Escape") e.target.blur(); return; }
    if (e.metaKey || e.ctrlKey || e.altKey) return;
    const rows = [...root.querySelectorAll("[data-iid]")];
    if (e.key === "/") { e.preventDefault(); $("is-q")?.focus(); }
    else if (e.key === "j" || e.key === "k") { k.sel = Math.max(0, Math.min(rows.length - 1, (k.sel ?? 0) + (e.key === "j" ? 1 : -1))); rows.forEach((r, i) => r.classList.toggle("is-sel", i === k.sel)); rows[k.sel]?.scrollIntoView({ block: "nearest" }); }
    else if (e.key === "Enter" && rows[k.sel]) G.open(rows[k.sel].dataset.iid);
    else if (e.key === "Escape" && k.view === "detail") G.close();
    else if (e.key === "l" && k.view !== "list") G.set("view", "list");
    else if (e.key === "b" && k.view !== "board") G.set("view", "board");
  }); }
  G.cur = key;
  const pr = (p) => `<span class="is-p is-p${p}" title="${PRIO[p] ?? ""}">${PICON[p] ?? "–"}</span>`;
  const st = (s) => `<span class="is-st is-${s.replace(" ", "-")}" title="${esc(s)}"></span>`;
  const row = (i, n) => `<a class="is-row${n === ui.sel ? " is-sel" : ""}" data-iid="${esc(i.id)}" onclick="globalThis.__hqIssues.open(this.dataset.iid)">${pr(i.priority)}<code class="is-id">${esc(i.id)}</code>${st(i.state)}<span class="is-t">${esc(i.title)}</span><span class="is-meta">${i.project ? `<span class="chip">${esc(i.project)}</span>` : ""}${i.owner ? `<span class="k">${esc(i.owner)}</span>` : ""}<span class="k">${esc(i.updated ?? "")}</span></span></a>`;
  const results = () => {
    const xs = hits(); let n = 0;
    if (ui.view === "board") return `<div class="k" style="margin-bottom:6px">${xs.length.toLocaleString()} issues</div><div class="is-board">${STATES.filter((s) => ui.states.has(s)).map((s) => { const cs = xs.filter((i) => i.state === s), lim = COL * (ui.col[s] ?? 1);
      return `<div class="is-col"><div class="is-colh">${st(s)} <b>${esc(s)}</b> <span class="k">${cs.length}</span></div>${cs.slice(0, lim).map((i) => `<a class="is-card${n++ === ui.sel ? " is-sel" : ""}" data-iid="${esc(i.id)}" onclick="globalThis.__hqIssues.open(this.dataset.iid)"><div class="k">${pr(i.priority)} <code>${esc(i.id)}</code></div><div class="is-ct">${esc(i.title.length > 120 ? i.title.slice(0, 119) + "…" : i.title)}</div><div class="k">${esc(i.owner ?? "")}</div></a>`).join("")}${cs.length > lim ? `<button class="pill" onclick="globalThis.__hqIssues.colMore('${esc(s)}')">${cs.length - lim} more</button>` : ""}</div>`; }).join("")}</div>`;
    const keyOf = (i) => (ui.group === "owner" ? i.owner ?? "no owner" : ui.group === "project" ? i.project ?? "no project" : ui.group === "state" ? i.state : "all");
    const groups = new Map(); for (const i of xs) { const g = keyOf(i); if (!groups.has(g)) groups.set(g, []); groups.get(g).push(i); }
    const order = ui.group === "state" ? STATES.filter((s) => groups.has(s)) : [...groups.keys()].sort((a, b) => groups.get(b).length - groups.get(a).length || String(a).localeCompare(String(b)));
    G.groups = order;
    return `<div class="k" style="margin-bottom:6px">${xs.length.toLocaleString()} issues${order.length > 1 ? ` in ${order.length} groups` : ""}</div>${order.length ? order.slice(0, 60).map((g, gi) => { const gs = groups.get(g), p = ui.page[g] ?? 0, pages = Math.ceil(gs.length / PAGE);
      return `<div class="is-group"><div class="is-gh">${ui.group === "state" ? st(g) : ""}<b>${esc(g)}</b><span class="k">${gs.length}</span>${pages > 1 ? `<span class="is-pg"><button class="pill" ${p ? "" : "disabled"} onclick="globalThis.__hqIssues.more(globalThis.__hqIssues.groups[${gi}],-1)">‹</button><span class="k">${p + 1} / ${pages}</span><button class="pill" ${p + 1 < pages ? "" : "disabled"} onclick="globalThis.__hqIssues.more(globalThis.__hqIssues.groups[${gi}],1)">›</button></span>` : ""}</div>${gs.slice(p * PAGE, p * PAGE + PAGE).map((i) => row(i, n++)).join("")}</div>`; }).join("") + (order.length > 60 ? `<div class="k">and ${order.length - 60} more groups: filter to see them</div>` : "") : `<div class="empty">No issue matches.</div>`}`;
  };
  const detail = () => { const i = all.find((x) => x.id === ui.open); if (!i) return `<div class="empty">No issue ${esc(ui.open)}.</div>`;
    return `<div class="crumbs"><a onclick="globalThis.__hqIssues.close()">Issues</a><span class="k">›</span><span>${esc(i.id)}</span></div><h2 class="is-dt">${esc(i.title)}</h2>
      <div class="is-props card"><dl><dt>id</dt><dd><code>${esc(i.id)}</code></dd><dt>state</dt><dd>${st(i.state)} ${esc(i.state)}</dd><dt>priority</dt><dd>${pr(i.priority)} ${PRIO[i.priority] ?? ""}</dd><dt>owner</dt><dd>${esc(i.owner ?? "—")}</dd><dt>project</dt><dd>${esc(i.project ?? "—")}</dd><dt>labels</dt><dd>${(i.labels ?? []).length ? i.labels.map((l) => `<span class="chip">${esc(l)}</span>`).join(" ") : "—"}</dd><dt>created</dt><dd>${esc(i.created ?? "—")}</dd><dt>updated</dt><dd>${esc(i.updated ?? "—")}</dd>${i.url ? `<dt>link</dt><dd><a href="${esc(i.url)}" target="_blank" rel="noopener">${esc(i.url)} ↗</a></dd>` : ""}</dl></div>
      <div class="k" style="margin-top:8px">from ${esc(is.source ?? "the company's tracker")} · Esc to go back</div>`; };
  const draw = () => {
    if (ui.view === "detail") return detail();
    const pill = (s) => `<button class="pill${ui.states.has(s) ? " on" : ""}" onclick="globalThis.__hqIssues.toggleState('${s}')">${st(s)} ${s} <span class="k">${is.states?.[s] ?? 0}</span></button>`;
    const sel = (k, label, opts) => `<select class="pill" onchange="globalThis.__hqIssues.set('${k}',this.value)"><option value="">${label}</option>${opts.map(([v, t]) => `<option value="${esc(v)}"${String(ui[k]) === String(v) ? " selected" : ""}>${esc(t)}</option>`).join("")}</select>`;
    return `<div class="filters"><button class="pill${ui.view === "list" ? " on" : ""}" onclick="globalThis.__hqIssues.set('view','list')" title="l">List</button><button class="pill${ui.view === "board" ? " on" : ""}" onclick="globalThis.__hqIssues.set('view','board')" title="b">Board</button><span class="sep"></span>${STATES.map(pill).join("")}</div>
      <div class="filters">${sel("owner", "Any owner", owners.map((o) => [o, o]))}${sel("project", "Any project", projects.slice(0, 400).map((o) => [o, o]))}${sel("prio", "Any priority", PRIO.map((p, i) => [String(i), p]))}${ui.view === "list" ? sel("group", "No grouping", [["state", "Group by state"], ["owner", "Group by owner"], ["project", "Group by project"]]).replace('<option value="">No grouping</option>', `<option value="none"${ui.group === "none" ? " selected" : ""}>No grouping</option>`) : ""}</div>
      <input id="is-q" class="search" placeholder="search issues ( / )" value="${esc(ui.q)}" oninput="globalThis.__hqIssues.q(this.value)">
      <div id="is-results">${results()}</div><div class="k" style="margin-top:8px">keys: j/k move · Enter opens · Esc back · / search · l list · b board</div>`;
  };
  const s = is.states ?? {};
  return `${STYLE}<h1>Issues</h1><div class="sub1">${is.total.toLocaleString()} issues from ${esc(is.source ?? "the company's tracker")} · ${(s.todo ?? 0) + (s["in progress"] ?? 0) + (s.review ?? 0)} active · ${(s.backlog ?? 0).toLocaleString()} in the backlog</div><div id="is-root">${draw()}</div>`;
}
const STYLE = `<style>
.is-row{display:grid;grid-template-columns:18px auto 12px minmax(0,1fr) auto;gap:8px;align-items:center;padding:6px 8px;border-bottom:1px solid var(--line);cursor:pointer;color:var(--text);text-decoration:none}.is-row:hover,.is-sel{background:color-mix(in srgb,var(--accent) 10%,transparent)}
.is-t{overflow:hidden;text-overflow:ellipsis;white-space:nowrap}.is-meta{display:flex;gap:8px;align-items:center;white-space:nowrap}.is-id{font-size:12px;color:var(--muted)}
.is-group{background:var(--panel);border:1px solid var(--line);border-radius:10px;margin-bottom:10px;overflow:hidden}.is-gh{display:flex;gap:8px;align-items:center;padding:8px 10px;background:var(--side);border-bottom:1px solid var(--line)}.is-pg{margin-left:auto;display:flex;gap:6px;align-items:center}
.is-st{display:inline-block;width:11px;height:11px;border-radius:50%;border:2px solid var(--muted);flex:none}.is-backlog{border-style:dashed}.is-todo{border-color:var(--muted)}.is-in-progress{border-color:var(--warn);background:linear-gradient(90deg,var(--warn) 50%,transparent 50%)}.is-review{border-color:var(--accent);background:linear-gradient(90deg,var(--accent) 75%,transparent 75%)}.is-done{border-color:var(--good);background:var(--good)}.is-canceled{border-color:var(--muted);background:var(--muted);opacity:.6}
.is-p{display:inline-block;width:16px;text-align:center;color:var(--muted);font-size:12px}.is-p1{color:var(--bad)}.is-p2{color:var(--warn)}
.is-board{display:grid;grid-auto-flow:column;grid-auto-columns:minmax(240px,1fr);gap:10px;overflow-x:auto;padding-bottom:6px}.is-col{background:var(--side);border:1px solid var(--line);border-radius:10px;padding:8px;display:flex;flex-direction:column;gap:6px;min-width:0}.is-colh{display:flex;gap:6px;align-items:center}
.is-card{display:block;background:var(--panel);border:1px solid var(--line);border-radius:8px;padding:8px;cursor:pointer;color:var(--text);text-decoration:none;overflow-wrap:anywhere}.is-ct{margin:4px 0}
.is-props dl{display:grid;grid-template-columns:110px minmax(0,1fr);gap:6px 12px;margin:0}.is-props dd{margin:0;overflow-wrap:anywhere}.is-dt{overflow-wrap:anywhere}
@media (max-width:700px){.is-row{grid-template-columns:18px auto 12px minmax(0,1fr)}.is-meta{display:none}.is-board{grid-auto-columns:minmax(220px,85%)}}
</style>`;
