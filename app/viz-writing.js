/* Seeing Research Design — Writing development and AUT tabs.
   R computes; this file draws, handles AUT coding keys, and runs the in-browser
   semantic-distance model (transformers.js, loaded only when asked). */
(function () {
  "use strict";
  const ERL = (window.ERL = window.ERL || {});

  function whenReady(cb) {
    if (window.Shiny && Shiny.addCustomMessageHandler && ERL.u) cb();
    else setTimeout(() => whenReady(cb), 40);
  }

  // ---- AUT coding: buttons and keyboard (defined early so buttons work at once) ----
  ERL.autCode = function (field, value, extra) {
    if (!window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue("aut_code", Object.assign({ field: field, value: String(value), t: Date.now() }, extra || {}), { priority: "event" });
  };
  const CAT_KEYS = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "q", "w", "e", "r", "t", "y"];
  document.addEventListener("keydown", (ev) => {
    const host = document.getElementById("aut-coder");
    if (!host || host.offsetParent === null) return;              // coding pane not visible
    const t = ev.target;
    if (t && (t.closest("input, textarea, select, [contenteditable]"))) return;
    if (ev.ctrlKey || ev.metaKey || ev.altKey) return;
    const k = ev.key.length === 1 ? ev.key.toLowerCase() : ev.key;
    let hit = true;
    const ci = CAT_KEYS.indexOf(k);
    if (ci >= 0) ERL.autCode("cat", ci + 1);
    else if (k === "i") ERL.autCode("valid", 1);
    else if (k === "a" || k === "s" || k === "d") ERL.autCode("elab", "asd".indexOf(k));
    else if (k === "j" || k === "k" || k === "l") ERL.autCode("orig", "jkl".indexOf(k));
    else if (k === "Enter" || k === "ArrowRight") ERL.autCode("nav", "next");
    else if (k === "ArrowLeft") ERL.autCode("nav", "prev");
    else if (k === "n") ERL.autCode("nav", "uncoded");
    else if (k === "Backspace" || k === "Delete") ERL.autCode("clear", 1);
    else hit = false;
    if (hit) ev.preventDefault();
  });

  whenReady(() => {
    const { svgIn, layer, yAxis, jit, T, Td, split } = ERL.u;
    const last = {};
    const reg = (name, fn) => {
      Shiny.addCustomMessageHandler(name, (m) => { last[name + (m.target || "")] = m; fn(m); });
      ERL.onLang(name, () => Object.keys(last).filter((k) => k.indexOf(name) === 0).forEach((k) => fn(last[k])));
    };

    // ---- dot plot: one dot per text / participant, bar = group mean ----
    reg("erl_wr_dots", (pl) => {
      const host = document.getElementById(pl.target);
      if (!host) return;
      const G = pl.groups || [];
      const rot = G.length > 4 || d3.max(G, (d) => ERL.t(d).length) * 6.5 > 600 / Math.max(1, G.length);
      const longest = d3.max(G, (d) => ERL.t(d).length) || 4;
      const W = 680, ih = 240, m = { t: 22, r: 16, b: rot ? Math.min(150, 24 + longest * 6.2 * 0.5) : 40, l: 58 };
      const H = ih + m.t + m.b;
      const svg = svgIn(pl.target, W, H);
      const iw = W - m.l - m.r;
      const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
      const vals = pl.pts.map((p) => p.v).filter((v) => v != null && isFinite(v));
      g.selectAll("*").remove();
      if (!vals.length) {
        g.append("text").attr("class", "lbl").attr("x", iw / 2).attr("y", ih / 2).attr("text-anchor", "middle")
          .call((s) => T(s, "Nothing to show yet", "まだ表示するものがありません"));
        return;
      }
      let [lo, hi] = d3.extent(vals);
      if (lo === hi) { lo -= 1; hi += 1; }
      const pad = (hi - lo) * 0.08;
      const y = d3.scaleLinear().domain([Math.min(0, lo) === 0 && lo >= 0 && lo < (hi - lo) ? 0 : lo - pad, hi + pad]).nice().range([ih, 0]);
      const x = d3.scaleBand().domain(d3.range(G.length)).range([0, iw]).padding(0.25);
      yAxis(g.append("g").attr("class", "grid"), y, iw, { grid: true, ticks: 5 });
      const bw = x.bandwidth();
      const xa = g.append("g").attr("class", "ax").attr("transform", `translate(0,${ih})`);
      xa.selectAll("text").data(G).join("text").attr("class", "lbl").attr("text-anchor", rot ? "end" : "middle")
        .attr("transform", (d, i) => rot ? `translate(${x(i) + bw / 2},14) rotate(-28)` : `translate(${x(i) + bw / 2},18)`)
        .call((s) => Td(s, (d) => d));
      g.append("text").attr("class", "ax-title").attr("transform", `translate(${-44},${ih / 2}) rotate(-90)`).attr("text-anchor", "middle")
        .call((s) => T(s, pl.ylab));
      const pts = pl.pts.filter((p) => p.v != null && isFinite(p.v));
      g.append("g").selectAll("circle").data(pts).join("circle")
        .attr("cx", (p, i) => x(p.g) + bw / 2 + jit(i, 3) * bw * 0.7).attr("cy", (p) => y(p.v)).attr("r", pts.length > 300 ? 2.4 : 4)
        .attr("class", "dot-treat").style("opacity", 0.75)
        .append("title").text((p) => `${p.id}: ${p.v}`);
      g.append("g").selectAll("line").data(pl.means.map((v, i) => ({ v, i })).filter((d) => d.v != null && isFinite(d.v))).join("line")
        .attr("x1", (d) => x(d.i) + 2).attr("x2", (d) => x(d.i) + bw - 2).attr("y1", (d) => y(d.v)).attr("y2", (d) => y(d.v))
        .attr("class", "stroke-accent").style("stroke-width", 3);
      g.append("g").selectAll("text").data(pl.means.map((v, i) => ({ v, i })).filter((d) => d.v != null && isFinite(d.v))).join("text")
        .attr("class", "lbl-mono").attr("x", (d) => x(d.i) + bw - 2).attr("y", (d) => y(d.v) - 6).attr("text-anchor", "end")
        .style("font-size", "11px").text((d) => d3.format(".3~f")(d.v));
    });

    // ---- causal connectives per 100 words: horizontal stacked bars per group ----
    reg("erl_wr_causal", (pl) => {
      const G = pl.groups || [], C = pl.cats || [];
      const both = ERL.lang === "both";
      const rowH = 30, W = 680, m = { t: 34, r: 30, b: 30, l: both ? 200 : 150 };
      const H = m.t + m.b + Math.max(1, G.length) * rowH;
      const svg = svgIn(pl.target, W, H);
      const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
      g.selectAll("*").remove();
      const tot = pl.vals.map((v) => d3.sum(v));
      const x = d3.scaleLinear().domain([0, pl.unit === "%" ? 100 : Math.max(1e-9, d3.max(tot)) * 1.08]).nice().range([0, W - m.l - m.r]);
      const y = d3.scaleBand().domain(d3.range(G.length)).range([0, G.length * rowH]).padding(0.22);
      g.append("g").attr("class", "ax").attr("transform", `translate(0,${G.length * rowH})`).call(d3.axisBottom(x).ticks(6)).select(".domain").remove();
      g.selectAll("text.glab").data(G).join("text").attr("class", "lbl glab").attr("x", -8).attr("y", (d, i) => y(i) + y.bandwidth() / 2 + 4)
        .attr("text-anchor", "end").call((s) => Td(s, (d) => d));
      G.forEach((grp, gi) => {
        let acc = 0;
        C.forEach((c, ci) => {
          const v = pl.vals[gi][ci] || 0;
          g.append("rect").attr("x", x(acc)).attr("y", y(gi)).attr("width", Math.max(0, x(acc + v) - x(acc))).attr("height", y.bandwidth())
            .style("fill", c.col).style("opacity", 0.85).append("title").text(`${ERL.t(c.lab)}: ${d3.format(".2f")(v)}${pl.unit || ""}`);
          if (pl.unit === "%" && v >= 6) g.append("text").attr("class", "lbl-mono").attr("x", x(acc + v / 2)).attr("y", y(gi) + y.bandwidth() / 2 + 4)
            .attr("text-anchor", "middle").style("font-size", "10px").style("fill", "#fff").style("stroke", "none").style("paint-order", "normal").style("pointer-events", "none").text(d3.format(".0f")(v));
          acc += v;
        });
        if (pl.unit !== "%") g.append("text").attr("class", "lbl-mono").attr("x", x(acc) + 5).attr("y", y(gi) + y.bandwidth() / 2 + 4).style("font-size", "11px").text(d3.format(".2f")(acc));
      });
      // legend (widths estimated from label length; wraps onto more rows when needed)
      const maxW = W - m.l - m.r, pos = [];
      let lx = 0, row = 0;
      C.forEach((c) => {
        const w = 24 + Array.from(ERL.t(c.lab)).reduce((a, ch) => a + (ch.charCodeAt(0) > 0x2e80 ? 11 : 6.2), 0);
        if (lx > 0 && lx + w > maxW) { lx = 0; row += 1; }
        pos.push([lx, row]); lx += w;
      });
      const extra = row * 16;
      svg.attr("viewBox", `0 0 ${W} ${H + extra}`);
      g.attr("transform", `translate(${m.l},${m.t + extra})`);
      const lg = g.append("g").attr("transform", `translate(0,${-22 - extra})`);
      C.forEach((c, i) => {
        const it = lg.append("g").attr("transform", `translate(${pos[i][0]},${pos[i][1] * 16})`);
        it.append("rect").attr("width", 10).attr("height", 10).attr("y", -9).style("fill", c.col);
        it.append("text").attr("class", "lbl").attr("x", 14).style("font-size", "11px").call((s) => T(s, c.lab));
      });
    });

    // ---- in-browser semantic distance (transformers.js) ----
    let extractor = null, loadedModel = null;
    const TJS = "https://cdn.jsdelivr.net/npm/@huggingface/transformers@3";
    Shiny.addCustomMessageHandler("erl_aut_sd", async (m) => {
      const report = (o) => Shiny.setInputValue("aut_sd_status", Object.assign({ t: Date.now() }, o), { priority: "event" });
      try {
        report({ stage: "load", msg: "Loading the model library…" });
        const lib = await import(/* webpackIgnore: true */ TJS);
        lib.env.allowLocalModels = false;
        if (!extractor || loadedModel !== m.model) {
          const seen = {};
          extractor = await lib.pipeline("feature-extraction", m.model, {
            dtype: "q8",
            progress_callback: (p) => {
              if (p.status === "progress" && p.file && /onnx|safetensors|bin/.test(p.file)) {
                const pct = Math.floor(p.progress || 0);
                if (seen[p.file] !== pct) { seen[p.file] = pct; report({ stage: "download", file: p.file, pct }); }
              }
            }
          });
          loadedModel = m.model;
        }
        const embed = async (texts) => (await extractor(texts, { pooling: "mean", normalize: true })).tolist();
        const items = Array.from(new Set(m.items));
        const iv = await embed(items);
        const itemVec = {};
        items.forEach((it, i) => { itemVec[it] = iv[i]; });
        const n = m.responses.length, B = 16, dist = new Array(n).fill(null);
        for (let i = 0; i < n; i += B) {
          const chunk = m.responses.slice(i, i + B);
          const v = await embed(chunk);
          v.forEach((vec, j) => {
            const a = itemVec[m.items[i + j]];
            let dot = 0;
            for (let k = 0; k < a.length; k++) dot += a[k] * vec[k];
            dist[i + j] = 1 - dot;
          });
          report({ stage: "embed", done: Math.min(i + B, n), total: n });
        }
        Shiny.setInputValue("aut_sd_result", { row_id: m.row_id, dist: dist, model: m.model, t: Date.now() }, { priority: "event" });
      } catch (e) {
        console.error(e);
        report({ stage: "error", msg: "Could not compute semantic distance: " + String((e && e.message) || e) });
      }
    });
  });
})();
