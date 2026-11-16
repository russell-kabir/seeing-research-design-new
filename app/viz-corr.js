/* Seeing Research Design — Correlation & R² tab and Research clinic.
   R computes; this file draws. Uses the helpers exported by viz.js as ERL.u. */
(function () {
  "use strict";
  const ERL = (window.ERL = window.ERL || {});

  function whenReady(cb) {
    if (window.Shiny && Shiny.addCustomMessageHandler && ERL.u) cb();
    else setTimeout(() => whenReady(cb), 40);
  }

  whenReady(() => {
    const { svgIn, layer, tr, xAxis, yAxis, jit, T, Td } = ERL.u;
    const GCOL = ["var(--treat)", "var(--accent)", "var(--violet)", "var(--gold)", "var(--ctrl)", "#3b7dd8"];
    const last = {};
    const reg = (name, fn) => {
      Shiny.addCustomMessageHandler(name, (m) => { last[name] = m; fn(m); });
      ERL.onLang(name, () => { if (last[name]) fn(last[name]); });
    };
    const fmtN = d3.format("~s");
    const tickFmt = (dom) => (Math.abs(dom[1] - dom[0]) >= 1000 ? d3.format("~s") : d3.format("~g"));
    const z = (v) => { const m = d3.mean(v), s = d3.deviation(v) || 1; return v.map((x) => (x - m) / s); };

    // =================================================================
    // 1. Spurious correlations
    // =================================================================
    function spLines(pl) {
      const W = 680, H = 290, m = { t: 46, r: 64, b: 30, l: 64 };
      const svg = svgIn("sp-lines", W, H);
      const iw = W - m.l - m.r, ih = H - m.t - m.b;
      const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
      const x = d3.scaleLinear().domain(d3.extent(pl.years)).range([0, iw]);
      const ya = d3.scaleLinear().domain(d3.extent(pl.a)).nice().range([ih, 0]);
      const yb = d3.scaleLinear().domain(d3.extent(pl.b)).nice().range([ih, 0]);
      xAxis(layer(g, "xax").attr("transform", `translate(0,${ih})`), x, { values: pl.years.length > 12 ? pl.years.filter((y, i) => i % 2 === 0) : pl.years, fmt: d3.format("d") });
      const la = layer(g, "yaxa");
      la.call(d3.axisLeft(ya).ticks(5).tickFormat(tickFmt(ya.domain()))).classed("ax", true).select(".domain").remove();
      la.selectAll("text").style("fill", "var(--treat)");
      const lb = layer(g, "yaxb").attr("transform", `translate(${iw},0)`);
      lb.call(d3.axisRight(yb).ticks(5).tickFormat(tickFmt(yb.domain()))).classed("ax", true).select(".domain").remove();
      lb.selectAll("text").style("fill", "var(--accent)");
      const series = [
        { k: "a", vals: pl.a, y: ya, cls: "treat", lab: pl.alab, col: "var(--treat)" },
        { k: "b", vals: pl.b, y: yb, cls: "accent", lab: pl.blab, col: "var(--accent)" },
      ];
      series.forEach((s) => {
        const L = layer(g, "ser-" + s.k);
        let path = L.select("path");
        if (path.empty()) path = L.append("path").attr("fill", "none").attr("stroke-width", 2.5).style("stroke", s.col);
        tr(path, 500).attr("d", d3.line().x((d, i) => x(pl.years[i])).y((d) => s.y(d))(s.vals));
        const dots = L.selectAll("circle").data(s.vals);
        dots.exit().remove();
        dots.enter().append("circle").attr("r", 4.2).merge(dots)
          .style("fill", (d, i) => (pl.keep[i] ? s.col : "var(--paper)")).style("stroke", s.col).attr("stroke-width", 1.6)
          .call((sel) => tr(sel, 500).attr("cx", (d, i) => x(pl.years[i])).attr("cy", (d) => s.y(d)));
      });
      // legend
      const lg = layer(g, "legend").selectAll("g").data(series);
      const le = lg.enter().append("g");
      le.append("rect").attr("width", 12).attr("height", 3).attr("y", -4);
      le.append("text").attr("class", "lbl").attr("x", 16);
      const lm = le.merge(lg).attr("transform", (d, i) => `translate(0,${i ? -12 : -30})`);
      lm.select("rect").style("fill", (d) => d.col).attr("x", 0);
      lm.select("text").attr("text-anchor", "start").attr("x", 16)
        .style("fill", (d) => d.col)
        .each(function (d) { const [e, j] = ERL.u.split(d.lab); T(d3.select(this), e + (d.k === "a" ? " (left axis)" : " (right axis)"), j + (d.k === "a" ? "（左の軸）" : "（右の軸）")); });
      // dropped-year note
      const drop = pl.years.filter((y, i) => !pl.keep[i]);
      const dn = layer(g, "dropnote").selectAll("text").data(drop);
      dn.exit().remove();
      dn.enter().append("text").attr("class", "lbl muted-lbl").attr("text-anchor", "middle").merge(dn)
        .attr("x", (d) => x(d)).attr("y", ih - 6).call((s) => Td(s, () => "left out ｜ 除外"));
    }

    function spScatter(pl) {
      const W = 680, H = 250, m = { t: 14, r: 20, b: 40, l: 64 };
      const svg = svgIn("sp-scatter", W, H);
      const iw = W - m.l - m.r, ih = H - m.t - m.b;
      const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
      const pts = pl.sx.map((v, i) => ({ x: v, y: pl.sy[i], yr: pl.syears[i] }));
      const x = d3.scaleLinear().domain(d3.extent(pl.sx)).nice().range([0, iw]);
      const y = d3.scaleLinear().domain(d3.extent(pl.sy)).nice().range([ih, 0]);
      yAxis(layer(g, "yax"), y, iw, { grid: true, fmt: tickFmt(y.domain()) });
      xAxis(layer(g, "xax").attr("transform", `translate(0,${ih})`), x, { fmt: tickFmt(x.domain()), ticks: 6 });
      const ch = pl.mode === "changes";
      const xl = layer(g, "xt").selectAll("text").data([0]);
      xl.enter().append("text").attr("class", "ax-title").attr("text-anchor", "middle").merge(xl).attr("x", iw / 2).attr("y", ih + 34)
        .call((s) => T(s, (ch ? "Change in: " : "") + ERL.u.split(pl.alab)[0].replace(/ \(.*/, ""), (ch ? "変化：" : "") + ERL.u.split(pl.alab)[1].replace(/（.*/, "")));
      const yl = layer(g, "yt").selectAll("text").data([0]);
      yl.enter().append("text").attr("class", "ax-title").attr("text-anchor", "middle").merge(yl).attr("transform", `translate(-50,${ih / 2}) rotate(-90)`)
        .call((s) => T(s, (ch ? "Change in: " : "") + ERL.u.split(pl.blab)[0].replace(/ \(.*/, ""), (ch ? "変化：" : "") + ERL.u.split(pl.blab)[1].replace(/（.*/, "")));
      const fit = layer(g, "fit");
      let ln = fit.select("line");
      if (ln.empty()) ln = fit.append("line").attr("class", "stroke-accent").attr("stroke-width", 2).attr("stroke-dasharray", "6 4");
      if (pl.b1 != null && isFinite(pl.b1)) {
        const [x0, x1] = x.domain();
        tr(ln, 500).style("opacity", 1).attr("x1", x(x0)).attr("x2", x(x1)).attr("y1", y(pl.b0 + pl.b1 * x0)).attr("y2", y(pl.b0 + pl.b1 * x1));
      } else ln.style("opacity", 0);
      const ds = layer(g, "dots").selectAll("g.pt").data(pts, (d) => d.yr);
      ds.exit().remove();
      const de = ds.enter().append("g").attr("class", "pt");
      de.append("circle").attr("r", 5).attr("class", "fill-ink").attr("opacity", 0.75);
      de.append("text").attr("class", "lbl-mono").attr("dx", 7).attr("dy", 3).style("font-size", "10px");
      const dm = de.merge(ds);
      dm.select("text").text((d) => d.yr);
      tr(dm, 500).attr("transform", (d) => `translate(${x(d.x)},${y(d.y)})`);
    }

    function spDredge(pl) {
      const dr = pl.dredge;
      if (!dr || !dr.hist) return;
      const W = 680, H = 200, m = { t: 14, r: 16, b: 34, l: 40 };
      const svg = svgIn("sp-dredge", W, H);
      const half = (W - m.l - m.r) / 2 - 20;
      const ih = H - m.t - m.b;
      const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
      // histogram of r
      const x = d3.scaleLinear().domain([-1, 1]).range([0, half]);
      const ymax = d3.max(dr.hist) || 1;
      const y = d3.scaleLinear().domain([0, ymax]).nice().range([ih, 0]);
      const hg = layer(g, "h");
      xAxis(layer(hg, "xax").attr("transform", `translate(0,${ih})`), x, { values: [-1, -0.5, 0, 0.5, 1] });
      yAxis(layer(hg, "yax"), y, half, { ticks: 4 });
      const bw = half / dr.hist.length;
      const bars = layer(hg, "bars").selectAll("rect").data(dr.hist);
      bars.enter().append("rect").attr("y", ih).attr("height", 0).merge(bars)
        .attr("x", (d, i) => i * bw + 0.5).attr("width", bw - 1)
        .attr("class", (d, i) => (Math.abs(-1 + (i + 0.5) * 0.1 / 2) >= Math.abs(dr.robs) ? "fill-accent" : "fill-ctrl"))
        .call((s) => tr(s, 500).attr("y", (d) => y(d)).attr("height", (d) => ih - y(d)));
      const ob = layer(hg, "obs");
      if (ob.select("line").empty()) { ob.append("line").attr("class", "stroke-ink").attr("stroke-width", 2); ob.append("text").attr("class", "lbl").attr("text-anchor", "middle"); }
      ob.select("line").attr("x1", x(dr.robs)).attr("x2", x(dr.robs)).attr("y1", -4).attr("y2", ih);
      T(ob.select("text").attr("x", x(dr.robs)).attr("y", -4 + 0), "real r = " + dr.robs.toFixed(2), "実際の r = " + dr.robs.toFixed(2));
      const xt = layer(hg, "xt").selectAll("text").data([0]);
      xt.enter().append("text").attr("class", "ax-title").attr("text-anchor", "middle").merge(xt).attr("x", half / 2).attr("y", ih + 30)
        .call((s) => T(s, "r with a random series", "ランダムなデータとの r"));
      // best match line chart (standardized)
      const lg = layer(g, "best").attr("transform", `translate(${half + 50},0)`);
      const yrs = dr.years;
      const xs = d3.scaleLinear().domain(d3.extent(yrs)).range([0, half]);
      const za = z(pl.sx), zb = z(dr.best);
      const ys = d3.scaleLinear().domain(d3.extent(za.concat(zb))).nice().range([ih, 0]);
      xAxis(layer(lg, "xax").attr("transform", `translate(0,${ih})`), xs, { values: yrs.filter((v, i) => i % Math.ceil(yrs.length / 5) === 0), fmt: d3.format("d") });
      [[za, "var(--treat)", "a"], [zb, "var(--accent)", "b"]].forEach(([v, col, k]) => {
        let p = lg.select("path." + k);
        if (p.empty()) p = lg.append("path").attr("class", k).attr("fill", "none").attr("stroke-width", 2.2);
        tr(p.style("stroke", col), 500).attr("d", d3.line().x((d, i) => xs(yrs[i])).y((d) => ys(d))(v));
      });
      const bt = layer(lg, "bt").selectAll("text").data([0]);
      bt.enter().append("text").attr("class", "lbl").attr("x", 0).attr("y", -2).merge(bt)
        .call((s) => T(s, `best random match: r = ${dr.rbest.toFixed(2)} (both standardized)`, `いちばん一致したランダムなデータ：r = ${dr.rbest.toFixed(2)}（どちらも標準化）`));
    }

    reg("erl_sp", (pl) => { spLines(pl); spScatter(pl); spDredge(pl); });

    // =================================================================
    // 2. ENRO scatter, two-circle Venn, correlation matrix
    // =================================================================
    function lensArea(R, d) {
      if (d >= 2 * R) return 0;
      if (d <= 0) return Math.PI * R * R;
      return 2 * R * R * Math.acos(d / (2 * R)) - (d / 2) * Math.sqrt(4 * R * R - d * d);
    }
    // distance between two equal circles (radius R) whose overlap is a share `f` of one circle
    function distFor(R, f) {
      f = Math.max(0, Math.min(1, f));
      const target = f * Math.PI * R * R;
      let lo = 0, hi = 2 * R;
      for (let k = 0; k < 50; k++) { const mid = (lo + hi) / 2; if (lensArea(R, mid) > target) lo = mid; else hi = mid; }
      return (lo + hi) / 2;
    }

    function enScatter(pl) {
      const W = 680, H = 330, m = { t: 14, r: 18, b: 42, l: 56 };
      const svg = svgIn("en-scatter", W, H);
      const iw = W - m.l - m.r, ih = H - m.t - m.b;
      const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
      const xs = pl.pts.map((p) => p[0]), ys = pl.pts.map((p) => p[1]);
      const x = d3.scaleLinear().domain(d3.extent(xs)).nice().range([0, iw]);
      const y = d3.scaleLinear().domain(d3.extent(ys)).nice().range([ih, 0]);
      yAxis(layer(g, "yax"), y, iw, { grid: true, fmt: d3.format("~g") });
      xAxis(layer(g, "xax").attr("transform", `translate(0,${ih})`), x, { fmt: d3.format("~g") });
      const xt = layer(g, "xt").selectAll("text").data([0]);
      xt.enter().append("text").attr("class", "ax-title").attr("text-anchor", "middle").merge(xt).attr("x", iw / 2).attr("y", ih + 36).call((s) => T(s, pl.xlab));
      const yt = layer(g, "yt").selectAll("text").data([0]);
      yt.enter().append("text").attr("class", "ax-title").attr("text-anchor", "middle").merge(yt).attr("transform", `translate(-44,${ih / 2}) rotate(-90)`)
        .call((s) => T(s, ERL.u.split(pl.ylab)[0].replace(/ \(.*/, ""), ERL.u.split(pl.ylab)[1].replace(/（.*/, "")));
      // jitter discrete values
      const uniq = (v) => new Set(v).size;
      const jx = pl.jitter && uniq(xs) < 40 ? (x.invert(7) - x.invert(0)) : 0;
      const jy = pl.jitter && uniq(ys) < 40 ? (y.invert(0) - y.invert(7)) : 0;
      const pts = pl.pts.map((p, i) => ({ i, x: p[0], y: p[1], g: p[2], px: x(p[0] + jit(i, 1) * jx), py: y(p[1] + jit(i, 2) * jy) }));
      // means
      const M = layer(g, "means");
      if (M.select(".mx").empty()) { M.append("line").attr("class", "mx stroke-ink").attr("stroke-dasharray", "2 4").attr("opacity", 0.4); M.append("line").attr("class", "my stroke-ink").attr("stroke-dasharray", "2 4").attr("opacity", 0.4); }
      tr(M.select(".mx"), 500).attr("x1", x(pl.mx)).attr("x2", x(pl.mx)).attr("y1", 0).attr("y2", ih);
      tr(M.select(".my"), 500).attr("x1", 0).attr("x2", iw).attr("y1", y(pl.my)).attr("y2", y(pl.my));
      // residuals
      const fitY = (d) => (pl.grouped ? pl.gfits[d.g].b0 + pl.gfits[d.g].b1 * d.x : pl.b0 + pl.b1 * d.x);
      const rs = layer(g, "resid").selectAll("line").data(pl.resid && pl.line ? pts : [], (d) => d.i);
      rs.exit().remove();
      rs.enter().append("line").attr("stroke-width", 1).attr("opacity", 0.5).merge(rs)
        .style("stroke", (d) => (pl.grouped ? GCOL[d.g % 6] : "var(--accent)"))
        .attr("x1", (d) => d.px).attr("x2", (d) => d.px).attr("y1", (d) => d.py).attr("y2", (d) => y(fitY(d)));
      // dots
      const ds = layer(g, "dots").selectAll("circle").data(pts, (d) => d.i);
      ds.exit().transition().duration(300).attr("r", 0).remove();
      ds.enter().append("circle").attr("r", 0).attr("cx", (d) => d.px).attr("cy", (d) => d.py).attr("opacity", 0.7).merge(ds)
        .style("fill", (d) => (pl.grouped ? GCOL[d.g % 6] : "var(--treat)"))
        .call((s) => tr(s, 550).attr("r", 4).attr("cx", (d) => d.px).attr("cy", (d) => d.py));
      // fit lines
      const [x0, x1] = x.domain();
      const fits = !pl.line ? [] : pl.grouped ? pl.gfits.map((f, k) => ({ k, b0: f.b0, b1: f.b1, col: GCOL[k % 6] })) : [{ k: 0, b0: pl.b0, b1: pl.b1, col: "var(--accent)" }];
      const fl = layer(g, "fits").selectAll("line").data(fits.filter((f) => f.b1 != null && isFinite(f.b1)), (d) => d.k);
      fl.exit().remove();
      fl.enter().append("line").attr("stroke-width", 3).attr("x1", x(x0)).attr("x2", x(x1)).attr("y1", y(pl.my)).attr("y2", y(pl.my)).merge(fl)
        .style("stroke", (d) => d.col)
        .call((s) => tr(s, 550).attr("x1", x(x0)).attr("x2", x(x1)).attr("y1", (d) => y(d.b0 + d.b1 * x0)).attr("y2", (d) => y(d.b0 + d.b1 * x1)));
      // group legend
      const lg = layer(g, "glegend").selectAll("g").data(pl.grouped ? pl.lev : []);
      lg.exit().remove();
      const le = lg.enter().append("g");
      le.append("circle").attr("r", 5);
      le.append("text").attr("class", "lbl").attr("x", 9).attr("dy", 4);
      const lm = le.merge(lg).attr("transform", (d, i) => `translate(${8 + i * 110},6)`);
      lm.select("circle").style("fill", (d, i) => GCOL[i % 6]);
      lm.select("text").text((d) => d);
    }

    function enVenn(pl) {
      const W = 320, H = 205;
      const svg = svgIn("en-venn", W, H);
      const R = 62, f = pl.r == null ? 0 : pl.r * pl.r, d = distFor(R, f);
      const cx = W / 2, cy = 88;
      const g = layer(svg, "inner");
      const c = [{ k: "x", x: cx - d / 2, lab: pl.xlab, col: "var(--treat)" }, { k: "y", x: cx + d / 2, lab: pl.ylab, col: "var(--accent)" }];
      const cs = g.selectAll("circle.vc").data(c, (q) => q.k);
      cs.enter().append("circle").attr("class", "vc").attr("r", R).attr("cy", cy).attr("cx", cx).attr("stroke-width", 2).merge(cs)
        .style("fill", (q) => q.col).style("fill-opacity", 0.22).style("stroke", (q) => q.col)
        .call((s) => tr(s, 550).attr("cx", (q) => q.x));
      const tl = g.selectAll("text.vl").data(c, (q) => q.k);
      tl.enter().append("text").attr("class", "vl lbl").attr("text-anchor", "middle").merge(tl).attr("y", (q) => cy + R + (q.k === "x" ? 20 : 38))
        .style("fill", (q) => q.col).call((s) => Td(s, (q) => ERL.u.split(q.lab).map((t) => t.replace(/ \(.*/, "").replace(/（.*/, "")).join(" ｜ ")))
        .call((s) => tr(s, 550).attr("x", (q) => q.x));
      let ov = g.select("text.ov");
      if (ov.empty()) ov = g.append("text").attr("class", "ov lbl-strong").attr("text-anchor", "middle");
      ov.attr("x", cx).attr("y", cy + 5).text(`r² = ${Math.round(100 * f)}%`);
    }

    function enMatrix(pl) {
      const n = pl.nv.length;
      const W = 360, cell = Math.min(22, 230 / n), lw = 112;
      const H = lw + n * cell + 6;
      const svg = svgIn("en-matrix", W, H);
      const g = layer(svg, "inner").attr("transform", `translate(${lw},${lw - 4})`);
      const col = d3.scaleDiverging(d3.interpolateRdBu).domain([1, 0, -1]);
      const cells = [];
      pl.M.forEach((row, i) => row.forEach((v, j) => cells.push({ i, j, v })));
      const cs = g.selectAll("rect").data(cells, (d) => d.i + "-" + d.j);
      cs.exit().remove();
      cs.enter().append("rect").attr("rx", 2).style("cursor", "pointer")
        .on("click", (ev, d) => { if (d.i !== d.j) Shiny.setInputValue("en_pick", { x: pl.nv[d.j], y: pl.nv[d.i], t: Date.now() }, { priority: "event" }); })
        .append("title");
      g.selectAll("rect").data(cells, (d) => d.i + "-" + d.j)
        .attr("x", (d) => d.j * cell).attr("y", (d) => d.i * cell).attr("width", cell - 1.5).attr("height", cell - 1.5)
        .style("fill", (d) => (d.v == null ? "var(--paper-2)" : col(d.v)))
        .attr("stroke", (d) => ((d.i === pl.sel[1] && d.j === pl.sel[0]) || (d.i === pl.sel[0] && d.j === pl.sel[1]) ? "var(--ink)" : "none")).attr("stroke-width", 2)
        .select("title").text((d) => `${ERL.t(pl.nvlab[d.i])} × ${ERL.t(pl.nvlab[d.j])}: r = ${d.v == null ? "—" : d.v.toFixed(2)}`);
      const rl = g.selectAll("text.rl").data(pl.nvlab);
      rl.exit().remove();
      rl.enter().append("text").attr("class", "rl lbl").attr("text-anchor", "end").style("font-size", "9.5px").merge(rl)
        .attr("x", -4).attr("y", (d, i) => i * cell + cell * 0.65).call((s) => Td(s, (d) => d));
      const cl = g.selectAll("text.cl").data(pl.nvlab);
      cl.exit().remove();
      cl.enter().append("text").attr("class", "cl lbl").attr("text-anchor", "start").style("font-size", "9.5px").merge(cl)
        .attr("transform", (d, i) => `translate(${i * cell + cell * 0.65},-4) rotate(-55)`).call((s) => Td(s, (d) => d));
    }

    reg("erl_en", (pl) => { enScatter(pl); enVenn(pl); enMatrix(pl); });

    // =================================================================
    // 3. Three-circle Euler diagram (after Heiss, 2021)
    // =================================================================
    const VCOL = { A: "var(--ctrl)", D: "var(--treat)", E: "var(--gold)", G: "var(--violet)", F: "var(--ctrl)", B: "var(--ctrl)", C: "var(--ctrl)" };
    const VOP = { A: 0.18, D: 0.75, E: 0.75, G: 0.8, F: 0.12, B: 0.06, C: 0.06 };
    function vnEuler(pl) {
      const W = 680, H = 360;
      const svg = svgIn("vn-euler", W, H);
      const R = 92;
      const show = pl.show;
      const use1 = show !== "x2", use2 = show !== "x1";
      const dY1 = distFor(R, pl.ry1 * pl.ry1), dY2 = distFor(R, pl.ry2 * pl.ry2), d12 = distFor(R, pl.r12 * pl.r12);
      // place Y at the origin, X1 on the x-axis, X2 by the law of cosines
      let P = { Y: [0, 0], X1: [dY1, 0], X2: [0, 0] };
      if (dY1 > 1e-6) {
        const px = (dY1 * dY1 + dY2 * dY2 - d12 * d12) / (2 * dY1);
        P.X2 = [px, Math.sqrt(Math.max(0, dY2 * dY2 - px * px))];
      } else P.X2 = [dY2, 0];
      if (!use2) P.X2 = [P.X1[0], P.X1[1]];
      if (!use1) P.X1 = [P.X2[0], P.X2[1]];
      // rotate so Y sits on top and the predictors below
      const ctr = use1 && use2 ? [(P.X1[0] + P.X2[0]) / 2, (P.X1[1] + P.X2[1]) / 2] : use1 ? P.X1 : P.X2;
      const ang = Math.atan2(P.Y[1] - ctr[1], P.Y[0] - ctr[0]);
      const rot = -Math.PI / 2 - ang;
      const rp = (p) => [Math.cos(rot) * p[0] - Math.sin(rot) * p[1], Math.sin(rot) * p[0] + Math.cos(rot) * p[1]];
      Object.keys(P).forEach((k) => { P[k] = rp(P[k]); });
      if (use1 && use2 && P.X1[0] > P.X2[0]) Object.keys(P).forEach((k) => { P[k] = [-P[k][0], P[k][1]]; });
      if (!use1 && use2 && P.X2[0] < P.Y[0]) Object.keys(P).forEach((k) => { P[k] = [-P[k][0], P[k][1]]; });
      if (use1 && !use2 && P.X1[0] > P.Y[0]) Object.keys(P).forEach((k) => { P[k] = [-P[k][0], P[k][1]]; });
      const ks = ["Y"].concat(use1 ? ["X1"] : []).concat(use2 ? ["X2"] : []);
      const minx = d3.min(ks, (k) => P[k][0]) - R, maxx = d3.max(ks, (k) => P[k][0]) + R;
      const miny = d3.min(ks, (k) => P[k][1]) - R, maxy = d3.max(ks, (k) => P[k][1]) + R;
      const sc = Math.min(1, (W - 40) / (maxx - minx), (H - 60) / (maxy - miny));
      const ox = W / 2 - sc * (minx + maxx) / 2, oy = (H - 20) / 2 - sc * (miny + maxy) / 2 + 4;
      const C = {};
      ["Y", "X1", "X2"].forEach((k) => { C[k] = { x: ox + sc * P[k][0], y: oy + sc * P[k][1], r: k === "Y" || (k === "X1" ? use1 : use2) ? R * sc : 0 }; });

      // defs: one clipPath and one "outside" mask per circle
      let defs = svg.select("defs");
      if (defs.empty()) {
        defs = svg.append("defs");
        ["Y", "X1", "X2"].forEach((k) => {
          defs.append("clipPath").attr("id", "vclip-" + k).append("circle");
          const mk = defs.append("mask").attr("id", "vout-" + k);
          mk.append("rect").attr("x", 0).attr("y", 0).attr("width", W).attr("height", H).attr("fill", "white");
          mk.append("circle").attr("fill", "black");
        });
      }
      ["Y", "X1", "X2"].forEach((k) => {
        tr(defs.select("#vclip-" + k + " circle"), 550).attr("cx", C[k].x).attr("cy", C[k].y).attr("r", C[k].r);
        tr(defs.select("#vout-" + k + " circle"), 550).attr("cx", C[k].x).attr("cy", C[k].y).attr("r", C[k].r);
      });
      // regions: [name, inside, outside]
      const regions = [["A", ["Y"], ["X1", "X2"]], ["D", ["Y", "X1"], ["X2"]], ["E", ["Y", "X2"], ["X1"]], ["G", ["Y", "X1", "X2"], []],
        ["F", ["X1", "X2"], ["Y"]], ["B", ["X1"], ["Y", "X2"]], ["C", ["X2"], ["Y", "X1"]]];
      const rg = layer(svg, "regions");
      if (rg.select("g.reg").empty()) {
        regions.forEach(([name, ins, outs]) => {
          let node = rg.append("g").attr("class", "reg reg-" + name);
          ins.forEach((k) => { node = node.append("g").attr("clip-path", `url(#vclip-${k})`); });
          outs.forEach((k) => { node = node.append("g").attr("mask", `url(#vout-${k})`); });
          node.append("rect").attr("x", 0).attr("y", 0).attr("width", W).attr("height", H).style("fill", VCOL[name]).style("fill-opacity", VOP[name]);
        });
      }
      // outlines
      const oc = layer(svg, "outlines").selectAll("circle").data(["Y", "X1", "X2"]);
      oc.enter().append("circle").attr("fill", "none").attr("stroke-width", 2).merge(oc)
        .style("stroke", (k) => (k === "Y" ? "var(--ink)" : "var(--ink-2)"))
        .call((s) => tr(s, 550).attr("cx", (k) => C[k].x).attr("cy", (k) => C[k].y).attr("r", (k) => C[k].r));
      // names
      const names = { Y: pl.ylab, X1: pl.x1lab, X2: pl.x2lab };
      const nm = layer(svg, "names").selectAll("text").data(["Y", "X1", "X2"]);
      nm.enter().append("text").attr("class", "lbl-strong").attr("text-anchor", "middle").merge(nm)
        .call((s) => Td(s, (k) => names[k]))
        .call((s) => tr(s, 550).style("opacity", (k) => (C[k].r > 0 ? 1 : 0))
          .attr("x", (k) => (k === "Y" ? C[k].x : k === "X1" ? C[k].x - C[k].r * 0.35 : C[k].x + C[k].r * 0.35))
          .attr("y", (k) => (k === "Y" ? C[k].y - C[k].r - 8 : C[k].y + C[k].r + 18)));
      // percentage labels at region centroids (grid sampling)
      const inside = (k, px, py) => C[k].r > 0 && (px - C[k].x) ** 2 + (py - C[k].y) ** 2 <= C[k].r ** 2;
      const vals = show === "both" ? { A: pl.A, D: pl.D, E: pl.E, G: pl.G } : show === "x1" ? { A: 1 - pl.ry1 * pl.ry1, D: pl.ry1 * pl.ry1 } : { A: 1 - pl.ry2 * pl.ry2, E: pl.ry2 * pl.ry2 };
      const labs = [];
      regions.forEach(([name, ins, outs]) => {
        if (!(name in vals)) return;
        let sx = 0, sy = 0, n = 0;
        for (let px = 0; px < W; px += 4) for (let py = 0; py < H; py += 4) {
          if (ins.every((k) => inside(k, px, py)) && outs.every((k) => !inside(k, px, py))) { sx += px; sy += py; n++; }
        }
        if (n > 3 && vals[name] > 0.005) labs.push({ name, x: sx / n, y: sy / n, v: vals[name] });
      });
      const pl2 = layer(svg, "pcts").selectAll("text").data(labs, (d) => d.name);
      pl2.exit().remove();
      pl2.enter().append("text").attr("class", "lbl-strong vn-pct").attr("text-anchor", "middle").attr("x", (d) => d.x).attr("y", (d) => d.y).merge(pl2)
        .text((d) => Math.round(100 * d.v) + "%")
        .call((s) => tr(s, 550).attr("x", (d) => d.x).attr("y", (d) => d.y + 4));
      // same-variable warning
      let wt = svg.select("text.vwarn");
      if (wt.empty()) wt = svg.append("text").attr("class", "vwarn lbl fill-accent").attr("x", W / 2).attr("y", H - 6).attr("text-anchor", "middle");
      if (pl.same) T(wt, "Pick three different variables.", "3つの違う変数を選んでください。"); else wt.text("");
    }

    function vnBar(pl) {
      const W = 680, H = 104, m = { t: 10, r: 10, b: 34, l: 10 };
      const svg = svgIn("vn-bar", W, H);
      const iw = W - m.l - m.r;
      const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
      const show = pl.show;
      const parts = show === "both"
        ? [["D", Math.max(0, pl.D), "unique to predictor 1 ｜ 説明変数1に固有"], ["G", Math.max(0, pl.G), "shared ｜ 共有"], ["E", Math.max(0, pl.E), "unique to predictor 2 ｜ 説明変数2に固有"], ["A", Math.max(0, pl.A), "unexplained ｜ 説明されない"]]
        : show === "x1" ? [["D", pl.ry1 * pl.ry1, "predictor 1 ｜ 説明変数1"], ["A", 1 - pl.ry1 * pl.ry1, "unexplained ｜ 説明されない"]]
          : [["E", pl.ry2 * pl.ry2, "predictor 2 ｜ 説明変数2"], ["A", 1 - pl.ry2 * pl.ry2, "unexplained ｜ 説明されない"]];
      const tot = d3.sum(parts, (p) => p[1]) || 1;
      let acc = 0;
      const segs = parts.map(([k, v, lab]) => { const s = { k, v, lab, x0: acc / tot }; acc += v; s.x1 = acc / tot; return s; });
      const x = d3.scaleLinear().domain([0, 1]).range([0, iw]);
      const bs = g.selectAll("rect.seg").data(segs, (d) => d.k);
      bs.exit().transition().duration(300).attr("width", 0).remove();
      bs.enter().append("rect").attr("class", "seg").attr("y", 0).attr("height", 30).attr("x", (d) => x(d.x0)).attr("width", 0).merge(bs)
        .style("fill", (d) => VCOL[d.k]).style("fill-opacity", (d) => (d.k === "A" ? 0.3 : 0.85))
        .call((s) => tr(s, 550).attr("x", (d) => x(d.x0)).attr("width", (d) => Math.max(0, x(d.x1) - x(d.x0) - 1)));
      const ts = g.selectAll("text.sv").data(segs, (d) => d.k);
      ts.exit().remove();
      ts.enter().append("text").attr("class", "sv lbl-strong").attr("text-anchor", "middle").attr("y", 20).merge(ts)
        .text((d) => (d.v >= 0.04 ? Math.round(100 * d.v) + "%" : ""))
        .call((s) => tr(s, 550).attr("x", (d) => x((d.x0 + d.x1) / 2)));
      const tl = g.selectAll("text.sl").data(segs, (d) => d.k);
      tl.exit().remove();
      tl.enter().append("text").attr("class", "sl lbl").attr("text-anchor", "middle").merge(tl).attr("y", (d, i) => 46 + (i % 2) * 16)
        .call((s) => Td(s, (d) => (d.v >= 0.08 ? d.lab : "")))
        .call((s) => tr(s, 550).attr("x", (d) => x((d.x0 + d.x1) / 2)));
    }

    reg("erl_vn", (pl) => { vnEuler(pl); vnBar(pl); });

    // =================================================================
    // Research clinic
    // =================================================================
    ERL.clientId = function () {
      try {
        let id = localStorage.getItem("srd-client");
        if (!id) { id = Math.random().toString(36).slice(2) + Date.now().toString(36); localStorage.setItem("srd-client", id); }
        return id;
      } catch (e) { return "anon-" + Math.random().toString(36).slice(2); }
    };
    Shiny.addCustomMessageHandler("erl_clinic_copy", (m) => { const t = document.getElementById("cl_copy_text"); if (t) t.value = m.text; });
    ERL.copyClinic = function (b) {
      const t = document.getElementById("cl_copy_text");
      if (!t) return;
      const done = () => { b.dataset.orig = b.dataset.orig || b.innerHTML; b.textContent = ERL.t("Copied ✓", "コピーしました ✓"); setTimeout(() => { b.innerHTML = b.dataset.orig; }, 1800); };
      const fallback = () => { t.classList.add("show"); t.focus(); t.select(); try { document.execCommand("copy"); done(); } catch (e) {} t.classList.remove("show"); };
      if (navigator.clipboard) navigator.clipboard.writeText(t.value).then(done, fallback); else fallback();
    };
    Shiny.addCustomMessageHandler("erl_clinic_post", async (m) => {
      try {
        const body = JSON.stringify({ action: "post", code: m.code, client: ERL.clientId(), row: m.row });
        const res = await fetch(m.url, { method: "POST", body, headers: { "Content-Type": "text/plain;charset=utf-8" } });
        const j = await res.json();
        Shiny.setInputValue("clinic_post_result", j, { priority: "event" });
      } catch (e) {
        Shiny.setInputValue("clinic_post_result", { ok: false, error: String(e) }, { priority: "event" });
      }
    });
    Shiny.addCustomMessageHandler("erl_clinic_list", async (m) => {
      try {
        const res = await fetch(m.url + "?action=list&code=" + encodeURIComponent(m.code));
        const j = await res.json();
        Shiny.setInputValue("clinic_board", j, { priority: "event" });
      } catch (e) {
        Shiny.setInputValue("clinic_board", { ok: false, rows: [], error: String(e) }, { priority: "event" });
      }
    });

    function clinicBoard(pl) {
      const rows = pl.rows;
      const both = ERL.lang === "both";
      const W = 960, rowH = 24, m = { t: 48, r: 40, b: 10, l: both ? 480 : ERL.lang === "ja" ? 240 : 290 };
      const H = m.t + m.b + rows.length * rowH;
      const svg = svgIn("cl-board-chart", W, H);
      const iw = W - m.l - m.r;
      const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
      const mx = Math.max(3, d3.max(rows, (r) => r.help + r.unsure + r.ok) || 0);
      const x = d3.scaleLinear().domain([0, mx]).range([0, iw]);
      const cols = ["var(--accent)", "var(--gold)", "var(--treat)"], keys = ["help", "unsure", "ok"];
      const rg = g.selectAll("g.br").data(rows, (d) => d.id);
      rg.exit().remove();
      const re = rg.enter().append("g").attr("class", "br");
      re.append("text").attr("class", "lbl").attr("text-anchor", "end").attr("x", -8).attr("y", rowH / 2 + 4);
      keys.forEach((k) => re.append("rect").attr("class", "b-" + k).attr("y", 3).attr("height", rowH - 7).attr("rx", 3));
      re.append("text").attr("class", "lbl-mono tot").attr("y", rowH / 2 + 4);
      const rm = re.merge(rg);
      tr(rm, 500).attr("transform", (d, i) => `translate(0,${i * rowH})`);
      rm.select("text.lbl").call((s) => Td(s, (d) => d.lab));
      keys.forEach((k, ki) => {
        rm.select("rect.b-" + k).style("fill", cols[ki]).style("opacity", k === "ok" ? 0.45 : 0.85)
          .call((s) => tr(s, 500).attr("x", (d) => x(keys.slice(0, ki).reduce((a, kk) => a + d[kk], 0))).attr("width", (d) => Math.max(0, x(d[k]) - (d[k] ? 1 : 0))));
      });
      rm.select("text.tot").text((d) => (d.help + d.unsure + d.ok ? d.help + d.unsure + d.ok : ""))
        .call((s) => tr(s, 500).attr("x", (d) => x(d.help + d.unsure + d.ok) + 6));
      const lg = layer(svg, "lg").selectAll("g").data(pl.keys);
      const le = lg.enter().append("g");
      le.append("rect").attr("width", 12).attr("height", 12).attr("rx", 2).attr("y", -10);
      le.append("text").attr("class", "lbl").attr("x", 16);
      const lm = le.merge(lg).attr("transform", (d, i) => `translate(${m.l + i * (both ? 190 : 120)},34)`);
      lm.select("rect").style("fill", (d, i) => cols[i]).style("opacity", (d, i) => (i === 2 ? 0.45 : 0.85));
      lm.select("text").call((s) => Td(s, (d) => d));
      let nt = svg.select("text.nposts");
      if (nt.empty()) nt = svg.append("text").attr("class", "nposts lbl-strong").attr("text-anchor", "start").attr("x", 0).attr("y", 14);
      T(nt, `${pl.n} post${pl.n === 1 ? "" : "s"}`, `投稿 ${pl.n} 件`);
    }
    reg("erl_clinic_board", clinicBoard);
  });
})();
