/* Seeing Research Design — D3 rendering layer.
   Python (Shiny) computes; this file draws and animates. */
(function () {
  "use strict";
  const ERL = (window.ERL = window.ERL || {});
  const DUR = 700;
  const ease = d3.easeCubicInOut;
  const tr = (sel, ms) => sel.transition().duration(ms == null ? DUR : ms).ease(ease);
  const fmt1 = d3.format("+.1f");

  function whenShiny(cb) {
    if (window.Shiny && Shiny.addCustomMessageHandler) cb();
    else setTimeout(() => whenShiny(cb), 40);
  }

  // Create (once) an SVG with a fixed viewBox that scales with its container.
  function svgIn(id, W, H) {
    const host = d3.select("#" + id);
    let svg = host.select("svg.chart");
    if (svg.empty()) {
      svg = host.append("svg").attr("class", "chart").attr("preserveAspectRatio", "xMidYMid meet");
    }
    svg.attr("viewBox", `0 0 ${W} ${H}`);
    return svg;
  }
  function layer(svg, name) {
    let g = svg.select("g." + name);
    if (g.empty()) g = svg.append("g").attr("class", name);
    return g;
  }
  // deterministic pseudo-random jitter in [-.5, .5]
  function jit(i, salt) {
    const x = Math.sin((i + 1) * 12.9898 + (salt || 0) * 78.233) * 43758.5453;
    return x - Math.floor(x) - 0.5;
  }
  function yAxis(g, y, W, opts) {
    opts = opts || {};
    g.classed("ax", true);
    g.interrupt().selectAll("*").interrupt();
    g.call(d3.axisLeft(y).ticks(opts.ticks || 5).tickSize(opts.grid ? -W : 4).tickFormat(opts.fmt || null));
    g.selectAll(".tick").style("opacity", 1);
    g.selectAll(".tick line").attr("stroke-dasharray", opts.grid ? "2 3" : null);
    g.select(".domain").remove();
  }
  function xAxis(g, x, opts) {
    opts = opts || {};
    g.classed("ax", true);
    const ax = d3.axisBottom(x).ticks(opts.ticks || 7).tickSizeOuter(0);
    if (opts.fmt) ax.tickFormat(opts.fmt);
    if (opts.values) ax.tickValues(opts.values);
    g.interrupt().selectAll("*").interrupt();
    g.call(ax);
    g.selectAll(".tick").style("opacity", 1);
  }

  // =====================================================================
  // DESIGNS
  // =====================================================================
  const D = { W: 680, H: 340, m: { t: 18, r: 24, b: 38, l: 50 } };
  let designKind = null;

  function designChart(msg) {
    const pl = msg.payload;
    const svg = svgIn("design-chart", D.W, D.H);
    const kind = pl.kind;
    if (kind !== designKind) {
      // crossfade: drop the old layer(s)
      svg.selectAll("g.k-layer").filter(function () { return this.dataset.kind !== kind; })
        .classed("k-layer", false).transition().duration(300).style("opacity", 0).remove();
      designKind = kind;
    }
    let g = svg.select(`g.k-layer[data-kind="${kind}"]`);
    if (g.empty()) {
      g = svg.append("g").attr("class", "k-layer").attr("data-kind", kind).style("opacity", 0);
      tr(g, 400).style("opacity", 1);
    }
    ({ dots: drawDots, its: drawITS, rdd: drawRDD, factorial: drawFactorial, race: drawRace, ladder: drawLadder })[kind](g, pl);
  }

  function yDomainFrom(vals, extra) {
    const all = vals.concat(extra || []);
    let lo = d3.min(all), hi = d3.max(all);
    lo = Math.min(25, Math.floor((lo - 3) / 10) * 10);
    hi = Math.max(85, Math.ceil((hi + 3) / 10) * 10);
    return [lo, hi];
  }

  function bracket(g, x, y0, y1, label, side) {
    side = side || 1;
    const b = layer(g, "bracket-g");
    const w = 8 * side;
    const path = `M${x},${y0} h${w} V${y1} h${-w}`;
    let p = b.select("path");
    if (p.empty()) p = b.append("path").attr("class", "bracket").attr("d", path);
    tr(p).attr("d", path);
    let t = b.select("text");
    if (t.empty()) t = b.append("text").attr("class", "lbl-mono").attr("x", x + w + 6 * side).attr("y", (y0 + y1) / 2);
    t.attr("text-anchor", side > 0 ? "start" : "end").attr("dominant-baseline", "middle").text(label);
    tr(t).attr("x", x + w + 6 * side).attr("y", (y0 + y1) / 2);
  }

  function drawDots(g, pl) {
    const { W, H, m } = D;
    const iw = W - m.l - m.r, ih = H - m.t - m.b;
    const inner = layer(g, "inner").attr("transform", `translate(${m.l},${m.t})`);
    const allY = [];
    pl.groups.forEach((gr) => gr.pts.forEach((p) => allY.push(p[1])));
    const y = d3.scaleLinear().domain(yDomainFrom(allY, [pl.ghost.y])).range([ih, 0]);
    const ncol = pl.cols.length;
    const x = d3.scalePoint().domain(pl.cols).range(ncol === 1 ? [iw * 0.5, iw * 0.5] : [iw * 0.24, iw * 0.7]);
    const G = pl.groups.length;
    const spread = ncol === 1 ? 120 : 64;
    const offset = (gi) => (G === 1 ? 0 : (gi - (G - 1) / 2) * (ncol === 1 ? 150 : 70));

    yAxis(layer(inner, "yax"), y, iw, { grid: true });
    let yt = inner.select("text.ytitle");
    if (yt.empty()) yt = inner.append("text").attr("class", "ax-title ytitle").attr("transform", `translate(-38,${ih / 2}) rotate(-90)`).attr("text-anchor", "middle").text("Test score");

    // column labels
    const cl = layer(inner, "cols").selectAll("text").data(pl.cols, (d) => d);
    cl.exit().transition().duration(300).style("opacity", 0).remove();
    cl.enter().append("text").attr("class", "lbl-strong").attr("text-anchor", "middle").attr("y", ih + 28).style("opacity", 0)
      .attr("x", (d) => x(d)).merge(cl).call((s) => tr(s).style("opacity", 1).attr("x", (d) => x(d)));

    // reference line (d1)
    const ref = layer(inner, "ref").selectAll("g").data(pl.refline ? [pl.refline] : []);
    ref.exit().remove();
    const re = ref.enter().append("g");
    re.append("line").attr("stroke", "currentColor").attr("class", "stroke-ink").attr("stroke-dasharray", "2 4").attr("opacity", 0.6);
    re.append("text").attr("class", "lbl").attr("text-anchor", "end");
    re.merge(ref).select("line").attr("x1", 0).attr("x2", iw).attr("y1", (d) => y(d.y)).attr("y2", (d) => y(d.y));
    re.merge(ref).select("text").attr("x", iw).attr("y", (d) => y(d.y) - 6).text((d) => d.label);

    // dots
    const dots = [];
    pl.groups.forEach((gr, gi) => {
      gr.pts.forEach((p) => {
        dots.push({ id: `${gr.cls}-${p[2]}-${p[0]}`, cls: gr.cls, cx: x(pl.cols[p[0]]) + offset(gi) + jit(p[2], gi) * spread * 0.5, cy: y(p[1]) });
      });
    });
    const ds = layer(inner, "dots").selectAll("circle").data(dots, (d) => d.id);
    ds.exit().transition().duration(350).attr("r", 0).remove();
    ds.enter().append("circle").attr("r", 0).attr("cx", (d) => d.cx).attr("cy", ih)
      .attr("class", (d) => "dot-" + d.cls).attr("opacity", 0.55)
      .merge(ds).call((s) => tr(s).attr("r", 3.6).attr("cx", (d) => d.cx).attr("cy", (d) => d.cy).delay((d, i) => (i % 40) * 4));

    // group means + slope lines
    const ms = [];
    pl.groups.forEach((gr, gi) => {
      const pts = gr.means.map((mv, ci) => [x(pl.cols[ci]) + offset(gi), y(mv)]);
      ms.push({ id: gr.cls, cls: gr.cls, name: gr.name, pts });
    });
    const line = d3.line();
    const ml = layer(inner, "means").selectAll("g.mean").data(ms, (d) => d.id);
    ml.exit().transition().duration(300).style("opacity", 0).remove();
    const me = ml.enter().append("g").attr("class", "mean").style("opacity", 0);
    me.append("path").attr("fill", "none").attr("stroke-width", 2.5);
    me.append("g").attr("class", "bars");
    me.append("text").attr("class", "lbl").attr("font-weight", 600);
    const mm = me.merge(ml);
    tr(mm).style("opacity", 1);
    mm.select("path").attr("class", (d) => "stroke-" + d.cls).call((s) => tr(s).attr("d", (d) => (d.pts.length > 1 ? line(d.pts) : null)));
    mm.each(function (d) {
      const bars = d3.select(this).select("g.bars").selectAll("line").data(d.pts);
      bars.exit().remove();
      bars.enter().append("line").attr("stroke-width", 4).attr("stroke-linecap", "round")
        .attr("x1", (p) => p[0] - 26).attr("x2", (p) => p[0] + 26).attr("y1", (p) => p[1]).attr("y2", (p) => p[1])
        .merge(bars).attr("class", "stroke-" + d.cls)
        .call((s) => tr(s).attr("x1", (p) => p[0] - 26).attr("x2", (p) => p[0] + 26).attr("y1", (p) => p[1]).attr("y2", (p) => p[1]));
    });
    mm.select("text").attr("class", (d) => "lbl fill-" + d.cls).attr("text-anchor", (d) => (d.cls === "ctrl" ? "end" : "start"))
      .text((d) => d.name).call((s) => tr(s)
        .attr("x", (d) => (d.cls === "ctrl" ? d.pts[0][0] - 32 : d.pts[d.pts.length - 1][0] + 32))
        .attr("y", (d) => (d.cls === "ctrl" ? d.pts[0][1] : d.pts[d.pts.length - 1][1]) + 4));

    // ghost counterfactual
    const gh = pl.ghost;
    const treat = pl.groups.find((gg) => gg.cls === "treat");
    const ti = pl.groups.indexOf(treat);
    const gx = x(pl.cols[gh.c]) + offset(ti);
    const gdata = [{ gx, gy: y(gh.y), fx: gh.fromc != null ? x(pl.cols[gh.fromc]) + offset(ti) : null, fy: gh.fromy != null ? y(gh.fromy) : null }];
    const ghost = layer(inner, "ghost-g").selectAll("g").data(gdata);
    const ge = ghost.enter().append("g");
    ge.append("path").attr("class", "ghost").attr("stroke-width", 2);
    ge.append("circle").attr("class", "ghost-dot").attr("r", 6);
    ge.append("text").attr("class", "lbl fill-accent").attr("text-anchor", "middle");
    const gm = ge.merge(ghost);
    gm.select("path").call((s) => tr(s).attr("d", (d) => (d.fx != null ? `M${d.fx},${d.fy} L${d.gx},${d.gy}` : `M${d.gx - 30},${d.gy} L${d.gx + 30},${d.gy}`)));
    gm.select("circle").call((s) => tr(s).attr("cx", (d) => d.gx).attr("cy", (d) => d.gy));
    gm.select("text").text("without intervention").call((s) => tr(s).attr("x", (d) => d.gx).attr("y", (d) => d.gy + (pl.est >= 0 ? 22 : -14)));

    // estimate bracket (what the design compares)
    const tPost = treat.means[treat.means.length - 1];
    const ctrl = pl.groups.find((gg) => gg.cls === "ctrl");
    const cmpY = tPost - pl.est;
    const lab = "estimate " + fmt1(pl.est);
    const bx = gx + 46;
    bracket(inner, bx, y(cmpY), y(tPost), lab);
    let cmpLine = inner.select("line.cmp");
    if (cmpLine.empty()) cmpLine = inner.append("line").attr("class", "cmp stroke-ink").attr("stroke-dasharray", "1 3").attr("opacity", 0.7);
    tr(cmpLine).attr("x1", gx - 30).attr("x2", bx).attr("y1", y(cmpY)).attr("y2", y(cmpY));
  }

  function drawITS(g, pl) {
    const { W, H, m } = D;
    const iw = W - m.l - m.r - 60, ih = H - m.t - m.b;
    const inner = layer(g, "inner").attr("transform", `translate(${m.l},${m.t})`);
    const flat = pl.students.flat().concat(pl.means);
    const y = d3.scaleLinear().domain(yDomainFrom(flat)).range([ih, 0]);
    const x = d3.scaleLinear().domain([0.6, 6.4]).range([0, iw]);
    yAxis(layer(inner, "yax"), y, iw, { grid: true });
    xAxis(layer(inner, "xax").attr("transform", `translate(0,${ih})`), x, { values: [1, 2, 3, 4, 5, 6], fmt: (d) => "M" + d });
    const iv = layer(inner, "iv");
    if (iv.select("rect").empty()) {
      iv.append("rect").attr("class", "fill-treat-soft");
      iv.append("text").attr("class", "lbl fill-treat").attr("text-anchor", "middle").text("intervention");
    }
    iv.select("rect").attr("x", x(3.5)).attr("width", x(6.4) - x(3.5)).attr("y", 0).attr("height", ih).attr("opacity", 0.5);
    iv.select("text").attr("x", x(4.95)).attr("y", 14);
    const line = d3.line().x((d, i) => x(i + 1)).y((d) => y(d));
    const st = layer(inner, "students").selectAll("path").data(pl.students);
    st.exit().remove();
    st.enter().append("path").attr("fill", "none").attr("class", "stroke-ctrl").attr("opacity", 0.35).attr("stroke-width", 1)
      .merge(st).call((s) => tr(s).attr("d", line));
    const pre = layer(inner, "fit");
    const lp = d3.line().x((d) => x(d[0])).y((d) => y(d[1]));
    let p1 = pre.select("path.pre"), p2 = pre.select("path.cf"), p3 = pre.select("path.post");
    if (p1.empty()) {
      p2 = pre.append("path").attr("class", "cf ghost").attr("stroke-width", 2);
      p1 = pre.append("path").attr("class", "pre stroke-ink").attr("stroke-width", 2).attr("fill", "none");
      p3 = pre.append("path").attr("class", "post stroke-treat").attr("stroke-width", 2.5).attr("fill", "none");
      pre.append("text").attr("class", "cflab lbl fill-accent");
    }
    tr(p1).attr("d", lp([pl.pre_line[0], [3.5, pl.pre_line[0][1] + (pl.pre_line[1][1] - pl.pre_line[0][1]) * 2.5 / 5]]));
    tr(p2).attr("d", lp([[3.5, pl.pre_line[0][1] + (pl.pre_line[1][1] - pl.pre_line[0][1]) * 2.5 / 5], pl.pre_line[1]]));
    tr(p3).attr("d", lp(pl.post_line));
    tr(pre.select("text.cflab")).attr("x", x(6) + 8).attr("y", y(pl.pre_line[1][1]) + 4).text("projected trend");
    const mp = layer(inner, "mpts").selectAll("circle").data(pl.means);
    mp.enter().append("circle").attr("r", 5.5).attr("cx", (d, i) => x(i + 1)).attr("cy", ih)
      .merge(mp).attr("class", (d, i) => (i >= 3 ? "fill-treat" : "fill-ink"))
      .call((s) => tr(s).attr("cx", (d, i) => x(i + 1)).attr("cy", (d) => y(d)));
    bracket(inner, x(4) - 14, y(pl.jump.y0), y(pl.jump.y1), "level change " + fmt1(pl.jump.y1 - pl.jump.y0), -1);
    let yt = inner.select("text.ytitle");
    if (yt.empty()) inner.append("text").attr("class", "ax-title ytitle").attr("transform", `translate(-38,${ih / 2}) rotate(-90)`).attr("text-anchor", "middle").text("Test score (class mean, black/teal)");
  }

  function drawRDD(g, pl) {
    const { W, H, m } = D;
    const iw = W - m.l - m.r, ih = H - m.t - m.b;
    const inner = layer(g, "inner").attr("transform", `translate(${m.l},${m.t})`);
    const xs = pl.pts.map((p) => p[0]), ys = pl.pts.map((p) => p[1]);
    const x = d3.scaleLinear().domain([Math.min(20, d3.min(xs) - 2), Math.max(80, d3.max(xs) + 2)]).range([0, iw]);
    const y = d3.scaleLinear().domain(yDomainFrom(ys)).range([ih, 0]);
    yAxis(layer(inner, "yax"), y, iw, { grid: true });
    xAxis(layer(inner, "xax").attr("transform", `translate(0,${ih})`), x);
    let xt = inner.select("text.xtitle");
    if (xt.empty()) {
      inner.append("text").attr("class", "ax-title xtitle").attr("x", iw).attr("y", ih + 32).attr("text-anchor", "end").text("Diagnostic pretest score →");
      inner.append("text").attr("class", "ax-title ytitle").attr("transform", `translate(-38,${ih / 2}) rotate(-90)`).attr("text-anchor", "middle").text("Posttest score");
    }
    const cut = layer(inner, "cut");
    if (cut.select("line").empty()) {
      cut.append("rect").attr("class", "fill-treat-soft").attr("opacity", 0.45);
      cut.append("line").attr("class", "stroke-accent").attr("stroke-width", 1.5).attr("stroke-dasharray", "4 3");
      cut.append("text").attr("class", "lbl fill-accent").attr("text-anchor", "middle").text("cutoff");
      cut.append("text").attr("class", "lbl fill-treat sup").attr("text-anchor", "start").text("← extra support");
    }
    cut.select("rect").attr("x", 0).attr("width", x(pl.cut)).attr("y", 0).attr("height", ih);
    cut.select("line").attr("x1", x(pl.cut)).attr("x2", x(pl.cut)).attr("y1", 0).attr("y2", ih);
    cut.select("text").attr("x", x(pl.cut)).attr("y", -4);
    cut.select("text.sup").attr("x", 6).attr("y", 14);
    const ds = layer(inner, "dots").selectAll("circle").data(pl.pts, (d, i) => i);
    ds.exit().transition().duration(300).attr("r", 0).remove();
    ds.enter().append("circle").attr("r", 0).attr("cx", (d) => x(d[0])).attr("cy", (d) => y(d[1]))
      .merge(ds).attr("class", (d) => (d[2] ? "dot-treat" : "dot-ctrl")).attr("opacity", 0.55)
      .call((s) => tr(s).attr("r", 3.2).attr("cx", (d) => x(d[0])).attr("cy", (d) => y(d[1])));
    const lp = d3.line().x((d) => x(d[0])).y((d) => y(d[1]));
    const fl = layer(inner, "fits");
    let a = fl.select("path.t"), b = fl.select("path.c");
    if (a.empty()) {
      a = fl.append("path").attr("class", "t stroke-treat").attr("stroke-width", 3).attr("fill", "none");
      b = fl.append("path").attr("class", "c stroke-ink").attr("stroke-width", 3).attr("fill", "none");
    }
    tr(a).attr("d", lp(pl.tline));
    tr(b).attr("d", lp(pl.cline));
    bracket(inner, x(pl.cut) + 6, y(pl.jump.y0), y(pl.jump.y1), "jump " + fmt1(pl.jump.y1 - pl.jump.y0));
  }

  function drawFactorial(g, pl) {
    const { W, H, m } = D;
    const iw = W - m.l - m.r - 110, ih = H - m.t - m.b;
    const inner = layer(g, "inner").attr("transform", `translate(${m.l},${m.t})`);
    const all = pl.cells.flatMap((c) => c.pts);
    const y = d3.scaleLinear().domain(yDomainFrom(all)).range([ih, 0]);
    const x = d3.scalePoint().domain(pl.alab).range([iw * 0.18, iw * 0.82]);
    yAxis(layer(inner, "yax"), y, iw, { grid: true });
    const cl = layer(inner, "cols").selectAll("text").data(pl.alab);
    cl.enter().append("text").attr("class", "lbl-strong").attr("text-anchor", "middle").attr("y", ih + 28).merge(cl).attr("x", (d) => x(d)).text((d) => d);
    const off = (b) => (b ? 16 : -16);
    const pts = [];
    pl.cells.forEach((c) => c.pts.forEach((v, i) => pts.push({ id: `${c.a}${c.b}-${i}`, b: c.b, cx: x(pl.alab[c.a]) + off(c.b) + jit(i, c.a * 2 + c.b) * 40, cy: y(v) })));
    const ds = layer(inner, "dots").selectAll("circle").data(pts, (d) => d.id);
    ds.exit().transition().duration(300).attr("r", 0).remove();
    ds.enter().append("circle").attr("r", 0).attr("cx", (d) => d.cx).attr("cy", ih)
      .merge(ds).attr("class", (d) => (d.b ? "dot-treat" : "dot-ctrl")).attr("opacity", 0.3)
      .call((s) => tr(s).attr("r", 2.8).attr("cx", (d) => d.cx).attr("cy", (d) => d.cy));
    const lines = [0, 1].map((b) => ({ b, pts: [0, 1].map((a) => { const c = pl.cells.find((cc) => cc.a === a && cc.b === b); return [x(pl.alab[a]) + off(b), y(c.mean)]; }) }));
    const ln = layer(inner, "ilines").selectAll("g").data(lines, (d) => d.b);
    const le = ln.enter().append("g");
    le.append("path").attr("stroke-width", 3).attr("fill", "none");
    le.append("text").attr("class", "lbl").attr("font-weight", 600);
    const lm = le.merge(ln);
    lm.select("path").attr("class", (d) => (d.b ? "stroke-treat" : "stroke-ctrl")).call((s) => tr(s).attr("d", (d) => d3.line()(d.pts)));
    lm.select("text").attr("class", (d) => "lbl " + (d.b ? "fill-treat" : "fill-ctrl")).text((d) => pl.blab[d.b])
      .call((s) => tr(s).attr("x", (d) => d.pts[1][0] + 14).attr("y", (d) => d.pts[1][1] + 4));
    const mp = layer(inner, "mp").selectAll("circle").data(lines.flatMap((l) => l.pts.map((p) => ({ p, b: l.b }))));
    mp.enter().append("circle").attr("r", 6).merge(mp).attr("class", (d) => (d.b ? "fill-treat" : "fill-ctrl"))
      .call((s) => tr(s).attr("cx", (d) => d.p[0]).attr("cy", (d) => d.p[1]));
    let t = inner.select("text.xtitle");
    if (t.empty()) inner.append("text").attr("class", "ax-title xtitle").attr("x", iw / 2).attr("y", ih + 34).attr("text-anchor", "middle");
  }

  function drawRace(g, pl) {
    const W = D.W, H = D.H, m = { t: 14, r: 20, b: 34, l: 238 };
    const iw = W - m.l - m.r, ih = H - m.t - m.b;
    const inner = layer(g, "inner").attr("transform", `translate(${m.l},${m.t})`);
    const all = pl.rows.flatMap((r) => r.ests).concat([pl.truth]);
    let lo = d3.quantile(all.slice().sort(d3.ascending), 0.002), hi = d3.quantile(all.slice().sort(d3.ascending), 0.998);
    lo = Math.min(lo, pl.truth - 4); hi = Math.max(hi, pl.truth + 4);
    const x = d3.scaleLinear().domain([lo, hi]).nice().range([0, iw]).clamp(true);
    const y = d3.scaleBand().domain(pl.rows.map((r) => r.key)).range([0, ih]).padding(0.18);
    xAxis(layer(inner, "xax").attr("transform", `translate(0,${ih})`), x, { fmt: d3.format("+") });
    let xt = inner.select("text.xtitle");
    if (xt.empty()) xt = inner.append("text").attr("class", "ax-title xtitle").attr("text-anchor", "middle").text("estimated effect (points)");
    xt.attr("x", iw / 2).attr("y", ih + 32);
    const rows = layer(inner, "rows").selectAll("g.rrow").data(pl.rows, (d) => d.key);
    const re = rows.enter().append("g").attr("class", "rrow");
    re.append("rect").attr("class", "band-bg").attr("rx", 6);
    re.append("text").attr("class", "lbl").attr("text-anchor", "end").attr("x", -10);
    re.append("g").attr("class", "pts");
    re.append("path").attr("class", "fill-ink mean");
    const rm = re.merge(rows);
    rm.attr("transform", (d) => `translate(0,${y(d.key)})`);
    rm.select("rect").attr("x", -6).attr("width", iw + 12).attr("height", y.bandwidth()).attr("opacity", (d, i) => (i % 2 ? 0 : 0.6));
    rm.select("text").attr("y", y.bandwidth() / 2 + 4).text((d) => d.name);
    rm.each(function (d) {
      const c = d3.select(this).select("g.pts").selectAll("circle").data(d.ests);
      c.enter().append("circle").attr("r", 2.6).attr("cx", x(pl.truth)).attr("cy", (v, i) => y.bandwidth() / 2 + jit(i, 3) * y.bandwidth() * 0.75)
        .attr("opacity", 0.38)
        .merge(c).attr("class", d.tier === "Strong" ? "dot-treat" : d.tier === "Weak" ? "fill-accent" : d.tier === "Moderate" ? "fill-accent" : "dot-ctrl")
        .call((s) => tr(s, 900).delay((v, i) => i * 3).attr("cx", (v) => x(v)));
      c.exit().remove();
    });
    rm.select("path.mean").attr("d", d3.symbol(d3.symbolDiamond, 70)()).call((s) => tr(s, 900).attr("transform", (d) => `translate(${x(d.mean)},${y.bandwidth() / 2})`));
    let tl = inner.select("line.truth");
    if (tl.empty()) {
      tl = inner.append("line").attr("class", "truth stroke-accent").attr("stroke-width", 2);
      inner.append("text").attr("class", "tlab lbl fill-accent").attr("text-anchor", "middle");
    }
    tr(tl).attr("x1", x(pl.truth)).attr("x2", x(pl.truth)).attr("y1", -6).attr("y2", ih);
    tr(inner.select("text.tlab")).attr("x", x(pl.truth)).attr("y", -2).text("true effect");
  }

  function drawLadder(g, pl) {
    const W = D.W, H = D.H;
    const tiers = pl.tiers;
    const colW = (W - 20) / tiers.length;
    const inner = layer(g, "inner").attr("transform", "translate(10,10)");
    const arrowY = H - 46;
    let ax = inner.select("g.arrow");
    if (ax.empty()) {
      ax = inner.append("g").attr("class", "arrow");
      ax.append("line").attr("class", "stroke-ink").attr("x1", 0).attr("x2", W - 30).attr("y1", arrowY).attr("y2", arrowY).attr("stroke-width", 1.5);
      ax.append("path").attr("class", "fill-ink").attr("d", `M${W - 30},${arrowY - 5} l10,5 l-10,5z`);
      ax.append("text").attr("class", "ax-title").attr("x", 0).attr("y", arrowY + 22).text("less control over alternative explanations");
      ax.append("text").attr("class", "ax-title").attr("x", W - 20).attr("y", arrowY + 22).attr("text-anchor", "end").text("stronger evidence of causality");
    }
    const cols = inner.selectAll("g.tcol").data(tiers);
    const ce = cols.enter().append("g").attr("class", "tcol");
    ce.append("text").attr("class", "lbl-strong").attr("y", 18);
    ce.merge(cols).attr("transform", (d, i) => `translate(${i * colW},0)`).select("text").attr("x", colW / 2 - 6).attr("text-anchor", "middle").text((d) => d);
    const pills = [];
    tiers.forEach((t, ti) => pl.designs.filter((d) => d.tier === t).forEach((d, k) => pills.push({ ...d, ti, k })));
    const pw = colW - 16, ph = 48;
    const pg = inner.selectAll("g.pill").data(pills, (d) => d.key);
    const pe = pg.enter().append("g").attr("class", "pill").style("cursor", "pointer").style("opacity", 0)
      .on("click", (ev, d) => ERL.gotoStep(d.key));
    pe.append("rect").attr("rx", 10).attr("width", pw).attr("height", ph);
    pe.append("text").attr("x", pw / 2).attr("text-anchor", "middle");
    pe.merge(pg).attr("transform", (d) => `translate(${d.ti * colW + 2},${40 + d.k * (ph + 10)})`)
      .call((s) => tr(s).delay((d, i) => i * 60).style("opacity", 1));
    pe.select("text").each(function (d) {
      const words = d.name.split(" ");
      const lines = [];
      let cur = "";
      words.forEach((w) => { if ((cur + " " + w).trim().length > 19) { lines.push(cur); cur = w; } else cur = (cur + " " + w).trim(); });
      lines.push(cur);
      const t = d3.select(this);
      lines.slice(0, 3).forEach((ln, i) => t.append("tspan").attr("x", pw / 2).attr("y", ph / 2 + 4 + (i - (Math.min(lines.length, 3) - 1) / 2) * 14).text(ln));
    });
    pe.select("rect").attr("stroke-width", 1.5).style("stroke", (d) => d.tier === "Strong" ? "var(--treat)" : d.tier === "Weak" ? "var(--accent)" : d.tier === "Moderate" ? "var(--gold)" : "var(--violet)");
  }

  // =====================================================================
  // Scrollytelling
  // =====================================================================
  let currentStep = null;
  function setStep(key) {
    if (key === currentStep) return;
    currentStep = key;
    document.querySelectorAll(".step").forEach((s) => s.classList.toggle("active", s.dataset.step === key));
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue("design_step", key);
  }
  ERL.gotoStep = function (key) {
    const el = document.getElementById("step-" + key);
    if (el) el.scrollIntoView({ behavior: "smooth", block: "center" });
    setStep(key);
  };
  function initScrolly() {
    const steps = document.querySelectorAll(".step");
    if (!steps.length) return setTimeout(initScrolly, 200);
    const io = new IntersectionObserver((entries) => {
      entries.forEach((e) => { if (e.isIntersecting) setStep(e.target.dataset.step); });
    }, { rootMargin: "-45% 0px -50% 0px", threshold: 0 });
    steps.forEach((s) => {
      io.observe(s);
      s.addEventListener("click", () => { if (!s.classList.contains("active")) ERL.gotoStep(s.dataset.step); });
    });
    setStep("intro");
  }

  // =====================================================================
  // COHEN'S d
  // =====================================================================
  function cohenChart(pl) {
    const W = 680, H = 270, m = { t: 34, r: 16, b: 30, l: 16 };
    const svg = svgIn("cohen-chart", W, H);
    const iw = W - m.l - m.r, ih = H - m.t - m.b;
    const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
    const xmax = Math.max(4, pl.d + 3.6);
    const x = d3.scaleLinear().domain([-3.6, xmax]).range([0, iw]);
    const y = d3.scaleLinear().domain([0, 0.42]).range([ih, 0]);
    xAxis(layer(g, "xax").attr("transform", `translate(0,${ih})`), x, { fmt: (v) => (v === 0 ? "0 SD" : v) });
    const pts = pl.x.map((xx, i) => ({ x: xx, c: pl.c[i], t: pl.t[i] })).filter((p) => p.x <= xmax);
    const area = (k) => d3.area().x((d) => x(d.x)).y0(ih).y1((d) => y(d[k])).curve(d3.curveBasis);
    const ov = d3.area().x((d) => x(d.x)).y0(ih).y1((d) => y(Math.min(d.c, d.t))).curve(d3.curveBasis);
    const lineK = (k) => d3.line().x((d) => x(d.x)).y((d) => y(d[k])).curve(d3.curveBasis);
    const L = layer(g, "curves");
    if (L.select(".ac").empty()) {
      L.append("path").attr("class", "ac fill-ctrl-soft");
      L.append("path").attr("class", "at fill-treat-soft");
      L.append("path").attr("class", "ao").attr("fill", "url(#hatch)");
      L.append("path").attr("class", "lc stroke-ctrl").attr("fill", "none").attr("stroke-width", 2.5);
      L.append("path").attr("class", "lt stroke-treat").attr("fill", "none").attr("stroke-width", 2.5);
      const defs = svg.append("defs");
      const p = defs.append("pattern").attr("id", "hatch").attr("width", 6).attr("height", 6).attr("patternUnits", "userSpaceOnUse").attr("patternTransform", "rotate(45)");
      p.append("line").attr("x1", 0).attr("y1", 0).attr("x2", 0).attr("y2", 6).attr("class", "stroke-ink").attr("stroke-width", 1).attr("opacity", 0.25);
    }
    tr(L.select(".ac"), 450).attr("d", area("c")(pts));
    tr(L.select(".at"), 450).attr("d", area("t")(pts));
    tr(L.select(".ao"), 450).attr("d", ov(pts));
    tr(L.select(".lc"), 450).attr("d", lineK("c")(pts));
    tr(L.select(".lt"), 450).attr("d", lineK("t")(pts));
    const M = layer(g, "means");
    if (M.select(".mc").empty()) {
      M.append("line").attr("class", "mc stroke-ctrl").attr("stroke-dasharray", "3 3");
      M.append("line").attr("class", "mt stroke-treat").attr("stroke-dasharray", "3 3");
      M.append("text").attr("class", "tc lbl fill-ctrl").attr("text-anchor", "end").text("Control");
      M.append("text").attr("class", "tt lbl fill-treat").attr("text-anchor", "start").text("Intervention");
      M.append("path").attr("class", "arr stroke-ink").attr("fill", "none").attr("stroke-width", 1.5);
      M.append("text").attr("class", "dl lbl-strong").attr("text-anchor", "middle");
    }
    const top = y(0.399) - 2;
    tr(M.select(".mc"), 450).attr("x1", x(0)).attr("x2", x(0)).attr("y1", ih).attr("y2", top);
    tr(M.select(".mt"), 450).attr("x1", x(pl.d)).attr("x2", x(pl.d)).attr("y1", ih).attr("y2", top);
    tr(M.select(".tc"), 450).attr("x", x(0) - 8).attr("y", top + 10);
    tr(M.select(".tt"), 450).attr("x", x(pl.d) + 8).attr("y", top + 10);
    tr(M.select(".arr"), 450).attr("d", `M${x(0)},${top - 12} H${x(pl.d)}`);
    tr(M.select(".dl"), 450).attr("x", (x(0) + x(pl.d)) / 2).attr("y", top - 18).text("d = " + pl.d.toFixed(2));
  }

  function powerChart(pl) {
    const W = 680, H = 230, m = { t: 14, r: 120, b: 36, l: 44 };
    const svg = svgIn("power-chart", W, H);
    const iw = W - m.l - m.r, ih = H - m.t - m.b;
    const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
    const x = d3.scaleLog().domain([4, 5000]).range([0, iw]);
    const y = d3.scaleLinear().domain([0, 1]).range([ih, 0]);
    yAxis(layer(g, "yax"), y, iw, { grid: true, fmt: d3.format(".0%"), ticks: 5 });
    xAxis(layer(g, "xax").attr("transform", `translate(0,${ih})`), x, { values: [5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000], fmt: d3.format(",") });
    let xt = g.select("text.xtitle");
    if (xt.empty()) g.append("text").attr("class", "ax-title xtitle").attr("x", iw / 2).attr("y", ih + 32).attr("text-anchor", "middle").text("students per group (log scale)");
    const data0 = pl.ns.map((n, i) => [n, pl.p0[i]]), data1 = pl.ns.map((n, i) => [n, pl.p1[i]]);
    const ln = d3.line().x((d) => x(d[0])).y((d) => y(d[1]));
    const L = layer(g, "lines");
    if (L.select(".l0").empty()) {
      L.append("line").attr("class", "e80 stroke-accent").attr("stroke-dasharray", "4 4");
      L.append("text").attr("class", "e80t lbl fill-accent").text("80%");
      L.append("path").attr("class", "l1 stroke-treat").attr("fill", "none").attr("stroke-width", 2).attr("stroke-dasharray", "6 3");
      L.append("path").attr("class", "l0 stroke-ink").attr("fill", "none").attr("stroke-width", 2.5);
      L.append("text").attr("class", "t0 lbl");
      L.append("text").attr("class", "t1 lbl fill-treat");
      L.append("line").attr("class", "cur stroke-ink").attr("opacity", 0.4);
      L.append("circle").attr("class", "c0 fill-ink").attr("r", 5);
      L.append("circle").attr("class", "c1 fill-treat").attr("r", 5);
    }
    L.select(".e80").attr("x1", 0).attr("x2", iw).attr("y1", y(0.8)).attr("y2", y(0.8));
    L.select(".e80t").attr("x", 4).attr("y", y(0.8) - 5);
    tr(L.select(".l0"), 450).attr("d", ln(data0));
    tr(L.select(".l1"), 450).attr("d", ln(data1));
    tr(L.select(".t0"), 450).attr("x", iw + 6).attr("y", y(pl.p0[pl.p0.length - 1]) + 16).text("posttest only");
    tr(L.select(".t1"), 450).attr("x", iw + 6).attr("y", y(pl.p1[pl.p1.length - 1]) + 2).text("with pretest");
    const cx = x(Math.max(4, Math.min(5000, pl.n)));
    tr(L.select(".cur"), 450).attr("x1", cx).attr("x2", cx).attr("y1", 0).attr("y2", ih);
    tr(L.select(".c0"), 450).attr("cx", cx).attr("cy", y(pl.cur0 || 0));
    tr(L.select(".c1"), 450).attr("cx", cx).attr("cy", y(pl.cur1 || 0));
  }

  // =====================================================================
  // p-values
  // =====================================================================
  function pvCharts(pl) {
    // --- t distribution under H0
    {
      const W = 680, H = 170, m = { t: 12, r: 16, b: 26, l: 16 };
      const svg = svgIn("p-tdist", W, H);
      const iw = W - m.l - m.r, ih = H - m.t - m.b;
      const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
      const x = d3.scaleLinear().domain([-6, 6]).range([0, iw]);
      const y = d3.scaleLinear().domain([0, 0.42]).range([ih, 0]);
      xAxis(layer(g, "xax").attr("transform", `translate(0,${ih})`), x, { values: [-6, -4, -2, 0, 2, 4, 6] });
      const pts = pl.x.map((xx, i) => [xx, pl.y[i]]);
      const ar = (f) => d3.area().x((d) => x(d[0])).y0(ih).y1((d) => y(d[1]))(pts.filter(f));
      const tobs = pl.last ? Math.abs(pl.last.t) : null;
      const L = layer(g, "dist");
      if (L.select(".crit").empty()) {
        L.append("path").attr("class", "crit fill-ctrl-soft");
        L.append("path").attr("class", "pl fill-accent").attr("opacity", 0.55);
        L.append("path").attr("class", "pr fill-accent").attr("opacity", 0.55);
        L.append("path").attr("class", "cv stroke-ink").attr("fill", "none").attr("stroke-width", 2);
        L.append("line").attr("class", "tl stroke-accent").attr("stroke-width", 2);
        L.append("text").attr("class", "tt lbl fill-accent").attr("text-anchor", "middle");
        L.append("text").attr("class", "h0 lbl").attr("x", x(0)).attr("y", 10).attr("text-anchor", "middle");
        L.append("line").attr("class", "c1 stroke-ink").attr("stroke-dasharray", "2 3");
        L.append("line").attr("class", "c2 stroke-ink").attr("stroke-dasharray", "2 3");
      }
      L.select(".crit").attr("d", d3.area().x((d) => x(d[0])).y0(ih).y1((d) => y(d[1]))(pts));
      L.select(".cv").attr("d", d3.line().x((d) => x(d[0])).y((d) => y(d[1]))(pts));
      L.select(".h0").text(`t-distribution if H₀ is true (df = ${pl.df})`);
      [".c1", ".c2"].forEach((c, i) => L.select(c).attr("x1", x((i ? 1 : -1) * pl.tcrit)).attr("x2", x((i ? 1 : -1) * pl.tcrit)).attr("y1", ih).attr("y2", y(0.25)));
      if (tobs != null) {
        const tt = Math.min(tobs, 5.95);
        L.select(".pl").attr("d", ar((d) => d[0] <= -tt) || "");
        L.select(".pr").attr("d", ar((d) => d[0] >= tt) || "");
        const xo = x(Math.max(-5.95, Math.min(5.95, pl.last.t)));
        tr(L.select(".tl"), 400).attr("x1", xo).attr("x2", xo).attr("y1", ih).attr("y2", y(0.3)).style("opacity", 1);
        tr(L.select(".tt"), 400).attr("x", xo).attr("y", y(0.3) - 6).style("opacity", 1).text(`t = ${pl.last.t.toFixed(2)}`);
      } else {
        L.select(".pl").attr("d", null); L.select(".pr").attr("d", null);
        L.select(".tl").style("opacity", 0); L.select(".tt").style("opacity", 0);
      }
    }
    // --- p histogram
    {
      const W = 680, H = 230, m = { t: 16, r: 16, b: 34, l: 46 };
      const svg = svgIn("p-hist", W, H);
      const iw = W - m.l - m.r, ih = H - m.t - m.b;
      const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
      const x = d3.scaleLinear().domain([0, 1]).range([0, iw]);
      const ymax = Math.max(5, d3.max(pl.counts) || 0);
      const y = d3.scaleLinear().domain([0, ymax]).nice().range([ih, 0]);
      yAxis(layer(g, "yax"), y, iw, { grid: true, ticks: 4, fmt: d3.format(",~d") });
      xAxis(layer(g, "xax").attr("transform", `translate(0,${ih})`), x, { values: [0, 0.05, 0.2, 0.4, 0.6, 0.8, 1], fmt: (v) => (v === 0.05 ? ".05" : d3.format(".1~f")(v)) });
      let xt = g.select("text.xtitle");
      if (xt.empty()) g.append("text").attr("class", "ax-title xtitle").attr("x", iw / 2).attr("y", ih + 32).attr("text-anchor", "middle").text("p-value");
      const bw = iw / 20;
      const bars = layer(g, "bars").selectAll("rect").data(pl.counts);
      bars.enter().append("rect").attr("x", (d, i) => i * bw + 1).attr("width", bw - 2).attr("y", ih).attr("height", 0).attr("rx", 2)
        .merge(bars).attr("class", (d, i) => ((i + 1) * 0.05 <= pl.alpha + 1e-9 ? "fill-treat" : "fill-ctrl"))
        .call((s) => tr(s, 500).attr("y", (d) => y(d)).attr("height", (d) => ih - y(d)));
      const A = layer(g, "alpha");
      if (A.select("line").empty()) {
        A.append("line").attr("class", "stroke-accent").attr("stroke-width", 1.5).attr("stroke-dasharray", "4 3");
        A.append("text").attr("class", "lbl fill-accent");
      }
      A.select("line").attr("x1", x(pl.alpha)).attr("x2", x(pl.alpha)).attr("y1", -6).attr("y2", ih);
      A.select("text").attr("x", x(pl.alpha) + 5).attr("y", 2).text("α = " + pl.alpha);
      // expected-if-H0 line
      const exp = pl.total / 20;
      const E = layer(g, "exp");
      if (E.select("line").empty()) {
        E.append("line").attr("class", "stroke-ink").attr("stroke-dasharray", "1 3").attr("opacity", 0.7);
        E.append("text").attr("class", "lbl").attr("text-anchor", "end");
      }
      tr(E.select("line"), 500).attr("x1", 0).attr("x2", iw).attr("y1", y(exp)).attr("y2", y(exp)).style("opacity", pl.total ? 1 : 0);
      tr(E.select("text"), 500).attr("x", iw).attr("y", y(exp) - 5).style("opacity", pl.total ? 1 : 0).text("flat line = what H₀ predicts");
    }
    // --- dance of CIs
    {
      const W = 680, H = 230, m = { t: 8, r: 16, b: 30, l: 16 };
      const svg = svgIn("p-dance", W, H);
      const iw = W - m.l - m.r, ih = H - m.t - m.b;
      const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
      const x = d3.scaleLinear().domain([-1.2, Math.max(1.6, pl.d + 1.2)]).range([0, iw]).clamp(true);
      xAxis(layer(g, "xax").attr("transform", `translate(0,${ih})`), x);
      const rec = pl.recent.slice().reverse();
      const rowH = ih / 25;
      const Z = layer(g, "zero");
      if (Z.select(".z").empty()) {
        Z.append("line").attr("class", "z stroke-ink").attr("opacity", 0.5);
        Z.append("line").attr("class", "tv stroke-accent").attr("stroke-width", 1.5).attr("stroke-dasharray", "4 3");
        Z.append("text").attr("class", "tvt lbl fill-accent").attr("text-anchor", "middle");
      }
      Z.select(".z").attr("x1", x(0)).attr("x2", x(0)).attr("y1", 0).attr("y2", ih);
      tr(Z.select(".tv"), 400).attr("x1", x(pl.d)).attr("x2", x(pl.d)).attr("y1", 0).attr("y2", ih);
      tr(Z.select(".tvt"), 400).attr("x", x(pl.d)).attr("y", ih + 26).text("true d");
      const keyBase = pl.total;
      const rows = layer(g, "rows").selectAll("g.ci").data(rec, (d, i) => keyBase - i);
      rows.exit().transition().duration(300).style("opacity", 0).remove();
      const re = rows.enter().append("g").attr("class", "ci").attr("transform", `translate(0,${-rowH})`).style("opacity", 0);
      re.append("line").attr("stroke-width", 2);
      re.append("circle").attr("r", 3.5);
      const rm = re.merge(rows);
      rm.call((s) => tr(s, 450).attr("transform", (d, i) => `translate(0,${i * rowH + rowH / 2})`).style("opacity", (d, i) => 1 - i / 34));
      rm.select("line").attr("class", (d) => (d[3] < pl.alpha ? "stroke-treat" : "stroke-ctrl")).attr("x1", (d) => x(d[1])).attr("x2", (d) => x(d[2]));
      rm.select("circle").attr("class", (d) => (d[3] < pl.alpha ? "fill-treat" : "fill-ctrl")).attr("cx", (d) => x(d[0]));
    }
  }

  // =====================================================================
  // Chi-square
  // =====================================================================
  const CAT = ["var(--treat)", "var(--accent)", "var(--violet)"];
  function chiCharts(pl) {
    if (pl.error) return;
    {
      const W = 680, H = 270, m = { t: 34, r: 16, b: 34, l: 46 };
      const svg = svgIn("chi-bars", W, H);
      const iw = W - m.l - m.r, ih = H - m.t - m.b;
      const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
      const R = pl.obs.length, C = pl.obs[0].length;
      const x0 = d3.scaleBand().domain(d3.range(R)).range([0, iw]).paddingInner(0.22).paddingOuter(0.08);
      const x1 = d3.scaleBand().domain(d3.range(C)).range([0, x0.bandwidth()]).padding(0.12);
      const ymax = d3.max(pl.obs.flat().concat(pl.exp.flat()));
      const y = d3.scaleLinear().domain([0, ymax * 1.12]).nice().range([ih, 0]);
      yAxis(layer(g, "yax"), y, iw, { grid: true, ticks: 5 });
      const rl = layer(g, "rlab").selectAll("text").data(pl.rl);
      rl.exit().remove();
      rl.enter().append("text").attr("class", "lbl-strong").attr("text-anchor", "middle").merge(rl)
        .attr("x", (d, i) => x0(i) + x0.bandwidth() / 2).attr("y", ih + 22).text((d) => d);
      const cells = [];
      pl.obs.forEach((row, i) => row.forEach((v, j) => cells.push({ id: `${i}-${j}`, i, j, o: v, e: pl.exp[i][j], r: pl.resid[i][j] })));
      const cg = layer(g, "cells").selectAll("g.cell").data(cells, (d) => d.id);
      cg.exit().remove();
      const ce = cg.enter().append("g").attr("class", "cell");
      ce.append("rect").attr("class", "o").attr("rx", 3).attr("y", ih).attr("height", 0);
      ce.append("rect").attr("class", "e").attr("fill", "none").attr("stroke-dasharray", "4 3").attr("stroke-width", 1.5).attr("class", "e stroke-ink");
      ce.append("text").attr("class", "rt lbl-mono").attr("text-anchor", "middle");
      const cm = ce.merge(cg);
      cm.attr("transform", (d) => `translate(${x0(d.i) + x1(d.j)},0)`);
      cm.select("rect.o").style("fill", (d) => CAT[d.j]).attr("opacity", 0.8).attr("width", x1.bandwidth())
        .call((s) => tr(s, 500).attr("y", (d) => y(d.o)).attr("height", (d) => ih - y(d.o)));
      cm.select("rect.e").attr("width", x1.bandwidth()).call((s) => tr(s, 500).attr("y", (d) => y(d.e)).attr("height", (d) => ih - y(d.e)));
      cm.select("text.rt").attr("x", x1.bandwidth() / 2).text((d) => (d.r >= 0 ? "+" : "") + d.r.toFixed(1))
        .call((s) => tr(s, 500).attr("y", (d) => y(Math.max(d.o, d.e)) - 6));
      // legend
      const lg = layer(g, "legend").attr("transform", `translate(${iw - pl.cl.length * 120},-20)`).selectAll("g").data(pl.cl);
      lg.exit().remove();
      const le = lg.enter().append("g");
      le.append("rect").attr("width", 11).attr("height", 11).attr("rx", 2).attr("y", -9);
      le.append("text").attr("class", "lbl").attr("x", 16);
      const lm = le.merge(lg).attr("transform", (d, i) => `translate(${i * 120},0)`);
      lm.select("rect").style("fill", (d, i) => CAT[i]);
      lm.select("text").text((d) => d);
    }
    {
      const W = 680, H = 200, m = { t: 14, r: 16, b: 30, l: 16 };
      const svg = svgIn("chi-dist", W, H);
      const iw = W - m.l - m.r, ih = H - m.t - m.b;
      const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
      const xmax = pl.x[pl.x.length - 1];
      const x = d3.scaleLinear().domain([0, xmax]).range([0, iw]);
      const ymax = Math.min(d3.max(pl.y), pl.df === 1 ? 1.2 : 10);
      const y = d3.scaleLinear().domain([0, ymax * 1.08]).range([ih, 0]).clamp(true);
      xAxis(layer(g, "xax").attr("transform", `translate(0,${ih})`), x);
      const pts = pl.x.map((xx, i) => [xx, pl.y[i]]);
      const ar = (f) => d3.area().x((d) => x(d[0])).y0(ih).y1((d) => y(d[1]))(pts.filter(f));
      const L = layer(g, "d");
      if (L.select(".full").empty()) {
        L.append("path").attr("class", "full fill-ctrl-soft");
        L.append("path").attr("class", "crit fill-ctrl").attr("opacity", 0.35);
        L.append("path").attr("class", "pa fill-accent").attr("opacity", 0.6);
        L.append("path").attr("class", "cv stroke-ink").attr("fill", "none").attr("stroke-width", 2);
        L.append("line").attr("class", "cl stroke-ink").attr("stroke-dasharray", "2 3");
        L.append("text").attr("class", "ct lbl").attr("text-anchor", "middle");
        L.append("line").attr("class", "ol stroke-accent").attr("stroke-width", 2.5);
        L.append("text").attr("class", "ot lbl fill-accent").attr("text-anchor", "middle").attr("font-weight", 600);
      }
      L.select(".full").attr("d", ar(() => true));
      L.select(".crit").attr("d", ar((d) => d[0] >= pl.crit));
      L.select(".pa").attr("d", ar((d) => d[0] >= pl.chi2));
      L.select(".cv").attr("d", d3.line().x((d) => x(d[0])).y((d) => y(d[1]))(pts));
      L.select(".cl").attr("x1", x(pl.crit)).attr("x2", x(pl.crit)).attr("y1", ih).attr("y2", y(ymax * 0.6));
      L.select(".ct").attr("x", x(pl.crit)).attr("y", y(ymax * 0.6) - 5).text("critical value (α = .05)");
      tr(L.select(".ol"), 450).attr("x1", x(pl.chi2)).attr("x2", x(pl.chi2)).attr("y1", ih).attr("y2", y(ymax * 0.92));
      tr(L.select(".ot"), 450).attr("x", Math.min(iw - 40, x(pl.chi2))).attr("y", y(ymax * 0.92) - 6).text("X² = " + pl.chi2.toFixed(2));
    }
  }

  // =====================================================================
  // Distributions (with falling samples)
  // =====================================================================
  let distLast = { key: null, n: 0 };
  function distChart(pl) {
    const W = 680, H = 340, m = { t: 26, r: 18, b: 32, l: 46 };
    const svg = svgIn("dist-chart", W, H);
    const iw = W - m.l - m.r, ih = H - m.t - m.b;
    const g = layer(svg, "inner").attr("transform", `translate(${m.l},${m.t})`);
    const [lo, hi] = pl.domain;
    const x = d3.scaleLinear().domain(pl.discrete ? [lo - 0.6, hi + 0.6] : [lo, hi]).range([0, iw]);
    const cdf = pl.mode === "cdf";
    const pdfMax = pl.discrete ? d3.max(pl.pmf, (d) => d[1]) : d3.max(pl.curve, (d) => d[1]);
    // sample histogram (density scaled)
    const s = pl.samples || [];
    let hist = [];
    if (s.length && !cdf) {
      if (pl.discrete) {
        const cnt = d3.rollup(s, (v) => v.length, (v) => Math.round(v));
        hist = Array.from(cnt, ([k, c]) => ({ x0: k - 0.4, x1: k + 0.4, h: c / s.length }));
      } else {
        const bins = d3.bin().domain([lo, hi]).thresholds(d3.range(lo, hi + 1e-9, (hi - lo) / 60))(s);
        hist = bins.filter((b) => b.x1 > b.x0).map((b) => ({ x0: b.x0, x1: b.x1, h: b.length / s.length / (b.x1 - b.x0) }));
      }
    }
    const hmax = hist.length ? d3.max(hist, (d) => d.h) : 0;
    const y = d3.scaleLinear().domain(cdf ? [0, 1] : [0, Math.max(pdfMax, Math.min(hmax, pdfMax * 1.6)) * 1.12]).range([ih, 0]);
    yAxis(layer(g, "yax"), y, iw, { grid: true, ticks: 5, fmt: cdf ? d3.format(".0%") : d3.format(".2~f") });
    xAxis(layer(g, "xax").attr("transform", `translate(0,${ih})`), x, { ticks: pl.discrete ? 10 : 8, fmt: d3.format("~g") });

    const H1 = layer(g, "hist").selectAll("rect").data(hist, (d) => d.x0);
    H1.exit().transition().duration(300).attr("height", 0).attr("y", ih).remove();
    H1.enter().append("rect").attr("class", "fill-accent").attr("opacity", 0.35).attr("y", ih).attr("height", 0)
      .merge(H1).attr("x", (d) => x(d.x0) + 0.5).attr("width", (d) => Math.max(1, x(d.x1) - x(d.x0) - 1))
      .call((sel) => tr(sel, 400).attr("y", (d) => y(Math.min(d.h, y.domain()[1]))).attr("height", (d) => ih - y(Math.min(d.h, y.domain()[1]))));

    const a = pl.a, b = pl.b;
    const S = layer(g, "shape");
    S.selectAll("*").remove();
    if (pl.discrete) {
      const data = cdf ? pl.cdf : pl.pmf;
      const bw = Math.max(2, (x(1) - x(0)) * 0.62);
      S.selectAll("rect").data(data).enter().append("rect")
        .attr("x", (d) => x(d[0]) - bw / 2).attr("width", bw).attr("y", (d) => y(d[1])).attr("height", (d) => ih - y(d[1])).attr("rx", 2)
        .attr("class", (d) => (d[0] >= Math.ceil(a) && d[0] <= Math.floor(b) ? "fill-treat" : "fill-ctrl"))
        .attr("opacity", (d) => (d[0] >= Math.ceil(a) && d[0] <= Math.floor(b) ? 0.85 : 0.45));
    } else {
      const data = cdf ? pl.cdf : pl.curve;
      if (!cdf) {
        S.append("path").attr("class", "fill-treat").attr("opacity", 0.32)
          .attr("d", d3.area().x((d) => x(d[0])).y0(ih).y1((d) => y(d[1]))(data.filter((d) => d[0] >= a && d[0] <= b)));
        if (pl.ref) S.append("path").attr("class", "stroke-ink").attr("fill", "none").attr("stroke-dasharray", "4 4").attr("opacity", 0.55)
          .attr("d", d3.line().x((d) => x(d[0])).y((d) => y(d[1]))(pl.ref));
      }
      S.append("path").attr("class", "stroke-treat").attr("fill", "none").attr("stroke-width", 2.5)
        .attr("d", d3.line().x((d) => x(d[0])).y((d) => y(d[1]))(data));
    }
    if (cdf) {
      const F = (v) => {
        const arr = pl.cdf;
        if (pl.discrete) { let r = 0; arr.forEach((d) => { if (d[0] <= v) r = d[1]; }); return r; }
        const i = d3.bisector((d) => d[0]).left(arr, v);
        return arr[Math.min(arr.length - 1, Math.max(0, i))][1];
      };
      const Fa = pl.discrete ? F(Math.ceil(a) - 1) : F(a), Fb = F(b);
      [[a, Fa], [b, Fb]].forEach(([v, fv]) => {
        S.append("line").attr("class", "stroke-accent").attr("stroke-dasharray", "3 3").attr("x1", x(v)).attr("x2", x(v)).attr("y1", ih).attr("y2", y(fv));
        S.append("line").attr("class", "stroke-accent").attr("stroke-dasharray", "3 3").attr("x1", 0).attr("x2", x(v)).attr("y1", y(fv)).attr("y2", y(fv));
        S.append("circle").attr("class", "fill-accent").attr("r", 4).attr("cx", x(v)).attr("cy", y(fv));
      });
      S.append("rect").attr("class", "fill-accent").attr("opacity", 0.25).attr("x", -10).attr("width", 6).attr("y", y(Fb)).attr("height", Math.max(0, y(Fa) - y(Fb)));
    } else {
      [a, b].forEach((v) => S.append("line").attr("class", "stroke-accent").attr("stroke-width", 1.5).attr("x1", x(v)).attr("x2", x(v)).attr("y1", ih).attr("y2", 0).attr("opacity", 0.6));
    }
    S.append("text").attr("class", "lbl-strong").attr("x", iw).attr("y", -8).attr("text-anchor", "end")
      .text(`P(${a} ≤ X ≤ ${b}) = ${(pl.prob * 100).toFixed(1)}%`);

    // falling sample dots for new draws
    const key = pl.dist;
    const nNew = s.length - pl.new_from;
    if (s.length > distLast.n && key === distLast.key && nNew > 0 && nNew <= 60 && !cdf) {
      const fresh = s.slice(pl.new_from);
      const F = layer(g, "fall");
      F.selectAll("circle.drop").data(fresh).enter().append("circle").attr("class", "drop fill-accent").attr("r", 4.5)
        .attr("cx", (v) => x(v)).attr("cy", -10).attr("opacity", 0)
        .transition().delay((v, i) => i * 22).duration(0).attr("opacity", 0.9)
        .transition().duration(650).ease(d3.easeBounceOut).attr("cy", ih - 4)
        .transition().duration(400).attr("opacity", 0).remove();
    }
    distLast = { key, n: s.length };
  }

  // =====================================================================
  // CLT
  // =====================================================================
  let cltBusy = false;
  function cltChart(pl) {
    const W = 680, H = 500, m = { l: 46, r: 18 };
    const svg = svgIn("clt-chart", W, H);
    const iw = W - m.l - m.r;
    const P = { y: 26, h: 120 }, Sm = { y: 190, h: 50 }, M = { y: 290, h: 170 };
    const g = layer(svg, "inner").attr("transform", `translate(${m.l},0)`);
    const [lo, hi] = pl.pop.domain;
    const x = d3.scaleLinear().domain([lo, hi]).range([0, iw]);

    // titles
    const titles = [["Population", P.y - 10, pl.pop.label], ["One sample (n = " + pl.n + ")", Sm.y - 10, "its mean is marked ▲"],
      ["Sampling distribution of the mean", M.y - 10, pl.means.length.toLocaleString() + " sample means"]];
    const tt = layer(g, "titles").selectAll("g").data(titles);
    const te = tt.enter().append("g");
    te.append("text").attr("class", "lbl-strong");
    te.append("text").attr("class", "lbl sub").attr("text-anchor", "end");
    const tm = te.merge(tt);
    tm.select(".lbl-strong").attr("x", 0).attr("y", (d) => d[1]).text((d) => d[0]);
    tm.select(".sub").attr("x", iw).attr("y", (d) => d[1]).text((d) => d[2]);

    // population
    const pg = layer(g, "pop").attr("transform", `translate(0,${P.y})`);
    const ph = d3.max(pl.pop.hist, (d) => d[2]);
    const py = d3.scaleLinear().domain([0, ph]).range([P.h, 0]);
    const pb = pg.selectAll("rect").data(pl.pop.hist);
    pb.exit().remove();
    pb.enter().append("rect").attr("class", "fill-ctrl").attr("opacity", 0.55).attr("y", P.h).attr("height", 0)
      .merge(pb).call((s) => tr(s, 600).attr("x", (d) => x(d[0]) + 0.5).attr("width", (d) => Math.max(1, x(d[1]) - x(d[0]) - (pl.pop.discrete ? 14 : 1)))
        .attr("y", (d) => py(d[2])).attr("height", (d) => P.h - py(d[2])));
    let mu = pg.select("line.mu");
    if (mu.empty()) {
      mu = pg.append("line").attr("class", "mu stroke-ink").attr("stroke-width", 1.5).attr("stroke-dasharray", "3 3");
      pg.append("text").attr("class", "mut lbl").attr("text-anchor", "middle");
    }
    tr(mu, 600).attr("x1", x(pl.pop.mu)).attr("x2", x(pl.pop.mu)).attr("y1", -4).attr("y2", P.h);
    tr(pg.select("text.mut"), 600).attr("x", x(pl.pop.mu)).attr("y", P.h + 14).text("μ = " + pl.pop.mu.toFixed(1));

    // sample strip
    const sg = layer(g, "samp").attr("transform", `translate(0,${Sm.y})`);
    if (sg.select("line.base").empty()) sg.append("line").attr("class", "base stroke-ink").attr("opacity", 0.25).attr("x1", 0).attr("x2", iw).attr("y1", Sm.h / 2).attr("y2", Sm.h / 2);
    sg.select("line.base").attr("x2", iw);

    // means histogram
    const mg = layer(g, "means").attr("transform", `translate(0,${M.y})`);
    xAxis(layer(mg, "xax").attr("transform", `translate(0,${M.h})`), x, { ticks: pl.pop.discrete ? 5 : 10 });
    const nb = pl.pop.discrete ? 80 : 80;
    const thr = d3.range(lo, hi + 1e-9, (hi - lo) / nb);
    const bins = d3.bin().domain([lo, hi]).thresholds(thr)(pl.means);
    const bmax = d3.max(bins, (b) => b.length) || 1;
    const bw = (hi - lo) / nb;
    const curveMax = pl.means.length * bw * (1 / (pl.se * Math.sqrt(2 * Math.PI)));
    const my = d3.scaleLinear().domain([0, Math.max(5, bmax, pl.normal && pl.means.length ? Math.min(curveMax, bmax * 1.6) : 0) * 1.08]).range([M.h, 0]);
    const mb = layer(mg, "bars").selectAll("rect").data(bins);
    mb.exit().remove();
    mb.enter().append("rect").attr("class", "fill-treat").attr("y", M.h).attr("height", 0)
      .merge(mb).attr("x", (b) => x(b.x0) + 0.3).attr("width", (b) => Math.max(0.5, x(b.x1) - x(b.x0) - 0.6))
      .call((s) => tr(s, cltBusy ? 300 : 500).attr("y", (b) => my(b.length)).attr("height", (b) => M.h - my(b.length)));
    let nc = mg.select("path.nc");
    if (nc.empty()) nc = mg.append("path").attr("class", "nc stroke-accent").attr("fill", "none").attr("stroke-width", 2.5);
    if (pl.normal && pl.means.length > 0) {
      const xs = d3.range(lo, hi, (hi - lo) / 300);
      const pdf = (v) => Math.exp(-0.5 * ((v - pl.pop.mu) / pl.se) ** 2) / (pl.se * Math.sqrt(2 * Math.PI));
      tr(nc, 500).style("opacity", 1).attr("d", d3.line().x((v) => x(v)).y((v) => my(Math.min(my.domain()[1], pl.means.length * bw * pdf(v))))(xs));
    } else tr(nc, 300).style("opacity", 0);

    // animate a single draw: dots rise from the population and fall into the strip, then the mean drops
    if (pl.anim && pl.sample) {
      const n = pl.sample.length;
      const mean = d3.mean(pl.sample);
      sg.selectAll("*:not(.base)").remove();
      const r = n > 50 ? 3 : 4.5;
      const dots = sg.selectAll("circle.sd").data(pl.sample).enter().append("circle").attr("class", "sd fill-ink").attr("r", r).attr("opacity", 0.75)
        .attr("cx", (v, i) => x(v) + (pl.pop.discrete ? jit(i, 9) * 26 : 0)).attr("cy", (v, i) => -(Sm.y - P.y) + P.h - 10 - Math.abs(jit(i, 5)) * 60);
      dots.transition().delay((v, i) => i * Math.min(30, 600 / n)).duration(550).ease(d3.easeCubicIn).attr("cy", (v, i) => Sm.h / 2 + jit(i, 4) * 18);
      const t0 = Math.min(30, 600 / n) * n + 600;
      sg.append("path").attr("class", "fill-accent").attr("d", d3.symbol(d3.symbolTriangle, 90)()).attr("transform", `translate(${x(mean)},${Sm.h / 2})`)
        .attr("opacity", 0).transition().delay(t0).duration(200).attr("opacity", 1);
      const tri = sg.append("path").attr("class", "fill-accent").attr("d", d3.symbol(d3.symbolTriangle, 90)()).attr("transform", `translate(${x(mean)},${Sm.h / 2})`).attr("opacity", 0);
      tri.transition().delay(t0).duration(200).attr("opacity", 1)
        .transition().duration(700).ease(d3.easeCubicIn).attr("transform", `translate(${x(mean)},${M.y - Sm.y + M.h - 6})`)
        .transition().duration(250).attr("opacity", 0).remove();
    } else if (pl.sample) {
      sg.selectAll("*:not(.base)").remove();
      const mean = d3.mean(pl.sample);
      sg.selectAll("circle.sd").data(pl.sample).enter().append("circle").attr("class", "sd fill-ink").attr("r", pl.sample.length > 50 ? 3 : 4).attr("opacity", 0.6)
        .attr("cx", (v, i) => x(v) + (pl.pop.discrete ? jit(i, 9) * 26 : 0)).attr("cy", (v, i) => Sm.h / 2 + jit(i, 4) * 18);
      sg.append("path").attr("class", "fill-accent").attr("d", d3.symbol(d3.symbolTriangle, 90)()).attr("transform", `translate(${x(mean)},${Sm.h / 2})`);
    } else {
      sg.selectAll("*:not(.base)").remove();
    }
  }

  // =====================================================================
  // PRISMA export
  // =====================================================================
  ERL.downloadPrisma = function (fmt) {
    const svgEl = document.getElementById("prisma-svg");
    if (!svgEl) return;
    const src = new XMLSerializer().serializeToString(svgEl);
    const save = (blob, name) => {
      const a = document.createElement("a");
      a.href = URL.createObjectURL(blob);
      a.download = name;
      document.body.appendChild(a);
      a.click();
      setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 500);
    };
    if (fmt === "svg") return save(new Blob([src], { type: "image/svg+xml" }), "PRISMA2020_flow_diagram.svg");
    const vb = svgEl.viewBox.baseVal;
    const scale = 3;
    const img = new Image();
    img.onload = () => {
      const c = document.createElement("canvas");
      c.width = vb.width * scale;
      c.height = vb.height * scale;
      const ctx = c.getContext("2d");
      ctx.fillStyle = "#fff";
      ctx.fillRect(0, 0, c.width, c.height);
      ctx.drawImage(img, 0, 0, c.width, c.height);
      c.toBlob((b) => save(b, "PRISMA2020_flow_diagram.png"), "image/png");
    };
    img.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(src);
  };

  // =====================================================================
  // Wiring
  // =====================================================================
  whenShiny(() => {
    Shiny.addCustomMessageHandler("erl_design", designChart);
    Shiny.addCustomMessageHandler("erl_cohen", cohenChart);
    Shiny.addCustomMessageHandler("erl_power", powerChart);
    Shiny.addCustomMessageHandler("erl_pv", pvCharts);
    Shiny.addCustomMessageHandler("erl_chi", chiCharts);
    Shiny.addCustomMessageHandler("erl_dist", distChart);
    Shiny.addCustomMessageHandler("erl_clt", cltChart);
  });
  $(document).on("shiny:connected", () => initScrolly());

  // home cards → tabs
  document.addEventListener("click", (e) => {
    const a = e.target.closest("[data-goto]");
    if (!a) return;
    e.preventDefault();
    const tab = document.querySelector(`.navbar a[data-value="${a.dataset.goto}"]`);
    if (tab) { tab.click(); window.scrollTo({ top: 0, behavior: "smooth" }); }
  });
  // re-run scroll detection when the Designs tab becomes visible
  $(document).on("shown.bs.tab", (e) => {
    if (e.target && e.target.dataset.value === "Designs") {
      window.scrollTo({ top: 0 });
      const s = currentStep; currentStep = null; setStep(s || "intro");
    }
  });
})();
