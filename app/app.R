# =============================================================================
# Seeing Research Design — an interactive companion for teaching educational
# research methods. R Shiny version, designed to run in the browser via
# Shinylive (webR) or on any Shiny server.
#
# R computes everything (base R stats only); D3 (viz.js) draws and animates.
# Messages go R -> JS with session$sendCustomMessage().
#
# Design content is paraphrased (with page references) from:
#   Nygren, T. (2026). Experimental methods and statistics for educational
#   research: With examples from teaching and learning in history and social
#   studies. Uppsala University Publications. CC BY-NC-ND.
# =============================================================================

library(shiny)
library(bslib)

invisible(runif(1))  # initialise the RNG so with_seed() can restore the session's own stream

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -----------------------------------------------------------------------------
# Small helpers
# -----------------------------------------------------------------------------

rr <- function(x, k = 2) {
  x <- round(as.numeric(x), k)
  x[!is.finite(x)] <- NA
  x
}
arr <- function(x, k = 2) I(rr(x, k))            # always a JSON array
scal <- function(x, k = 2) { v <- rr(x, k); if (length(v) == 0) NA else v[1] }

signed <- function(x, k = 1) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !is.finite(x)) return("—")
  if (round(x, k) == 0) return(formatC(0, format = "f", digits = k))
  paste0(if (x > 0) "+" else "−", formatC(abs(x), format = "f", digits = k, big.mark = ","))
}

fmt_p <- function(p) {
  if (is.null(p) || is.na(p)) return("—")
  if (p < 0.001) return("< .001")
  sub("^0", "", formatC(p, format = "f", digits = 3))
}

fnum <- function(x, k = 2) formatC(x, format = "f", digits = k, big.mark = ",")
fint <- function(x) formatC(x, format = "d", big.mark = ",")
esc <- htmltools::htmlEscape

welch <- function(a, b) {
  c(diff = mean(b) - mean(a), se = sqrt(var(a) / length(a) + var(b) / length(b)))
}

ols <- function(X, y) {
  fit <- lm.fit(X, y)
  b <- fit$coefficients
  res <- fit$residuals
  dfree <- max(1, length(y) - ncol(X))
  s2 <- sum(res^2) / dfree
  XtXi <- chol2inv(qr.R(fit$qr))
  list(b = unname(b), se = sqrt(diag(s2 * XtXi)), df = dfree)
}

# Evaluate expr with a fixed seed without disturbing the global RNG stream
with_seed <- function(seed, expr) {
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
  set.seed(seed)
  expr
}

skewness <- function(x) { m <- mean(x); s <- sqrt(mean((x - m)^2)); mean((x - m)^3) / s^3 }

# -----------------------------------------------------------------------------
# Research designs (Nygren, 2026, Ch. 1 §3–4 and Ch. 5)
# -----------------------------------------------------------------------------

TIER_ORDER <- c("Weak", "Moderate", "Quasi-experimental", "Strong")

DESIGNS <- list(
  list(key = "d1", tier = "Weak", name = "One-group posttest-only",
       rows = list(c("", "X", "O")), noneq = FALSE,
       idea = "One class gets the new teaching and is tested afterwards. That is the whole study.",
       example = "A teacher runs a role-play on how the EU makes decisions, then gives a knowledge test.",
       problem = paste("There is no pretest and no comparison group, so the posttest has nothing to be compared with.",
                       "What students already knew, how much they would have grown anyway, and the intervention itself",
                       "all end up in the same number."),
       studies = "Fine for a quick classroom check, but it says little about whether the method works.",
       page = "pp. 13, 62–63", threats = c(sel = "no", mat = "no", test = "ok", base = "no")),
  list(key = "d2", tier = "Weak", name = "Posttest-only, non-equivalent groups",
       rows = list(c("", "X", "O"), c("", "", "O")), noneq = TRUE,
       idea = "A class that got the intervention is compared with another existing class after teaching. Nobody was randomly assigned.",
       example = "Class 9A does the EU role-play, class 9B has ordinary lessons, and both take the same test.",
       problem = paste("Existing classes can differ before anything happens (prior knowledge, motivation, the teacher).",
                       "If 9A was already ahead, that head start gets read as an effect of the intervention."),
       studies = "Comparing a high-achieving class with a struggling one is the classic trap.",
       page = "pp. 13, 63", threats = c(sel = "no", mat = "ok", test = "ok", base = "no")),
  list(key = "d3", tier = "Moderate", name = "One-group pretest–posttest",
       rows = list(c("O", "X", "O")), noneq = FALSE,
       idea = "One class is tested, receives the intervention, and is tested again. Each student is their own comparison.",
       example = "Reading comprehension is measured at the start of the semester, a new method is introduced, and it is measured again at the end.",
       problem = paste("You can see change, but some of it would have happened anyway: students mature, learn in other subjects,",
                       "or get better just from taking the pretest (the test effect). Test fatigue can push results the other way."),
       studies = "McGrew (2020) on lateral reading and Roozenbeek & van der Linden (2019) on the Bad News game began with this design as exploratory pilots.",
       page = "pp. 14, 63–64, 68", threats = c(sel = "ok", mat = "no", test = "no", base = "ok")),
  list(key = "d4", tier = "Strong", name = "Posttest-only control group (RCT)",
       rows = list(c("R", "X", "O"), c("R", "C", "O")), noneq = FALSE,
       idea = "Students are randomly assigned to the intervention or to regular teaching, and both groups take the same posttest.",
       example = "Karpicke & Blunt (2011) randomly assigned students to four study strategies. Retrieval practice beat concept mapping and rereading a week later.",
       problem = paste("Randomization makes the groups equal on average, so the estimate is unbiased. With no pretest there are no test",
                       "effects, but you also can't check that the groups started level, and the estimate is noisier."),
       studies = "Useful when a pretest would itself teach students, or tire them out.",
       page = "pp. 15, 20–21, 64–65", threats = c(sel = "ok", mat = "ok", test = "ok", base = "no")),
  list(key = "d5", tier = "Strong", name = "Pretest–posttest control group (RCT)",
       rows = list(c("R", "O", "X", "O"), c("R", "O", "C", "O")), noneq = FALSE,
       idea = "Students are randomized, everyone takes the same pretest and posttest at the same time, and only one group receives the intervention.",
       example = "Axelsson et al. (2021) on digital civic literacy workshops. Persson et al. (2020) randomized 59 classes (more than 1,200 students) to deliberative or teacher-led instruction.",
       problem = paste("This is the strongest two-group design. The pretest confirms the groups started level and, used as a covariate,",
                       "soaks up noise. In the best case you need about half as many students as in a posttest-only RCT (p. 18)."),
       studies = "Test effects still occur, but they hit both groups equally.",
       page = "pp. 15, 23–27, 65", threats = c(sel = "ok", mat = "ok", test = "ok", base = "ok")),
  list(key = "d6", tier = "Strong", name = "Factorial design",
       rows = list(c("R", "X<sub>A1B1</sub>", "O"), c("R", "X<sub>A1B2</sub>", "O"),
                   c("R", "X<sub>A2B1</sub>", "O"), c("R", "X<sub>A2B2</sub>", "O")), noneq = FALSE,
       idea = "Two or more independent variables are varied at the same time, so you can estimate each one's effect and their interaction.",
       example = "Students are randomized to role-play or lecture (factor A), crossed with small or large groups (factor B).",
       problem = paste("It answers 'which components matter, and do they depend on each other?' The cost is more groups,",
                       "and more students in each one."),
       studies = "Use the extra sliders below to give group size its own effect and an interaction.",
       page = "pp. 16, 65–66", threats = c(sel = "ok", mat = "ok", test = "ok", base = "no")),
  list(key = "d7", tier = "Quasi-experimental", name = "Pretest–posttest, no randomization",
       rows = list(c("", "O", "X", "O"), c("", "O", "C", "O")), noneq = TRUE,
       idea = "Existing classes are compared, but everyone takes a pretest, so differences at the start can be seen and adjusted for.",
       example = "Reisman's (2012) Reading Like a Historian study (236 students, five high schools). Wineburg et al. (2022) used a matched control group.",
       problem = paste("Comparing gains removes a head start, but only if both classes would otherwise have grown at the same rate.",
                       "Reliable effects need many classes and teachers."),
       studies = "Often the most practical design in ordinary schools.",
       page = "pp. 16, 21–23, 66–67", threats = c(sel = "partly", mat = "ok", test = "ok", base = "ok")),
  list(key = "d8", tier = "Quasi-experimental", name = "Interrupted time series",
       rows = list(c("O", "O", "O", "X", "O", "O", "O")), noneq = FALSE,
       idea = "One group is measured several times before and after the intervention. The trend before it shows what would have happened anyway.",
       example = "A new interdisciplinary project starts after three monthly assessments and is followed by three more.",
       problem = paste("Projecting the pre-intervention trend forward separates a real jump from ongoing growth. A naive comparison",
                       "of the last pretest with the first posttest counts one period of growth as effect."),
       studies = "More advanced versions alternate treatments within the same group over time.",
       page = "pp. 17, 67", threats = c(sel = "ok", mat = "ok", test = "partly", base = "ok")),
  list(key = "d9", tier = "Quasi-experimental", name = "Regression discontinuity",
       rows = list(c("C", "O", "X", "O"), c("C", "O", "", "O")), noneq = TRUE,
       idea = "Students are assigned by a cutoff: everyone below a diagnostic score gets the intervention.",
       example = "Students scoring below the cutoff on a diagnostic test get extra support. Those at or above it don't.",
       problem = paste("The supported students were weaker to begin with, so a naive comparison makes support look harmful.",
                       "Instead, you look for a jump in the regression lines right at the cutoff."),
       studies = "This is an ethical choice when the intervention should go to the students who need it most.",
       page = "pp. 17, 67", threats = c(sel = "ok", mat = "ok", test = "partly", base = "ok"))
)
names(DESIGNS) <- vapply(DESIGNS, `[[`, "", "key")

THREATS <- c(sel = "Prior differences between groups", mat = "Growth that happens anyway",
             test = "Test effects / fatigue", base = "Baseline is known")

SD <- 10; RHO <- 0.7; BASE <- 50; RDD_CUT <- 45

gen_two <- function(n, delta, sel, mat, randomized) {
  shift <- if (randomized) 0 else sel
  g <- c(rep(0, n), rep(1, n))
  a <- BASE + shift * g + rnorm(2 * n, 0, SD * sqrt(RHO))
  e <- SD * sqrt(1 - RHO)
  pre <- a + rnorm(2 * n, 0, e)
  post <- a + mat + delta * g + rnorm(2 * n, 0, e)
  list(g = g, pre = pre, post = post)
}

# Simulate one study with design `key`; returns list(out = estimate info, pl = payload for D3)
run_design <- function(key, p, payload = TRUE) {
  n <- p$n; delta <- p$delta; sel <- p$sel; mat <- p$mat
  out <- list(); pl <- NULL

  if (key %in% c("d1", "d2", "d3", "d4", "d5", "d7")) {
    randomized <- key %in% c("d4", "d5")
    w <- gen_two(n, delta, sel, mat, randomized)
    g <- w$g; pre <- w$pre; post <- w$post
    T <- g == 1; C <- g == 0
    if (key == "d1") {
      out <- list(est = mean(post[T]) - BASE, se = sd(post[T]) / sqrt(n),
                  label = "Class mean minus a guessed ‘typical’ score of 50",
                  bias_expl = sprintf("the class's head start (%s) and growth that happens anyway (%s)", signed(sel), signed(mat)))
    } else if (key %in% c("d2", "d4")) {
      wl <- welch(post[C], post[T])
      out <- list(est = wl[["diff"]], se = wl[["se"]], label = "Posttest difference between the groups",
                  bias_expl = if (key == "d2") sprintf("the intervention class's head start (%s)", signed(sel))
                              else "randomization made the groups equal on average")
    } else if (key == "d3") {
      gain <- post[T] - pre[T]
      out <- list(est = mean(gain), se = sd(gain) / sqrt(n), label = "Average gain from pretest to posttest",
                  bias_expl = sprintf("growth that would have happened anyway (%s)", signed(mat)))
    } else if (key == "d5") {
      fit <- ols(cbind(1, g, pre), post)
      wl <- welch(post[C], post[T])
      se <- fit$se[2]
      out <- list(est = fit$b[2], se = se, label = "Posttest difference adjusted for the pretest (ANCOVA)",
                  bias_expl = "randomization makes the groups equal, and the pretest soaks up noise",
                  extra = sprintf("Without the pretest, the same data give SE = %.2f. Adjusting for the pretest brings it down to %.2f, a %.0f%% smaller margin of error.",
                                  wl[["se"]], se, 100 * (1 - se / wl[["se"]])))
    } else {
      wl <- welch(post[C] - pre[C], post[T] - pre[T])
      out <- list(est = wl[["diff"]], se = wl[["se"]], label = "Difference in gains (treatment gain − comparison gain)",
                  bias_expl = "both classes grow at the same rate here, so comparing gains cancels the head start")
    }
    if (payload) {
      two <- key %in% c("d3", "d5", "d7")
      cols <- if (two) c("Pretest", "Posttest") else "Posttest"
      show <- if (key %in% c("d1", "d3")) list(list("Intervention class", "treat", T))
              else list(list("Comparison", "ctrl", C), list("Intervention", "treat", T))
      groups <- lapply(show, function(s) {
        m <- s[[3]]
        vals <- if (two) list(pre[m], post[m]) else list(post[m])
        pts <- do.call(rbind, lapply(seq_along(vals), function(ci)
          cbind(ci - 1, round(vals[[ci]], 1), seq_along(vals[[ci]]) - 1)))
        list(name = s[[1]], cls = s[[2]], pts = pts, means = arr(vapply(vals, mean, 0)))
      })
      tpost <- mean(post[T])
      ghost <- list(c = length(cols) - 1, y = scal(tpost - delta),
                    fromc = if (two) 0 else NA, fromy = if (two) scal(mean(pre[T])) else NA)
      pl <- list(kind = "dots", cols = I(cols), groups = groups, ghost = ghost,
                 refline = if (key == "d1") list(y = BASE, label = "guessed ‘typical’ score") else NA,
                 noneq = DESIGNS[[key]]$noneq, randomized = randomized)
    }

  } else if (key == "d6") {
    fa <- delta; fb <- p$fb; fab <- p$fab
    cells <- list(); m <- matrix(0, 2, 2)
    for (ia in 0:1) for (ib in 0:1) {
      y <- BASE + mat + fa * ia + fb * ib + fab * ia * ib + rnorm(n, 0, SD)
      m[ia + 1, ib + 1] <- mean(y)
      cells[[length(cells) + 1]] <- list(a = ia, b = ib, mean = scal(mean(y)), pts = arr(y, 1))
    }
    A <- (m[2, 1] + m[2, 2]) / 2 - (m[1, 1] + m[1, 2]) / 2
    B <- (m[1, 2] + m[2, 2]) / 2 - (m[1, 1] + m[2, 1]) / 2
    AB <- (m[2, 2] - m[1, 2]) - (m[2, 1] - m[1, 1])
    se_main <- sqrt(SD^2 * 4 * 0.25 / n)
    out <- list(est = A, se = se_main, label = "Main effect of role-play (averaged over group size)",
                bias_expl = "randomization",
                factorial = list(A = A, B = B, AB = AB, se_main = se_main, se_int = sqrt(SD^2 * 4 / n),
                                 tA = fa + fab / 2, tB = fb + fab / 2, tAB = fab))
    if (payload) pl <- list(kind = "factorial", cells = cells, alab = I(c("Lecture", "Role-play")),
                            blab = I(c("Large groups", "Small groups")))

  } else if (key == "d8") {
    waves <- 1:6
    slope <- mat / 3
    a <- BASE + rnorm(n, 0, SD * sqrt(RHO))
    e <- SD * sqrt(1 - RHO)
    Y <- outer(a, rep(1, 6)) + outer(rep(1, n), slope * (waves - 1) + delta * (waves >= 4)) + matrix(rnorm(n * 6, 0, e), n)
    mw <- colMeans(Y)
    fit <- ols(cbind(1, waves, as.numeric(waves >= 4)), mw)
    b <- fit$b
    naive <- mw[4] - mw[3]
    out <- list(est = b[3], se = fit$se[3], tcrit = qt(0.975, fit$df),
                label = "Level change beyond the projected trend",
                bias_expl = "the pre-intervention trend is projected forward",
                extra = sprintf("A naive ‘last pretest vs. first posttest’ comparison gives %s points, because it counts one period of ordinary growth (%s) as effect.",
                                signed(naive), signed(slope)))
    if (payload) pl <- list(kind = "its", waves = I(waves), means = arr(mw),
                            students = round(Y[seq_len(min(n, 14)), , drop = FALSE], 1),
                            pre_line = rbind(c(1, rr(b[1] + b[2])), c(6, rr(b[1] + b[2] * 6))),
                            post_line = rbind(c(4, rr(b[1] + b[2] * 4 + b[3])), c(6, rr(b[1] + b[2] * 6 + b[3]))),
                            jump = list(x = 4, y0 = scal(b[1] + b[2] * 4), y1 = scal(b[1] + b[2] * 4 + b[3])))

  } else if (key == "d9") {
    N <- 2 * n
    a <- BASE + rnorm(N, 0, SD * sqrt(RHO))
    e <- SD * sqrt(1 - RHO)
    pre <- a + rnorm(N, 0, e)
    T <- as.numeric(pre < RDD_CUT)
    post <- a + mat + delta * T + rnorm(N, 0, e)
    xc <- pre - RDD_CUT
    fit <- ols(cbind(1, T, xc, T * xc), post)
    b <- fit$b
    naive <- if (sum(T) > 0 && sum(T) < N) mean(post[T == 1]) - mean(post[T == 0]) else NA
    out <- list(est = b[2], se = fit$se[2], label = "Jump between the regression lines at the cutoff",
                bias_expl = "the comparison is made right at the cutoff",
                extra = sprintf("A naive comparison of supported vs. unsupported students gives %s points. Support looks harmful because the supported students started lower.",
                                signed(naive)))
    if (payload) {
      lo <- min(pre); hi <- max(pre)
      pl <- list(kind = "rdd", pts = cbind(round(pre, 1), round(post, 1), T), cut = RDD_CUT,
                 tline = rbind(c(lo, rr(b[1] + b[2] + (b[3] + b[4]) * (lo - RDD_CUT))), c(RDD_CUT, rr(b[1] + b[2]))),
                 cline = rbind(c(RDD_CUT, rr(b[1])), c(hi, rr(b[1] + b[3] * (hi - RDD_CUT)))),
                 jump = list(x = RDD_CUT, y0 = scal(b[1]), y1 = scal(b[1] + b[2])))
    }
  }
  out$exp_bias <- switch(key, d1 = sel + mat, d2 = sel, d3 = mat, 0)
  if (!is.null(pl)) pl$est <- scal(out$est)
  list(out = out, pl = pl)
}

RACE_KEYS <- c("d1", "d2", "d3", "d4", "d5", "d7", "d8", "d9")

race <- function(p, seed, reps = 150) {
  lapply(seq_along(RACE_KEYS), function(ki) {
    k <- RACE_KEYS[ki]
    ests <- with_seed(seed * 7919 + ki, vapply(seq_len(reps), function(i) run_design(k, p, FALSE)$out$est, 0))
    list(key = k, name = DESIGNS[[k]]$name, tier = DESIGNS[[k]]$tier, ests = arr(ests, 1),
         mean = scal(mean(ests)), sd = scal(sd(ests)))
  })
}

notation_html <- function(d) {
  rows_html <- character(0)
  nr <- length(d$rows)
  for (i in seq_len(nr)) {
    cells <- vapply(d$rows[[i]], function(tok) {
      if (tok == "") '<span class="tok tok-empty"></span>'
      else if (tok == "R") '<span class="tok tok-r" title="Random assignment">R</span>'
      else if (tok == "C" && d$key == "d9") '<span class="tok tok-cut" title="Assigned by cutoff score">C</span>'
      else if (tok == "O") '<span class="tok tok-o" title="Observation / test">O</span>'
      else if (tok == "C") '<span class="tok tok-c" title="Control / regular teaching">C</span>'
      else sprintf('<span class="tok tok-x" title="Intervention">%s</span>', tok)
    }, "")
    rows_html <- c(rows_html, sprintf('<div class="nrow">%s</div>', paste(cells, collapse = "")))
    if (d$noneq && i < nr) rows_html <- c(rows_html, '<div class="nsep" title="Groups not formed at random"></div>')
  }
  chips <- vapply(names(THREATS), function(k) {
    s <- d$threats[[k]]
    icon <- c(ok = "✓", no = "✕", partly = "~")[[s]]
    sprintf('<span class="chip chip-%s"><b>%s</b> %s</span>', s, icon, THREATS[[k]])
  }, "")
  paste0('<div class="notation"><div class="nlabel">Design notation</div><div class="nrows">',
         paste(rows_html, collapse = ""), '</div></div><div class="chips">', paste(chips, collapse = ""), "</div>")
}

# -----------------------------------------------------------------------------
# PRISMA 2020 flow diagram (SVG)
# -----------------------------------------------------------------------------

PRISMA_DEFAULTS <- list(
  prev_studies = 0, prev_reports = 0,
  db_text = "ERIC, 412\nScopus, 538\nWeb of Science, 301\nAPA PsycInfo, 249",
  reg_text = "",
  dupes = 412, auto_excl = 0, other_removed = 0,
  screened = 1088, screen_excl = 951,
  sought = 137, not_retrieved = 6,
  assessed = 131,
  excl_text = "Not an experimental or quasi-experimental design, 54\nNo learning outcome measured, 23\nNot a school population, 19\nFull text not in English, 6",
  oth_text = "Websites, 14\nOrganisations, 5\nCitation searching, 22",
  oth_sought = 41, oth_not_retrieved = 3, oth_assessed = 38,
  oth_excl_text = "Not an experimental design, 21\nNo learning outcome measured, 12",
  studies = 32, reports = 34
)
PRISMA_NUM <- names(PRISMA_DEFAULTS)[vapply(PRISMA_DEFAULTS, is.numeric, TRUE)]
PRISMA_TXT <- names(PRISMA_DEFAULTS)[vapply(PRISMA_DEFAULTS, is.character, TRUE)]

# "Label, 12" per line -> data.frame(label, n)
parse_lines <- function(txt) {
  lines <- trimws(strsplit(txt %||% "", "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(data.frame(label = character(0), n = numeric(0)))
  res <- lapply(lines, function(ln) {
    m <- regmatches(ln, regexec("^(.*),\\s*([0-9][0-9 ,.]*)$", ln))[[1]]
    if (length(m) == 3) {
      v <- suppressWarnings(as.numeric(gsub("[ ,]", "", m[3])))
      if (!is.na(v)) return(data.frame(label = trimws(m[2]), n = round(v)))
    }
    data.frame(label = ln, n = NA_real_)
  })
  do.call(rbind, res)
}
psum <- function(df) sum(df$n, na.rm = TRUE)

ntxt <- function(v) paste0("n = ", if (is.null(v) || length(v) == 0 || is.na(v)) "?" else fint(as.integer(v)))

wrap_text <- function(text, width_px, fs) {
  maxc <- max(8, floor(width_px / (fs * 0.53)))
  words <- strsplit(text, " ", fixed = TRUE)[[1]]
  lines <- character(0); cur <- ""
  for (w in words) {
    if (nchar(cur) + nchar(w) + (if (nzchar(cur)) 1 else 0) <= maxc) {
      cur <- if (nzchar(cur)) paste(cur, w) else w
    } else {
      if (nzchar(cur)) lines <- c(lines, cur)
      cur <- w
    }
  }
  if (nzchar(cur)) lines <- c(lines, cur)
  if (!length(lines)) "" else lines
}

prisma_svg <- function(v, show_prev, show_other) {
  FS <- 12.5; LH <- 16.5; PADX <- 10; PADY <- 9
  W <- 232; GAP <- 44; HGAP <- 56
  PH_X <- 8; PH_W <- 30; COL_HEAD <- 46

  x <- PH_X + PH_W + 20
  prev_x <- NA
  if (show_prev) { prev_x <- x; x <- x + W + HGAP }
  ml <- x; mr <- x + W + GAP
  x <- mr + W + HGAP
  ol <- orr <- NA
  if (show_other) { ol <- x; orr <- x + W + GAP; x <- orr + W } else x <- mr + W
  total_w <- x + 12

  db <- parse_lines(v$db_text); reg <- parse_lines(v$reg_text)
  excl <- parse_lines(v$excl_text); oth <- parse_lines(v$oth_text); oexcl <- parse_lines(v$oth_excl_text)
  db_total <- psum(db); reg_total <- psum(reg)

  B <- list()
  add <- function(name, xx, items) {
    # items: list of c(text, indent "0"/"1")
    lines <- list()
    for (it in items) {
      ind <- it[2] == "1"
      for (ln in wrap_text(it[1], W - 2 * PADX - (if (ind) 14 else 0), FS)) lines[[length(lines) + 1]] <- list(ln, ind)
    }
    B[[name]] <<- list(x = xx, w = W, lines = lines, h = 2 * PADY + LH * length(lines))
    name
  }
  I0 <- function(t) c(t, "0"); I1 <- function(t) c(t, "1")
  lab_items <- function(df) lapply(seq_len(nrow(df)), function(i) I1(sprintf("%s (%s)", df$label[i], ntxt(df$n[i]))))

  ident <- c(list(I0("Records identified from*:"), I1(sprintf("Databases (%s)", ntxt(db_total)))),
             if (nrow(db) > 1) lab_items(db) else list(),
             list(I1(sprintf("Registers (%s)", ntxt(reg_total)))),
             if (nrow(reg) > 1) lab_items(reg) else list())
  rows <- list()
  r1 <- c(add("ident", ml, ident),
          add("removed", mr, list(I0("Records removed before screening:"),
                                  I1(sprintf("Duplicate records removed (%s)", ntxt(v$dupes))),
                                  I1(sprintf("Records marked as ineligible by automation tools (%s)", ntxt(v$auto_excl))),
                                  I1(sprintf("Records removed for other reasons (%s)", ntxt(v$other_removed))))))
  if (show_prev) r1 <- c(r1, add("prev", prev_x, list(I0("Studies included in previous version of review"), I0(sprintf("(%s)", ntxt(v$prev_studies))),
                                                      I0(""), I0("Reports of studies included in previous version of review"),
                                                      I0(sprintf("(%s)", ntxt(v$prev_reports))))))
  if (show_other) r1 <- c(r1, add("oth_ident", ol, c(list(I0("Records identified from:")), lab_items(oth))))
  rows[[1]] <- r1
  rows[[2]] <- c(add("screened", ml, list(I0("Records screened"), I0(sprintf("(%s)", ntxt(v$screened))))),
                 add("screen_excl", mr, list(I0("Records excluded**"), I0(sprintf("(%s)", ntxt(v$screen_excl))))))
  r3 <- c(add("sought", ml, list(I0("Reports sought for retrieval"), I0(sprintf("(%s)", ntxt(v$sought))))),
          add("not_ret", mr, list(I0("Reports not retrieved"), I0(sprintf("(%s)", ntxt(v$not_retrieved))))))
  if (show_other) r3 <- c(r3, add("oth_sought", ol, list(I0("Reports sought for retrieval"), I0(sprintf("(%s)", ntxt(v$oth_sought))))),
                          add("oth_not_ret", orr, list(I0("Reports not retrieved"), I0(sprintf("(%s)", ntxt(v$oth_not_retrieved))))))
  rows[[3]] <- r3
  r4 <- c(add("assessed", ml, list(I0("Reports assessed for eligibility"), I0(sprintf("(%s)", ntxt(v$assessed))))),
          add("excl", mr, c(list(I0("Reports excluded:")), lab_items(excl))))
  if (show_other) r4 <- c(r4, add("oth_assessed", ol, list(I0("Reports assessed for eligibility"), I0(sprintf("(%s)", ntxt(v$oth_assessed))))),
                          add("oth_excl", orr, c(list(I0("Reports excluded:")), lab_items(oexcl))))
  rows[[4]] <- r4
  new_lab <- if (show_prev) "New studies included in review" else "Studies included in review"
  rep_lab <- if (show_prev) "Reports of new included studies" else "Reports of included studies"
  rows[[5]] <- add("included", ml, list(I0(new_lab), I0(sprintf("(%s)", ntxt(v$studies))), I0(""),
                                        I0(rep_lab), I0(sprintf("(%s)", ntxt(v$reports)))))
  if (show_prev) {
    z <- function(a) if (is.null(a) || is.na(a)) 0 else a
    rows[[6]] <- add("total", ml, list(I0("Total studies included in review"), I0(sprintf("(%s)", ntxt(z(v$prev_studies) + z(v$studies)))),
                                       I0(""), I0("Reports of total included studies"),
                                       I0(sprintf("(%s)", ntxt(z(v$prev_reports) + z(v$reports))))))
  }

  y <- 12 + COL_HEAD + 22
  row_y <- list()
  for (ri in seq_along(rows)) {
    row <- rows[[ri]]
    h <- max(vapply(row, function(nm) B[[nm]]$h, 0))
    for (nm in row) { B[[nm]]$y <- y; if (nm != "prev") B[[nm]]$h <- h }
    row_y[[ri]] <- c(y, h)
    y <- y + h + 34
  }
  total_h <- y - 34 + 40

  S <- character(0)
  add_s <- function(...) S <<- c(S, paste0(...))
  add_s(sprintf('<svg xmlns="http://www.w3.org/2000/svg" id="prisma-svg" viewBox="0 0 %s %s" width="%s" height="%s" font-family="Arial, Helvetica, sans-serif" font-size="%s">',
                total_w, total_h, total_w, total_h, FS))
  add_s('<defs><marker id="ah" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="#222"/></marker></defs>')
  add_s(sprintf('<rect width="%s" height="%s" fill="#ffffff"/>', total_w, total_h))

  header <- function(x0, x1, text, fill) {
    add_s(sprintf('<rect x="%s" y="12" width="%s" height="%s" rx="8" fill="%s" stroke="#222" stroke-width="1"/>', x0, x1 - x0, COL_HEAD, fill))
    lines <- wrap_text(text, x1 - x0 - 20, 13.5)
    y0 <- 12 + COL_HEAD / 2 - (length(lines) - 1) * 8 + 5
    for (i in seq_along(lines))
      add_s(sprintf('<text x="%s" y="%s" text-anchor="middle" font-weight="bold" font-size="13.5">%s</text>', (x0 + x1) / 2, y0 + (i - 1) * 16, esc(lines[i])))
  }
  if (show_prev) header(prev_x, prev_x + W, "Previous studies", "#d9d9d9")
  header(ml, mr + W, if (show_prev) "Identification of new studies via databases and registers" else "Identification of studies via databases and registers", "#ffc000")
  if (show_other) header(ol, orr + W, if (show_prev) "Identification of new studies via other methods" else "Identification of studies via other methods", "#d9d9d9")

  phase <- function(y0, y1, text) {
    add_s(sprintf('<rect x="%s" y="%s" width="%s" height="%s" rx="8" fill="#9dc3e6" stroke="#222" stroke-width="1"/>', PH_X, y0, PH_W, y1 - y0))
    cx <- PH_X + PH_W / 2; cy <- (y0 + y1) / 2
    add_s(sprintf('<text x="%s" y="%s" transform="rotate(-90 %s %s)" text-anchor="middle" dominant-baseline="central" font-weight="bold" font-size="13.5">%s</text>', cx, cy, cx, cy, text))
  }
  r1y <- row_y[[1]]
  phase(r1y[1], r1y[1] + max(r1y[2], if (show_prev) B$prev$h else 0), "Identification")
  phase(row_y[[2]][1], row_y[[4]][1] + row_y[[4]][2], "Screening")
  last <- row_y[[length(row_y)]]
  phase(row_y[[5]][1], last[1] + last[2], "Included")

  centered <- c("screened", "screen_excl", "sought", "not_ret", "assessed", "included", "total", "prev",
                "oth_sought", "oth_not_ret", "oth_assessed")
  for (nm in names(B)) {
    b <- B[[nm]]
    add_s(sprintf('<rect x="%s" y="%s" width="%s" height="%s" fill="#ffffff" stroke="#222" stroke-width="1.2"/>', b$x, b$y, b$w, b$h))
    ty <- b$y + PADY + 12
    for (ln in b$lines) {
      if (nm %in% centered) add_s(sprintf('<text x="%s" y="%s" text-anchor="middle">%s</text>', b$x + b$w / 2, ty, esc(ln[[1]])))
      else add_s(sprintf('<text x="%s" y="%s">%s</text>', b$x + PADX + (if (ln[[2]]) 14 else 0), ty, esc(ln[[1]])))
      ty <- ty + LH
    }
  }
  arrow <- function(x0, y0, x1, y1)
    add_s(sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#222" stroke-width="1.3" marker-end="url(#ah)"/>', x0, y0, x1, y1))
  poly <- function(pts)
    add_s(sprintf('<polyline points="%s" fill="none" stroke="#222" stroke-width="1.3" marker-end="url(#ah)"/>',
                  paste(vapply(pts, function(p) paste0(p[1], ",", p[2]), ""), collapse = " ")))
  right <- function(a, b) { A <- B[[a]]; Bb <- B[[b]]; yy <- A$y + A$h / 2; arrow(A$x + A$w, yy, Bb$x - 1, yy) }
  down <- function(a, b) { A <- B[[a]]; Bb <- B[[b]]; xx <- A$x + A$w / 2; arrow(xx, A$y + A$h, xx, Bb$y - 1) }

  right("ident", "removed"); down("ident", "screened"); right("screened", "screen_excl")
  down("screened", "sought"); right("sought", "not_ret"); down("sought", "assessed")
  right("assessed", "excl"); down("assessed", "included")
  if (show_other) {
    down("oth_ident", "oth_sought"); right("oth_sought", "oth_not_ret")
    down("oth_sought", "oth_assessed"); right("oth_assessed", "oth_excl")
    a_ <- B$oth_assessed; inc <- B$included
    xx <- a_$x + a_$w / 2; yy <- inc$y + inc$h / 2
    poly(list(c(xx, a_$y + a_$h), c(xx, yy), c(inc$x + inc$w + 1, yy)))
  }
  if (show_prev) {
    down("included", "total")
    pv <- B$prev; tot <- B$total
    xx <- pv$x + pv$w / 2; yy <- tot$y + tot$h / 2
    poly(list(c(xx, pv$y + pv$h), c(xx, yy), c(tot$x - 1, yy)))
  }
  fy <- total_h - 22
  add_s(sprintf('<text x="%s" y="%s" font-size="10.5" fill="#333">*Consider, if feasible to do so, reporting the number of records identified from each database or register searched (rather than the total number across all databases/registers).</text>', PH_X, fy))
  add_s(sprintf('<text x="%s" y="%s" font-size="10.5" fill="#333">**If automation tools were used, indicate how many records were excluded by a human and how many were excluded by automation tools.</text>', PH_X, fy + 13))
  add_s("</svg>")
  list(svg = paste(S, collapse = ""),
       tot = list(db_total = db_total, reg_total = reg_total, excl_total = psum(excl),
                  oth_total = psum(oth), oexcl_total = psum(oexcl)))
}

# -----------------------------------------------------------------------------
# Distributions
# -----------------------------------------------------------------------------

DISTS <- list(
  norm = list(label = "Normal", domain = c(-10, 10), discrete = FALSE),
  t = list(label = "Student's t", domain = c(-6, 6), discrete = FALSE),
  chi2 = list(label = "Chi-square (χ²)", domain = c(0, 40), discrete = FALSE),
  f = list(label = "F", domain = c(0, 6), discrete = FALSE),
  binom = list(label = "Binomial", domain = c(0, 50), discrete = TRUE),
  poisson = list(label = "Poisson", domain = c(0, 30), discrete = TRUE),
  unif = list(label = "Uniform", domain = c(0, 10), discrete = FALSE),
  expon = list(label = "Exponential", domain = c(0, 10), discrete = FALSE)
)

DIST_NOTES <- c(
  norm = "The bell curve behind z-scores and the 68–95–99.7 rule (Nygren, Fig. 6.1). Many test-score distributions are roughly normal.",
  t = "Used by t-tests. Heavier tails than the normal when samples are small, and almost identical to it once df passes about 30. Compare with the dashed normal.",
  chi2 = "The reference distribution for the χ² test of independence, used with counts such as correct vs. incorrect by group.",
  f = "Used by ANOVA: the ratio of between-group (systematic) to within-group (random) variance.",
  binom = "The number of correct answers out of n items when each is correct with probability p.",
  poisson = "Counts of events per period, such as hours absent or questions asked in a lesson.",
  unif = "Every value between a and b is equally likely. Useful as the shape of p-values when H₀ is true.",
  expon = "Waiting times, such as minutes until a student asks for help. Strongly right-skewed."
)

# d/p/r functions plus mean and variance for each distribution
dist_obj <- function(key, P) {
  switch(key,
    norm = list(d = function(x) dnorm(x, P$mu, P$sigma), p = function(x) pnorm(x, P$mu, P$sigma),
                r = function(k) rnorm(k, P$mu, P$sigma), mean = P$mu, var = P$sigma^2),
    t = list(d = function(x) dt(x, P$df), p = function(x) pt(x, P$df), r = function(k) rt(k, P$df),
             mean = if (P$df > 1) 0 else NA, var = if (P$df > 2) P$df / (P$df - 2) else NA),
    chi2 = list(d = function(x) dchisq(x, P$df), p = function(x) pchisq(x, P$df), r = function(k) rchisq(k, P$df),
                mean = P$df, var = 2 * P$df),
    f = list(d = function(x) df(x, P$d1, P$d2), p = function(x) pf(x, P$d1, P$d2), r = function(k) rf(k, P$d1, P$d2),
             mean = if (P$d2 > 2) P$d2 / (P$d2 - 2) else NA,
             var = if (P$d2 > 4) 2 * P$d2^2 * (P$d1 + P$d2 - 2) / (P$d1 * (P$d2 - 2)^2 * (P$d2 - 4)) else NA),
    binom = list(d = function(x) dbinom(x, P$bn, P$bp), p = function(x) pbinom(x, P$bn, P$bp),
                 r = function(k) rbinom(k, P$bn, P$bp), mean = P$bn * P$bp, var = P$bn * P$bp * (1 - P$bp)),
    poisson = list(d = function(x) dpois(x, P$lam), p = function(x) ppois(x, P$lam), r = function(k) rpois(k, P$lam),
                   mean = P$lam, var = P$lam),
    unif = {
      lo <- min(P$ua, P$ub); hi <- max(P$ua, P$ub); if (hi - lo < 0.1) hi <- lo + 0.1
      list(d = function(x) dunif(x, lo, hi), p = function(x) punif(x, lo, hi), r = function(k) runif(k, lo, hi),
           mean = (lo + hi) / 2, var = (hi - lo)^2 / 12)
    },
    expon = list(d = function(x) dexp(x, P$rate), p = function(x) pexp(x, P$rate), r = function(k) rexp(k, P$rate),
                 mean = 1 / P$rate, var = 1 / P$rate^2)
  )
}

# -----------------------------------------------------------------------------
# Central limit theorem populations
# -----------------------------------------------------------------------------

make_pops <- function() {
  with_seed(2026, {
    N <- 20000
    clip <- function(x) pmin(pmax(x, 0), 100)
    mix <- runif(N) < 0.5
    pops <- list(
      scores = list(label = "Test scores (bell-shaped)", vals = clip(rnorm(N, 65, 12)), domain = c(0, 100), discrete = FALSE),
      homework = list(label = "Minutes of homework (right-skewed)", vals = clip(rgamma(N, 1.4, scale = 14)), domain = c(0, 100), discrete = FALSE),
      likert = list(label = "A Likert item, 1–5 (lumpy)", vals = as.numeric(sample(1:5, N, TRUE, prob = c(.06, .09, .15, .42, .28))),
                    domain = c(0.5, 5.5), discrete = TRUE),
      bimodal = list(label = "Two very different classes (bimodal)", vals = clip(ifelse(mix, rnorm(N, 32, 8), rnorm(N, 76, 8))),
                     domain = c(0, 100), discrete = FALSE),
      uniform = list(label = "Uniform, 0–100 (flat)", vals = runif(N, 0, 100), domain = c(0, 100), discrete = FALSE)
    )
    lapply(pops, function(pp) {
      edges <- if (pp$discrete) seq(0.5, 5.5, by = 1) else seq(pp$domain[1], pp$domain[2], length.out = 41)
      h <- hist(pp$vals, breaks = edges, plot = FALSE)$density
      pp$hist <- cbind(rr(head(edges, -1)), rr(edges[-1]), rr(h, 5))
      pp$mu <- mean(pp$vals)
      pp$sigma <- sqrt(mean((pp$vals - pp$mu)^2))
      pp
    })
  })
}
POPS <- make_pops()

# -----------------------------------------------------------------------------
# UI helpers
# -----------------------------------------------------------------------------

slider <- function(id, label, mn, mx, val, step = 1) sliderInput(id, label, min = mn, max = mx, value = val, step = step, ticks = FALSE)
fig <- function(id, cls = "fig") div(id = id, class = cls)
kicker <- function(t) div(class = "kicker", t)
fig_head <- function(title, sub) div(class = "fig-head", span(class = "fig-title", title), span(class = "fig-sub", sub))
step_div <- function(key, ..., cls = "") div(id = paste0("step-", key), class = paste("step", cls), `data-step` = key, ...)
mini <- function(t) span(class = "mini-label", t)
btn <- function(id, label, cls = "btn-ghost") actionButton(id, label, class = cls)

design_step <- function(d) {
  tier_cls <- tolower(strsplit(d$tier, "-")[[1]][1])
  step_div(d$key,
    div(class = "step-meta", span(class = paste0("tier tier-", tier_cls), d$tier), span(class = "pageref", d$page)),
    h3(d$name), p(d$idea),
    div(class = "example", mini("In the classroom"), p(d$example)),
    p(d$problem),
    p(class = "muted small", d$studies))
}

cards <- list(
  c("01", "Research designs", "Scroll through nine designs, from weak to strong, and watch each one try to recover a known true effect.", "Designs"),
  c("02", "PRISMA 2020", "Build a publication-ready PRISMA 2020 flow diagram, check its arithmetic, and export SVG or PNG.", "PRISMA 2020"),
  c("03", "Cohen's d & power", "See what d = 0.5 looks like as two overlapping classes, and how many students you need to detect it.", "Effect size"),
  c("04", "p-values", "Run hundreds of simulated classroom experiments and see how p-values behave with and without a real effect.", "p-values"),
  c("05", "Chi-square", "Enter a contingency table, compare observed with expected counts, and find X² on its distribution.", "Chi-square"),
  c("06", "Distributions", "Normal, t, χ², F, binomial and more: shade probabilities and draw samples.", "Distributions"),
  c("07", "Central limit theorem", "Draw samples from skewed and lumpy populations and watch the sample means turn into a bell curve.", "CLT")
)

home <- nav_panel("Home", value = "Home",
  div(class = "home wrap",
    div(class = "hero-text",
      kicker("An interactive companion for educational research methods"),
      h1(HTML("Seeing <em>research design</em>")),
      p(class = "lede", "How do we know a teaching method works? These interactive figures cover the designs in Nygren's ",
        em("Experimental methods and statistics for educational research"),
        " (2026), the PRISMA 2020 flow diagram for systematic reviews, and the statistics students need to read an experiment: ",
        "effect sizes, p-values, χ², distributions, and the central limit theorem.")),
    div(class = "cards", lapply(cards, function(cd)
      tags$a(class = "card", href = "#", `data-goto` = cd[4],
             div(span(class = "card-num", cd[1]), h4(cd[2]), p(cd[3]))))),
    div(class = "credits",
      p(class = "small muted", "Design descriptions are paraphrased, with page references, from Nygren, T. (2026). ",
        em("Experimental methods and statistics for educational research: With examples from teaching and learning in history and social studies."),
        " Uppsala University Publications (CC BY-NC-ND). PRISMA 2020: Page et al. (2021), ", em("BMJ, 372"),
        ", n71. All data in this app are simulated or illustrative."))))

designs_tab <- nav_panel("Designs", value = "Designs",
  div(class = "story wrap",
    div(class = "story-col",
      step_div("intro", cls = "intro",
        kicker("Nygren (2026), Chapters 1 & 5"),
        h2("From weak to strong designs"),
        p("Every design here studies the same simulated world. A new teaching method raises test scores by a ",
          strong("true effect"), " that you set. Some classes start ahead of others, and every class grows a little over time anyway."),
        p("In real research the true effect is never known. Here it is, so you can see which designs recover it and which are fooled. ",
          "Scroll down: the figure on the right follows along."),
        p(mini("Read the figure: "), "dots are students, ", span(class = "c-treat", "teal"), " is the intervention group, ",
          span(class = "c-ctrl", "grey"), " is the comparison group, and the ", span(class = "c-ghost", "dashed ghost"),
          " is the counterfactual: where the intervention group would have ended up without the intervention.")),
      lapply(DESIGNS, design_step),
      step_div("race", cls = "race",
        kicker("Putting it together"),
        h3("Run every design 150 times"),
        p("One study is one draw. Repeat each design many times in the same world and two things appear: ",
          strong("bias"), " (does the cloud center on the true effect?) and ", strong("precision"), " (how wide is the cloud?)."),
        p("Try a large head start with no growth, then the reverse. Randomized designs stay centered on the truth either way, ",
          "and the pretest–posttest RCT has the tightest cloud. Raising n tightens every cloud but cannot remove bias."),
        p(class = "muted small", "Nygren (p. 18): for d = 0.5 you need roughly 60–70 students per group for 80% power. The power figure under ",
          em("Effect size"), " lets you check this.")),
      div(class = "step-spacer")),
    div(class = "fig-col sticky",
      div(class = "sticky-inner",
        uiOutput("design_header"),
        fig("design-chart", "fig fig-design"),
        uiOutput("design_readout"),
        div(class = "world",
          div(class = "world-head", mini("Simulated world"), actionButton("design_rerun", "↻ New sample", class = "btn-ghost btn-sm")),
          div(class = "world-grid",
            slider("w_delta", "True effect (points)", -5, 15, 5, 0.5),
            slider("w_sel", "Head start of intervention class (non-random designs)", -10, 10, 6, 0.5),
            slider("w_mat", "Growth that happens anyway", 0, 12, 6, 0.5),
            slider("w_n", "Students per group", 10, 150, 30, 5)),
          conditionalPanel("input.design_step === 'd6'",
            div(class = "world-grid",
              slider("w_fb", "Effect of small groups", -10, 10, 3, 0.5),
              slider("w_fab", "Interaction (role-play × small)", -10, 10, 4, 0.5))),
          p(class = "small muted", "Scores have SD = 10, so a 5-point effect is d ≈ 0.5."))))))

prisma_tab <- nav_panel("PRISMA 2020", value = "PRISMA 2020",
  div(class = "tool wrap",
    div(class = "ctrl-col",
      kicker("Systematic reviews"),
      h2("PRISMA 2020 flow diagram"),
      p("Fill in the counts from your search and screening. The diagram follows the official PRISMA 2020 template (Page et al., 2021) ",
        "and redraws as you type. Database and exclusion-reason boxes take one ", code("Label, n"), " per line."),
      div(class = "switches",
        input_switch("pr_prev", "Updated review (include previous studies)", FALSE),
        input_switch("pr_other", "Include other methods (websites, citation searching…)", TRUE)),
      accordion(id = "pr_acc", open = c("Identification", "Screening"), multiple = TRUE,
        accordion_panel("Identification",
          textAreaInput("pr_db_text", "Databases (Label, n per line)", PRISMA_DEFAULTS$db_text, rows = 4),
          textAreaInput("pr_reg_text", "Registers (Label, n per line)", PRISMA_DEFAULTS$reg_text, rows = 2, placeholder = "e.g. ClinicalTrials.gov, 4"),
          div(class = "grid3",
            numericInput("pr_dupes", "Duplicates removed", PRISMA_DEFAULTS$dupes, min = 0),
            numericInput("pr_auto_excl", "Removed by automation", PRISMA_DEFAULTS$auto_excl, min = 0),
            numericInput("pr_other_removed", "Removed, other reasons", PRISMA_DEFAULTS$other_removed, min = 0)),
          conditionalPanel("input.pr_prev",
            div(class = "grid2",
              numericInput("pr_prev_studies", "Studies in previous version", PRISMA_DEFAULTS$prev_studies, min = 0),
              numericInput("pr_prev_reports", "Reports in previous version", PRISMA_DEFAULTS$prev_reports, min = 0)))),
        accordion_panel("Screening",
          div(class = "grid2",
            numericInput("pr_screened", "Records screened", PRISMA_DEFAULTS$screened, min = 0),
            numericInput("pr_screen_excl", "Records excluded", PRISMA_DEFAULTS$screen_excl, min = 0)),
          div(class = "grid2",
            numericInput("pr_sought", "Reports sought", PRISMA_DEFAULTS$sought, min = 0),
            numericInput("pr_not_retrieved", "Not retrieved", PRISMA_DEFAULTS$not_retrieved, min = 0)),
          numericInput("pr_assessed", "Reports assessed for eligibility", PRISMA_DEFAULTS$assessed, min = 0),
          textAreaInput("pr_excl_text", "Reports excluded, with reasons (Reason, n per line)", PRISMA_DEFAULTS$excl_text, rows = 4)),
        accordion_panel("Other methods",
          textAreaInput("pr_oth_text", "Records identified from (Source, n per line)", PRISMA_DEFAULTS$oth_text, rows = 3),
          div(class = "grid2",
            numericInput("pr_oth_sought", "Reports sought", PRISMA_DEFAULTS$oth_sought, min = 0),
            numericInput("pr_oth_not_retrieved", "Not retrieved", PRISMA_DEFAULTS$oth_not_retrieved, min = 0)),
          numericInput("pr_oth_assessed", "Reports assessed for eligibility", PRISMA_DEFAULTS$oth_assessed, min = 0),
          textAreaInput("pr_oth_excl_text", "Reports excluded, with reasons", PRISMA_DEFAULTS$oth_excl_text, rows = 3)),
        accordion_panel("Included",
          div(class = "grid2",
            numericInput("pr_studies", "Studies included", PRISMA_DEFAULTS$studies, min = 0),
            numericInput("pr_reports", "Reports of included studies", PRISMA_DEFAULTS$reports, min = 0))),
        accordion_panel("Save / load",
          p(class = "small muted", "Save your numbers as a CSV and load them again later."),
          downloadButton("pr_csv", "Download values (CSV)", class = "btn-ghost btn-sm"),
          fileInput("pr_upload", NULL, accept = ".csv", buttonLabel = "Load CSV…", placeholder = "No file"),
          actionButton("pr_reset", "Reset to example", class = "btn-ghost btn-sm")))),
    div(class = "fig-col wide",
      div(class = "sticky-inner",
        div(class = "toolbar",
          actionButton("pr_fix", "Fix the arithmetic", class = "btn-ghost btn-sm"),
          tags$button(class = "btn btn-ghost btn-sm", onclick = "ERL.downloadPrisma('svg')", "Download SVG"),
          tags$button(class = "btn btn-primary btn-sm", onclick = "ERL.downloadPrisma('png')", "Download PNG")),
        uiOutput("prisma_checks"),
        div(class = "prisma-paper", uiOutput("prisma_out")),
        p(class = "small muted", "From: Page MJ, McKenzie JE, Bossuyt PM, et al. The PRISMA 2020 statement: an updated guideline for reporting systematic reviews. BMJ 2021;372:n71. Diagram template is CC BY 4.0.")))))

effect_tab <- nav_panel("Effect size", value = "Effect size",
  div(class = "tool wrap",
    div(class = "ctrl-col",
      kicker("Cohen's d"),
      h2("What does an effect of d look like?"),
      p("Cohen's d is the difference between two group means measured in standard deviations. d = 0.5 means the average student ",
        "in the intervention group scores half an SD above the average control student."),
      slider("cd_d", "Cohen's d", 0, 2.5, 0.5, 0.01),
      slider("cd_sd", "Your test's SD (points)", 1, 25, 10, 1),
      slider("cd_cer", "Control group success rate (for NNT)", 0.05, 0.95, 0.2, 0.01),
      div(class = "note", mini("Benchmarks"),
        p("Cohen (1988): 0.2 small · 0.5 medium · 0.8 large. Kraft (2020), for field experiments in schools: < 0.05 small · 0.05–0.20 medium · ≥ 0.20 large. ",
          "Nygren (p. 86) notes that effects below 0.20 can matter if the intervention is cheap and reaches many students.")),
      tags$hr(),
      kicker("Power"),
      h3("How many students do you need?"),
      p("Power is the chance that a study detects an effect that really exists. Smaller effects need many more students (Nygren, pp. 17–18, 85)."),
      slider("cd_n", "Students per group", 5, 500, 30, 1),
      radioButtons("cd_alpha", "α (two-tailed)", c(".05" = "0.05", ".01" = "0.01", ".10" = "0.1"), selected = "0.05", inline = TRUE),
      slider("cd_rho", "Pretest–posttest correlation (if pretest used)", 0, 0.9, 0.6, 0.05)),
    div(class = "fig-col",
      div(class = "sticky-inner",
        fig("cohen-chart"),
        uiOutput("cohen_tiles"),
        fig_head("Statistical power", "two independent groups, two-tailed t-test"),
        fig("power-chart"),
        uiOutput("power_tiles")))))

pv_tab <- nav_panel("p-values", value = "p-values",
  div(class = "tool wrap",
    div(class = "ctrl-col",
      kicker("Hypothesis testing"),
      h2("Watch p-values dance"),
      p("Each click runs simulated classroom experiments: two groups, a t-test, a p-value. ",
        "The p-value is the probability of a difference at least this extreme ", em("if H₀ were true"), " (Nygren, p. 83)."),
      slider("pv_d", "True effect (Cohen's d)", 0, 1.5, 0.5, 0.05),
      slider("pv_n", "Students per group", 5, 200, 30, 1),
      radioButtons("pv_alpha", "α", c(".05" = "0.05", ".01" = "0.01", ".10" = "0.1"), selected = "0.05", inline = TRUE),
      div(class = "btn-row",
        btn("pv_1", "Run 1 study", "btn-primary"), btn("pv_100", "+100"), btn("pv_1000", "+1000"), btn("pv_reset", "Reset", "btn-link")),
      uiOutput("pv_stats"),
      div(class = "note", mini("Things to try"),
        tags$ul(
          tags$li("Set d = 0 and run 1000 studies. The p-values spread out flat, and about α of them are 'significant' by chance alone: Type I errors."),
          tags$li("Set d = 0.5 with n = 20. Most studies miss a real effect: Type II errors. Then raise n to 64."),
          tags$li("Watch the confidence intervals below: same truth, very different stories from study to study.")))),
    div(class = "fig-col",
      div(class = "sticky-inner",
        fig_head("This study's t on the H₀ distribution", "shaded area = p"),
        fig("p-tdist"),
        fig_head("Distribution of p-values", "across all simulated studies"),
        fig("p-hist"),
        fig_head("The dance of the confidence intervals", "last 25 studies · 95% CI of d"),
        fig("p-dance")))))

chi_tab <- nav_panel("Chi-square", value = "Chi-square",
  div(class = "tool wrap",
    div(class = "ctrl-col",
      kicker("Categorical outcomes"),
      h2("The χ² test of independence"),
      p("Is the outcome category (for example, judging a fake post correctly or not) independent of the group? ",
        "χ² adds up how far each observed count is from the count expected if the groups didn't differ."),
      div(class = "grid2",
        selectInput("cs_rows", "Rows", c("2", "3"), selected = "2"),
        selectInput("cs_cols", "Columns", c("2", "3"), selected = "2")),
      uiOutput("cs_grid"),
      input_switch("cs_yates", "Yates continuity correction (2×2 only)", FALSE),
      slider("cs_mult", "Multiply every count by", 0.25, 10, 1, 0.25),
      div(class = "note", mini("Try this"),
        p("Move the multiplier. The proportions stay the same, so the effect size (Cramér's V) stays the same, but X² and p change a lot. ",
          "Statistical significance is not the same as importance.")),
      p(class = "small muted", "Example counts are illustrative and do not come from a real study.")),
    div(class = "fig-col",
      div(class = "sticky-inner",
        fig_head("Observed vs. expected counts", "outlined bars = expected under independence · labels = residuals"),
        fig("chi-bars"),
        uiOutput("cs_tiles"),
        fig_head("Where X² falls", "χ² distribution with the table's df"),
        fig("chi-dist")))))

dist_choices <- setNames(names(DISTS), vapply(DISTS, `[[`, "", "label"))
dist_tab <- nav_panel("Distributions", value = "Distributions",
  div(class = "tool wrap",
    div(class = "ctrl-col",
      kicker("Probability distributions"),
      h2("A field guide to distributions"),
      selectInput("ds_dist", "Distribution", dist_choices, selected = "norm"),
      uiOutput("ds_note"),
      conditionalPanel("input.ds_dist === 'norm'", slider("ds_mu", "Mean μ", -5, 5, 0, 0.1), slider("ds_sigma", "SD σ", 0.3, 4, 1, 0.1)),
      conditionalPanel("input.ds_dist === 't' || input.ds_dist === 'chi2'", slider("ds_df", "Degrees of freedom", 1, 40, 5, 1)),
      conditionalPanel("input.ds_dist === 'f'", slider("ds_d1", "df₁ (between groups)", 1, 30, 3, 1), slider("ds_d2", "df₂ (within groups)", 1, 100, 30, 1)),
      conditionalPanel("input.ds_dist === 'binom'", slider("ds_bn", "Number of items n", 1, 50, 20, 1), slider("ds_bp", "P(correct) p", 0.01, 0.99, 0.6, 0.01)),
      conditionalPanel("input.ds_dist === 'poisson'", slider("ds_lam", "Rate λ", 0.2, 20, 4, 0.1)),
      conditionalPanel("input.ds_dist === 'unif'", slider("ds_ua", "Lower a", 0, 10, 2, 0.1), slider("ds_ub", "Upper b", 0, 10, 8, 0.1)),
      conditionalPanel("input.ds_dist === 'expon'", slider("ds_rate", "Rate λ", 0.1, 3, 0.5, 0.05)),
      sliderInput("ds_range", "Shade P(a ≤ X ≤ b)", min = -10, max = 10, value = c(-1, 1), step = 0.1, ticks = FALSE),
      radioButtons("ds_mode", NULL, c("Density / mass" = "pdf", "Cumulative (CDF)" = "cdf"), selected = "pdf", inline = TRUE),
      div(class = "btn-row",
        btn("ds_1", "Draw 1", "btn-primary"), btn("ds_50", "Draw 50"), btn("ds_1000", "+1000"), btn("ds_reset", "Clear", "btn-link"))),
    div(class = "fig-col",
      div(class = "sticky-inner", fig("dist-chart"), uiOutput("ds_tiles")))))

clt_tab <- nav_panel("CLT", value = "CLT",
  div(class = "tool wrap",
    div(class = "ctrl-col",
      kicker("Central limit theorem"),
      h2("Why means become normal"),
      p("Choose a population, however odd its shape. Draw a sample of n students and record its mean. Repeat many times. ",
        "The ", strong("sampling distribution"), " of those means becomes bell-shaped, and its spread shrinks as ",
        span(class = "mono", "σ/√n"), ". This is why t-tests work on skewed classroom data and why bigger samples give more precise estimates."),
      selectInput("clt_pop", "Population", setNames(names(POPS), vapply(POPS, `[[`, "", "label")), selected = "homework"),
      slider("clt_n", "Sample size n", 1, 100, 5, 1),
      div(class = "btn-row",
        btn("clt_1", "Draw 1 sample", "btn-primary"), btn("clt_25", "+25"), btn("clt_1000", "+1000"), btn("clt_reset", "Reset", "btn-link")),
      input_switch("clt_normal", "Overlay the normal curve the CLT predicts", TRUE),
      uiOutput("clt_stats"),
      div(class = "note", mini("Things to try"),
        tags$ul(
          tags$li("Bimodal population with n = 1: the means look just like the population. Raise n to 2, then 10, then 30."),
          tags$li("The Likert item: even 1–5 ratings give near-normal means by n ≈ 15."),
          tags$li("Compare the SD of the means with σ/√n. Quadrupling n halves the spread.")))),
    div(class = "fig-col", div(class = "sticky-inner", fig("clt-chart")))))

ui <- page_navbar(
  home, designs_tab, prisma_tab, effect_tab, pv_tab, chi_tab, dist_tab, clt_tab,
  title = tags$span(class = "brand", tags$span(class = "brand-mark", "◐"), " Seeing Research Design"),
  id = "nav",
  theme = bs_theme(version = 5),
  fillable = FALSE,
  window_title = "Seeing Research Design",
  header = tagList(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Newsreader:ital,opsz,wght@0,6..72,400;0,6..72,500;0,6..72,600;1,6..72,400&family=IBM+Plex+Sans:wght@400;500;600&family=IBM+Plex+Mono:wght@400;500&display=swap"),
    includeCSS("style.css"),
    includeScript("d3.min.js"),
    includeScript("viz.js")
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

tiles_html <- function(tiles) {
  # tiles: list of c(label, number, sub, accent "0"/"1")
  HTML(paste0('<div class="tiles">', paste(vapply(tiles, function(t)
    sprintf('<div class="tile%s"><span class="tile-label">%s</span><span class="tile-num">%s</span><span class="tile-sub">%s</span></div>',
            if (length(t) > 3 && t[4] == "1") " accent" else "", t[1], t[2], t[3]), ""), collapse = ""), "</div>"))
}

power_two <- function(d, n, alpha, rho = 0) {
  n <- as.numeric(n)
  dfv <- 2 * n - 2 - (if (rho > 0) 1 else 0)
  ncp <- d * sqrt(n / 2) / sqrt(1 - rho^2)
  tc <- qt(1 - alpha / 2, dfv)
  pw <- suppressWarnings(1 - pt(tc, dfv, ncp) + pt(-tc, dfv, ncp))
  # pt() with a large ncp can lose precision; fall back to the normal approximation
  z <- qnorm(1 - alpha / 2)
  approx <- pnorm(ncp - z) + pnorm(-ncp - z)
  pw <- ifelse(is.finite(pw), pw, approx)
  pmin(pmax(pw, 0), 1)
}

n_needed <- function(d, alpha, rho = 0, target = 0.8) {
  if (d <= 0) return(NA)
  lo <- 2; hi <- 100000
  while (lo < hi) {
    mid <- (lo + hi) %/% 2
    if (power_two(d, mid, alpha, rho) >= target) hi <- mid else lo <- mid + 1
  }
  lo
}

server <- function(input, output, session) {

  # ======================= Designs =======================
  seed <- reactiveVal(11)
  observeEvent(input$design_rerun, seed(seed() + 1))

  world <- reactive(list(n = as.integer(input$w_n), delta = input$w_delta, sel = input$w_sel,
                         mat = input$w_mat, fb = input$w_fb, fab = input$w_fab))
  cur_step <- reactive(input$design_step %||% "intro")

  design_result <- reactive({
    key <- cur_step(); p <- world()
    if (key == "intro") {
      return(list(msg = list(key = key, payload = list(kind = "ladder",
        designs = lapply(DESIGNS, function(d) list(key = d$key, name = d$name, tier = d$tier)) |> unname(),
        tiers = I(TIER_ORDER))), est = NULL))
    }
    if (key == "race") {
      rows <- race(p, seed())
      return(list(msg = list(key = key, payload = list(kind = "race", rows = rows, truth = p$delta)), est = rows))
    }
    res <- with_seed(seed(), run_design(key, p))
    res$pl$truth <- p$delta
    list(msg = list(key = key, payload = res$pl), est = res$out)
  })

  observe(session$sendCustomMessage("erl_design", design_result()$msg))

  output$design_header <- renderUI({
    key <- cur_step()
    if (key == "intro") return(fig_head("The design ladder", "Click a design to jump to it"))
    if (key == "race") return(fig_head("150 replications of each design", "each dot is one study's estimate · line = true effect · ◆ = average"))
    d <- DESIGNS[[key]]
    div(class = "fig-head design-head", div(class = "fig-title", d$name), HTML(notation_html(d)))
  })

  output$design_readout <- renderUI({
    key <- cur_step(); est <- design_result()$est; p <- world()
    if (key == "intro") {
      return(div(class = "readout", p(class = "small", "Notation follows Campbell & Stanley as used by Nygren: ",
        HTML(paste('<span class="tok tok-r">R</span> random assignment, <span class="tok tok-o">O</span> observation (test),',
                   '<span class="tok tok-x">X</span> intervention, <span class="tok tok-c">C</span> control teaching,',
                   '<span class="tok tok-cut">C</span> assignment by cutoff. A dashed line between rows means the groups were not randomized.')))))
    }
    if (key == "race") {
      trs <- vapply(est, function(row) sprintf('<tr><td>%s</td><td class="num">%s</td><td class="num">%.1f</td></tr>',
                                               esc(row$name), signed(row$mean - p$delta), row$sd), "")
      return(div(class = "readout", HTML(paste0('<table class="mini-table"><thead><tr><th>Design</th><th class="num">Bias</th><th class="num">Spread (SD)</th></tr></thead><tbody>',
                                                paste(trs, collapse = ""), "</tbody></table>"))))
    }
    if (key == "d6") {
      f <- est$factorial
      return(div(class = "readout", HTML(paste0(
        '<div class="ro-grid three">',
        sprintf('<div><span class="ro-label">Role-play (A)</span><span class="ro-num">%s</span><span class="ro-sub">truth %s · ±%.1f</span></div>', signed(f$A), signed(f$tA), 1.96 * f$se_main),
        sprintf('<div><span class="ro-label">Small groups (B)</span><span class="ro-num">%s</span><span class="ro-sub">truth %s · ±%.1f</span></div>', signed(f$B), signed(f$tB), 1.96 * f$se_main),
        sprintf('<div><span class="ro-label">Interaction A×B</span><span class="ro-num">%s</span><span class="ro-sub">truth %s · ±%.1f</span></div>', signed(f$AB), signed(f$tAB), 1.96 * f$se_int),
        '</div><p class="small muted">Main effects are averaged over the other factor. If the lines aren\'t parallel, there is an interaction: ',
        'the effect of role-play depends on group size. Interactions need about four times as many students to estimate as precisely as main effects.</p>'))))
    }
    e <- est$est; se <- est$se
    tc <- est$tcrit %||% 1.96
    lo <- e - tc * se; hi <- e + tc * se
    bias <- e - p$delta
    eb <- est$exp_bias
    covered <- lo <= p$delta && p$delta <= hi
    if (abs(eb) >= 0.5) {
      vcls <- "bad"
      verdict <- sprintf("Biased. On average this design is off by %s points, because the estimate absorbs %s. This sample is off by %s.",
                         signed(eb), est$bias_expl, signed(bias))
    } else if (key %in% c("d1", "d2", "d3")) {
      vcls <- "warn"
      verdict <- "No head start or background growth is set in this world, so the design happens to work. In a real classroom you can't know that."
    } else if (covered) {
      vcls <- "good"
      verdict <- sprintf("Unbiased: %s. This sample's error (%s) is sampling noise, and the 95%% CI covers the true effect.", est$bias_expl, signed(bias))
    } else {
      vcls <- "warn"
      verdict <- sprintf("Unbiased: %s. This sample's CI happens to miss the true effect, which happens in about 5%% of studies. Click “New sample”.", est$bias_expl)
    }
    extra <- if (!is.null(est$extra)) sprintf('<p class="small">%s</p>', esc(est$extra)) else ""
    div(class = "readout", HTML(paste0(
      '<div class="ro-grid">',
      sprintf('<div><span class="ro-label">%s</span><span class="ro-num">%s</span><span class="ro-sub">95%% CI [%.1f, %.1f]</span></div>', esc(est$label), signed(e), lo, hi),
      sprintf('<div><span class="ro-label">True effect <i>(only knowable in a simulation)</i></span><span class="ro-num truth">%s</span><span class="ro-sub">d ≈ %.2f</span></div></div>',
              signed(p$delta), p$delta / SD),
      sprintf('<p class="verdict %s">%s</p>%s', vcls, esc(verdict), extra))))
  })

  # ======================= PRISMA =======================
  pr_vals <- reactive({
    v <- list()
    for (k in PRISMA_NUM) { x <- input[[paste0("pr_", k)]]; v[[k]] <- if (is.null(x) || is.na(x)) NA else as.integer(x) }
    for (k in PRISMA_TXT) v[[k]] <- input[[paste0("pr_", k)]] %||% ""
    v
  })
  prisma_built <- reactive({
    v <- pr_vals()
    c(prisma_svg(v, isTRUE(input$pr_prev), isTRUE(input$pr_other)), list(v = v))
  })
  output$prisma_out <- renderUI(HTML(prisma_built()$svg))

  z0 <- function(x) if (is.null(x) || is.na(x)) 0 else x

  output$prisma_checks <- renderUI({
    pb <- prisma_built(); v <- pb$v; tot <- pb$tot
    ident <- tot$db_total + tot$reg_total
    checks <- list(
      list("Records screened = identified − removed", ident - z0(v$dupes) - z0(v$auto_excl) - z0(v$other_removed), v$screened),
      list("Reports sought = screened − excluded", z0(v$screened) - z0(v$screen_excl), v$sought),
      list("Reports assessed = sought − not retrieved", z0(v$sought) - z0(v$not_retrieved), v$assessed))
    inc <- z0(v$assessed) - tot$excl_total
    if (isTRUE(input$pr_other)) {
      checks[[length(checks) + 1]] <- list("Other methods: reports sought ≤ records identified", NULL, v$oth_sought)
      checks[[length(checks) + 1]] <- list("Other methods: assessed = sought − not retrieved", z0(v$oth_sought) - z0(v$oth_not_retrieved), v$oth_assessed)
      inc <- inc + z0(v$oth_assessed) - tot$oexcl_total
    }
    checks[[length(checks) + 1]] <- list("Reports included = assessed − excluded (all routes)", inc, v$reports)
    nbad <- 0; items <- character(0)
    for (ch in checks) {
      lab <- ch[[1]]; exp <- ch[[2]]; act <- ch[[3]]
      if (is.null(exp)) { ok <- z0(act) <= tot$oth_total; msg <- sprintf("%d ≤ %d", as.integer(z0(act)), as.integer(tot$oth_total)) }
      else {
        ok <- !is.na(act) && exp == act
        msg <- paste0(if (is.na(act)) "?" else act, if (ok) "" else sprintf(" (expected %s)", fint(as.integer(exp))))
      }
      if (!ok) nbad <- nbad + 1
      items <- c(items, sprintf('<li class="%s"><b>%s</b> %s: <span class="mono">%s</span></li>',
                                if (ok) "ok" else "warn", if (ok) "✓" else "!", esc(lab), msg))
    }
    if (!is.na(v$studies) && !is.na(v$reports) && v$studies > v$reports) {
      items <- c(items, '<li class="warn"><b>!</b> More studies than reports. Each study needs at least one report.</li>')
      nbad <- nbad + 1
    }
    head <- if (nbad == 0) "All counts add up." else
      sprintf("%d count%s don't add up. Check them, or click “Fix the arithmetic”.", nbad, if (nbad > 1) "s" else "")
    HTML(sprintf('<details class="checks %s"><summary>%s</summary><ul>%s</ul></details>',
                 if (nbad == 0) "allok" else "haswarn", head, paste(items, collapse = "")))
  })

  observeEvent(input$pr_fix, {
    pb <- prisma_built(); v <- pb$v; tot <- pb$tot
    screened <- tot$db_total + tot$reg_total - z0(v$dupes) - z0(v$auto_excl) - z0(v$other_removed)
    sought <- screened - z0(v$screen_excl)
    assessed <- sought - z0(v$not_retrieved)
    updateNumericInput(session, "pr_screened", value = screened)
    updateNumericInput(session, "pr_sought", value = sought)
    updateNumericInput(session, "pr_assessed", value = assessed)
    reports <- assessed - tot$excl_total
    if (isTRUE(input$pr_other)) {
      oa <- z0(v$oth_sought) - z0(v$oth_not_retrieved)
      updateNumericInput(session, "pr_oth_assessed", value = oa)
      reports <- reports + oa - tot$oexcl_total
    }
    updateNumericInput(session, "pr_reports", value = reports)
    if (!is.na(v$studies) && v$studies > reports) updateNumericInput(session, "pr_studies", value = reports)
  })

  pr_apply <- function(vals) {
    for (k in intersect(names(vals), PRISMA_NUM)) updateNumericInput(session, paste0("pr_", k), value = vals[[k]])
    for (k in intersect(names(vals), PRISMA_TXT)) updateTextAreaInput(session, paste0("pr_", k), value = vals[[k]])
  }
  observeEvent(input$pr_reset, pr_apply(PRISMA_DEFAULTS))

  output$pr_csv <- downloadHandler(
    filename = "prisma2020_values.csv",
    content = function(file) {
      v <- pr_vals()
      df <- data.frame(field = c("show_previous", "show_other_methods", PRISMA_NUM, PRISMA_TXT),
                       value = c(as.integer(isTRUE(input$pr_prev)), as.integer(isTRUE(input$pr_other)),
                                 vapply(PRISMA_NUM, function(k) if (is.na(v[[k]])) "" else as.character(v[[k]]), ""),
                                 vapply(PRISMA_TXT, function(k) v[[k]], "")))
      write.csv(df, file, row.names = FALSE)
    })

  observeEvent(input$pr_upload, {
    f <- input$pr_upload
    tryCatch({
      df <- read.csv(f$datapath, stringsAsFactors = FALSE, colClasses = "character")
      vals <- list()
      for (i in seq_len(nrow(df))) {
        k <- trimws(df$field[i]); val <- df$value[i]
        if (k == "show_previous") update_switch("pr_prev", value = trimws(val) %in% c("1", "TRUE", "true"))
        else if (k == "show_other_methods") update_switch("pr_other", value = trimws(val) %in% c("1", "TRUE", "true"))
        else if (k %in% PRISMA_NUM) vals[[k]] <- if (nzchar(trimws(val))) as.integer(as.numeric(val)) else NA
        else if (k %in% PRISMA_TXT) vals[[k]] <- val
      }
      pr_apply(vals)
      showNotification("Loaded PRISMA values.", type = "message", duration = 3)
    }, error = function(e) showNotification(paste("Could not read that CSV:", conditionMessage(e)), type = "error"))
  })

  # ======================= Cohen's d & power =======================
  observe({
    d <- input$cd_d
    x <- seq(-4, 6.5, length.out = 241)
    session$sendCustomMessage("erl_cohen", list(d = d, x = arr(x, 3), c = arr(dnorm(x), 4), t = arr(dnorm(x, d), 4)))
  })

  output$cohen_tiles <- renderUI({
    d <- input$cd_d; sdv <- input$cd_sd; cer <- input$cd_cer
    u3 <- pnorm(d); ovl <- 2 * pnorm(-abs(d) / 2); ps <- pnorm(d / sqrt(2))
    denom <- pnorm(d + qnorm(cer)) - cer
    nnt <- if (denom > 1e-9) 1 / denom else NA
    kraft <- if (d < 0.05) "small" else if (d < 0.20) "medium" else "large"
    cl <- if (d < 0.2) "negligible" else if (d < 0.5) "small" else if (d < 0.8) "medium" else "large"
    tiles_html(list(
      c("Difference in points", fnum(d * sdv, 1), sprintf("with SD = %g", sdv)),
      c("Cohen's U₃", sprintf("%.0f%%", 100 * u3), "of the intervention group score above the control mean"),
      c("Overlap", sprintf("%.0f%%", 100 * ovl), "of the two distributions overlap"),
      c("Probability of superiority", sprintf("%.0f%%", 100 * ps), "chance a random intervention student beats a random control student"),
      c("NNT", if (is.na(nnt)) "∞" else fnum(nnt, 1), sprintf("students to teach for one extra ‘success’ (control rate %.0f%%)", 100 * cer)),
      c("Labels", cl, sprintf("by Cohen · “%s” by Kraft (2020)", kraft))))
  })

  power_calc <- reactive({
    d <- input$cd_d; alpha <- as.numeric(input$cd_alpha); rho <- input$cd_rho; n <- as.integer(input$cd_n)
    ns <- unique(round(exp(seq(log(4), log(5000), length.out = 120))))
    list(d = d, alpha = alpha, rho = rho, n = n, ns = I(ns),
         p0 = arr(power_two(d, ns, alpha), 4), p1 = arr(power_two(d, ns, alpha, rho), 4),
         cur0 = scal(power_two(d, n, alpha), 4), cur1 = scal(power_two(d, n, alpha, rho), 4),
         need0 = n_needed(d, alpha), need1 = n_needed(d, alpha, rho))
  })
  observe(session$sendCustomMessage("erl_power", power_calc()))

  output$power_tiles <- renderUI({
    pc <- power_calc()
    nd0 <- if (is.na(pc$need0)) "—" else fint(pc$need0)
    nd1 <- if (is.na(pc$need1)) "—" else fint(pc$need1)
    tiles_html(list(
      c(sprintf("Power at n = %d", pc$n), sprintf("%.0f%%", 100 * pc$cur0), "posttest-only comparison"),
      c(sprintf("…with pretest (ρ = %.2f)", pc$rho), sprintf("%.0f%%", 100 * pc$cur1), "ANCOVA, pretest as covariate", "1"),
      c("n per group for 80%", nd0, sprintf("posttest only · %s with pretest", nd1))))
  })

  # ======================= p-values =======================
  empty_pv <- function() list(ps = numeric(0), recent = matrix(numeric(0), 0, 4), last = NULL, added = 0)
  pv_store <- reactiveVal(empty_pv())
  observeEvent(list(input$pv_d, input$pv_n), pv_store(empty_pv()))
  observeEvent(input$pv_reset, pv_store(empty_pv()))

  pv_run <- function(k) {
    d <- isolate(input$pv_d); n <- as.integer(isolate(input$pv_n))
    X <- matrix(rnorm(k * n), k); Y <- matrix(rnorm(k * n, d), k)
    mx <- rowMeans(X); my <- rowMeans(Y)
    vx <- rowSums((X - mx)^2) / (n - 1); vy <- rowSums((Y - my)^2) / (n - 1)
    sp <- sqrt((vx + vy) / 2); se <- sp * sqrt(2 / n)
    tv <- (my - mx) / se; dfv <- 2 * n - 2
    pv <- 2 * pt(-abs(tv), dfv)
    tc <- qt(0.975, dfv)
    dhat <- (my - mx) / sp
    lo <- (my - mx) - tc * se; hi <- (my - mx) + tc * se
    st <- isolate(pv_store())
    idx <- max(1, k - 24):k
    rec <- rbind(st$recent, cbind(rr(dhat[idx], 3), rr(lo[idx], 3), rr(hi[idx], 3), rr(pv[idx], 5)))
    rec <- tail(rec, 25)
    last <- list(ctrl = arr(X[k, ]), treat = arr(Y[k, ]), t = scal(tv[k], 3), df = dfv, p = scal(pv[k], 6), d = scal(dhat[k], 3))
    pv_store(list(ps = tail(c(st$ps, pv), 50000), recent = rec, last = last, added = k))
  }
  observeEvent(input$pv_1, pv_run(1))
  observeEvent(input$pv_100, pv_run(100))
  observeEvent(input$pv_1000, pv_run(1000))

  pv_payload <- reactive({
    st <- pv_store(); alpha <- as.numeric(input$pv_alpha); n <- as.integer(input$pv_n); d <- input$pv_d
    dfv <- 2 * n - 2
    ps <- st$ps
    counts <- if (length(ps)) hist(ps, breaks = seq(0, 1, length.out = 21), plot = FALSE)$counts else rep(0, 20)
    x <- seq(-6, 6, length.out = 241)
    list(alpha = alpha, d = d, n = n, df = dfv, counts = I(counts), total = length(ps),
         sig = sum(ps < alpha), power = if (d > 0) scal(power_two(d, n, alpha), 4) else alpha,
         tcrit = scal(qt(1 - alpha / 2, dfv), 4), x = arr(x, 3), y = arr(dt(x, dfv), 5),
         last = if (is.null(st$last)) NA else st$last,
         recent = if (nrow(st$recent)) st$recent else I(list()), added = st$added)
  })
  observe(session$sendCustomMessage("erl_pv", pv_payload()))

  output$pv_stats <- renderUI({
    pl <- pv_payload(); tot <- pl$total; last <- pl$last
    share <- if (tot) sprintf("%.1f%%", 100 * pl$sig / tot) else "—"
    exp_lab <- if (pl$d > 0) "Theoretical power" else "Expected false positives (α)"
    lt <- if (is.list(last)) c("Last study", paste("p =", fmt_p(last$p)), sprintf("t(%d) = %.2f, d̂ = %.2f", last$df, last$t, last$d))
          else c("Last study", "—", "run a study")
    tiles_html(list(lt,
      c("Studies run", fint(tot), sprintf("%s with p &lt; %g", fint(pl$sig), pl$alpha)),
      c("Share significant", share, sprintf("%s: %.1f%%", exp_lab, 100 * pl$power), "1")))
  })

  # ======================= Chi-square =======================
  CS_DEFAULT <- matrix(c(68, 32, 14, 52, 48, 21, 45, 40, 18), 3, byrow = TRUE)
  ROWLAB <- c("Video tutorial", "Regular teaching", "Text tutorial")
  COLLAB <- c("Correct", "Incorrect", "Unsure")

  output$cs_grid <- renderUI({
    R <- as.integer(input$cs_rows); Cc <- as.integer(input$cs_cols)
    head <- c(list(div(class = "cg-corner")), lapply(seq_len(Cc), function(j) textInput(paste0("cs_cl", j - 1), NULL, COLLAB[j])))
    cells <- list()
    for (i in seq_len(R)) {
      cells[[length(cells) + 1]] <- textInput(paste0("cs_rl", i - 1), NULL, ROWLAB[i])
      for (j in seq_len(Cc)) cells[[length(cells) + 1]] <- numericInput(sprintf("cs_%d_%d", i - 1, j - 1), NULL, CS_DEFAULT[i, j], min = 0)
    }
    div(class = "cs-grid", style = sprintf("grid-template-columns: 1.4fr repeat(%d, 1fr);", Cc), head, cells)
  })

  cs_calc <- reactive({
    R <- as.integer(input$cs_rows); Cc <- as.integer(input$cs_cols)
    obs <- matrix(0, R, Cc)
    for (i in seq_len(R)) for (j in seq_len(Cc)) {
      v <- input[[sprintf("cs_%d_%d", i - 1, j - 1)]]
      if (is.null(v)) return(NULL)
      obs[i, j] <- if (is.na(v)) 0 else max(0, v)
    }
    rl <- vapply(seq_len(R), function(i) { x <- input[[paste0("cs_rl", i - 1)]]; if (is.null(x) || !nzchar(x)) paste("Row", i) else x }, "")
    cl <- vapply(seq_len(Cc), function(j) { x <- input[[paste0("cs_cl", j - 1)]]; if (is.null(x) || !nzchar(x)) paste("Col", j) else x }, "")
    obs <- round(obs * input$cs_mult)
    if (any(colSums(obs) == 0) || any(rowSums(obs) == 0)) return(list(error = "Every row and column needs at least one count."))
    yates <- isTRUE(input$cs_yates) && R == 2 && Cc == 2
    ct <- suppressWarnings(chisq.test(obs, correct = yates))
    chi2v <- unname(ct$statistic); pv <- ct$p.value; dfv <- unname(ct$parameter); ex <- ct$expected
    N <- sum(obs)
    V <- sqrt(chi2v / (N * (min(R, Cc) - 1)))
    resid <- (obs - ex) / sqrt(ex)
    crit <- qchisq(0.95, dfv)
    xmax <- max(crit * 1.8, chi2v * 1.15, 8)
    x <- seq(0.001, xmax, length.out = 300)
    list(obs = unname(obs), exp = round(unname(ex), 2), resid = round(unname(resid), 2), rl = I(rl), cl = I(cl),
         chi2 = scal(chi2v, 3), p = pv, df = dfv, V = scal(V, 3), N = N, crit = scal(crit, 3),
         x = arr(x, 3), y = arr(dchisq(x, dfv), 5), minexp = min(ex), yates = yates)
  })
  observe({ cc <- cs_calc(); if (!is.null(cc)) session$sendCustomMessage("erl_chi", cc) })

  output$cs_tiles <- renderUI({
    cc <- cs_calc()
    if (is.null(cc)) return(div())
    if (!is.null(cc$error)) return(div(class = "verdict bad", cc$error))
    warn <- if (cc$minexp < 5) '<p class="verdict bad small">Some expected counts are below 5, so the χ² approximation may be poor. Consider Fisher\'s exact test.</p>' else ""
    tagList(
      tiles_html(list(
        c(paste0("X²", if (cc$yates) " (Yates)" else ""), fnum(cc$chi2, 2), sprintf("df = %d, N = %s", as.integer(cc$df), fint(cc$N))),
        c("p-value", fmt_p(cc$p), sprintf("critical X² at α = .05: %.2f", cc$crit), "1"),
        c(paste0("Cramér's V", if (cc$df == 1) " (= φ)" else ""), fnum(cc$V, 2), "effect size, unaffected by N"))),
      HTML(warn),
      p(class = "small muted mono", sprintf("χ²(%d, N = %d) = %.2f, p %s %s, V = %.2f", as.integer(cc$df), as.integer(cc$N), cc$chi2,
                                            if (cc$p < .001) "<" else "=", sub("< ", "", fmt_p(cc$p)), cc$V)))
  })

  # ======================= Distributions =======================
  ds_samples <- reactiveVal(list(vals = numeric(0), new_from = 0))
  ds_params <- reactive(list(mu = input$ds_mu, sigma = input$ds_sigma, df = input$ds_df, d1 = input$ds_d1, d2 = input$ds_d2,
                             bn = input$ds_bn, bp = input$ds_bp, lam = input$ds_lam, ua = input$ds_ua, ub = input$ds_ub, rate = input$ds_rate))

  observeEvent(input$ds_dist, {
    key <- input$ds_dist
    dom <- DISTS[[key]]$domain
    defaults <- list(norm = c(-1, 1), t = c(-2, 2), chi2 = c(0, 5), f = c(0, 2), binom = c(10, 14), poisson = c(2, 6), unif = c(3, 5), expon = c(0, 2))
    updateSliderInput(session, "ds_range", min = dom[1], max = dom[2], value = defaults[[key]], step = if (DISTS[[key]]$discrete) 1 else 0.1)
  })
  observeEvent(list(input$ds_dist, ds_params()), ds_samples(list(vals = numeric(0), new_from = 0)))
  observeEvent(input$ds_reset, ds_samples(list(vals = numeric(0), new_from = 0)))

  ds_draw <- function(k) {
    dobj <- dist_obj(isolate(input$ds_dist), isolate(ds_params()))
    cur <- isolate(ds_samples())$vals
    vals <- tail(c(cur, round(dobj$r(k), 3)), 20000)
    ds_samples(list(vals = vals, new_from = max(0, length(vals) - k)))
  }
  observeEvent(input$ds_1, ds_draw(1))
  observeEvent(input$ds_50, ds_draw(50))
  observeEvent(input$ds_1000, ds_draw(1000))

  ds_calc <- reactive({
    key <- input$ds_dist; meta <- DISTS[[key]]
    dobj <- dist_obj(key, ds_params())
    lo <- meta$domain[1]; hi <- meta$domain[2]
    a <- max(lo, input$ds_range[1]); b <- min(hi, input$ds_range[2])
    out <- list(dist = key, discrete = meta$discrete, domain = I(meta$domain), a = a, b = b, mode = input$ds_mode)
    if (meta$discrete) {
      ks <- lo:hi
      out$pmf <- cbind(ks, rr(dobj$d(ks), 5))
      out$cdf <- cbind(ks, rr(dobj$p(ks), 5))
      prob <- dobj$p(floor(b)) - dobj$p(ceiling(a) - 1)
    } else {
      x <- seq(lo, hi, length.out = 400)
      y <- dobj$d(x)
      cap <- c(chi2 = 0.6, f = 1.6, expon = 3.2)[key]
      if (!is.na(cap)) y <- pmin(y, cap)
      y[!is.finite(y)] <- if (!is.na(cap)) cap else 0
      out$curve <- cbind(rr(x, 3), rr(y, 5))
      out$cdf <- cbind(rr(x, 3), rr(dobj$p(x), 5))
      if (key == "t") out$ref <- cbind(rr(x, 3), rr(dnorm(x), 5))
      prob <- dobj$p(b) - dobj$p(a)
    }
    s <- ds_samples()
    c(out, list(prob = scal(prob, 4), mean = scal(dobj$mean, 3), var = scal(dobj$var, 3),
                samples = I(s$vals), new_from = s$new_from))
  })
  observe(session$sendCustomMessage("erl_dist", ds_calc()))

  output$ds_note <- renderUI(p(class = "small dist-note", DIST_NOTES[[input$ds_dist]]))

  output$ds_tiles <- renderUI({
    cc <- ds_calc(); s <- as.numeric(cc$samples)
    emp <- if (length(s)) sprintf("in the samples: %.1f%% of %s draws", 100 * mean(s >= cc$a & s <= cc$b), fint(length(s))) else "shaded area"
    tiles_html(list(
      c(sprintf("P(%g ≤ X ≤ %g)", cc$a, cc$b), sprintf("%.1f%%", 100 * cc$prob), emp, "1"),
      c("Mean", if (is.na(cc$mean)) "undefined" else fnum(cc$mean, 2), if (length(s)) sprintf("sample: %.2f", mean(s)) else "draw some samples"),
      c("SD", if (is.na(cc$var)) "undefined" else fnum(sqrt(cc$var), 2), if (length(s) > 1) sprintf("sample: %.2f", sd(s)) else "")))
  })

  # ======================= CLT =======================
  empty_clt <- function() list(means = numeric(0), sample = NULL, anim = FALSE)
  clt_store <- reactiveVal(empty_clt())
  observeEvent(list(input$clt_pop, input$clt_n), clt_store(empty_clt()))
  observeEvent(input$clt_reset, clt_store(empty_clt()))

  clt_draw <- function(k) {
    pop <- POPS[[isolate(input$clt_pop)]]; n <- as.integer(isolate(input$clt_n))
    cur <- isolate(clt_store())$means
    S <- matrix(sample(pop$vals, k * n, replace = TRUE), k)
    clt_store(list(means = tail(c(cur, round(rowMeans(S), 3)), 30000), sample = round(S[k, ], 2), anim = (k == 1)))
  }
  observeEvent(input$clt_1, clt_draw(1))
  observeEvent(input$clt_25, clt_draw(25))
  observeEvent(input$clt_1000, clt_draw(1000))

  clt_payload <- reactive({
    pop <- POPS[[input$clt_pop]]; n <- as.integer(input$clt_n); st <- clt_store()
    list(pop = list(hist = pop$hist, mu = pop$mu, sigma = pop$sigma, domain = I(pop$domain), discrete = pop$discrete, label = pop$label),
         n = n, means = I(st$means), sample = if (is.null(st$sample)) NA else I(st$sample), anim = st$anim,
         se = pop$sigma / sqrt(n), normal = isTRUE(input$clt_normal))
  })
  observe(session$sendCustomMessage("erl_clt", clt_payload()))

  output$clt_stats <- renderUI({
    pl <- clt_payload(); m <- as.numeric(pl$means); k <- length(m)
    tiles_html(list(
      c("Population", sprintf("μ = %.2f", pl$pop$mu), sprintf("σ = %.2f", pl$pop$sigma)),
      c(sprintf("SD of the %s means", fint(k)), if (k > 1) fnum(sd(m), 2) else "—", sprintf("CLT predicts σ/√n = %.2f", pl$se), "1"),
      c("Skewness of means", if (k > 2) fnum(skewness(m), 2) else "—", "0 = symmetric, like the normal")))
  })
}

shinyApp(ui, server)
