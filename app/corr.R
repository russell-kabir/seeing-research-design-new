# =============================================================================
# Correlation & R² tab
#   1. Spurious correlations (Tyler Vigen, tylervigen.com, CC BY 4.0)
#   2. Real data: a Japanese subset of the ENglish Reading Online (ENRO) project
#      (Siegelman et al., 2024, Language Learning, 74(1), 249–294)
#   3. R² as overlapping circles (after Heiss, 2021, "Visualizing the explained
#      variance of a regression with Euler diagrams")
# Sourced by app.R, which defines the bilingual helpers L(), P(), Lh(), Lc(), sel().
# =============================================================================

# ---- 1. Spurious correlations ------------------------------------------------
# Values copied from the tylervigen.com pages linked below (CC BY 4.0, attribute Tyler Vigen).
VIGEN <- list(
  edu = list(
    title = c("Bachelor's degrees in Education (US) vs. receptionists in North Dakota",
              "米国の教育学の学士号の数 と ノースダコタ州の受付係の人数"),
    a = c("Bachelor's degrees awarded in Education (US)", "教育学の学士号の授与数（米国）"),
    b = c("Receptionists in North Dakota", "ノースダコタ州の受付係の人数"),
    years = 2012:2021,
    va = c(105656, 104698, 98838, 91596, 87221, 85130, 82599, 83952, 85052, 89398),
    vb = c(2880, 2790, 2530, 2190, 1800, 1700, 1570, 1470, 1610, 1870),
    src = "NCES Digest of Education Statistics; BLS Occupational Employment Statistics",
    url = "https://tylervigen.com/spurious/correlation/2654",
    story = c(paste("Fewer new teachers… so fewer people answering phones in Fargo? Both numbers simply fell and then rose over the same decade.",
                    "Anything that trends over time will correlate with anything else that trends over time. Time (and the economy) is the lurking third variable."),
              paste("新しい教員が減ったから、ファーゴで電話に出る人も減った…？ 実際は、どちらの数も同じ10年間に下がってから上がっただけです。",
                    "時間とともに変化するものどうしは、何でも相関してしまいます。隠れた第三の変数は「時間（と経済）」です。"))),
  bee = list(
    title = c("Letters in the winning word of the Scripps National Spelling Bee vs. visitors to Disneyland",
              "スクリップス全米スペリング大会の優勝単語の文字数 と ディズニーランドの来場者数"),
    a = c("Letters in the winning spelling-bee word", "スペリング大会の優勝単語の文字数"),
    b = c("Visitors to Disneyland, Anaheim (millions)", "ディズニーランド（アナハイム）の来場者数（百万人）"),
    years = 2007:2021,
    va = c(9, 7, 9, 8, 12, 9, 7, 12, 15, 12, 8, 8, 13, 0, 7),
    vb = c(14.87, 14.721, 15.9, 15.98, 16.14, 15.963, 16.202, 16.769, 18.278, 17.943, 18.3, 18.666, 18.666, 3.674, 8.573),
    src = "Scripps (winning words); TEA / AECOM theme index (visitors)",
    url = "https://tylervigen.com/spurious/correlation/2646",
    story = c(paste("Longer English words bring more people to Disneyland? Look at 2020: the bee was cancelled (0 letters) and Disneyland was closed by COVID-19.",
                    "One shared shock in one year does most of the work. Leave 2020 out and watch r shrink."),
              paste("英単語が長いほど、ディズニーランドに人が集まる…？ 2020年を見てください。大会は中止（0文字）、ディズニーランドは新型コロナで休園していました。",
                    "1年だけの共通のショックが、相関のほとんどを作っています。2020年を外すと r がどうなるか見てみましょう。")))
)

cor_info <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]; n <- length(x)
  if (n < 3 || sd(x) == 0 || sd(y) == 0) return(list(n = n, r = NA, r2 = NA, p = NA, lo = NA, hi = NA, b0 = NA, b1 = NA))
  r <- cor(x, y)
  tval <- r * sqrt((n - 2) / max(1e-12, 1 - r^2))
  p <- 2 * pt(-abs(tval), n - 2)
  ci <- if (n > 3) tanh(atanh(max(min(r, 0.999999), -0.999999)) + c(-1, 1) * qnorm(0.975) / sqrt(n - 3)) else c(NA, NA)
  b1 <- r * sd(y) / sd(x); b0 <- mean(y) - b1 * mean(x)
  list(n = n, r = r, r2 = r^2, p = p, lo = ci[1], hi = ci[2], b0 = b0, b1 = b1, t = tval)
}

p_txt <- function(p) if (is.na(p)) "" else if (p < .001) "p < .001" else paste("p =", fmt_p(p))

r_words <- function(r) {
  a <- abs(r)
  if (is.na(r)) return(c("—", "—"))
  s <- if (a < .1) c("almost no", "ほとんどない") else if (a < .3) c("a weak", "弱い") else if (a < .5) c("a moderate", "中程度の") else if (a < .7) c("a fairly strong", "かなり強い") else c("a very strong", "とても強い")
  d <- if (r >= 0) c("positive", "正の") else c("negative", "負の")
  c(paste(s[1], if (a < .1) "" else d[1], "correlation"), paste0(s[2], if (a < .1) "" else d[2], "相関"))
}

# ---- 2. ENRO data -------------------------------------------------------------
ENRO_DEFAULT <- read.csv("data/enro_japan_subset.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

ENRO_LABELS <- list(
  read_comp_score = c("Reading comprehension (proportion correct)", "読解（正答率）"),
  vocab_score = c("Vocabulary test (score)", "語彙テスト（得点）"),
  lisn_score = c("Listening comprehension (proportion correct)", "リスニング（正答率）"),
  grammar_score = c("Grammar test (proportion correct)", "文法テスト（正答率）"),
  spell_score = c("Spelling test (proportion correct)", "スペリング（正答率）"),
  lextale_score = c("LexTALE vocabulary test (proportion correct)", "LexTALE 語彙テスト（正答率）"),
  ld_score = c("Lexical decision (accuracy)", "語彙性判断（正確さ）"),
  ortho_score = c("Orthographic task (accuracy)", "綴り知識の課題（正確さ）"),
  segment_score = c("Segmentation task (score)", "分節課題（得点）"),
  rate_mean = c("Reading rate (rate_mean)", "読みの速さ（rate_mean）"),
  motivation = c("Reading motivation (questionnaire mean)", "読みの動機づけ（質問紙の平均）"),
  eng_read_proficiency = c("Self-rated reading proficiency", "読解力の自己評価"),
  eng_speech_proficiency = c("Self-rated speaking proficiency", "スピーキング力の自己評価"),
  age = c("Age", "年齢")
)
GROUP_LABELS <- list(gender = c("Gender", "性別"), degree = c("Degree program", "課程"),
                     eng_status_imputed = c("English status (L1/L2)", "英語の位置づけ（L1／L2）"),
                     source_lang = c("First language", "第一言語"))

var_lab <- function(v) ENRO_LABELS[[v]] %||% c(v, v)
grp_lab <- function(v) GROUP_LABELS[[v]] %||% c(v, v)
num_vars <- function(df) names(df)[vapply(df, function(x) is.numeric(x) && sum(is.finite(x)) >= 10 && length(unique(x[is.finite(x)])) > 2, TRUE) & names(df) != "id"]
grp_vars <- function(df) names(df)[vapply(df, function(x) { u <- unique(x[!is.na(x) & x != ""]); !is.numeric(x) && length(u) >= 2 && length(u) <= 8 }, TRUE)]
var_choices <- function(vs) setNames(vs, vapply(vs, function(v) { l <- var_lab(v); Lc(l[1], l[2]) }, ""))

# ---- 3. Euler / Venn decomposition (standardized variables, so every circle has area 1)
venn_parts <- function(y, x1, x2) {
  ok <- is.finite(y) & is.finite(x1) & is.finite(x2)
  y <- y[ok]; x1 <- x1[ok]; x2 <- x2[ok]; n <- length(y)
  if (n < 5) return(NULL)
  ry1 <- cor(y, x1); ry2 <- cor(y, x2); r12 <- cor(x1, x2)
  R2 <- if (abs(r12) < 0.9999) (ry1^2 + ry2^2 - 2 * ry1 * ry2 * r12) / (1 - r12^2) else max(ry1^2, ry2^2)
  sr1 <- R2 - ry2^2          # unique to X1 (squared semi-partial)
  sr2 <- R2 - ry1^2          # unique to X2
  G <- R2 - sr1 - sr2        # shared by Y, X1 and X2
  b1 <- if (abs(r12) < 0.9999) (ry1 - ry2 * r12) / (1 - r12^2) else NA
  b2 <- if (abs(r12) < 0.9999) (ry2 - ry1 * r12) / (1 - r12^2) else NA
  list(n = n, ry1 = ry1, ry2 = ry2, r12 = r12, R2 = R2, D = sr1, E = sr2, G = G, A = 1 - R2,
       F = r12^2 - G, B = 1 - sr1 - r12^2, C = 1 - sr2 - r12^2, b1 = b1, b2 = b2)
}

# ---- UI -------------------------------------------------------------------------
vig_choices <- c(setNames("edu", Lc("Teacher education × receptionists", "教員養成 × 受付係")),
                 setNames("bee", Lc("Spelling bee × Disneyland", "スペリング大会 × ディズニーランド")))

corr_tab <- nav_panel(L("Correlation & R²", "相関と R²"), value = "Correlation",
  div(class = "wrap corr-wrap",
    div(class = "corr-intro",
      kicker(L("Correlation, regression and R²", "相関・回帰・R²")),
      h2(L("Things that move together", "一緒に動くもの")),
      P("A correlation (r) tells you how closely two things move together, from −1 to +1. R² is r squared: the share of the differences in one variable that the other one “accounts for”. Neither tells you what causes what.",
        "相関係数 r は、2つのものがどのくらい一緒に動くかを −1 から +1 で表します。R² は r を2乗したもので、一方の変数のばらつきのうち、もう一方で「説明できる」割合です。どちらも、何が何の原因かは教えてくれません。", cls = "lede-sm")),
    navset_underline(id = "corr_sub",
      nav_panel(tagList("1 · ", L("Spurious correlations", "見せかけの相関")), value = "spurious",
        div(class = "tool",
          div(class = "ctrl-col",
            kicker(L("Correlation is not causation", "相関は因果ではない")),
            h3(L("Spurious correlations", "見せかけの相関")),
            P(tagList("Tyler Vigen's ", tags$a(href = "https://www.tylervigen.com/spurious-correlations", target = "_blank", "Spurious Correlations"),
                      " project compares thousands of unrelated yearly series and keeps the ones that happen to match. Pick one:"),
              tagList("Tyler Vigen の ", tags$a(href = "https://www.tylervigen.com/spurious-correlations", target = "_blank", "Spurious Correlations"),
                      " は、関係のない何千もの年次データを比べて、たまたま一致したものを集めたプロジェクトです。1つ選んでみましょう。")),
            sel("sp_ex", L("Example", "例"), vig_choices, selected = "edu"),
            radioButtons("sp_mode", L("Correlate…", "何を相関させる？"),
                         choiceNames = list(L("the yearly values", "毎年の値"), L("the year-to-year changes", "前年からの変化")),
                         choiceValues = c("levels", "changes"), selected = "levels"),
            input_switch("sp_drop", L("Leave out 2020 (pandemic year)", "2020年（パンデミックの年）を外す"), FALSE),
            uiOutput("sp_story"),
            tags$hr(),
            kicker(L("Data dredging", "データの浚渫（データ・ドレッジング）")),
            P("How hard is it to find a “strong” correlation by luck? Make 1,000 completely random series with the same number of years and see how well the best one matches.",
              "偶然だけで「強い」相関を見つけるのは、どのくらい簡単でしょうか。同じ年数の完全にランダムなデータを1,000個作り、いちばん一致したものがどれくらい似ているか見てみましょう。"),
            div(class = "btn-row", btn("sp_dredge", L("Dredge 1,000 random series", "ランダムなデータを1,000個さらう"), "btn-primary"),
                btn("sp_dredge_clear", L("Clear", "消す"), "btn-link")),
            uiOutput("sp_dredge_txt"),
            p(class = "small muted", "Data and charts courtesy of Tyler Vigen, tylervigen.com (CC BY 4.0). ",
              uiOutput("sp_src", inline = TRUE))),
          div(class = "fig-col",
            div(class = "sticky-inner",
              uiOutput("sp_head"),
              fig("sp-lines"),
              uiOutput("sp_tiles"),
              fig_head(L("The same data as a scatterplot", "同じデータを散布図で"), L("each dot is one year", "点 = 1年")),
              fig("sp-scatter"),
              conditionalPanel("output.sp_has_dredge",
                fig_head(L("1,000 random series vs. the first variable", "ランダムなデータ1,000個 と 1つめの変数"), L("histogram of r · the best match is drawn above in orange", "r のヒストグラム · いちばん一致したものを上の図にオレンジで表示")),
                fig("sp-dredge")))))),
      nav_panel(tagList("2 · ", L("Real data: ENRO", "実データ：ENRO")), value = "enro",
        div(class = "tool",
          div(class = "ctrl-col",
            kicker(L("English Reading Online (ENRO)", "English Reading Online (ENRO)")),
            h3(L("What goes with reading comprehension?", "読解力と一緒に動くものは？")),
            uiOutput("en_about"),
            uiOutput("en_pickers"),
            div(class = "switches",
              input_switch("en_line", L("Show the regression line", "回帰直線を表示"), TRUE),
              input_switch("en_resid", L("Show residuals (what the line misses)", "残差（直線で説明できない部分）を表示"), FALSE),
              input_switch("en_jitter", L("Jitter overlapping dots", "重なった点を少しずらす"), TRUE)),
            div(class = "note", mini(L("Things to try", "試してみよう")),
              tags$ul(
                LI("Vocabulary vs. reading comprehension: r ≈ .5, so vocabulary accounts for about a quarter of the differences in reading scores (R² ≈ .27).",
                   "語彙と読解：r ≈ .5 なので、語彙は読解の点のばらつきの約4分の1を説明します（R² ≈ .27）。"),
                LI("Reading rate vs. comprehension: a negative r. Do faster readers understand less, or do careful readers slow down?",
                   "読みの速さと読解：r は負。速く読む人ほど理解が浅いのか、それとも丁寧に読む人ほど遅くなるのか？"),
                LI("Split by a group. Is the correlation the same in each group? With small groups, r jumps around a lot.",
                   "グループで分けてみましょう。どのグループでも相関は同じでしょうか？ グループが小さいと r は大きく揺れます。"),
                LI("Click any square in the correlation matrix to plot that pair.",
                   "相関行列のマスをクリックすると、その組み合わせを散布図にできます。"))),
            tags$details(class = "upload-box",
              tags$summary(L("Use your own CSV (e.g. the full ENRO file)", "自分の CSV を使う（例：ENRO の全データ）")),
              P("Columns with numbers become variables; text columns with 2–8 categories (for example L1 vs. L2) become groups. Nothing is uploaded anywhere: the file stays in your browser.",
                "数値の列は変数に、2〜8カテゴリーの文字の列（例：L1／L2）はグループになります。ファイルはどこにも送られず、ブラウザの中だけで使われます。", cls = "small"),
              fileInput("en_upload", NULL, accept = c(".csv", "text/csv"), buttonLabel = L("Choose CSV…", "CSV を選ぶ…"), placeholder = "No file / ファイルなし"),
              actionButton("en_reset", L("Back to the ENRO Japan subset", "ENRO 日本サブセットに戻す"), class = "btn-ghost btn-sm"))),
          div(class = "fig-col",
            div(class = "sticky-inner",
              uiOutput("en_head"),
              fig("en-scatter"),
              uiOutput("en_tiles"),
              uiOutput("en_plain"),
              div(class = "grid-venn",
                div(fig_head(L("r² as overlap", "重なりとしての r²"), L("each circle = all the differences in one variable", "円 = 1つの変数のばらつき全体")), fig("en-venn")),
                div(fig_head(L("Correlation matrix", "相関行列"), L("click a square", "マスをクリック")), fig("en-matrix"))),
              uiOutput("en_groups"))))),
      nav_panel(tagList("3 · ", L("R² as circles", "円で見る R²")), value = "venn",
        div(class = "tool",
          div(class = "ctrl-col",
            kicker(L("After Heiss (2021)", "Heiss (2021) にならって")),
            h3(L("Seeing R² with overlapping circles", "円の重なりで R² を見る")),
            P(tagList("Andrew Heiss draws each variable as a circle whose area is all of its variance. Where circles overlap, the variables share variance. ",
                      strong("R² is the part of the outcome's circle covered by the predictors.")),
              tagList("Andrew Heiss は、各変数を「その変数のばらつき（分散）全体」を面積とする円で描きます。円が重なる部分は、変数どうしが共有するばらつきです。",
                      strong("R² は、結果変数の円のうち、説明変数の円で覆われている割合です。"))),
            uiOutput("vn_pickers"),
            radioButtons("vn_show", L("Show", "表示"),
                         choiceNames = list(L("Outcome + predictor 1", "結果 + 説明変数1"), L("Outcome + predictor 2", "結果 + 説明変数2"), L("Outcome + both predictors", "結果 + 両方の説明変数")),
                         choiceValues = c("x1", "x2", "both"), selected = "both"),
            div(class = "note", mini(L("How to read it", "読み方")),
              tags$ul(
                LI(tagList(span(class = "sw sw-d"), "only predictor 1 explains (its unique share, sr²)"), tagList(span(class = "sw sw-d"), "説明変数1だけが説明する部分（固有の寄与、sr²）")),
                LI(tagList(span(class = "sw sw-e"), "only predictor 2 explains"), tagList(span(class = "sw sw-e"), "説明変数2だけが説明する部分")),
                LI(tagList(span(class = "sw sw-g"), "both predictors explain together: it can't be credited to either one (multicollinearity)"),
                   tagList(span(class = "sw sw-g"), "両方が一緒に説明する部分：どちらの手柄とも言えない（多重共線性）")),
                LI(tagList(span(class = "sw sw-a"), "left unexplained (1 − R²)"), tagList(span(class = "sw sw-a"), "説明されずに残る部分（1 − R²）")))),
            P("Circles are drawn for standardized variables, so every circle has the same area. Like Heiss's eulerr diagrams, the three-way overlap is approximate; the numbers in the table are exact.",
              "変数は標準化しているので、すべての円は同じ面積です。Heiss の eulerr の図と同じく、3つの円の重なりの形は近似です。表の数値は正確です。", cls = "small muted")),
          div(class = "fig-col",
            div(class = "sticky-inner",
              uiOutput("vn_head"),
              fig("vn-euler"),
              fig_head(L("Where the outcome's variance goes", "結果変数のばらつきの内訳"), L("100% = the outcome's whole circle", "100% = 結果変数の円全体")),
              fig("vn-bar"),
              uiOutput("vn_table")))))
    )))

# ---- Server -------------------------------------------------------------------------
corr_server <- function(input, output, session) {

  # ===== 1. Spurious =====
  sp_data <- reactive({
    ex <- VIGEN[[input$sp_ex %||% "edu"]]
    yrs <- ex$years; a <- ex$va; b <- ex$vb
    keep <- if (isTRUE(input$sp_drop)) yrs != 2020 else rep(TRUE, length(yrs))
    mode <- input$sp_mode %||% "levels"
    if (mode == "changes") {
      # year-to-year change; drop pairs that would straddle a removed year
      ya <- yrs[keep]; aa <- a[keep]; bb <- b[keep]
      da <- diff(aa); db <- diff(bb); dy <- ya[-1]
      cons <- diff(ya) == 1
      x <- da[cons]; y <- db[cons]; py <- dy[cons]
    } else { x <- a[keep]; y <- b[keep]; py <- yrs[keep] }
    list(ex = ex, keep = keep, x = x, y = y, years = py, mode = mode, ci = cor_info(x, y))
  })

  dredge <- reactiveVal(NULL)
  observeEvent(list(input$sp_ex, input$sp_mode, input$sp_drop, input$sp_dredge_clear), dredge(NULL), ignoreInit = TRUE)
  observeEvent(input$sp_dredge, {
    d <- sp_data(); n <- length(d$x)
    K <- 1000
    # random walks (things that drift over time, like most yearly series)
    W <- matrix(rnorm(K * (n + 1)), K)
    W <- t(apply(W, 1, cumsum))
    S <- if (d$mode == "changes") W[, -1, drop = FALSE] - W[, -(n + 1), drop = FALSE] else W[, -1, drop = FALSE]
    rs <- apply(S, 1, function(s) suppressWarnings(cor(s, d$x)))
    best <- which.max(abs(rs))
    dredge(list(rs = rs, best = S[best, ], rbest = rs[best], n = n,
                share = mean(abs(rs) >= abs(d$ci$r), na.rm = TRUE), share7 = mean(abs(rs) >= 0.7, na.rm = TRUE)))
  })
  output$sp_has_dredge <- reactive(!is.null(dredge()))
  outputOptions(output, "sp_has_dredge", suspendWhenHidden = FALSE)

  observe({
    d <- sp_data(); ex <- d$ex; dr <- dredge()
    yrs <- ex$years
    hist <- if (!is.null(dr)) hist(dr$rs, breaks = seq(-1, 1, by = 0.05), plot = FALSE)$counts else NULL
    session$sendCustomMessage("erl_sp", list(
      years = I(yrs), a = I(ex$va), b = I(ex$vb), keep = I(d$keep),
      alab = Lc(ex$a[1], ex$a[2]), blab = Lc(ex$b[1], ex$b[2]),
      mode = d$mode, sx = arr(d$x, 4), sy = arr(d$y, 4), syears = I(d$years),
      r = scal(d$ci$r, 3), b0 = scal(d$ci$b0, 5), b1 = scal(d$ci$b1, 6),
      dredge = if (is.null(dr)) NA else list(hist = I(hist), best = arr(dr$best, 4), rbest = scal(dr$rbest, 3), robs = scal(d$ci$r, 3),
                                             years = I(d$years), n = dr$n)))
  })

  output$sp_head <- renderUI({
    ex <- sp_data()$ex
    fig_head(L(ex$title[1], ex$title[2]), L(paste0(min(ex$years), "–", max(ex$years), " · two y-axes, each scaled to its own variable"),
                                             paste0(min(ex$years), "〜", max(ex$years), "年 · 縦軸は2本（変数ごとに目盛りが違う）")))
  })
  output$sp_story <- renderUI({
    ex <- sp_data()$ex
    div(class = "note story-note", mini(L("What's going on?", "何が起きている？")), P(ex$story[1], ex$story[2]))
  })
  output$sp_src <- renderUI({ ex <- sp_data()$ex; tags$span(ex$src, " · ", tags$a(href = ex$url, target = "_blank", "source page")) })

  output$sp_tiles <- renderUI({
    d <- sp_data(); ci <- d$ci
    if (is.na(ci$r)) return(div())
    w <- r_words(ci$r)
    tagList(
      tiles_html(list(
        c(Lh("Correlation r", "相関係数 r"), fnum(ci$r, 2), Lh(w[1], w[2]), "1"),
        c("R²", sprintf("%.0f%%", 100 * ci$r2), Lh("of the ups and downs “shared”", "の上下が「共通」")),
        c(Lh("Data points", "データ点"), ci$n, p_txt(ci$p)))),
      P(sprintf("With only %d points, the 95%% CI for r runs from %.2f to %.2f. A small p-value here does not make the link real: this pair was chosen because it matched.",
                ci$n, ci$lo, ci$hi),
        sprintf("データ点は %d 個だけなので、r の95%%信頼区間は %.2f から %.2f まで。p が小さくても関係が本物になるわけではありません。この組み合わせは「一致したから」選ばれたのです。",
                ci$n, ci$lo, ci$hi), cls = "small muted"))
  })

  output$sp_dredge_txt <- renderUI({
    dr <- dredge(); d <- sp_data()
    if (is.null(dr)) return(NULL)
    div(class = "verdict warn",
      P(sprintf("The best of 1,000 random series reached r = %.2f. %.0f%% of them were at least as strong as the real pair (|r| ≥ %.2f), and %.0f%% passed |r| ≥ .70.",
                dr$rbest, 100 * dr$share, abs(d$ci$r), 100 * dr$share7),
        sprintf("ランダムなデータ1,000個のうち、いちばん一致したものは r = %.2f。実際の組み合わせと同じくらい強いもの（|r| ≥ %.2f）が %.0f%%、|r| ≥ .70 のものが %.0f%% ありました。",
                dr$rbest, abs(d$ci$r), 100 * dr$share, 100 * dr$share7)))
  })

  # ===== 2. ENRO =====
  en_df <- reactiveVal(ENRO_DEFAULT)
  en_src <- reactiveVal("enro")
  observeEvent(input$en_upload, {
    f <- input$en_upload
    tryCatch({
      df <- read.csv(f$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      if (length(num_vars(df)) < 2) stop("need at least two numeric columns")
      en_df(df); en_src(f$name)
      showNotification(L(sprintf("Loaded %s (%d rows).", f$name, nrow(df)), sprintf("%s を読み込みました（%d 行）。", f$name, nrow(df))), type = "message")
    }, error = function(e) showNotification(tagList(L("Could not use that CSV:", "その CSV は使えません："), " ", conditionMessage(e)), type = "error"))
  })
  observeEvent(input$en_reset, { en_df(ENRO_DEFAULT); en_src("enro") })

  output$en_about <- renderUI({
    df <- en_df()
    if (en_src() == "enro")
      P(tagList(sprintf("%d Japanese university students who are L2 readers of English, a subset of the ", nrow(df)),
                tags$a(href = "https://doi.org/10.1111/lang.12586", target = "_blank", "ENRO project"),
                " (7,000+ readers in 19 countries). Every dot is one student."),
        tagList(sprintf("英語を第二言語として読む日本の大学生 %d 人のデータです。19か国・7,000人以上が参加した ", nrow(df)),
                tags$a(href = "https://doi.org/10.1111/lang.12586", target = "_blank", "ENRO プロジェクト"),
                " の一部です。点1つが学生1人です。"), cls = "small")
    else P(sprintf("Your file: %s · %d rows.", en_src(), nrow(df)), sprintf("あなたのファイル：%s · %d 行", en_src(), nrow(df)), cls = "small")
  })

  output$en_pickers <- renderUI({
    df <- en_df(); nv <- num_vars(df); gv <- grp_vars(df)
    dx <- if ("vocab_score" %in% nv) "vocab_score" else nv[1]
    dy <- if ("read_comp_score" %in% nv) "read_comp_score" else nv[2]
    tagList(
      div(class = "grid2",
        sel("en_x", L("X: predictor (across)", "X：説明変数（横軸）"), var_choices(nv), selected = isolate(if (!is.null(input$en_x) && input$en_x %in% nv) input$en_x else dx)),
        sel("en_y", L("Y: outcome (up)", "Y：結果変数（縦軸）"), var_choices(nv), selected = isolate(if (!is.null(input$en_y) && input$en_y %in% nv) input$en_y else dy))),
      sel("en_g", L("Split by group", "グループで分ける"),
          c(setNames("none", Lc("No groups", "分けない")), setNames(gv, vapply(gv, function(g) { l <- grp_lab(g); Lc(l[1], l[2]) }, ""))), selected = "none"))
  })
  observeEvent(input$en_pick, {
    pk <- input$en_pick
    updateSelectInput(session, "en_x", selected = pk$x)
    updateSelectInput(session, "en_y", selected = pk$y)
  })

  en_calc <- reactive({
    df <- en_df(); xv <- input$en_x; yv <- input$en_y
    req(xv %in% names(df), yv %in% names(df))
    g <- input$en_g %||% "none"
    x <- suppressWarnings(as.numeric(df[[xv]])); y <- suppressWarnings(as.numeric(df[[yv]]))
    gr <- if (g != "none" && g %in% names(df)) as.character(df[[g]]) else rep("all", nrow(df))
    ok <- is.finite(x) & is.finite(y) & !is.na(gr) & gr != ""
    x <- x[ok]; y <- y[ok]; gr <- gr[ok]
    ci <- cor_info(x, y)
    lev <- if (g == "none") "all" else names(sort(table(gr), decreasing = TRUE))
    groups <- lapply(lev, function(l) { m <- gr == l; c(list(name = l, n = sum(m)), cor_info(x[m], y[m])[c("r", "b0", "b1", "lo", "hi", "p")]) })
    nv <- num_vars(df)
    nv <- nv[seq_len(min(length(nv), 14))]
    M <- suppressWarnings(cor(as.matrix(df[nv]), use = "pairwise.complete.obs"))
    list(xv = xv, yv = yv, g = g, x = x, y = y, gr = gr, lev = lev, ci = ci, groups = groups, nv = nv, M = M)
  })

  observe({
    cc <- en_calc()
    lx <- var_lab(cc$xv); ly <- var_lab(cc$yv)
    session$sendCustomMessage("erl_en", list(
      pts = cbind(rr(cc$x, 4), rr(cc$y, 4), match(cc$gr, cc$lev) - 1),
      xlab = Lc(lx[1], lx[2]), ylab = Lc(ly[1], ly[2]),
      lev = I(cc$lev), grouped = cc$g != "none",
      b0 = scal(cc$ci$b0, 6), b1 = scal(cc$ci$b1, 6), r = scal(cc$ci$r, 3),
      gfits = lapply(cc$groups, function(gg) list(b0 = scal(gg$b0, 6), b1 = scal(gg$b1, 6))),
      line = isTRUE(input$en_line), resid = isTRUE(input$en_resid), jitter = isTRUE(input$en_jitter),
      mx = scal(mean(cc$x), 4), my = scal(mean(cc$y), 4),
      nv = I(cc$nv), nvlab = I(vapply(unname(cc$nv), function(v) { l <- var_lab(v); Lc(sub(" \\(.*", "", l[1]), sub("（.*", "", l[2])) }, "")),
      M = round(cc$M, 2), sel = I(c(match(cc$xv, cc$nv) - 1, match(cc$yv, cc$nv) - 1))))
  })

  output$en_head <- renderUI({
    cc <- en_calc(); lx <- var_lab(cc$xv); ly <- var_lab(cc$yv)
    fig_head(L(paste(ly[1], "vs.", lx[1]), paste0(ly[2], " と ", lx[2])), L(sprintf("n = %d students", cc$ci$n), sprintf("n = %d 人", cc$ci$n)))
  })

  output$en_tiles <- renderUI({
    ci <- en_calc()$ci
    if (is.na(ci$r)) return(div(class = "verdict warn", L("Not enough data for these two variables.", "この2つの変数ではデータが足りません。")))
    w <- r_words(ci$r)
    tiles_html(list(
      c(Lh("Correlation r", "相関係数 r"), fnum(ci$r, 2), Lh(sprintf("95%% CI [%.2f, %.2f]", ci$lo, ci$hi), sprintf("95%%信頼区間 [%.2f, %.2f]", ci$lo, ci$hi)), "1"),
      c("R²", sprintf("%.0f%%", 100 * ci$r2), Lh("of the differences in Y go with X", "の Y のばらつきが X と一緒に動く")),
      c(Lh("Slope b", "傾き b"), formatC(ci$b1, format = "g", digits = 3), Lh("change in Y for +1 in X", "X が1増えたときの Y の変化")),
      c("p", fmt_p(ci$p), sprintf("t(%d) = %.2f", ci$n - 2, ci$t))))
  })

  output$en_plain <- renderUI({
    cc <- en_calc(); ci <- cc$ci
    if (is.na(ci$r)) return(NULL)
    lx <- var_lab(cc$xv); ly <- var_lab(cc$yv)
    lxs <- tolower(sub(" \\(.*", "", lx[1])); lys <- tolower(sub(" \\(.*", "", ly[1]))
    lxj <- sub("（.*", "", lx[2]); lyj <- sub("（.*", "", ly[2])
    w <- r_words(ci$r)
    dir_en <- if (ci$r >= 0) "higher" else "lower"; dir_ja <- if (ci$r >= 0) "高い" else "低い"
    div(class = "plain",
      mini(L("In plain words", "ことばで言うと")),
      P(sprintf("There is %s between %s and %s (r = %.2f). Students with higher %s tended to have %s %s. %s explains about %.0f%% of the differences in %s; the other %.0f%% comes from everything else. This is a correlation in one group of students, so it does not show that %s causes %s.",
                w[1], lxs, lys, ci$r, lxs, dir_en, lys, sub("^(.)", "\\U\\1", lxs, perl = TRUE), 100 * ci$r2, lys, 100 * (1 - ci$r2), lxs, lys),
        sprintf("%sと%sの間には%sがあります（r = %.2f）。%sが高い学生ほど、%sが%s傾向がありました。%sは%sのばらつきの約 %.0f%% を説明し、残りの %.0f%% はそれ以外のすべてによるものです。これは1つの学生集団の中の相関なので、%sが%sの原因だとは言えません。",
                lxj, lyj, w[2], ci$r, lxj, lyj, dir_ja, lxj, lyj, 100 * ci$r2, 100 * (1 - ci$r2), lxj, lyj)))
  })

  output$en_groups <- renderUI({
    cc <- en_calc()
    if (cc$g == "none") return(NULL)
    rows <- vapply(seq_along(cc$groups), function(i) {
      gg <- cc$groups[[i]]
      sprintf('<tr><td><span class="gdot g%d"></span>%s</td><td class="num">%d</td><td class="num">%s</td><td class="num">%s</td></tr>',
              (i - 1) %% 6, esc(gg$name), gg$n, if (is.na(gg$r)) "—" else fnum(gg$r, 2),
              if (is.na(gg$r)) "—" else sprintf("[%.2f, %.2f]", gg$lo, gg$hi))
    }, "")
    gl <- grp_lab(cc$g)
    div(class = "readout",
      HTML(paste0('<table class="mini-table"><thead><tr><th>', Lh(gl[1], gl[2]), '</th><th class="num">n</th><th class="num">r</th><th class="num">',
                  Lh("95% CI", "95%信頼区間"), '</th></tr></thead><tbody>', paste(rows, collapse = ""), '</tbody></table>')),
      P("Small groups give wide intervals: a difference in r between groups is only convincing if the intervals barely overlap.",
        "小さいグループでは区間が広くなります。グループ間の r の違いは、区間がほとんど重ならないときにだけ説得力があります。", cls = "small muted"))
  })

  # ===== 3. Venn / Euler =====
  output$vn_pickers <- renderUI({
    df <- en_df(); nv <- num_vars(df)
    dflt <- function(v, k) if (v %in% nv) v else nv[min(k, length(nv))]
    tagList(
      sel("vn_y", L("Outcome (Y)", "結果変数（Y）"), var_choices(nv), selected = dflt("read_comp_score", 1)),
      div(class = "grid2",
        sel("vn_x1", L("Predictor 1", "説明変数1"), var_choices(nv), selected = dflt("vocab_score", 2)),
        sel("vn_x2", L("Predictor 2", "説明変数2"), var_choices(nv), selected = dflt("lisn_score", 3))))
  })

  vn_calc <- reactive({
    df <- en_df(); req(input$vn_y, input$vn_x1, input$vn_x2)
    req(all(c(input$vn_y, input$vn_x1, input$vn_x2) %in% names(df)))
    num <- function(v) suppressWarnings(as.numeric(df[[v]]))
    v <- venn_parts(num(input$vn_y), num(input$vn_x1), num(input$vn_x2))
    list(v = v, y = input$vn_y, x1 = input$vn_x1, x2 = input$vn_x2)
  })

  observe({
    vc <- vn_calc(); v <- vc$v
    if (is.null(v)) return()
    lab <- function(k) { l <- var_lab(k); Lc(sub(" \\(.*", "", l[1]), sub("（.*", "", l[2])) }
    same <- length(unique(c(vc$y, vc$x1, vc$x2))) < 3
    session$sendCustomMessage("erl_vn", list(
      ry1 = scal(v$ry1, 4), ry2 = scal(v$ry2, 4), r12 = scal(v$r12, 4), R2 = scal(v$R2, 4),
      D = scal(v$D, 4), E = scal(v$E, 4), G = scal(v$G, 4), A = scal(v$A, 4), F = scal(v$F, 4),
      ylab = lab(vc$y), x1lab = lab(vc$x1), x2lab = lab(vc$x2), show = input$vn_show %||% "both", same = same))
  })

  output$vn_head <- renderUI({
    vc <- vn_calc(); v <- vc$v
    if (is.null(v)) return(NULL)
    show <- input$vn_show %||% "both"
    r2 <- switch(show, x1 = v$ry1^2, x2 = v$ry2^2, both = v$R2)
    fig_head(L(sprintf("R² = %.2f", r2), sprintf("R² = %.2f", r2)),
             L(sprintf("n = %d complete cases", v$n), sprintf("欠測のない %d 人", v$n)))
  })

  output$vn_table <- renderUI({
    vc <- vn_calc(); v <- vc$v
    if (is.null(v)) return(div(class = "verdict warn", L("Not enough complete cases.", "欠測のないデータが足りません。")))
    if (length(unique(c(vc$y, vc$x1, vc$x2))) < 3) return(div(class = "verdict warn", L("Pick three different variables.", "3つの違う変数を選んでください。")))
    l1 <- var_lab(vc$x1); l2 <- var_lab(vc$x2); ly <- var_lab(vc$y)
    s1 <- function(l) sub(" \\(.*", "", l[1]); s2 <- function(l) sub("（.*", "", l[2])
    pct <- function(x) sprintf("%.1f%%", 100 * x)
    rows <- list(
      c(Lh(sprintf("r(%s, %s)²", s1(ly), s1(l1)), sprintf("r(%s, %s)²", s2(ly), s2(l1))), pct(v$ry1^2), Lh("Y with predictor 1 alone", "説明変数1だけのときの R²")),
      c(Lh(sprintf("r(%s, %s)²", s1(ly), s1(l2)), sprintf("r(%s, %s)²", s2(ly), s2(l2))), pct(v$ry2^2), Lh("Y with predictor 2 alone", "説明変数2だけのときの R²")),
      c(Lh("R² with both", "両方を入れた R²"), pct(v$R2), Lh("not the sum of the two above!", "上の2つの合計ではない！")),
      c(Lh('<span class="sw sw-d"></span>unique to predictor 1', '<span class="sw sw-d"></span>説明変数1に固有'), pct(v$D), "sr₁²"),
      c(Lh('<span class="sw sw-e"></span>unique to predictor 2', '<span class="sw sw-e"></span>説明変数2に固有'), pct(v$E), "sr₂²"),
      c(Lh('<span class="sw sw-g"></span>shared by both', '<span class="sw sw-g"></span>両方で共有'), pct(v$G), Lh("R² − sr₁² − sr₂²", "R² − sr₁² − sr₂²")),
      c(Lh('<span class="sw sw-a"></span>unexplained', '<span class="sw sw-a"></span>説明されない'), pct(v$A), "1 − R²"),
      c(Lh("r between the predictors", "説明変数どうしの r"), fnum(v$r12, 2), Lh(sprintf("they overlap by %s", pct(v$r12^2)), sprintf("重なり %s", pct(v$r12^2)))),
      c(Lh("Standardized β", "標準化係数 β"), sprintf("%.2f / %.2f", v$b1, v$b2), Lh("β₁ (predictor 1) / β₂ (predictor 2)", "β₁（説明変数1）／β₂（説明変数2）")))
    trs <- vapply(rows, function(r) sprintf('<tr><td>%s</td><td class="num">%s</td><td class="muted">%s</td></tr>', r[1], r[2], r[3]), "")
    warn <- if (v$G < -0.005)
      P(sprintf("The shared area is negative (%.1f%%). This is suppression: together the predictors explain more than the sum of their single correlations would suggest. As Heiss notes, negative areas can't be drawn.", 100 * v$G),
        sprintf("共有部分がマイナスになっています（%.1f%%）。これは抑制（サプレッション）と呼ばれ、2つを一緒に入れると、それぞれの相関から予想されるより多く説明できる状態です。Heiss も指摘するように、マイナスの面積は描けません。", 100 * v$G), cls = "verdict warn small")
    else NULL
    tagList(div(class = "readout", HTML(paste0('<table class="mini-table vn-table"><tbody>', paste(trs, collapse = ""), '</tbody></table>'))), warn,
      P("Unique shares (sr²) are what each predictor adds when the other is already in the model. The shared part is why adding a second, correlated predictor raises R² by less than you might expect.",
        "固有の寄与（sr²）は、もう一方がすでにモデルに入っているときに、その説明変数が追加で説明する部分です。共有部分があるため、相関のある2つめの説明変数を加えても、R² は思ったほど増えません。", cls = "small muted"))
  })
}
