# =============================================================================
# Writing development · ライティングの発達
# Upload one paragraph per row, tick the measures you want, and download one row of
# results per paragraph. Metrics live in textmetrics.R; charts in viz-writing.js.
# Sourced by app.R, which defines L(), P(), Lh(), Lc(), sel(), kicker(), mini(), fig().
# =============================================================================

WR_DEMO <- read_text_csv("data/writing_demo.csv")$df

# Measure families shown as tick boxes. cols = result columns each family adds.
WR_FAMILIES <- list(
  counts = list(en = "Word & sentence counts", ja = "語数・文数",
                cols = c("n_words", "n_types", "n_sentences")),
  ld = list(en = "Lexical diversity (TTR, Guiraud, MATTR, MTLD, HD-D)", ja = "語彙の多様性（TTR・Guiraud・MATTR・MTLD・HD-D）",
            cols = c("ttr", "guiraud", "mattr50", "mtld", "hdd")),
  complex = list(en = "Sentence complexity (length, clause markers)", ja = "文の複雑さ（文の長さ・節の標識）",
                 cols = c("mls", "sub_per_sent", "coord_per_sent")),
  articles = list(en = "Articles (the, a/an) and a/an flags", ja = "冠詞（the, a/an）と a/an の要確認",
                  cols = c("art_the", "art_a_an", "art_per100", "art_def_share", "art_aan_flags")),
  causal = list(en = "Causal connectives (Halliday & Hasan)", ja = "因果の接続表現（Halliday & Hasan）",
                cols = c("causal_hh", "causal_hh_per100", "causal_hh_per_sent", paste0("c_", names(CAUSAL)), "causal_ext", "causal_review")),
  ithink = list(en = "“I think”", ja = "“I think”", cols = c("i_think", "i_think_per100"))
)

# Labels for result columns (used in the chart picker and tables)
WR_LABELS <- list(
  n_words = c("Words (tokens)", "語数（トークン）"), n_types = c("Different words (types)", "異なり語数（タイプ）"),
  n_sentences = c("Sentences", "文数"), mls = c("Mean length of sentence (words)", "平均文長（語）"),
  sub_per_sent = c("Subordinators per sentence", "1文あたりの従属接続詞・関係詞"),
  coord_per_sent = c("Coordinators per sentence", "1文あたりの等位接続詞"),
  ttr = c("TTR (types ÷ tokens)", "TTR（タイプ÷トークン）"), guiraud = c("Guiraud's R (types ÷ √tokens)", "Guiraud の R（タイプ÷√トークン）"),
  mattr50 = c("MATTR (50-word window)", "MATTR（50語の窓）"), mtld = c("MTLD", "MTLD"), hdd = c("HD-D (0–1)", "HD-D（0〜1）"),
  art_the = c("the (count)", "the（回数）"), art_a_an = c("a / an (count)", "a / an（回数）"),
  art_per100 = c("Articles per 100 words", "100語あたりの冠詞"), art_def_share = c("Share of articles that are 'the'", "冠詞のうち the の割合"),
  art_aan_flags = c("a/an flags (check by hand)", "a/an の要確認（手で確認）"),
  causal_hh = c("Causal connectives (H&H, count)", "因果の接続表現（H&H、回数）"),
  causal_hh_per100 = c("Causal connectives per 100 words", "100語あたりの因果の接続表現"),
  causal_hh_per_sent = c("Causal connectives per sentence", "1文あたりの因果の接続表現"),
  causal_ext = c("Extended connectives (not in H&H)", "拡張の接続表現（H&H 外）"),
  causal_review = c("Connective hits to review", "要確認の接続表現"),
  i_think = c("“I think” (count)", "“I think”（回数）"), i_think_per100 = c("“I think” per 100 words", "100語あたりの “I think”")
)
for (k in names(CAUSAL)) WR_LABELS[[paste0("c_", k)]] <- c(paste0("H&H: ", CAUSAL[[k]]$en), paste0("H&H：", CAUSAL[[k]]$ja))

wr_lab <- function(k) { l <- WR_LABELS[[k]]; if (is.null(l)) k else Lc(l[1], l[2]) }

CAUSAL_COLS <- list(`External/internal` = c("External/internal", "外的／内的", "var(--treat)"),
                    Internal = c("Internal", "内的", "var(--violet)"),
                    Conditional = c("Conditional", "条件", "var(--gold)"),
                    Respective = c("Respective", "観点", "var(--accent)"),
                    Extended = c("Extended (not in H&H)", "拡張（H&H 外）", "var(--ctrl)"))
causal_spec <- function(k) if (k == "extended") CAUSAL_EXT else CAUSAL[[k]]

# Halliday & Hasan's causal row as an HTML table
causal_table_html <- function() {
  rows <- lapply(c(CAUSAL, list(extended = CAUSAL_EXT)), function(s) {
    cc <- CAUSAL_COLS[[s$col]]
    tags$tr(tags$td(span(class = "wr-sw", style = paste0("background:", cc[3])), L(cc[1], cc[2])),
            tags$td(L(s$en, s$ja)), tags$td(class = "wr-ex", s$ex))
  })
  tags$table(class = "mini-table wr-hh", tags$thead(tags$tr(tags$th(L("Column", "列")), tags$th(L("Type", "種類")), tags$th(L("Examples", "例")))),
             tags$tbody(rows))
}

writing_tab <- nav_panel(L("Writing", "ライティング"), value = "Writing",
  div(class = "wrap corr-wrap",
    div(class = "corr-intro",
      kicker(L("Corpus tools for learner writing", "学習者の作文のためのコーパス・ツール")),
      h2(L("Measure writing development, one paragraph per row", "1行に1段落：ライティングの発達を測る")),
      P("Upload a CSV with one paragraph per row, tick the measures you want, and download one row of results per paragraph, ready for R, HAD or Excel. Everything runs in your browser: the texts never leave this computer.",
        "1行に1段落の CSV を読み込み、測りたい指標にチェックを入れると、段落ごとに1行の結果をダウンロードできます（R・HAD・Excel でそのまま使えます）。処理はすべてブラウザの中で行われ、テキストがこのコンピューターの外に出ることはありません。", cls = "lede-sm")),
    navset_underline(id = "wr_sub",
      nav_panel(tagList("1 · ", L("Data", "データ")), value = "data",
        div(class = "tool",
          div(class = "ctrl-col",
            h4(L("Load your paragraphs", "段落を読み込む")),
            P(tagList("One row per paragraph. Columns: ", code("student_id"), ", ", code("project"), ", ", code("attempt"), ", ",
                      code("version"), " (optional: written / edited), ", code("text"), ". In Excel, save as ", strong("CSV UTF-8"), "."),
              tagList("1行に1段落。列：", code("student_id"), "、", code("project"), "、", code("attempt"), "、",
                      code("version"), "（任意：written／edited）、", code("text"), "。Excel では ", strong("CSV UTF-8"), " で保存してください。"), cls = "small"),
            fileInput("wr_upload", NULL, accept = c(".csv", "text/csv"), buttonLabel = L("Choose CSV…", "CSV を選ぶ…"), placeholder = "No file / ファイルなし"),
            div(class = "btn-row",
              downloadButton("wr_template", L("Template (CSV)", "テンプレート（CSV）"), class = "btn-ghost btn-sm"),
              actionButton("wr_demo", L("Use the demo paragraphs", "デモの段落を使う"), class = "btn-ghost btn-sm")),
            uiOutput("wr_load_msg"),
            h4(L("Which column is which?", "どの列が何か")),
            div(class = "grid2",
              sel("wr_col_id", L("Student / text ID", "学生・テキスト ID"), c("—" = "")),
              sel("wr_col_text", L("Paragraph text", "段落のテキスト"), c("—" = ""))),
            div(class = "grid2",
              sel("wr_col_group", L("Compare groups by", "グループの比較に使う列"), c("(none)" = "")),
              sel("wr_col_group2", L("…and split by (optional)", "…さらに分ける列（任意）"), c("(none)" = "")))),
          div(class = "fig-col wide",
            div(class = "sticky-inner",
              uiOutput("wr_data_summary"),
              uiOutput("wr_preview"))))),
      nav_panel(tagList("2 · ", L("Measures", "指標")), value = "measures",
        div(class = "tool",
          div(class = "ctrl-col",
            checkboxGroupInput("wr_fams", L("Measures to compute", "計算する指標"),
                               choiceNames = unname(lapply(WR_FAMILIES, function(f) L(f$en, f$ja))), choiceValues = names(WR_FAMILIES),
                               selected = names(WR_FAMILIES)),
            radioButtons("wr_mtld", L("MTLD convention", "MTLD の方式"), inline = FALSE,
                         choiceNames = list(L("McCarthy & Jarvis (2010): factor ends at TTR ≤ .72", "McCarthy & Jarvis (2010)：TTR ≤ .72 で区切る"),
                                            L("TAALED / Python lexical_diversity: TTR < .72 and ≥ 10 words", "TAALED／Python 版：TTR < .72 かつ10語以上")),
                         choiceValues = c("mj", "taaled")),
            input_switch("wr_ext", L("Also count extended connectives (thus, that's why…)", "拡張の接続表現（thus, that's why など）も数える"), TRUE),
            div(class = "btn-row",
              downloadButton("wr_results_csv", L("Results: one row per paragraph", "結果：1段落1行"), class = "btn-primary btn-sm"),
              downloadButton("wr_summary_csv", L("Group means (CSV)", "グループ平均（CSV）"), class = "btn-ghost btn-sm")),
            div(class = "note", mini(L("Read the numbers carefully", "数値の読み方")),
              tags$ul(
                LI("HD-D needs at least 42 words and MATTR at least 50, otherwise the cell is empty. MTLD and TTR are unstable for very short paragraphs.",
                   "HD-D は42語以上、MATTR は50語以上が必要で、足りないときは空欄になります。とても短い段落では MTLD と TTR は不安定です。"),
                LI("Sentence complexity here counts surface markers (because, which, when…), not parsed clauses. Use it to compare texts processed the same way, such as written vs. edited versions.",
                   "ここでの文の複雑さは、構文解析ではなく表面の標識（because, which, when など）を数えたものです。同じ方法で処理したテキストどうし（例：原文と添削版）の比較に使ってください。"),
                LI("a/an flags are spelling-based guesses for a person to check (an hour is fine; a university is fine).",
                   "a/an の要確認は綴りからの推測なので、人が確認してください（an hour や a university は正しい）。")))),
          div(class = "fig-col wide",
            div(class = "sticky-inner",
              div(class = "grid2",
                sel("wr_metric", L("Show", "表示する指標"), c("MTLD" = "mtld")),
                sel("wr_chart_group", L("Group", "グループ"), c("(none)" = ""))),
              fig_head(L("One dot per paragraph", "点は1段落"), L("bar = group mean", "線＝グループ平均")),
              fig("wr-dots"),
              uiOutput("wr_group_table"))))),
      nav_panel(tagList("3 · ", L("Causal connectives", "因果の接続表現")), value = "causal",
        div(class = "tool",
          div(class = "ctrl-col",
            h4(L("Halliday & Hasan (1976): causal relations", "Halliday & Hasan (1976)：因果関係")),
            P("Each hit is matched from a word list, longest phrase first. Ambiguous words get a rule and, if still unclear, a review flag: so (intensifier?), then (time?), for (preposition?), otherwise (conditional or respective?).",
              "語のリストから、長い句を優先して照合します。あいまいな語には規則を当て、それでも分からないときは要確認の印を付けます：so（強調？）、then（時間？）、for（前置詞？）、otherwise（条件か観点か？）。", cls = "small"),
            causal_table_html(),
            div(class = "switches",
              input_switch("wr_kwic_review", L("Show only hits to review", "要確認のものだけ表示"), FALSE)),
            sel("wr_kwic_cat", L("Type", "種類"), c("All ｜ すべて" = "")),
            downloadButton("wr_kwic_csv", L("Concordance (CSV) for hand-checking", "コンコーダンス（CSV）：手で確認する用"), class = "btn-ghost btn-sm"),
            P(tagList("The CSV has an empty ", code("keep"), " column: write 1 or 0 for each hit, then recount in Excel or R."),
              tagList("CSV には空の ", code("keep"), " 列があります。各行に 1 か 0 を書き、Excel や R で数え直してください。"), cls = "small muted")),
          div(class = "fig-col wide",
            div(class = "sticky-inner",
              fig_head(L("Causal connectives per 100 words", "100語あたりの因果の接続表現"), L("by group and column of the table", "グループ別・表の列別")),
              fig("wr-causal"),
              uiOutput("wr_kwic"))))),
      nav_panel(tagList("4 · ", L("Read a paragraph", "段落を読む")), value = "read",
        div(class = "tool",
          div(class = "ctrl-col",
            sel("wr_read_row", L("Paragraph", "段落"), c("—" = "")),
            uiOutput("wr_read_tiles"),
            div(class = "note", mini(L("Key", "凡例")),
              div(class = "wr-key", lapply(names(CAUSAL_COLS), function(k) span(class = "wr-chip", style = paste0("border-color:", CAUSAL_COLS[[k]][3]),
                                                                              L(CAUSAL_COLS[[k]][1], CAUSAL_COLS[[k]][2]))),
                  span(class = "wr-chip wr-flag", L("a/an to check", "a/an 要確認")),
                  span(class = "wr-chip wr-rev", L("review", "要確認"))))),
          div(class = "fig-col wide",
            div(class = "sticky-inner", uiOutput("wr_read_text"))))))))

# ---- Server -------------------------------------------------------------------------
writing_server <- function(input, output, session) {

  wr_data <- reactiveVal(WR_DEMO)
  wr_msg <- reactiveVal(list(ok = TRUE, text = Lh("Showing the demo paragraphs. Load your own CSV to replace them.",
                                                  "デモの段落を表示しています。自分の CSV を読み込むと置き換わります。")))

  set_cols <- function(df) {
    cols <- names(df)
    id <- guess_col(cols, c("studentid", "id", "anonid", "participant", "participantid", "textid", "filename", "file"), cols[1])
    tx <- guess_col(cols, c("text", "paragraph", "essay", "response", "writing", "content", "body"),
                    cols[which.max(vapply(df, function(v) mean(nchar(v)), 0))])
    gr <- guess_col(cols, c("project", "task", "version", "group", "condition", "type"), "")
    g2 <- if (identical(gr, "project") && "version" %in% cols) "version" else ""
    updateSelectInput(session, "wr_col_id", choices = cols, selected = id)
    updateSelectInput(session, "wr_col_text", choices = cols, selected = tx)
    updateSelectInput(session, "wr_col_group", choices = c("(none)" = "", cols), selected = gr)
    updateSelectInput(session, "wr_col_group2", choices = c("(none)" = "", cols), selected = g2)
  }
  observeEvent(TRUE, set_cols(WR_DEMO), once = TRUE)

  observeEvent(input$wr_upload, {
    tryCatch({
      r <- read_text_csv(input$wr_upload$datapath)
      if (!nrow(r$df)) stop("No rows found.")
      wr_data(r$df); set_cols(r$df)
      wr_msg(list(ok = TRUE, text = paste0(Lh(sprintf("Loaded %d rows from %s.", nrow(r$df), esc(input$wr_upload$name)),
                                               sprintf("%s から %d 行を読み込みました。", esc(input$wr_upload$name), nrow(r$df))),
                                           if (!is.null(r$note)) paste0(" ", esc(r$note)) else "")))
    }, error = function(e) wr_msg(list(ok = FALSE, text = paste(Lh("Could not read that CSV:", "CSV を読み込めませんでした："), esc(conditionMessage(e))))))
  })
  observeEvent(input$wr_demo, {
    wr_data(WR_DEMO); set_cols(WR_DEMO)
    wr_msg(list(ok = TRUE, text = Lh("Showing the demo paragraphs.", "デモの段落を表示しています。")))
  })
  output$wr_load_msg <- renderUI({ m <- wr_msg(); div(class = paste("verdict small", if (m$ok) "good" else "bad"), HTML(m$text)) })

  output$wr_template <- downloadHandler(filename = "writing_template.csv",
    content = function(file) file.copy("data/templates/writing_template.csv", file))

  cols_ok <- reactive({
    df <- wr_data()
    !is.null(input$wr_col_text) && input$wr_col_text %in% names(df) && !is.null(input$wr_col_id) && input$wr_col_id %in% names(df)
  })

  # group label for each row: one or two columns pasted together
  group_of <- function(df, g1, g2) {
    g1 <- g1 %||% ""; g2 <- g2 %||% ""
    a <- if (nzchar(g1) && g1 %in% names(df)) df[[g1]] else rep("", nrow(df))
    b <- if (nzchar(g2) && g2 %in% names(df)) df[[g2]] else rep("", nrow(df))
    out <- ifelse(nzchar(b), paste(a, b, sep = " · "), a)
    out[!nzchar(out)] <- "all"
    out
  }

  # ---- compute everything once per data/options ----
  results <- reactive({
    req(cols_ok())
    df <- wr_data()
    txt <- df[[input$wr_col_text]]
    taaled <- identical(input$wr_mtld, "taaled")
    ext <- isTRUE(input$wr_ext)
    rows <- lapply(txt, function(t) writing_measures(t, extended = ext, taaled = taaled))
    res <- as.data.frame(do.call(rbind, lapply(rows, function(r) unlist(r))), stringsAsFactors = FALSE)
    res
  })

  kwic <- reactive({
    req(cols_ok())
    df <- wr_data(); txt <- df[[input$wr_col_text]]; ids <- df[[input$wr_col_id]]
    grp <- group_of(df, input$wr_col_group, input$wr_col_group2)
    out <- lapply(seq_along(txt), function(i) {
      t <- norm_text(txt[i]); h <- find_causal(t, isTRUE(input$wr_ext))
      if (!nrow(h)) return(NULL)
      k <- lapply(seq_len(nrow(h)), function(j) kwic_line(t, h$start[j], h$end[j]))
      data.frame(row = i, id = ids[i], group = grp[i], type = h$key,
                 column = vapply(h$key, function(x) causal_spec(x)$col, ""),
                 left = vapply(k, `[[`, "", "left"), hit = vapply(k, `[[`, "", "hit"), right = vapply(k, `[[`, "", "right"),
                 review = ifelse(h$review, h$why, ""), stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, out)
    if (is.null(out)) data.frame(row = integer(0), id = character(0), group = character(0), type = character(0), column = character(0),
                                 left = character(0), hit = character(0), right = character(0), review = character(0)) else out
  })

  sel_cols <- reactive({
    f <- input$wr_fams %||% character(0)
    as.character(unique(unlist(lapply(WR_FAMILIES[intersect(names(WR_FAMILIES), f)], `[[`, "cols"))))
  })

  results_out <- reactive({
    df <- wr_data(); res <- results()
    cols <- intersect(sel_cols(), names(res))
    out <- cbind(df[, setdiff(names(df), input$wr_col_text), drop = FALSE], round(res[, cols, drop = FALSE], 4))
    out$text <- df[[input$wr_col_text]]
    out
  })

  # ---- data tab ----
  output$wr_data_summary <- renderUI({
    req(cols_ok())
    df <- wr_data(); res <- results()
    n <- nrow(df); ids <- length(unique(df[[input$wr_col_id]]))
    short42 <- sum(res$n_words < 42); short50 <- sum(res$n_words < 50)
    tiles <- list(
      list(L("Paragraphs", "段落"), fint(n), L(sprintf("%d IDs", ids), sprintf("ID %d 件", ids))),
      list(L("Words in total", "総語数"), fint(sum(res$n_words)), L(sprintf("median %d per paragraph", as.integer(median(res$n_words))),
                                                                  sprintf("段落あたり中央値 %d 語", as.integer(median(res$n_words))))),
      list(L("Under 42 words", "42語未満"), fint(short42), L("no HD-D for these", "HD-D は計算できない")),
      list(L("Under 50 words", "50語未満"), fint(short50), L("no MATTR for these", "MATTR は計算できない")))
    div(class = "tiles", lapply(tiles, function(t) div(class = "tile", div(class = "tile-label", t[[1]]), div(class = "tile-num", t[[2]]), div(class = "tile-sub", t[[3]]))))
  })

  output$wr_preview <- renderUI({
    req(cols_ok())
    df <- wr_data(); res <- results(); k <- min(nrow(df), 8)
    other <- setdiff(names(df), input$wr_col_text)
    tagList(
      h4(L(sprintf("First %d rows", k), sprintf("最初の %d 行", k))),
      tags$table(class = "mini-table wr-preview",
        tags$thead(tags$tr(lapply(other, tags$th), tags$th(L("Words", "語数")), tags$th(L("Text", "テキスト")))),
        tags$tbody(lapply(seq_len(k), function(i) tags$tr(
          lapply(other, function(cn) tags$td(df[[cn]][i])), tags$td(class = "num", res$n_words[i]),
          tags$td(class = "wr-snip", substr(df[[input$wr_col_text]][i], 1, 140), if (nchar(df[[input$wr_col_text]][i]) > 140) "…"))))))
  })

  # ---- measures tab ----
  observe({
    cols <- sel_cols(); cur <- isolate(input$wr_metric)
    if (!length(cols)) cols <- "n_words"
    ch <- setNames(cols, vapply(cols, wr_lab, ""))
    updateSelectInput(session, "wr_metric", choices = ch, selected = if (!is.null(cur) && cur %in% cols) cur else if ("mtld" %in% cols) "mtld" else cols[1])
  })
  observe({
    df <- wr_data(); cols <- names(df)
    cur <- isolate(input$wr_chart_group)
    updateSelectInput(session, "wr_chart_group", choices = c("(same as Data tab) ｜ （データタブと同じ）" = "", cols),
                      selected = if (!is.null(cur) && cur %in% cols) cur else "")
  })
  chart_groups <- reactive({
    df <- wr_data()
    g <- input$wr_chart_group %||% ""
    if (nzchar(g)) group_of(df, g, "") else group_of(df, input$wr_col_group, input$wr_col_group2)
  })

  observe({
    req(cols_ok(), input$wr_metric)
    res <- results(); k <- input$wr_metric
    req(k %in% names(res))
    df <- wr_data(); g <- chart_groups(); v <- res[[k]]
    lv <- unique(g)
    pts <- lapply(seq_along(v), function(i) list(g = match(g[i], lv) - 1, v = scal(v[i], 3), id = as.character(df[[input$wr_col_id]][i])))
    means <- vapply(lv, function(x) mean(v[g == x], na.rm = TRUE), 0)
    session$sendCustomMessage("erl_wr_dots", list(target = "wr-dots", groups = I(lv), pts = pts, means = arr(means, 3), ylab = wr_lab(k)))
  })

  group_summary <- reactive({
    res <- results(); cols <- intersect(sel_cols(), names(res)); g <- chart_groups()
    do.call(rbind, lapply(unique(g), function(x) {
      r <- res[g == x, cols, drop = FALSE]
      out <- data.frame(group = x, n = nrow(r), check.names = FALSE)
      for (cn in cols) {
        v <- r[[cn]][is.finite(r[[cn]])]
        out[[paste0(cn, "_mean")]] <- if (length(v)) round(mean(v), 4) else NA
        out[[paste0(cn, "_sd")]] <- if (length(v) > 1) round(sd(v), 4) else NA
      }
      out
    }))
  })

  output$wr_group_table <- renderUI({
    req(cols_ok(), input$wr_metric)
    res <- results(); k <- input$wr_metric; req(k %in% names(res)); g <- chart_groups(); lv <- unique(g)
    tags$table(class = "mini-table",
      tags$thead(tags$tr(tags$th(L("Group", "グループ")), tags$th("n"), tags$th("M"), tags$th("SD"), tags$th(L("Min", "最小")), tags$th(L("Max", "最大")))),
      tags$tbody(lapply(lv, function(x) { v <- res[[k]][g == x]; v2 <- v[is.finite(v)]
        tags$tr(tags$td(x), tags$td(class = "num", length(v)), tags$td(class = "num", if (length(v2)) fnum(mean(v2)) else "—"),
                tags$td(class = "num", if (length(v2) > 1) fnum(sd(v2)) else "—"),
                tags$td(class = "num", if (length(v2)) fnum(min(v2)) else "—"), tags$td(class = "num", if (length(v2)) fnum(max(v2)) else "—")) })))
  })

  output$wr_results_csv <- downloadHandler(filename = "writing_measures.csv",
    content = function(file) write.csv(results_out(), file, row.names = FALSE, fileEncoding = "UTF-8", na = ""))
  output$wr_summary_csv <- downloadHandler(filename = "writing_group_means.csv",
    content = function(file) write.csv(group_summary(), file, row.names = FALSE, fileEncoding = "UTF-8", na = ""))

  # ---- causal tab ----
  observe({
    keys <- c(names(CAUSAL), if (isTRUE(input$wr_ext)) "extended")
    cur <- isolate(input$wr_kwic_cat)
    ch <- c("All ｜ すべて" = "", setNames(keys, vapply(keys, function(k) Lc(causal_spec(k)$en, causal_spec(k)$ja), "")))
    updateSelectInput(session, "wr_kwic_cat", choices = ch, selected = if (!is.null(cur) && cur %in% keys) cur else "")
  })

  observe({
    req(cols_ok())
    res <- results(); g <- chart_groups(); lv <- unique(g); kw <- kwic()
    cats <- names(CAUSAL_COLS)
    if (!isTRUE(input$wr_ext)) cats <- setdiff(cats, "Extended")
    vals <- lapply(lv, function(x) {
      words <- sum(res$n_words[g == x])
      rows_in <- which(g == x)
      vapply(cats, function(cc) if (words > 0) 100 * sum(kw$column == cc & kw$row %in% rows_in) / words else 0, 0)
    })
    session$sendCustomMessage("erl_wr_causal", list(target = "wr-causal", groups = I(lv),
      cats = lapply(cats, function(cc) list(key = cc, lab = Lc(CAUSAL_COLS[[cc]][1], CAUSAL_COLS[[cc]][2]), col = CAUSAL_COLS[[cc]][3])),
      vals = lapply(vals, function(v) arr(v, 3))))
  })

  kwic_view <- reactive({
    kw <- kwic()
    if (isTRUE(input$wr_kwic_review)) kw <- kw[nzchar(kw$review), , drop = FALSE]
    if (nzchar(input$wr_kwic_cat %||% "")) kw <- kw[kw$type == input$wr_kwic_cat, , drop = FALSE]
    kw
  })

  output$wr_kwic <- renderUI({
    kw <- kwic_view(); n <- nrow(kw)
    if (!n) return(P("No hits for this selection.", "この条件に合うものはありません。", cls = "muted"))
    show <- kw[seq_len(min(n, 300)), , drop = FALSE]
    tagList(
      h4(L(sprintf("Concordance: %d hits%s", n, if (n > 300) " (first 300 shown)" else ""),
           sprintf("コンコーダンス：%d 件%s", n, if (n > 300) "（最初の300件を表示）" else ""))),
      div(class = "wr-kwic-wrap", tags$table(class = "mini-table wr-kwic",
        tags$thead(tags$tr(tags$th("ID"), tags$th(L("Type", "種類")), tags$th(class = "kl", ""), tags$th(L("Hit", "該当")), tags$th(""), tags$th(L("Review", "要確認")))),
        tags$tbody(lapply(seq_len(nrow(show)), function(i) {
          r <- show[i, ]; cc <- CAUSAL_COLS[[r$column]][3]
          tags$tr(tags$td(r$id), tags$td(class = "small", L(causal_spec(r$type)$en, causal_spec(r$type)$ja)),
                  tags$td(class = "kl", r$left), tags$td(class = "kh", style = paste0("border-bottom:2px solid ", cc), r$hit),
                  tags$td(class = "kr", r$right), tags$td(class = "small wr-why", r$review))
        })))))
  })

  output$wr_kwic_csv <- downloadHandler(filename = "causal_connectives_concordance.csv",
    content = function(file) {
      kw <- kwic()
      kw$type_label <- vapply(kw$type, function(k) causal_spec(k)$en, "")
      kw$keep <- ""
      write.csv(kw[, c("id", "group", "column", "type", "type_label", "left", "hit", "right", "review", "keep")], file,
                row.names = FALSE, fileEncoding = "UTF-8")
    })

  # ---- read tab ----
  observe({
    req(cols_ok())
    df <- wr_data(); ids <- df[[input$wr_col_id]]; g <- group_of(df, input$wr_col_group, input$wr_col_group2)
    lab <- paste0(seq_along(ids), ". ", ids, ifelse(g == "all", "", paste0(" · ", g)))
    cur <- isolate(input$wr_read_row)
    updateSelectInput(session, "wr_read_row", choices = setNames(as.character(seq_along(ids)), lab),
                      selected = if (!is.null(cur) && cur %in% as.character(seq_along(ids))) cur else "1")
  })

  output$wr_read_text <- renderUI({
    req(cols_ok(), input$wr_read_row)
    i <- as.integer(input$wr_read_row); df <- wr_data(); req(i >= 1, i <= nrow(df))
    t <- norm_text(df[[input$wr_col_text]][i])
    h <- find_causal(t, isTRUE(input$wr_ext)); a <- find_aan(t)
    marks <- rbind(
      if (nrow(h)) data.frame(start = h$start, end = h$end, kind = "c", key = h$key, why = h$why, stringsAsFactors = FALSE),
      if (nrow(a)) data.frame(start = a$start, end = a$end, kind = "a", key = "", why = "", stringsAsFactors = FALSE))
    html <- character(0); pos <- 1
    if (!is.null(marks) && nrow(marks)) {
      marks <- marks[order(marks$start), ]
      for (j in seq_len(nrow(marks))) {
        s <- marks$start[j]; e <- marks$end[j]
        if (s < pos) next
        html <- c(html, esc(substr(t, pos, s - 1)))
        seg <- esc(substr(t, s, e))
        if (marks$kind[j] == "a") html <- c(html, sprintf('<span class="wr-flag" title="a/an: check">%s</span>', seg))
        else {
          sp <- causal_spec(marks$key[j]); cc <- CAUSAL_COLS[[sp$col]][3]
          ttl <- paste0(sp$en, if (nzchar(marks$why[j])) paste0(" — review: ", marks$why[j]) else "")
          html <- c(html, sprintf('<span class="wr-c%s" style="border-color:%s;background:color-mix(in srgb, %s 16%%, transparent)" title="%s">%s</span>',
                                  if (nzchar(marks$why[j])) " wr-rev" else "", cc, cc, esc(ttl), seg))
        }
        pos <- e + 1
      }
    }
    html <- c(html, esc(substr(t, pos, nchar(t))))
    div(class = "wr-read", HTML(paste(html, collapse = "")))
  })

  output$wr_read_tiles <- renderUI({
    req(cols_ok(), input$wr_read_row)
    i <- as.integer(input$wr_read_row); res <- results(); req(i >= 1, i <= nrow(res))
    r <- res[i, ]
    f <- function(x, k = 2) if (is.na(x)) "—" else fnum(x, k)
    div(class = "tiles wr-tiles",
      div(class = "tile", div(class = "tile-label", L("Words", "語数")), div(class = "tile-num", fint(r$n_words)),
          div(class = "tile-sub", L(sprintf("%d sentences", r$n_sentences), sprintf("%d 文", r$n_sentences)))),
      div(class = "tile", div(class = "tile-label", "MTLD"), div(class = "tile-num", f(r$mtld, 1)), div(class = "tile-sub", paste("HD-D", f(r$hdd, 3)))),
      div(class = "tile", div(class = "tile-label", L("Causal / 100 w", "因果／100語")), div(class = "tile-num", f(r$causal_hh_per100, 2)),
          div(class = "tile-sub", L(sprintf("%d hits", r$causal_hh), sprintf("%d 件", r$causal_hh)))),
      div(class = "tile", div(class = "tile-label", L("Words / sentence", "1文の語数")), div(class = "tile-num", f(r$mls, 1)),
          div(class = "tile-sub", L(sprintf("%s subordinators / sent.", f(r$sub_per_sent)), sprintf("1文あたり従属標識 %s", f(r$sub_per_sent))))))
  })
}
