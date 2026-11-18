# =============================================================================
# Alternate Uses Task · 代替用途課題（AUT）
#   1. Data: one response per row (student_id, item, response_order, condition, response)
#   2. Code: Wahbeh et al. codebook — validity, category, elaboration, originality —
#      one response at a time, with keyboard shortcuts and keyword suggestions
#   3. Scores: fluency, flexibility, elaboration, originality per participant × item
#   4. Two raters: percent agreement and Krippendorff's alpha, plus a disagreement list
#   5. Semantic distance: in-browser multilingual model, or export/import for SemDis
# Sourced by app.R after textmetrics.R and aut_codebook.R. JS: viz-aut.js.
# =============================================================================

AUT_DEMO <- read_text_csv("data/aut_demo.csv")$df
AUT_KEYS <- c(as.character(1:9), "0", "q", "w", "e", "r", "t", "y")
AUT_SD_MODEL <- "Xenova/paraphrase-multilingual-MiniLM-L12-v2"
SEMDIS_URL <- "https://cap.ist.psu.edu/semdis"

aut_chars <- function(x) nchar(gsub("[\\s\\p{P}\\p{S}]", "", x, perl = TRUE))
aut_words <- function(x) {
  jp <- grepl("[\\p{Han}\\p{Hiragana}\\p{Katakana}]", x, perl = TRUE)
  w <- lengths(regmatches(x, gregexpr("\\S+", x, perl = TRUE)))
  ifelse(jp, NA_integer_, w)
}
num_or_na <- function(v) suppressWarnings(as.numeric(v))
kbd <- function(x) tags$kbd(x)
# a stable, HTML-safe input id for each item string (so stale selects never map to the wrong item)
aut_map_id <- function(it) paste0("aut_map_", paste(sprintf("%x", utf8ToInt(enc2utf8(it))), collapse = ""))

# Participant × item × condition scores from coded rows
aut_scores <- function(d) {
  if (!nrow(d)) return(data.frame())
  key <- paste(d$id, d$item, d$cond, sep = "\r")
  out <- lapply(split(seq_len(nrow(d)), factor(key, levels = unique(key))), function(ix) {
    r <- d[ix, , drop = FALSE]
    ok <- is.na(r$valid) | r$valid == 1
    coded <- (!is.na(r$valid) & r$valid == 0) | !is.na(r$category)
    v <- r[ok, , drop = FALSE]
    mx <- function(x) { x <- x[!is.na(x)]; if (length(x)) max(x) else NA_real_ }
    mn <- function(x) { x <- x[!is.na(x)]; if (length(x)) mean(x) else NA_real_ }
    data.frame(student_id = r$id[1], item = r$item[1], condition = r$cond[1],
               n_responses = nrow(r), n_uncoded = sum(!coded),
               fluency = sum(ok), flexibility = length(unique(v$category[!is.na(v$category)])),
               elaboration_max = mx(v$elaboration), elaboration_mean = round(mn(v$elaboration), 3),
               originality_max = mx(v$originality), originality_mean = round(mn(v$originality), 3),
               chars_mean = round(mn(v$n_chars), 2), words_mean = round(mn(v$n_words), 2),
               sd_local_mean = round(mn(v$sd_local), 4), sd_local_max = round(mx(v$sd_local), 4),
               semdis_mean = round(mn(v$semdis), 4), semdis_max = round(mx(v$semdis), 4),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, unname(out))
}

AUT_SCORE_LABELS <- list(
  fluency = c("Fluency", "流暢性"), flexibility = c("Flexibility", "柔軟性"),
  elaboration_max = c("Elaboration (max 0–2)", "精緻性（最大 0〜2）"), elaboration_mean = c("Elaboration (mean)", "精緻性（平均）"),
  originality_max = c("Originality (max 0–2)", "独創性（最大 0〜2）"), originality_mean = c("Originality (mean)", "独創性（平均）"),
  chars_mean = c("Characters per response (auto)", "1回答の文字数（自動）"), words_mean = c("Words per response (auto, English)", "1回答の語数（自動・英語）"),
  sd_local_mean = c("Semantic distance, in-browser (mean)", "意味的距離・ブラウザ内（平均）"), sd_local_max = c("Semantic distance, in-browser (max)", "意味的距離・ブラウザ内（最大）"),
  semdis_mean = c("SemDis prediction (mean)", "SemDis の予測値（平均）"), semdis_max = c("SemDis prediction (max)", "SemDis の予測値（最大）"))

aut_tab <- nav_panel(L("AUT", "AUT 採点"), value = "AUT",
  div(class = "wrap corr-wrap",
    div(class = "corr-intro",
      kicker(L("Alternate Uses Task", "代替用途課題（AUT）")),
      h2(L("Score Japanese AUT responses with the Wahbeh codebook", "Wahbeh のコードブックで日本語の AUT 回答を採点する")),
      P("Code each response for validity, category, elaboration and originality using the codebook from Wahbeh et al. (newspaper, brick, envelope, hanger, or your own item). The app turns the codes into fluency, flexibility, elaboration and originality scores, checks agreement between two raters, and adds semantic distance.",
        "Wahbeh らのコードブック（新聞・レンガ・封筒・ハンガー、または自分で作った項目）を使って、各回答の妥当性・カテゴリー・精緻性・独創性をコーディングします。コードから流暢性・柔軟性・精緻性・独創性の得点を出し、2人の評定者の一致を確かめ、意味的距離も加えられます。", cls = "lede-sm")),
    navset_underline(id = "aut_sub",
      nav_panel(tagList("1 · ", L("Data", "データ")), value = "data",
        div(class = "tool",
          div(class = "ctrl-col",
            h4(L("Load AUT responses", "AUT の回答を読み込む")),
            P(tagList("One idea per row: ", code("student_id"), ", ", code("item"), ", ", code("response_order"), ", ", code("condition"), ", ",
                      code("response"), ". To continue coding, load a file you downloaded from the Code tab: the codes come back with it."),
              tagList("1行に1つのアイデア：", code("student_id"), "、", code("item"), "、", code("response_order"), "、", code("condition"), "、",
                      code("response"), "。コーディングを続けるときは、「コーディング」タブでダウンロードしたファイルを読み込めば、コードも戻ります。"), cls = "small"),
            fileInput("aut_upload", NULL, accept = c(".csv", "text/csv"), buttonLabel = L("Choose CSV…", "CSV を選ぶ…"), placeholder = "No file / ファイルなし"),
            div(class = "btn-row",
              downloadButton("aut_template", L("Template (CSV)", "テンプレート（CSV）"), class = "btn-ghost btn-sm"),
              actionButton("aut_demo", L("Use the demo responses", "デモの回答を使う"), class = "btn-ghost btn-sm")),
            uiOutput("aut_load_msg"),
            h4(L("Which column is which?", "どの列が何か")),
            div(class = "grid2",
              sel("aut_col_id", L("Participant ID", "参加者 ID"), c("—" = "")),
              sel("aut_col_item", L("Item (object)", "項目（物）"), c("—" = ""))),
            div(class = "grid2",
              sel("aut_col_resp", L("Response", "回答"), c("—" = "")),
              sel("aut_col_order", L("Response order (optional)", "回答の順番（任意）"), c("(none)" = ""))),
            sel("aut_col_cond", L("Condition / time (optional)", "条件・時期（任意）"), c("(none)" = "")),
            input_switch("aut_split", L("Cells hold several ideas: split on line breaks and ; ；", "1つのセルに複数のアイデア：改行と ; ； で分ける"), FALSE)),
          div(class = "fig-col wide",
            div(class = "sticky-inner",
              uiOutput("aut_items_table"),
              uiOutput("aut_preview"))))),
      nav_panel(tagList("2 · ", L("Code", "コーディング")), value = "code",
        div(class = "tool aut-tool",
          div(class = "ctrl-col aut-main",
            div(class = "grid2",
              sel("aut_filter", L("Item to code", "コーディングする項目"), c("All ｜ すべて" = "")),
              textInput("aut_coder", L("Coder initials", "コーダーのイニシャル"), placeholder = "e.g. RK")),
            div(id = "aut-coder", uiOutput("aut_card")),
            div(class = "btn-row",
              tags$button(type = "button", class = "btn btn-ghost btn-sm", onclick = "ERL.autCode('nav','prev')", "← ", L("Previous", "前へ")),
              tags$button(type = "button", class = "btn btn-ghost btn-sm", onclick = "ERL.autCode('nav','next')", L("Next", "次へ"), " →"),
              tags$button(type = "button", class = "btn btn-primary btn-sm", onclick = "ERL.autCode('nav','uncoded')", L("Next uncoded", "次の未コード"), " (n)")),
            div(class = "note aut-keys", mini(L("Keyboard", "キーボード")),
              P(tagList(kbd("1"), "–", kbd("9"), " ", kbd("0"), " ", kbd("q"), kbd("w"), kbd("e"), "… ", "category · ", kbd("i"), " not possible · ",
                        kbd("a"), kbd("s"), kbd("d"), " elaboration 0/1/2 · ", kbd("j"), kbd("k"), kbd("l"), " originality 0/1/2 · ",
                        kbd("Enter"), " next · ", kbd("←"), " back · ", kbd("n"), " next uncoded · ", kbd("Backspace"), " clear"),
                tagList(kbd("1"), "〜", kbd("9"), " ", kbd("0"), " ", kbd("q"), kbd("w"), kbd("e"), "… カテゴリー · ", kbd("i"), " 不可能な用途 · ",
                        kbd("a"), kbd("s"), kbd("d"), " 精緻性 0/1/2 · ", kbd("j"), kbd("k"), kbd("l"), " 独創性 0/1/2 · ",
                        kbd("Enter"), " 次へ · ", kbd("←"), " 前へ · ", kbd("n"), " 次の未コード · ", kbd("Backspace"), " 消す"), cls = "small")),
            div(class = "btn-row",
              downloadButton("aut_codes_csv", L("Download coded responses (CSV)", "コード済みの回答をダウンロード（CSV）"), class = "btn-primary btn-sm"),
              actionButton("aut_fill", L("Fill uncoded categories from suggestions", "未コードのカテゴリーを候補で埋める"), class = "btn-ghost btn-sm")),
            P("Suggestions come from keyword lists, so a person should still check every response. Download often: codes are kept only in this browser tab.",
              "候補はキーワードのリストから出しているので、すべての回答を人が確認してください。コードはこのブラウザのタブの中にしかないので、こまめにダウンロードしてください。", cls = "small muted")),
          div(class = "fig-col aut-ref",
            div(class = "sticky-inner",
              uiOutput("aut_codebook_ui"))))),
      nav_panel(tagList("3 · ", L("Scores", "得点")), value = "scores",
        div(class = "tool",
          div(class = "ctrl-col",
            P("One row per participant × item × condition. Fluency counts valid responses; flexibility counts different categories among them. Wahbeh et al. gave one elaboration and one originality score per participant, so the app reports the highest response code (max) and the mean.",
              "参加者 × 項目 × 条件ごとに1行です。流暢性は妥当な回答の数、柔軟性はその中の異なるカテゴリーの数です。Wahbeh らは精緻性と独創性を参加者ごとに1つ付けたので、ここでは回答のコードの最大値と平均値を出します。", cls = "small"),
            div(class = "note", mini(L("Wahbeh codebook rules", "Wahbeh のコードブックの規則")),
              tags$ul(lapply(AUT_RULES, function(r) LI(r[1], r[2])))),
            sel("aut_score_metric", L("Show", "表示する得点"), setNames(names(AUT_SCORE_LABELS), vapply(names(AUT_SCORE_LABELS), function(k) Lc(AUT_SCORE_LABELS[[k]][1], AUT_SCORE_LABELS[[k]][2]), "")),
                selected = "fluency"),
            radioButtons("aut_score_by", L("Group by", "グループ"), inline = TRUE,
                         choiceNames = list(L("Item", "項目"), L("Condition", "条件")), choiceValues = c("item", "condition")),
            downloadButton("aut_scores_csv", L("Download scores (CSV)", "得点をダウンロード（CSV）"), class = "btn-primary btn-sm")),
          div(class = "fig-col wide",
            div(class = "sticky-inner",
              fig_head(L("One dot per participant", "点は1人の参加者"), L("bar = group mean", "線＝グループ平均")),
              fig("aut-score-dots"),
              uiOutput("aut_scores_table"))))),
      nav_panel(tagList("4 · ", L("Two raters", "2人の評定者")), value = "rel",
        div(class = "tool",
          div(class = "ctrl-col",
            P("Each rater codes the same file and downloads it from the Code tab. Load both files here. Responses are matched by row_id, so both raters must start from the same original CSV.",
              "2人がそれぞれ同じファイルをコーディングし、「コーディング」タブからダウンロードします。その2つをここで読み込みます。回答は row_id で対応づけるので、2人とも同じ元の CSV から始めてください。", cls = "small"),
            fileInput("aut_rel_a", L("Rater A", "評定者 A"), accept = ".csv", buttonLabel = L("Choose…", "選ぶ…"), placeholder = "No file / ファイルなし"),
            fileInput("aut_rel_b", L("Rater B", "評定者 B"), accept = ".csv", buttonLabel = L("Choose…", "選ぶ…"), placeholder = "No file / ファイルなし"),
            downloadButton("aut_rel_csv", L("Disagreements for a third rater (CSV)", "第三の評定者用の不一致リスト（CSV）"), class = "btn-ghost btn-sm"),
            P("As in Wahbeh et al., a third, more experienced rater can settle every disagreement. Krippendorff's α: nominal for validity and category, ordinal for elaboration and originality, interval for participant-level fluency and flexibility.",
              "Wahbeh らと同じく、不一致はより経験のある第三の評定者が決められます。Krippendorff の α は、妥当性とカテゴリーは名義、精緻性と独創性は順序、参加者レベルの流暢性と柔軟性は間隔の尺度で計算します。", cls = "small muted")),
          div(class = "fig-col wide",
            div(class = "sticky-inner", uiOutput("aut_rel_out"))))),
      nav_panel(tagList("5 · ", L("Semantic distance", "意味的距離")), value = "sd",
        div(class = "tool",
          div(class = "ctrl-col",
            h4(L("A. Compute in this browser", "A. このブラウザで計算する")),
            P("A multilingual sentence model turns the item and each response into vectors; semantic distance is 1 − cosine similarity. The model (about 120 MB) downloads once from Hugging Face and is cached; the responses never leave this computer.",
              "多言語の文モデルで項目と各回答をベクトルにし、意味的距離＝1 − コサイン類似度とします。モデル（約120 MB）は Hugging Face から一度だけダウンロードされ、保存されます。回答がこのコンピューターの外に出ることはありません。", cls = "small"),
            textInput("aut_sd_model", L("Model (Hugging Face ID)", "モデル（Hugging Face の ID）"), AUT_SD_MODEL),
            div(class = "btn-row", actionButton("aut_sd_run", L("Compute semantic distance", "意味的距離を計算する"), class = "btn-primary btn-sm")),
            uiOutput("aut_sd_status_ui"),
            h4(L("B. Official SemDis (Beaty & Johnson; Patterson et al.)", "B. 公式の SemDis（Beaty & Johnson；Patterson ら）")),
            P(tagList("Download a SemDis-ready CSV (", code("item"), ", ", code("response"), ", plus ", code("row_id"), "), score it at ",
                      tags$a(href = SEMDIS_URL, target = "_blank", rel = "noopener", "cap.ist.psu.edu/semdis"),
                      ", then load the result here. This sends the responses to Penn State's server, and Japanese is not one of the languages its multilingual model was validated on."),
              tagList("SemDis 用の CSV（", code("item"), "、", code("response"), "、", code("row_id"), "）をダウンロードし、",
                      tags$a(href = SEMDIS_URL, target = "_blank", rel = "noopener", "cap.ist.psu.edu/semdis"),
                      " で採点してから、結果をここに読み込みます。回答はペンシルベニア州立大学のサーバーに送られます。また、日本語はその多言語モデルの検証対象の言語に含まれていません。"), cls = "small"),
            downloadButton("aut_semdis_csv", L("SemDis upload file (CSV)", "SemDis 用ファイル（CSV）"), class = "btn-ghost btn-sm"),
            fileInput("aut_semdis_in", NULL, accept = ".csv", buttonLabel = L("Load SemDis results…", "SemDis の結果を読み込む…"), placeholder = "No file / ファイルなし"),
            uiOutput("aut_semdis_msg")),
          div(class = "fig-col wide",
            div(class = "sticky-inner",
              radioButtons("aut_sd_which", NULL, inline = TRUE, choiceNames = list(L("In-browser", "ブラウザ内"), "SemDis"), choiceValues = c("sd_local", "semdis")),
              fig_head(L("Semantic distance by item", "項目ごとの意味的距離"), L("one dot per response", "点は1つの回答")),
              fig("aut-sd-dots"),
              uiOutput("aut_sd_check"))))))))


# ---- Server -------------------------------------------------------------------------
aut_server <- function(input, output, session) {

  aut_file <- reactiveVal(list(df = AUT_DEMO, name = "demo"))
  aut_msg <- reactiveVal(list(ok = TRUE, text = Lh("Showing the demo responses (two items, three students).", "デモの回答を表示しています（2項目・3人）。")))
  codes <- reactiveVal(NULL)
  item_map <- reactiveVal(character(0))   # item -> codebook key ("newspaper", …, or "custom")
  custom_cb <- reactiveVal(list())         # item -> list of categories
  cur <- reactiveVal(1L)
  sd_local <- reactiveVal(numeric(0))      # named by row_id
  semdis <- reactiveVal(numeric(0))
  sd_status <- reactiveVal(NULL)

  set_cols <- function(df) {
    cols <- names(df)
    pick <- function(cands, fb) guess_col(cols, cands, fb)
    updateSelectInput(session, "aut_col_id", choices = cols, selected = pick(c("studentid", "id", "participant", "participantid", "subject", "pid"), cols[1]))
    updateSelectInput(session, "aut_col_item", choices = cols, selected = pick(c("item", "object", "prompt", "task", "target"), cols[min(2, length(cols))]))
    updateSelectInput(session, "aut_col_resp", choices = cols, selected = pick(c("response", "answer", "idea", "use", "text", "responses"), cols[length(cols)]))
    updateSelectInput(session, "aut_col_order", choices = c("(none)" = "", cols), selected = pick(c("responseorder", "order", "number", "n", "seq"), ""))
    updateSelectInput(session, "aut_col_cond", choices = c("(none)" = "", cols), selected = pick(c("condition", "time", "session", "group", "wave"), ""))
  }
  observeEvent(TRUE, set_cols(AUT_DEMO), once = TRUE)

  observeEvent(input$aut_upload, {
    tryCatch({
      r <- read_text_csv(input$aut_upload$datapath)
      if (!nrow(r$df)) stop("No rows found.")
      aut_file(list(df = r$df, name = input$aut_upload$name)); set_cols(r$df)
      has_codes <- any(c("category", "valid") %in% names(r$df))
      aut_msg(list(ok = TRUE, text = paste0(Lh(sprintf("Loaded %d rows from %s.", nrow(r$df), esc(input$aut_upload$name)),
                                               sprintf("%s から %d 行を読み込みました。", esc(input$aut_upload$name), nrow(r$df))),
                                            if (has_codes) paste0(" ", Lh("Existing codes were loaded too.", "既存のコードも読み込みました。")) else "",
                                            if (!is.null(r$note)) paste0(" ", esc(r$note)) else "")))
    }, error = function(e) aut_msg(list(ok = FALSE, text = paste(Lh("Could not read that CSV:", "CSV を読み込めませんでした："), esc(conditionMessage(e))))))
  })
  observeEvent(input$aut_demo, {
    aut_file(list(df = AUT_DEMO, name = "demo")); set_cols(AUT_DEMO)
    aut_msg(list(ok = TRUE, text = Lh("Showing the demo responses.", "デモの回答を表示しています。")))
  })
  output$aut_load_msg <- renderUI({ m <- aut_msg(); div(class = paste("verdict small", if (m$ok) "good" else "bad"), HTML(m$text)) })
  output$aut_template <- downloadHandler(filename = "aut_template.csv",
    content = function(file) file.copy("data/templates/aut_template.csv", file))

  # ---- long data, one response per row ----
  rows <- reactive({
    f <- aut_file(); df <- f$df; cols <- names(df)
    ci <- input$aut_col_id; cit <- input$aut_col_item; cr <- input$aut_col_resp
    req(ci %in% cols, cit %in% cols, cr %in% cols)
    get <- function(cn) if (!is.null(cn) && nzchar(cn) && cn %in% cols) df[[cn]] else rep("", nrow(df))
    base <- data.frame(src = seq_len(nrow(df)), id = trimws(df[[ci]]), item = trimws(df[[cit]]), order = get(input$aut_col_order),
                       cond = get(input$aut_col_cond), response = df[[cr]], stringsAsFactors = FALSE)
    base$row_id <- if ("row_id" %in% cols && !anyDuplicated(df$row_id) && all(nzchar(df$row_id))) df$row_id else sprintf("r%04d", base$src)
    if (isTRUE(input$aut_split)) {
      parts <- strsplit(base$response, "\\s*(?:\\n|;|；)\\s*", perl = TRUE)
      k <- lengths(parts)
      base <- base[rep(seq_len(nrow(base)), pmax(1, k)), , drop = FALSE]
      sub <- unlist(lapply(k, function(n) if (n <= 1) 1 else seq_len(n)))
      base$response <- unlist(lapply(parts, function(p) if (length(p)) p else ""))
      base$row_id <- ifelse(rep(k, pmax(1, k)) > 1, paste0(base$row_id, ".", sub), base$row_id)
      if (all(!nzchar(base$order))) base$order <- as.character(sub)
    }
    base$response <- trimws(base$response)
    base <- base[nzchar(base$response), , drop = FALSE]
    base$n_chars <- aut_chars(base$response); base$n_words <- aut_words(base$response)
    rownames(base) <- NULL
    base
  })

  # (re)initialize codes and the item → codebook map whenever the rows change
  observeEvent(rows(), {
    d <- rows(); df <- aut_file()$df
    pull <- function(cn) if (cn %in% names(df) && !isTRUE(input$aut_split)) num_or_na(df[[cn]][d$src]) else rep(NA_real_, nrow(d))
    v <- pull("valid"); v[is.na(v)] <- 1
    src <- if ("cat_source" %in% names(df) && !isTRUE(input$aut_split)) df$cat_source[d$src] else rep("", nrow(d))
    codes(data.frame(row_id = d$row_id, valid = v, category = pull("category"), elaboration = pull("elaboration"),
                     originality = pull("originality"), cat_source = ifelse(is.na(src), "", src), stringsAsFactors = FALSE))
    its <- unique(d$item); old <- item_map()
    im <- vapply(its, function(it) if (it %in% names(old)) old[[it]] else aut_item_key(it) %||% "custom", "")
    item_map(setNames(im, its))
    if ("sd_local" %in% names(df) && !isTRUE(input$aut_split)) sd_local(setNames(num_or_na(df$sd_local[d$src]), d$row_id))
    if ("semdis" %in% names(df) && !isTRUE(input$aut_split)) semdis(setNames(num_or_na(df$semdis[d$src]), d$row_id))
    cur(1L)
    its_lab <- vapply(its, function(it) { k <- item_map()[[it]]; if (k == "custom") paste0(it, " (custom ｜ 自作)") else paste0(it, " → ", AUT_CODEBOOK[[k]]$en, " ｜ ", AUT_CODEBOOK[[k]]$ja) }, "")
    updateSelectInput(session, "aut_filter", choices = c("All ｜ すべて" = "", setNames(its, its_lab)), selected = "")
  })

  cats_for <- function(item) {
    k <- item_map()[item]
    if (is.na(k)) return(list())
    if (k == "custom") custom_cb()[[item]] %||% list() else AUT_CODEBOOK[[k]]$cats
  }

  full <- reactive({
    d <- rows(); cd <- codes(); req(!is.null(cd), nrow(cd) == nrow(d), identical(cd$row_id, d$row_id))
    d$valid <- cd$valid; d$category <- cd$category; d$elaboration <- cd$elaboration; d$originality <- cd$originality; d$cat_source <- cd$cat_source
    s1 <- sd_local(); s2 <- semdis()
    d$sd_local <- if (length(s1)) unname(s1[d$row_id]) else NA_real_
    d$semdis <- if (length(s2)) unname(s2[d$row_id]) else NA_real_
    d$category_label <- vapply(seq_len(nrow(d)), function(i) {
      c <- d$category[i]; if (is.na(c)) return("")
      cc <- cats_for(d$item[i]); if (c >= 1 && c <= length(cc)) cc[[c]]$en else as.character(c) }, "")
    d
  })

  # ---- data tab ----
  output$aut_items_table <- renderUI({
    d <- rows(); im <- item_map()
    its <- unique(d$item)
    tagList(
      div(class = "tiles",
        div(class = "tile", div(class = "tile-label", L("Responses", "回答")), div(class = "tile-num", fint(nrow(d))), div(class = "tile-sub", L("one idea each", "1つずつのアイデア"))),
        div(class = "tile", div(class = "tile-label", L("Participants", "参加者")), div(class = "tile-num", fint(length(unique(d$id)))), div(class = "tile-sub", "")),
        div(class = "tile", div(class = "tile-label", L("Items", "項目")), div(class = "tile-num", fint(length(its))), div(class = "tile-sub", ""))),
      h4(L("Items and their codebooks", "項目とコードブック")),
      tags$table(class = "mini-table",
        tags$thead(tags$tr(tags$th(L("Item", "項目")), tags$th(L("Responses", "回答数")), tags$th(L("Codebook", "コードブック")))),
        tags$tbody(lapply(its, function(it) {
          k <- im[[it]] %||% "custom"
          tags$tr(tags$td(it), tags$td(class = "num", sum(d$item == it)),
                  tags$td(sel(aut_map_id(it), NULL,
                              c(setNames(names(AUT_CODEBOOK), vapply(AUT_CODEBOOK, function(cb) Lc(cb$en, cb$ja), "")), "Custom ｜ 自作" = "custom"),
                              selected = k)))
        }))),
      P("Items are matched to the Wahbeh codebook by name (e.g. レンガ → Brick). For any other object, choose Custom and type its categories on the Code tab.",
        "項目は名前で Wahbeh のコードブックに対応づけています（例：レンガ → Brick）。それ以外の物は「自作」を選び、「コーディング」タブでカテゴリーを入力してください。", cls = "small muted"))
  })
  observe({
    d <- rows(); its <- unique(d$item)
    vals <- lapply(its, function(it) input[[aut_map_id(it)]])
    if (any(vapply(vals, is.null, TRUE))) return()
    nm <- setNames(unlist(vals), its)
    if (!identical(nm, isolate(item_map()))) item_map(nm)
  })

  output$aut_preview <- renderUI({
    d <- rows(); k <- min(nrow(d), 10)
    tagList(h4(L(sprintf("First %d responses", k), sprintf("最初の %d 件の回答", k))),
      tags$table(class = "mini-table",
        tags$thead(tags$tr(tags$th("row_id"), tags$th("ID"), tags$th(L("Item", "項目")), tags$th("#"), tags$th(L("Condition", "条件")), tags$th(L("Response", "回答")))),
        tags$tbody(lapply(seq_len(k), function(i) tags$tr(tags$td(class = "small muted", d$row_id[i]), tags$td(d$id[i]), tags$td(d$item[i]),
                                                       tags$td(d$order[i]), tags$td(d$cond[i]), tags$td(d$response[i]))))))
  })

  # ---- coding ----
  view_idx <- reactive({
    d <- rows(); f <- input$aut_filter %||% ""
    if (nzchar(f)) which(d$item == f) else seq_len(nrow(d))
  })
  observeEvent(input$aut_filter, cur(1L), ignoreInit = TRUE)

  is_coded <- function(cd) (!is.na(cd$valid) & cd$valid == 0) | !is.na(cd$category)

  observeEvent(input$aut_code, {
    m <- input$aut_code; idx <- view_idx(); req(length(idx))
    p <- min(max(1L, cur()), length(idx)); i <- idx[p]
    cd <- codes(); req(!is.null(cd))
    f <- m$field; v <- m$value
    if (f == "nav") {
      if (v == "next") cur(min(p + 1L, length(idx)))
      else if (v == "prev") cur(max(p - 1L, 1L))
      else if (v == "uncoded") {
        un <- which(!is_coded(cd[idx, , drop = FALSE]))
        nx <- un[un > p]; if (!length(nx)) nx <- un
        if (length(nx)) cur(nx[1]) else showNotification(L("Every response here is coded.", "ここの回答はすべてコード済みです。"), type = "message", duration = 2)
      } else if (v == "goto") cur(as.integer(m$pos))
      return()
    }
    if (f == "cat") {
      v <- as.integer(v); nc <- length(cats_for(rows()$item[i]))
      if (is.na(v) || v < 1 || v > nc) return()
      cd$category[i] <- if (identical(cd$category[i], as.numeric(v)) && cd$cat_source[i] != "keyword") NA else v
      cd$cat_source[i] <- "coder"
      if (!is.na(cd$category[i])) cd$valid[i] <- 1
    } else if (f == "valid") {
      cd$valid[i] <- if (identical(cd$valid[i], 0)) 1 else 0
      if (cd$valid[i] == 0) cd$category[i] <- NA
    } else if (f == "elab") {
      v <- as.numeric(v); cd$elaboration[i] <- if (identical(cd$elaboration[i], v)) NA else v
    } else if (f == "orig") {
      v <- as.numeric(v); cd$originality[i] <- if (identical(cd$originality[i], v)) NA else v
    } else if (f == "clear") {
      cd$valid[i] <- 1; cd$category[i] <- NA; cd$elaboration[i] <- NA; cd$originality[i] <- NA; cd$cat_source[i] <- ""
    }
    codes(cd)
  })

  observeEvent(input$aut_fill, {
    d <- rows(); cd <- codes(); idx <- view_idx(); n <- 0
    for (i in idx) if (is.na(cd$category[i]) && !identical(cd$valid[i], 0)) {
      s <- aut_suggest(d$response[i], cats_for(d$item[i]))
      if (length(s)) { cd$category[i] <- s[1]; cd$cat_source[i] <- "keyword"; n <- n + 1 }
    }
    codes(cd)
    showNotification(L(sprintf("Filled %d categories from keywords. They are marked “keyword” until you confirm them.", n),
                       sprintf("キーワードから %d 件のカテゴリーを埋めました。確認するまで「keyword」の印が付きます。", n)), type = "message", duration = 4)
  })

  # share of participants (per item) who used each category: a hint for originality
  cat_share <- reactive({
    d <- full(); out <- list()
    for (it in unique(d$item)) {
      r <- d[d$item == it & (is.na(d$valid) | d$valid == 1) & !is.na(d$category), , drop = FALSE]
      np <- length(unique(d$id[d$item == it]))
      out[[it]] <- if (np) vapply(split(r$id, r$category), function(x) length(unique(x)) / np, 0) else numeric(0)
    }
    out
  })

  output$aut_card <- renderUI({
    idx <- view_idx(); d <- full()
    if (!length(idx)) return(P("No responses for this item.", "この項目の回答はありません。", cls = "muted"))
    p <- min(max(1L, cur()), length(idx)); i <- idx[p]; r <- d[i, ]
    cats <- cats_for(r$item); sug <- aut_suggest(r$response, cats)
    ncoded <- sum(is_coded(d[idx, , drop = FALSE]))
    share <- cat_share()[[r$item]] %||% numeric(0)
    k <- item_map()[[r$item]]
    btn_ <- function(field, value, label, on, extra = "", title = NULL)
      tags$button(type = "button", class = paste("aut-btn", if (on) "on" else "", extra), title = title,
                  onclick = sprintf("ERL.autCode('%s','%s')", field, value), label)
    tagList(
      div(class = "aut-progress", div(class = "aut-bar", style = sprintf("width:%.1f%%", 100 * ncoded / length(idx)))),
      div(class = "aut-meta small muted",
          sprintf("%d / %d · ", p, length(idx)), L(sprintf("%d coded", ncoded), sprintf("%d 件コード済み", ncoded)),
          " · ID ", strong(r$id), " · ", r$item, if (nzchar(r$cond)) paste0(" · ", r$cond), if (nzchar(r$order)) paste0(" · #", r$order)),
      div(class = paste("aut-response", if (identical(r$valid, 0)) "invalid" else ""), r$response),
      div(class = "aut-row",
        mini(L("Possible?", "可能な用途？")),
        btn_("valid", "1", L("Not possible with this object", "この物では不可能"), identical(r$valid, 0), "aut-invalid", "i")),
      div(class = "aut-row",
        mini(L("Category", "カテゴリー")),
        if (!length(cats)) div(class = "verdict warn small",
          L("No categories yet for this item. Add them in the panel on the right.", "この項目にはまだカテゴリーがありません。右のパネルで追加してください。"))
        else div(class = "aut-cats", lapply(seq_along(cats), function(j) {
          cc <- cats[[j]]; sh <- share[as.character(j)]
          btn_("cat", j, tagList(tags$kbd(AUT_KEYS[j] %||% ""), " ", L(cc$en, cc$ja),
                                 if (!is.na(sh)) span(class = "aut-share", sprintf(" %.0f%%", 100 * sh))),
               identical(r$category, as.numeric(j)), paste(if (j %in% sug) "sug" else "", if (j %in% sug[1] && identical(r$cat_source, "keyword") && identical(r$category, as.numeric(j))) "kw" else ""))
        })),
        if (length(sug) && length(cats)) div(class = "small muted", L("Suggested: ", "候補："),
          paste(vapply(sug, function(j) cats[[j]]$en, ""), collapse = ", "),
          if (identical(r$cat_source, "keyword")) span(class = "aut-kw", L(" · filled from keywords: confirm or change", " · キーワードで埋めた値：確認または変更"))),
        if (length(share)) div(class = "small muted", L("% = share of participants who used that category for this item.", "％＝この項目でそのカテゴリーを使った参加者の割合。"))),
      div(class = "grid2",
        div(class = "aut-row", mini(L("Elaboration", "精緻性")),
            div(class = "aut-cats", lapply(0:2, function(v) btn_("elab", v, tagList(tags$kbd(c("a", "s", "d")[v + 1]), " ", v), identical(r$elaboration, as.numeric(v)))))),
        div(class = "aut-row", mini(L("Originality", "独創性")),
            div(class = "aut-cats", lapply(0:2, function(v) btn_("orig", v, tagList(tags$kbd(c("j", "k", "l")[v + 1]), " ", v), identical(r$originality, as.numeric(v))))))))
  })

  output$aut_codebook_ui <- renderUI({
    idx <- view_idx(); d <- rows(); req(length(idx))
    p <- min(max(1L, cur()), length(idx)); it <- d$item[idx[p]]
    k <- item_map()[[it]] %||% "custom"
    if (k == "custom") {
      cats <- custom_cb()[[it]] %||% list()
      txt <- paste(vapply(cats, function(cc) paste(cc$en, cc$ja, paste(cc$kw, collapse = ", "), sep = " | "), ""), collapse = "\n")
      return(tagList(
        h4(L(sprintf("Custom codebook: %s", it), sprintf("自作のコードブック：%s", it))),
        P("One category per line: English | 日本語 | keyword, keyword. Keywords are optional and only suggest a category.",
          "1行に1カテゴリー：English | 日本語 | キーワード, キーワード。キーワードは任意で、候補を出すためだけに使います。", cls = "small"),
        textAreaInput("aut_custom_text", NULL, txt, rows = 10, width = "100%",
                      placeholder = "Shelter | 雨よけ | 雨, 傘, 濡れ\nWeapon | 武器 | 武器, 叩"),
        actionButton("aut_custom_save", L("Save categories", "カテゴリーを保存"), class = "btn-primary btn-sm"),
        div(class = "note", mini(L("Rules", "規則")), tags$ul(lapply(AUT_RULES, function(r) LI(r[1], r[2]))))))
    }
    cb <- AUT_CODEBOOK[[k]]
    tagList(
      h4(L(sprintf("Codebook: %s", cb$en), sprintf("コードブック：%s", cb$ja))),
      tags$details(class = "aut-ref-block", open = NA, tags$summary(L("Elaboration anchors", "精緻性の基準")),
        lapply(0:2, function(v) P(paste0(v, " · ", cb$elab[[v + 1]][1]), paste0(v, " · ", cb$elab[[v + 1]][2]), cls = "small"))),
      tags$details(class = "aut-ref-block", open = NA, tags$summary(L("Originality exemplars", "独創性の例")),
        lapply(c("2", "1", "0"), function(t) div(
          div(class = "mini-label", L(c(`0` = "0 · Non-original", `1` = "1 · Somewhat original", `2` = "2 · Original")[[t]],
                                      c(`0` = "0 · 独創的でない", `1` = "1 · やや独創的", `2` = "2 · 独創的")[[t]])),
          p(class = "small aut-ex", paste(vapply(cb$orig[[t]], `[`, "", 1), collapse = " · ")),
          p(class = "small aut-ex ja", lang = "ja", paste(vapply(cb$orig[[t]], `[`, "", 2), collapse = "・"))))),
      tags$details(class = "aut-ref-block", tags$summary(L("Not possible (examples)", "不可能な用途（例）")), p(class = "small", cb$invalid)),
      tags$details(class = "aut-ref-block", tags$summary(L("Scoring rules", "採点の規則")), tags$ul(lapply(AUT_RULES, function(r) LI(r[1], r[2])))),
      p(class = "small muted", "Codebook: Wahbeh, Cannard, Yount, Delorme & Radin, Supplemental Data (AUT scoring codebook)."))
  })

  observeEvent(input$aut_custom_save, {
    idx <- view_idx(); d <- rows(); p <- min(max(1L, cur()), length(idx)); it <- d$item[idx[p]]
    lines <- trimws(strsplit(input$aut_custom_text %||% "", "\n", fixed = TRUE)[[1]]); lines <- lines[nzchar(lines)]
    cats <- lapply(lines, function(ln) {
      parts <- trimws(strsplit(ln, "\\s*[|｜]\\s*", perl = TRUE)[[1]])
      en <- parts[1]; ja <- if (length(parts) >= 2 && nzchar(parts[2])) parts[2] else en
      kw <- if (length(parts) >= 3) trimws(strsplit(parts[3], "\\s*[,、，]\\s*", perl = TRUE)[[1]]) else character(0)
      list(en = en, ja = ja, kw = kw[nzchar(kw)])
    })
    cb <- custom_cb(); cb[[it]] <- cats; custom_cb(cb)
    showNotification(L(sprintf("Saved %d categories for %s.", length(cats), it), sprintf("%s のカテゴリーを %d 件保存しました。", it, length(cats))), type = "message", duration = 3)
  })

  coded_export <- reactive({
    d <- full()
    data.frame(row_id = d$row_id, student_id = d$id, item = d$item, response_order = d$order, condition = d$cond, response = d$response,
               valid = d$valid, category = d$category, category_label = d$category_label, elaboration = d$elaboration,
               originality = d$originality, cat_source = d$cat_source, coder = trimws(input$aut_coder %||% ""),
               n_chars = d$n_chars, n_words = d$n_words, sd_local = round(d$sd_local, 5), semdis = d$semdis, stringsAsFactors = FALSE)
  })
  output$aut_codes_csv <- downloadHandler(
    filename = function() { cdr <- gsub("[^A-Za-z0-9_-]", "", input$aut_coder %||% ""); paste0("aut_coded", if (nzchar(cdr)) paste0("_", cdr), ".csv") },
    content = function(file) write.csv(coded_export(), file, row.names = FALSE, fileEncoding = "UTF-8", na = ""))

  # ---- scores ----
  scores <- reactive(aut_scores(full()))
  output$aut_scores_csv <- downloadHandler(filename = "aut_scores.csv",
    content = function(file) write.csv(scores(), file, row.names = FALSE, fileEncoding = "UTF-8", na = ""))

  observe({
    s <- scores(); req(nrow(s)); k <- input$aut_score_metric; req(k %in% names(s))
    g <- if (identical(input$aut_score_by, "condition")) ifelse(nzchar(s$condition), s$condition, "all") else s$item
    lv <- unique(g); v <- s[[k]]
    pts <- lapply(seq_along(v), function(i) list(g = match(g[i], lv) - 1, v = scal(v[i], 3), id = s$student_id[i]))
    means <- vapply(lv, function(x) { y <- v[g == x]; y <- y[is.finite(y)]; if (length(y)) mean(y) else NA_real_ }, 0)
    session$sendCustomMessage("erl_wr_dots", list(target = "aut-score-dots", groups = I(lv), pts = pts, means = arr(means, 3),
                                                  ylab = Lc(AUT_SCORE_LABELS[[k]][1], AUT_SCORE_LABELS[[k]][2])))
  })

  output$aut_scores_table <- renderUI({
    s <- scores(); req(nrow(s))
    show <- c("student_id", "item", "condition", "n_responses", "n_uncoded", "fluency", "flexibility", "elaboration_max", "originality_max", "chars_mean", "sd_local_mean", "semdis_mean")
    show <- show[vapply(show, function(cn) !all(is.na(s[[cn]]) | s[[cn]] %in% ""), TRUE)]
    tags$table(class = "mini-table aut-scores",
      tags$thead(tags$tr(lapply(show, function(cn) tags$th(cn)))),
      tags$tbody(lapply(seq_len(min(nrow(s), 200)), function(i) tags$tr(lapply(show, function(cn) {
        v <- s[[cn]][i]; tags$td(class = if (is.numeric(v)) "num" else NULL, if (is.na(v)) "—" else as.character(v)) })))))
  })

  # ---- two raters ----
  read_coded <- function(f) {
    if (is.null(f)) return(NULL)
    df <- read_text_csv(f$datapath)$df
    need <- c("row_id", "valid", "category", "elaboration", "originality")
    miss <- setdiff(need, names(df))
    if (length(miss)) stop(paste("Missing columns:", paste(miss, collapse = ", "), "— use a file downloaded from the Code tab."))
    for (cn in c("valid", "category", "elaboration", "originality", "n_chars", "n_words", "sd_local", "semdis")) if (cn %in% names(df)) df[[cn]] <- num_or_na(df[[cn]])
    df
  }
  rel <- reactive({
    a <- tryCatch(read_coded(input$aut_rel_a), error = function(e) e)
    b <- tryCatch(read_coded(input$aut_rel_b), error = function(e) e)
    if (inherits(a, "error") || inherits(b, "error")) return(list(err = conditionMessage(if (inherits(a, "error")) a else b)))
    if (is.null(a) || is.null(b)) return(NULL)
    m <- merge(a, b, by = "row_id", suffixes = c("_A", "_B"))
    if (!nrow(m)) return(list(err = "No row_id values match between the two files."))
    m$valid_A[is.na(m$valid_A)] <- 1; m$valid_B[is.na(m$valid_B)] <- 1
    both_valid <- m$valid_A == 1 & m$valid_B == 1
    stat <- function(field, level, mask = rep(TRUE, nrow(m))) {
      x <- m[[paste0(field, "_A")]][mask]; y <- m[[paste0(field, "_B")]][mask]
      ok <- !is.na(x) & !is.na(y)
      list(field = field, level = level, n = sum(ok), agree = if (sum(ok)) mean(x[ok] == y[ok]) else NA,
           alpha = kripp_alpha(cbind(x, y), level))
    }
    resp <- list(stat("valid", "nominal"), stat("category", "nominal", both_valid),
                 stat("elaboration", "ordinal", both_valid), stat("originality", "ordinal", both_valid))
    prep <- function(df) {
      df$id <- df$student_id %||% df$row_id; df$item <- df$item %||% ""; df$cond <- df$condition %||% ""
      df$response <- df$response %||% ""; df$n_chars <- df$n_chars %||% NA; df$n_words <- df$n_words %||% NA
      df$sd_local <- df$sd_local %||% NA; df$semdis <- df$semdis %||% NA
      aut_scores(df)
    }
    pa <- prep(a); pb <- prep(b)
    pm <- merge(pa, pb, by = c("student_id", "item", "condition"), suffixes = c("_A", "_B"))
    pstat <- function(field, level) {
      x <- pm[[paste0(field, "_A")]]; y <- pm[[paste0(field, "_B")]]; ok <- !is.na(x) & !is.na(y)
      list(field = field, level = level, n = sum(ok), agree = if (sum(ok)) mean(x[ok] == y[ok]) else NA, alpha = kripp_alpha(cbind(x, y), level))
    }
    part <- list(pstat("fluency", "interval"), pstat("flexibility", "interval"),
                 pstat("elaboration_max", "ordinal"), pstat("originality_max", "ordinal"))
    diff <- m$valid_A != m$valid_B |
      (both_valid & ((is.na(m$category_A) != is.na(m$category_B)) | (!is.na(m$category_A) & !is.na(m$category_B) & m$category_A != m$category_B))) |
      (both_valid & !is.na(m$elaboration_A) & !is.na(m$elaboration_B) & m$elaboration_A != m$elaboration_B) |
      (both_valid & !is.na(m$originality_A) & !is.na(m$originality_B) & m$originality_A != m$originality_B)
    dis <- m[diff, intersect(c("row_id", "student_id_A", "item_A", "response_A", "valid_A", "valid_B", "category_A", "category_label_A", "category_B", "category_label_B",
                               "elaboration_A", "elaboration_B", "originality_A", "originality_B"), names(m)), drop = FALSE]
    list(resp = resp, part = part, n = nrow(m), na = nrow(a), nb = nrow(b), dis = dis)
  })

  output$aut_rel_out <- renderUI({
    r <- rel()
    if (is.null(r)) return(P("Load both raters' files to see agreement.", "2人のファイルを読み込むと一致度が表示されます。", cls = "muted"))
    if (!is.null(r$err)) return(div(class = "verdict bad", r$err))
    lab <- c(valid = "Validity ｜ 妥当性", category = "Category ｜ カテゴリー", elaboration = "Elaboration ｜ 精緻性", originality = "Originality ｜ 独創性",
             fluency = "Fluency ｜ 流暢性", flexibility = "Flexibility ｜ 柔軟性", elaboration_max = "Elaboration, max ｜ 精緻性（最大）",
             originality_max = "Originality, max ｜ 独創性（最大）")
    band <- function(a) if (is.na(a)) "" else if (a >= .8) "good" else if (a >= .667) "warn" else "bad"
    tbl <- function(st, title) tagList(h4(title),
      tags$table(class = "mini-table",
        tags$thead(tags$tr(tags$th(L("Code", "コード")), tags$th(L("Scale", "尺度")), tags$th("n"), tags$th(L("% agree", "一致率")), tags$th("α"))),
        tags$tbody(lapply(st, function(s) tags$tr(tags$td(lab[[s$field]]), tags$td(class = "small muted", s$level), tags$td(class = "num", s$n),
          tags$td(class = "num", if (is.na(s$agree)) "—" else sprintf("%.0f%%", 100 * s$agree)),
          tags$td(class = paste("num", band(s$alpha)), if (is.na(s$alpha)) "—" else sprintf("%.2f", s$alpha)))))))
    tagList(
      P(sprintf("Matched %d responses (rater A: %d, rater B: %d).", r$n, r$na, r$nb), sprintf("%d 件の回答を対応づけました（A：%d 件、B：%d 件）。", r$n, r$na, r$nb), cls = "small"),
      tbl(r$resp, L("Response level", "回答レベル")),
      tbl(r$part, L("Participant level (as reported by Wahbeh et al.)", "参加者レベル（Wahbeh らの報告と同じ）")),
      P("Krippendorff's guide: α ≥ .80 is reliable; .667–.80 allows tentative conclusions. Category and the 0–2 codes are compared only where both raters judged the use possible.",
        "Krippendorff の目安：α ≥ .80 で信頼できる、.667〜.80 は暫定的な結論なら可。カテゴリーと 0〜2 のコードは、2人とも「可能な用途」と判断した回答だけで比べています。", cls = "small muted"),
      h4(L(sprintf("Disagreements: %d", nrow(r$dis)), sprintf("不一致：%d 件", nrow(r$dis)))),
      if (nrow(r$dis)) tags$table(class = "mini-table",
        tags$thead(tags$tr(tags$th("row_id"), tags$th(L("Response", "回答")), tags$th("A"), tags$th("B"))),
        tags$tbody(lapply(seq_len(min(nrow(r$dis), 150)), function(i) { x <- r$dis[i, ]
          fmt <- function(s) paste0(if (identical(x[[paste0("valid_", s)]], 0)) "✕ " else "", x[[paste0("category_label_", s)]] %||% x[[paste0("category_", s)]] %||% "",
                                    " · e", x[[paste0("elaboration_", s)]] %||% "", " · o", x[[paste0("originality_", s)]] %||% "")
          tags$tr(tags$td(class = "small muted", x$row_id), tags$td(x$response_A %||% ""), tags$td(class = "small", fmt("A")), tags$td(class = "small", fmt("B"))) }))))
  })
  output$aut_rel_csv <- downloadHandler(filename = "aut_disagreements.csv",
    content = function(file) { r <- rel(); d <- if (is.null(r) || !is.null(r$err)) data.frame() else r$dis
      if (nrow(d)) { d$final_valid <- ""; d$final_category <- ""; d$final_elaboration <- ""; d$final_originality <- "" }
      write.csv(d, file, row.names = FALSE, fileEncoding = "UTF-8", na = "") })

  # ---- semantic distance ----
  observeEvent(input$aut_sd_run, {
    d <- rows(); req(nrow(d))
    model <- trimws(input$aut_sd_model %||% AUT_SD_MODEL); if (!nzchar(model)) model <- AUT_SD_MODEL
    sd_status(list(stage = "start", msg = "Starting…"))
    session$sendCustomMessage("erl_aut_sd", list(model = model, row_id = I(d$row_id), items = I(d$item), responses = I(d$response)))
  })
  observeEvent(input$aut_sd_status, sd_status(input$aut_sd_status))
  observeEvent(input$aut_sd_result, {
    r <- input$aut_sd_result
    v <- num_or_na(unlist(lapply(r$dist, function(x) if (is.null(x)) NA else x)))
    sd_local(setNames(v, unlist(r$row_id)))
    sd_status(list(stage = "done", msg = sprintf("Done: %d responses scored with %s.", sum(!is.na(v)), r$model)))
    updateRadioButtons(session, "aut_sd_which", selected = "sd_local")
  })
  output$aut_sd_status_ui <- renderUI({
    s <- sd_status(); if (is.null(s)) return(NULL)
    cls <- switch(s$stage %||% "", error = "bad", done = "good", "warn")
    txt <- switch(s$stage %||% "",
      download = sprintf("Downloading the model: %s %.0f%%", s$file %||% "", as.numeric(s$pct %||% 0)),
      embed = sprintf("Scoring responses: %s / %s", s$done %||% "?", s$total %||% "?"),
      s$msg %||% "")
    div(class = paste("verdict small", cls), txt,
        if (identical(s$stage, "error")) P(" Check the internet connection and the model ID. Some school networks block huggingface.co.",
                                           " インターネット接続とモデル ID を確認してください。学校のネットワークでは huggingface.co がブロックされていることがあります。", cls = "small"))
  })

  output$aut_semdis_csv <- downloadHandler(filename = "semdis_upload.csv",
    content = function(file) { d <- rows()
      write.csv(data.frame(row_id = d$row_id, item = d$item, response = d$response, stringsAsFactors = FALSE), file, row.names = FALSE, fileEncoding = "UTF-8") })
  semdis_msg <- reactiveVal(NULL)
  observeEvent(input$aut_semdis_in, {
    tryCatch({
      df <- read_text_csv(input$aut_semdis_in$datapath)$df
      if (!"prediction" %in% names(df)) stop("No 'prediction' column found.")
      d <- rows()
      ids <- if ("row_id" %in% names(df)) df$row_id else if (nrow(df) == nrow(d)) d$row_id else stop("No row_id column, and the row count differs.")
      v <- setNames(num_or_na(df$prediction), ids)
      semdis(v)
      semdis_msg(list(ok = TRUE, text = sprintf("Loaded %d SemDis scores (%d match these responses).", sum(!is.na(v)), sum(names(v) %in% d$row_id))))
      updateRadioButtons(session, "aut_sd_which", selected = "semdis")
    }, error = function(e) semdis_msg(list(ok = FALSE, text = conditionMessage(e))))
  })
  output$aut_semdis_msg <- renderUI({ m <- semdis_msg(); if (is.null(m)) return(NULL); div(class = paste("verdict small", if (m$ok) "good" else "bad"), m$text) })

  observe({
    d <- full(); k <- input$aut_sd_which %||% "sd_local"; v <- d[[k]]
    lv <- unique(d$item)
    pts <- lapply(which(is.finite(v)), function(i) list(g = match(d$item[i], lv) - 1, v = scal(v[i], 4), id = paste0(d$id[i], ": ", d$response[i])))
    means <- vapply(lv, function(x) { y <- v[d$item == x]; y <- y[is.finite(y)]; if (length(y)) mean(y) else NA_real_ }, 0)
    session$sendCustomMessage("erl_wr_dots", list(target = "aut-sd-dots", groups = I(lv), pts = pts, means = arr(means, 4),
                                                  ylab = if (k == "semdis") "SemDis prediction ｜ SemDis の予測値" else "Semantic distance (1 − cos) ｜ 意味的距離（1 − cos）"))
  })

  output$aut_sd_check <- renderUI({
    d <- full(); k <- input$aut_sd_which %||% "sd_local"; v <- d[[k]]
    if (!any(is.finite(v))) return(P("No semantic distance yet: compute it in the browser or load SemDis results.",
                                     "まだ意味的距離がありません。ブラウザで計算するか、SemDis の結果を読み込んでください。", cls = "muted"))
    rr_ <- function(y) { ok <- is.finite(v) & is.finite(y); if (sum(ok) >= 5 && sd(v[ok]) > 0 && sd(y[ok]) > 0) c(cor(v[ok], y[ok]), sum(ok)) else c(NA, sum(ok)) }
    rows_ <- list(list(L("Originality code (0–2)", "独創性のコード（0〜2）"), rr_(d$originality)),
                  list(L("Elaboration code (0–2)", "精緻性のコード（0〜2）"), rr_(d$elaboration)),
                  list(L("Response length (characters)", "回答の長さ（文字数）"), rr_(d$n_chars)))
    tagList(h4(L("Does semantic distance track your human codes?", "意味的距離は人のコードと一致する？")),
      tags$table(class = "mini-table",
        tags$thead(tags$tr(tags$th(L("Compared with", "比べる相手")), tags$th("r"), tags$th("n"))),
        tags$tbody(lapply(rows_, function(x) tags$tr(tags$td(x[[1]]), tags$td(class = "num", if (is.na(x[[2]][1])) "—" else sprintf("%.2f", x[[2]][1])),
                                                    tags$td(class = "num", x[[2]][2]))))),
      P("A useful validity check for Japanese: semantic distance should correlate with your originality codes more than with sheer response length. Response-level r ignores that responses are nested in participants.",
        "日本語での妥当性の確認に役立ちます。意味的距離は、単なる回答の長さよりも独創性のコードと強く相関するはずです。回答レベルの r は、回答が参加者の中に入れ子になっていることを考慮していません。", cls = "small muted"))
  })
}
