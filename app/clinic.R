# =============================================================================
# Research clinic · 研究相談室
# A Breakout-Lab-style tab where students self-assess their thesis project,
# write down their exact question, and (optionally) post it to a shared lab board.
#
# Shinylive runs in each student's browser, so the shared board lives in a Google
# Sheet behind a small Apps Script web app (apps_script/clinic_board.gs).
# Leave CLINIC_URL empty to run in demo mode: posts then stay in this browser tab.
# =============================================================================

CLINIC_URL  <- "https://script.google.com/macros/s/AKfycbyAGVoLPiyB9Sns8zzMsum48Vyzm_UsCBD4SyaNSBR-MFv4AY4XAfU2kWAppRdZzKFW/exec"          # paste the Apps Script web-app URL (ends in /exec) here
CLINIC_CODE <- "kabirlab"  # must match BOARD_CODE in apps_script/clinic_board.gs
CLINIC_MAX_CHARS <- 400

# Topics for the self-check. tab = the app tab that helps; read = where to read more.
CLINIC_TOPICS <- list(
  list(id = "rq", area = "plan", en = "Research question & hypotheses", ja = "研究課題と仮説", tab = "Designs",
       tip = c("Write your question as “Does X relate to / change Y, for whom?” and name one variable you will measure for each part.",
               "研究課題を「誰にとって、X は Y と関係するか／Y を変えるか」の形で書き、それぞれの部分で測る変数を1つずつ決めましょう。"),
       read = "Nygren (2026), Ch. 1"),
  list(id = "design", area = "plan", en = "Choosing a research design", ja = "研究デザインの選び方", tab = "Designs",
       tip = c("Scroll the Designs tab and find the strongest design you can actually run in your setting. Note which threats it leaves open.",
               "「研究デザイン」タブをスクロールし、自分の環境で実際にできる中で最も強いデザインを見つけましょう。残る弱点もメモしておきます。"),
       read = "Nygren (2026), Ch. 1 & 5"),
  list(id = "sample", area = "plan", en = "Sample size & power", ja = "サンプルサイズと検定力", tab = "Effect size",
       tip = c("Guess a realistic effect size from past studies, then use the power figure to see how many participants you need.",
               "先行研究から現実的な効果量を見積もり、検定力の図で何人必要か確かめましょう。"),
       read = "Plonsky (Ed., 2015); Nygren (2026), pp. 17–18"),
  list(id = "review", area = "plan", en = "Literature review / systematic review", ja = "文献レビュー／システマティック・レビュー", tab = "PRISMA 2020",
       tip = c("Keep a log of every database, search string and date. The PRISMA tab turns those counts into a flow diagram.",
               "使ったデータベース、検索語、日付をすべて記録しておきましょう。PRISMA タブで、その件数をフロー図にできます。"),
       read = "Page et al. (2021), PRISMA 2020"),
  list(id = "ethics", area = "plan", en = "Ethics & informed consent", ja = "研究倫理とインフォームド・コンセント", tab = NA,
       tip = c("Check what your university's ethics committee needs before you collect any data, especially with minors or your own students.",
               "データを集める前に、大学の倫理審査で何が必要か確認しましょう。未成年や自分の授業の学生が対象のときは特に大切です。"),
       read = "Your university's research ethics guidelines"),
  list(id = "measure", area = "data", en = "Reliability & validity of my measures", ja = "測定の信頼性と妥当性", tab = NA,
       tip = c("Find who made each scale, what it was validated with, and the reliability (e.g., Cronbach's α or ω) others report.",
               "各尺度を誰が作り、何で妥当性が確かめられ、他の研究でどのくらいの信頼性（クロンバックの α や ω など）が報告されているか調べましょう。"),
       read = "Plonsky (Ed., 2015)"),
  list(id = "translate", area = "data", en = "Translating a questionnaire (EN ⇄ JA)", ja = "質問紙の翻訳（英語⇄日本語）", tab = NA,
       tip = c("Plan forward translation, back-translation and a small pilot. Keep a table of the items you changed and why.",
               "順翻訳・逆翻訳（バックトランスレーション）・小さな予備調査を計画しましょう。変更した項目と理由を表にまとめておきます。"),
       read = "Brislin (1970), back-translation"),
  list(id = "clean", area = "data", en = "Data entry, cleaning & descriptive statistics", ja = "データ入力・整理・記述統計", tab = "Distributions",
       tip = c("Plot every variable before testing anything: look for impossible values, skew and floor/ceiling effects.",
               "検定の前に、すべての変数をグラフにしましょう。ありえない値、ゆがみ、床効果・天井効果がないか見ます。"),
       read = "Winter (2019); Larson-Hall (2016)"),
  list(id = "missing", area = "data", en = "Missing data & outliers", ja = "欠測値と外れ値", tab = NA,
       tip = c("Decide and write down your rules for missing answers and outliers before you look at the results.",
               "欠測や外れ値の扱いのルールは、結果を見る前に決めて書いておきましょう。"),
       read = "Larson-Hall (2016)"),
  list(id = "qual", area = "data", en = "Qualitative data & coding", ja = "質的データとコーディング", tab = NA,
       tip = c("Write a short codebook with a definition and one example for each code, and have someone else code a sample.",
               "各コードに定義と例を1つずつ書いた簡単なコードブックを作り、一部を他の人にもコーディングしてもらいましょう。"),
       read = "Braun & Clarke (2006), thematic analysis"),
  list(id = "groups", area = "stats", en = "Comparing groups (t-test, ANOVA)", ja = "グループの比較（t検定・分散分析）", tab = "p-values",
       tip = c("Say which groups you compare, on what outcome, and whether the same people are measured twice (paired) or not.",
               "どのグループを、どの結果変数で比べるのか、同じ人を2回測るのか（対応あり）どうかを言葉にしましょう。"),
       read = "Winter (2019); Larson-Hall (2016)"),
  list(id = "corr", area = "stats", en = "Correlation & regression (R²)", ja = "相関と回帰（R²）", tab = "Correlation",
       tip = c("Try your variables in the Correlation tab: look at the scatterplot first, then r, then what R² does and doesn't mean.",
               "「相関と R²」タブで自分の変数を試しましょう。まず散布図、次に r、そして R² が意味すること・しないことを確認します。"),
       read = "Winter (2019); Heiss (2021)"),
  list(id = "cat", area = "stats", en = "Categorical data (χ², proportions)", ja = "カテゴリーデータ（χ²・割合）", tab = "Chi-square",
       tip = c("Make the contingency table by hand first. Check that every expected count is at least 5.",
               "まず手でクロス表を作りましょう。期待度数がすべて5以上か確認します。"),
       read = "Larson-Hall (2016)"),
  list(id = "effect", area = "stats", en = "Effect sizes & how big is big", ja = "効果量と「どのくらい大きいか」", tab = "Effect size",
       tip = c("Report an effect size with a confidence interval for every main result, and compare it with field benchmarks, not just Cohen's.",
               "主な結果には必ず効果量と信頼区間を報告し、Cohen の基準だけでなく分野の目安とも比べましょう。"),
       read = "Plonsky (Ed., 2015); Kraft (2020)"),
  list(id = "pval", area = "stats", en = "p-values & statistical significance", ja = "p値と統計的有意性", tab = "p-values",
       tip = c("Run d = 0 in the p-values tab. Then write one sentence on what your p-value would and wouldn't tell a reader.",
               "p値タブで d = 0 を試し、自分の p値が読者に何を伝え、何を伝えないかを1文で書いてみましょう。"),
       read = "Nygren (2026), p. 83"),
  list(id = "software", area = "stats", en = "Using R, HAD or Excel", ja = "R・HAD・Excel の使い方", tab = NA,
       tip = c("Bring a small, anonymized copy of your data and the exact step where you get stuck (error message, screenshot).",
               "匿名化したデータの一部と、つまずいている具体的な手順（エラーメッセージやスクリーンショット）を持ってきましょう。"),
       read = "Winter (2019) for R; Shimizu (2016) for HAD"),
  list(id = "writing", area = "write", en = "Writing the methods & results (APA style)", ja = "方法と結果の書き方（APA スタイル）", tab = NA,
       tip = c("Find one published study close to yours and copy the structure (not the words) of its method and results sections.",
               "自分の研究に近い論文を1本見つけ、その方法・結果のセクションの構成（文章ではなく）をまねしてみましょう。"),
       read = "APA (2020), Publication Manual, 7th ed.")
)
names(CLINIC_TOPICS) <- vapply(CLINIC_TOPICS, `[[`, "", "id")
CLINIC_AREAS <- list(plan = c("Planning", "計画"), data = c("Data & measurement", "データと測定"),
                     stats = c("Statistics", "統計"), write = c("Writing", "執筆"))
RATING_VALUES <- c("ok", "unsure", "help")
RATING_LABELS <- list(ok = c("OK", "大丈夫"), unsure = c("A bit unsure", "少し不安"), help = c("Need help", "助けてほしい"))

CLINIC_LEVELS <- c(setNames("ba", Lc("Bachelor's thesis", "卒業論文")), setNames("ma", Lc("Master's thesis", "修士論文")),
                   setNames("phd", Lc("Doctoral dissertation", "博士論文")), setNames("other", Lc("Other project", "その他のプロジェクト")))
CLINIC_STAGES <- c(setNames("topic", Lc("Choosing a topic", "テーマ選び")), setNames("rq", Lc("Research question", "研究課題づくり")),
                   setNames("lit", Lc("Reading the literature", "先行研究を読む")), setNames("methods", Lc("Planning methods", "方法の計画")),
                   setNames("ethics", Lc("Ethics review", "倫理審査")), setNames("collect", Lc("Collecting data", "データ収集")),
                   setNames("analyze", Lc("Analyzing data", "データ分析")), setNames("write", Lc("Writing up", "執筆")))
CLINIC_DESIGNS <- list(exp = c("Experiment / RCT", "実験・RCT"), quasi = c("Quasi-experiment (intact classes)", "準実験（既存のクラス）"),
                       survey = c("Survey / correlational", "質問紙調査・相関研究"), corpus = c("Corpus or text analysis", "コーパス・テキスト分析"),
                       qual = c("Interviews / qualitative", "インタビュー・質的研究"), mixed = c("Mixed methods", "混合研究法"),
                       review = c("Systematic review / meta-analysis", "システマティック・レビュー／メタ分析"), unsure = c("Not sure yet", "まだ分からない"))
CLINIC_DATA <- list(tests = c("Test scores", "テストの点"), likert = c("Likert questionnaire", "リッカート尺度の質問紙"),
                    counts = c("Categories / counts", "カテゴリー・度数"), texts = c("Essays, texts or speech", "作文・テキスト・発話"),
                    interviews = c("Interviews / open answers", "インタビュー・自由記述"), none = c("No data yet", "まだデータはない"))

choice_tags <- function(lst) list(names = unname(lapply(lst, function(l) L(l[1], l[2]))), values = names(lst))

rating_row <- function(tp) {
  div(class = "cl-row",
    div(class = "cl-topic", L(tp$en, tp$ja)),
    radioButtons(paste0("cl_r_", tp$id), NULL, inline = TRUE, selected = character(0),
                 choiceNames = lapply(RATING_VALUES, function(v) span(class = paste0("cl-chip cl-", v), L(RATING_LABELS[[v]][1], RATING_LABELS[[v]][2]))),
                 choiceValues = RATING_VALUES))
}

clinic_tab <- nav_panel(L("Research clinic", "研究相談室"), value = "Clinic",
  div(class = "wrap clinic-wrap",
    div(class = "corr-intro",
      kicker(L("Breakout lab for thesis projects", "卒論・修論のためのブレイクアウト・ラボ")),
      h2(L("What do you need for your project?", "あなたのプロジェクトに必要なことは？")),
      P("Four short steps, about 10 minutes. Be honest: “need help” is the most useful answer, because it tells us what to cover in lab meetings. You don't need to share your project title.",
        "4つの短いステップで、約10分です。正直に答えてください。「助けてほしい」がいちばん役に立つ答えです。ラボミーティングで何を扱うべきかが分かるからです。プロジェクトのタイトルは書かなくても大丈夫です。", cls = "lede-sm"),
      div(class = "bk-privacy", L("Please don't write participants' names, students' details or anything you wouldn't want classmates to read.",
                                  "参加者の名前、生徒の個人情報、クラスメートに読まれたくないことは書かないでください。"))),
    navset_pill(id = "cl_step",
      nav_panel(tagList("1 · ", L("Your project", "プロジェクト")), value = "s1",
        div(class = "clinic-grid",
          div(class = "clinic-card",
            textInput("cl_name", L("Nickname or initials (optional)", "ニックネームかイニシャル（任意）"), placeholder = "e.g. RK"),
            div(class = "grid2",
              sel("cl_level", L("Project type", "プロジェクトの種類"), CLINIC_LEVELS, selected = "ba"),
              sel("cl_stage", L("Where are you now?", "いまの段階は？"), CLINIC_STAGES, selected = "rq")),
            textInput("cl_title", L("Working title or topic, in a few words (optional)", "仮タイトルかテーマ、数語で（任意）"),
                      placeholder = "e.g. vocabulary and reading in EFL / 例：EFL の語彙と読解")),
          div(class = "clinic-card",
            checkboxGroupInput("cl_design", L("Which design(s) are you thinking about?", "どのデザインを考えていますか？"),
                               choiceNames = choice_tags(CLINIC_DESIGNS)$names, choiceValues = choice_tags(CLINIC_DESIGNS)$values),
            checkboxGroupInput("cl_data", L("What kind of data?", "どんなデータ？"),
                               choiceNames = choice_tags(CLINIC_DATA)$names, choiceValues = choice_tags(CLINIC_DATA)$values))),
        div(class = "btn-row", actionButton("cl_next1", L("Next: self-check →", "次へ：セルフチェック →"), class = "btn-primary"))),
      nav_panel(tagList("2 · ", L("Self-check", "セルフチェック")), value = "s2",
        P("For each topic, how do you feel about it for your project right now? Skip anything that doesn't apply.",
          "それぞれのトピックについて、いまのプロジェクトではどう感じていますか？ 関係ないものは飛ばしてかまいません。", cls = "small"),
        div(class = "clinic-areas",
          lapply(names(CLINIC_AREAS), function(a) div(class = "clinic-card",
            h4(L(CLINIC_AREAS[[a]][1], CLINIC_AREAS[[a]][2])),
            lapply(Filter(function(tp) tp$area == a, CLINIC_TOPICS), rating_row)))),
        div(class = "btn-row", actionButton("cl_next2", L("Next: your question →", "次へ：質問 →"), class = "btn-primary"))),
      nav_panel(tagList("3 · ", L("Your question", "質問")), value = "s3",
        div(class = "clinic-grid",
          div(class = "clinic-card",
            textAreaInput("cl_question", L("My precise question (one or two sentences)", "具体的な質問（1〜2文）"), rows = 4, width = "100%",
                          placeholder = "e.g. Can I use a t-test if my two classes are different sizes? / 例：2つのクラスの人数が違っても t 検定を使えますか？"),
            textAreaInput("cl_tried", L("What have you tried or read so far? (optional)", "これまでに試したこと・読んだもの（任意）"), rows = 3, width = "100%"),
            uiOutput("cl_count"),
            checkboxGroupInput("cl_explore", L("What would you like to explore further in class or lab meetings?", "授業やラボミーティングで、もっと深めたいことは？"),
                               choiceNames = lapply(CLINIC_TOPICS, function(tp) L(tp$en, tp$ja)) |> unname(), choiceValues = names(CLINIC_TOPICS))),
          div(class = "clinic-card",
            h4(L("Your plan", "あなたのプラン")),
            uiOutput("cl_plan"),
            tags$textarea(id = "cl_copy_text", class = "visually-hidden-text", readonly = NA),
            div(class = "btn-row",
              tags$button(type = "button", class = "btn btn-ghost btn-sm", onclick = "ERL.copyClinic(this)", L("Copy summary", "まとめをコピー")),
              downloadButton("cl_download", L("Download (CSV)", "ダウンロード（CSV）"), class = "btn-ghost btn-sm"),
              actionButton("cl_post", L("Post to the lab board", "ラボの掲示板に投稿"), class = "btn-primary btn-sm")),
            uiOutput("cl_post_status")))),
      nav_panel(tagList("4 · ", L("Lab board", "ラボ掲示板")), value = "s4",
        div(class = "toolbar",
          actionButton("cl_refresh", L("↻ Refresh", "↻ 更新"), class = "btn-ghost btn-sm"),
          downloadButton("cl_board_csv", L("Download all posts (CSV)", "全投稿をダウンロード（CSV）"), class = "btn-ghost btn-sm"),
          uiOutput("cl_board_mode", inline = TRUE)),
        fig_head(L("Where the lab needs help", "ラボ全体で助けが必要なところ"), L("number of people per topic · sorted by “need help”", "トピックごとの人数 · 「助けてほしい」の多い順")),
        fig("cl-board-chart"),
        uiOutput("cl_board_list")))))

# ---- Server -------------------------------------------------------------------------
clinic_server <- function(input, output, session) {

  observeEvent(input$cl_next1, nav_select("cl_step", "s2"))
  observeEvent(input$cl_next2, nav_select("cl_step", "s3"))

  clip <- function(x) substr(trimws(x %||% ""), 1, CLINIC_MAX_CHARS)

  ratings <- reactive({
    r <- lapply(names(CLINIC_TOPICS), function(id) input[[paste0("cl_r_", id)]] %||% "")
    setNames(unlist(r), names(CLINIC_TOPICS))
  })

  me <- reactive(list(
    name = clip(input$cl_name), level = input$cl_level %||% "", stage = input$cl_stage %||% "",
    title = clip(input$cl_title), design = paste(input$cl_design, collapse = ";"), data = paste(input$cl_data, collapse = ";"),
    ratings = ratings(), question = clip(input$cl_question), tried = clip(input$cl_tried),
    explore = paste(input$cl_explore, collapse = ";")))

  output$cl_count <- renderUI({
    n <- nchar(input$cl_question %||% "")
    p(class = paste("small", if (n > CLINIC_MAX_CHARS) "text-danger" else "muted"),
      sprintf("%d / %d", n, CLINIC_MAX_CHARS), L(" characters", " 文字"))
  })

  plan_items <- reactive({
    r <- ratings(); ex <- input$cl_explore %||% character(0)
    ids <- c(names(r)[r == "help"], names(r)[r == "unsure"], setdiff(ex, names(r)[r %in% c("help", "unsure")]))
    unique(ids)
  })

  output$cl_plan <- renderUI({
    ids <- plan_items(); r <- ratings()
    if (!length(ids)) return(P("Rate a few topics in step 2 (or tick what you want to explore) and your plan appears here.",
                               "ステップ2でいくつかのトピックを評価する（または深めたいことにチェックを入れる）と、ここにプランが表示されます。", cls = "small muted"))
    items <- lapply(ids, function(id) {
      tp <- CLINIC_TOPICS[[id]]
      badge <- switch(r[[id]], help = span(class = "cl-chip cl-help", L("Need help", "助けてほしい")),
                      unsure = span(class = "cl-chip cl-unsure", L("A bit unsure", "少し不安")),
                      span(class = "cl-chip cl-explore", L("Explore", "深めたい")))
      link <- if (!is.na(tp$tab)) tags$a(href = "#", `data-goto` = tp$tab, class = "cl-link", L(paste("Open the", tp$tab, "tab →"), "このアプリの該当タブを開く →")) else NULL
      div(class = "cl-plan-item",
        div(class = "cl-plan-head", badge, strong(L(tp$en, tp$ja))),
        P(tp$tip[1], tp$tip[2], cls = "small"),
        div(class = "small muted", L("Read: ", "参考："), tp$read, " ", link))
    })
    q <- clip(input$cl_question)
    tagList(items,
      if (nzchar(q)) div(class = "cl-plan-item cl-q", mini(L("Your question", "あなたの質問")), p(q)) else
        P("Add your precise question on the left: it is the most useful thing to bring to a meeting.",
          "左に具体的な質問を書きましょう。ミーティングに持っていくいちばん役に立つものです。", cls = "small muted"))
  })

  summary_text <- reactive({
    m <- me(); ids <- plan_items(); r <- ratings()
    lines <- c("Research clinic · 研究相談室",
               paste0("Name: ", if (nzchar(m$name)) m$name else "—", " | Project: ", m$level, " | Stage: ", m$stage),
               if (nzchar(m$title)) paste0("Topic: ", m$title),
               "", "Priorities / 優先すること:")
    for (id in ids) {
      tp <- CLINIC_TOPICS[[id]]
      lines <- c(lines, sprintf("- [%s] %s / %s", if (nzchar(r[[id]])) r[[id]] else "explore", tp$en, tp$ja), paste0("    ", tp$tip[1]))
    }
    c(lines, "", paste0("My question / 質問: ", m$question), if (nzchar(m$tried)) paste0("Tried / 試したこと: ", m$tried)) |> paste(collapse = "\n")
  })
  observe(session$sendCustomMessage("erl_clinic_copy", list(text = summary_text())))

  row_df <- function(m) data.frame(
    updated = format(Sys.time(), "%Y-%m-%d %H:%M"), name = m$name, level = m$level, stage = m$stage, title = m$title,
    design = m$design, data = m$data, ratings = paste(sprintf("%s=%s", names(m$ratings), m$ratings)[nzchar(m$ratings)], collapse = ";"),
    question = m$question, tried = m$tried, explore = m$explore, stringsAsFactors = FALSE)

  output$cl_download <- downloadHandler(
    filename = function() "research_clinic_self_check.csv",
    content = function(file) write.csv(row_df(me()), file, row.names = FALSE, fileEncoding = "UTF-8"))

  # ---- board ----
  demo_posts <- reactiveVal(data.frame())
  board <- reactiveVal(data.frame())
  post_status <- reactiveVal(NULL)

  observeEvent(input$cl_post, {
    m <- me()
    if (!nzchar(m$question) && !any(nzchar(m$ratings))) {
      post_status(list(ok = FALSE, msg = Lh("Rate at least one topic or write a question first.", "先にトピックを1つ以上評価するか、質問を書いてください。")))
      return()
    }
    if (!nzchar(CLINIC_URL)) {
      d <- demo_posts(); row <- row_df(m); row$client <- "this-browser"
      d <- if (nrow(d)) rbind(d[d$client != "this-browser", , drop = FALSE], row) else row
      demo_posts(d); board(d)
      post_status(list(ok = TRUE, msg = Lh("Posted (demo mode: only you can see it until the lab board is connected).",
                                           "投稿しました（デモモード：掲示板が接続されるまで、あなたにしか見えません）。")))
    } else {
      session$sendCustomMessage("erl_clinic_post", list(url = CLINIC_URL, code = CLINIC_CODE, row = as.list(row_df(m))))
      post_status(list(ok = NA, msg = Lh("Sending…", "送信中…")))
    }
  })
  observeEvent(input$clinic_post_result, {
    res <- input$clinic_post_result
    if (isTRUE(res$ok)) {
      post_status(list(ok = TRUE, msg = Lh("Posted to the lab board. Thank you!", "ラボの掲示板に投稿しました。ありがとう！")))
      session$sendCustomMessage("erl_clinic_list", list(url = CLINIC_URL, code = CLINIC_CODE))
    } else post_status(list(ok = FALSE, msg = paste(Lh("Could not post:", "投稿できませんでした："), esc(res$error %||% "unknown error"))))
  })
  output$cl_post_status <- renderUI({
    st <- post_status(); if (is.null(st)) return(NULL)
    div(class = paste("verdict small", if (isTRUE(st$ok)) "good" else if (isFALSE(st$ok)) "bad" else "warn"), HTML(st$msg))
  })

  observeEvent(list(input$cl_refresh, input$cl_step), {
    if (identical(input$cl_step, "s4") || isTRUE(input$cl_refresh > 0)) {
      if (nzchar(CLINIC_URL)) session$sendCustomMessage("erl_clinic_list", list(url = CLINIC_URL, code = CLINIC_CODE))
      else board(demo_posts())
    }
  }, ignoreInit = TRUE)
  observeEvent(input$clinic_board, {
    rows <- input$clinic_board$rows
    if (is.null(rows) || !length(rows)) { board(data.frame()); return() }
    cols <- c("updated", "name", "level", "stage", "title", "design", "data", "ratings", "question", "tried", "explore")
    df <- do.call(rbind, lapply(rows, function(r) as.data.frame(lapply(setNames(cols, cols), function(k) as.character(r[[k]] %||% "")), stringsAsFactors = FALSE)))
    board(df)
  })

  output$cl_board_mode <- renderUI({
    if (nzchar(CLINIC_URL)) span(class = "small muted", L("Connected to the shared lab board.", "共有のラボ掲示板に接続しています。"))
    else span(class = "small muted", L("Demo mode: posts stay in this browser. Connect a Google Sheet to share them (see README).",
                                      "デモモード：投稿はこのブラウザの中だけです。共有するには Google スプレッドシートを接続します（README 参照）。"))
  })

  board_counts <- reactive({
    df <- board()
    ids <- names(CLINIC_TOPICS)
    cnt <- matrix(0, length(ids), 3, dimnames = list(ids, RATING_VALUES))
    if (nrow(df)) for (rs in df$ratings) {
      kv <- strsplit(strsplit(rs, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
      for (p in kv) if (length(p) == 2 && p[1] %in% ids && p[2] %in% RATING_VALUES) cnt[p[1], p[2]] <- cnt[p[1], p[2]] + 1
    }
    cnt
  })
  observe({
    cnt <- board_counts()
    rows <- lapply(rownames(cnt), function(id) list(id = id, lab = Lc(CLINIC_TOPICS[[id]]$en, CLINIC_TOPICS[[id]]$ja),
                                                    help = cnt[id, "help"], unsure = cnt[id, "unsure"], ok = cnt[id, "ok"]))
    rows <- rows[order(-cnt[, "help"], -cnt[, "unsure"])]
    session$sendCustomMessage("erl_clinic_board", list(rows = rows, n = nrow(board()),
      keys = I(c(Lc("Need help", "助けてほしい"), Lc("A bit unsure", "少し不安"), Lc("OK", "大丈夫")))))
  })

  output$cl_board_list <- renderUI({
    df <- board()
    if (!nrow(df)) return(P("No posts yet.", "まだ投稿はありません。", cls = "muted"))
    df <- df[nzchar(df$question), , drop = FALSE]
    if (!nrow(df)) return(NULL)
    lvl_lab <- setNames(sub(" ｜.*", "", names(CLINIC_LEVELS)), CLINIC_LEVELS)
    stg_lab <- setNames(sub(" ｜.*", "", names(CLINIC_STAGES)), CLINIC_STAGES)
    lk <- function(tab, k) { v <- unname(tab[k]); if (length(v) && !is.na(v)) v else k }
    div(class = "cl-board-cards",
      h4(L(sprintf("Questions (%d)", nrow(df)), sprintf("質問（%d 件）", nrow(df)))),
      lapply(seq_len(nrow(df)), function(i) {
        r <- df[i, ]
        div(class = "bk-card",
          div(class = "bk-card-head", if (nzchar(r$name)) r$name else "—",
              span(class = "muted small", " · ", lk(lvl_lab, r$level), " · ", lk(stg_lab, r$stage))),
          if (nzchar(r$title)) div(class = "bk-card-issue", r$title),
          p(r$question),
          if (nzchar(r$tried)) p(class = "small muted", L("Tried: ", "試したこと："), r$tried))
      }))
  })

  output$cl_board_csv <- downloadHandler(
    filename = function() "research_clinic_board.csv",
    content = function(file) write.csv(board(), file, row.names = FALSE, fileEncoding = "UTF-8"))
}
