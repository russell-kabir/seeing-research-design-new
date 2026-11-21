# Seeing Research Design

An interactive companion for teaching educational research methods. It covers the nine research designs in Nygren (2026), a PRISMA 2020 flow diagram builder, Cohen's d and power, p-values, chi-square, probability distributions, the central limit theorem, correlation and R², two corpus tools (Writing development and AUT scoring), and a Research clinic where students self-assess their thesis projects.

Everything is bilingual in plain English and Japanese. The **EN / EN+日 / 日本語** switch at the top right shows English, both, or Japanese; the choice is remembered in each browser.

**▶ Open the app:** https://russell-kabir.github.io/seeing-research-design-new/

The app runs entirely in your browser, so there's nothing to install. The first visit takes 10–30 seconds to load.

## Run it in R instead
```r
install.packages(c("shiny", "bslib"))
shiny::runGitHub("seeing-research-design-new", "russell-kabir", subdir = "app")
```

## Files
| | |
|---|---|
| `app/app.R` | The main app: UI, simulations, statistics, PRISMA diagram, bilingual helpers (`L()`, `P()`, `Lh()`, `Lc()`) |
| `app/textmetrics.R` | Text measures in base R: CSV reading (UTF-8 BOM / Shift-JIS), tokens, lexical diversity, articles, causal connectives, Krippendorff's α |
| `app/writing.R` | Writing development tab |
| `app/syntax.R` | Clause-based syntactic complexity (L2SCA measures from a udpipe dependency parse), loaded only when that box is ticked |
| `app/vocabprofile.R`, `app/data/wordlists/` | Vocabulary profiling and its word lists (NGSL/TSL, CEFR-J, lemmas) |
| `app/aut.R`, `app/aut_codebook.R` | AUT scoring tab and the Wahbeh codebook (with Japanese labels and keyword hints) |
| `app/viz-writing.js` | Charts, AUT coding keys, and in-browser semantic distance (transformers.js) |
| `app/data/templates/` | CSV templates for students: one paragraph per row, one AUT idea per row |
| `app/corr.R` | Correlation & R² tab: Tyler Vigen's spurious correlations, the ENRO reading data, and R² as overlapping circles (Heiss, 2021) |
| `app/clinic.R` | Research clinic tab: project self-check, plan, and lab board |
| `app/data/enro_japan_subset.csv` | 129 Japanese L2 readers from the ENRO project (de-identified: no IDs, dates or free-text fields) |
| `app/viz-corr.js` | D3 drawing for the two new tabs |
| `apps_script/clinic_board.gs` | Optional Google Sheet backend for the shared lab board |
| `app/viz.js` | D3 drawing and animation (R sends it data with `session$sendCustomMessage()`) |
| `app/style.css` | Styling |
| `app/d3.min.js` | D3 v7.9.0 |
| `.github/workflows/deploy.yml` | Rebuilds and republishes the website whenever `main` changes |

## Research clinic: connecting the shared lab board
Without setup the clinic runs in demo mode: each student can self-check, copy or download their plan, but posts stay in their own browser. To collect everyone's answers in one Google Sheet:

1. At script.google.com, create a project, paste `apps_script/clinic_board.gs`, and run `setup()`. This creates the Sheet.
2. **Deploy → New deployment → Web app**, Execute as *Me*, access *Anyone*. Copy the URL ending in `/exec`.
3. Paste it into `CLINIC_URL` at the top of `app/clinic.R` and push. The site rebuilds itself.

Run `clearPosts()` in the script editor before a new class. The board code (`kabirlab`) must match in both files.

## Using the full ENRO file
On the *Correlation & R²* tab, open **Use your own CSV** and load any CSV. Numeric columns become variables and text columns with 2–8 categories (for example L1 vs. L2, or country) become groups, so the full ENRO download can be used to compare Japanese and non-Japanese readers. The file never leaves the browser.

## Writing development tab
Load a CSV with **one paragraph per row** (`student_id, project, attempt, version, text`; save from Excel as *CSV UTF-8*). Tick the measures and download one row of results per paragraph:

* **Counts**: words, types, sentences.
* **Lexical diversity**: TTR, Guiraud's R, MATTR (50-word window), MTLD (McCarthy & Jarvis, 2010, or the TAALED convention), HD-D (42-word sample, 0–1 scale; ×42 for the sum some tools report). HD-D, MATTR and the TAALED MTLD match the Python `lexical_diversity` package.
* **Sentence complexity proxies**: mean length of sentence, subordinators and coordinators per sentence. These are surface counts, not parsed clauses, so use them to compare texts processed the same way (e.g. ICNALE/NICE written vs. edited).
* **Clause-based syntactic complexity** (off by default; tick it to use): the L2SCA counts and ratios of Lu (2010), computed from a Universal Dependencies parse with the `udpipe` R package and its English EWT model (~16 MB, downloaded from GitHub once per visit). The tab gives T-units, clauses, MLT, MLC, C/S, C/T, T/S, DC/C, DC/T, CT/T, CP/C, CP/T, CN/C, CN/T, VP/T, plus the number of sentence-opening signpost words ("First of all,", "In conclusion,") and MLT with those words removed. Signposts lengthen T-units without adding clauses, so compare the two MLTs when the teaching targets signposting. The rules follow the L2SCA Tregex definitions, so treat the values as close estimates of L2SCA rather than identical ones. On L2SCA's two sample texts the counts are within one or two units of the original. In the browser, `udpipe` is installed from the webR package repository when the box is ticked. If that fails, the tab says so and the other measures still work. In desktop R, run `install.packages("udpipe")` first.
* **Articles**: the, a/an, rate per 100 words, and spelling-based a/an flags to check by hand.
* **Causal connectives** after Halliday & Hasan (1976): every cell of the causal row, plus an optional "extended" set (thus, that's why…). Ambiguous words (so, then, for, otherwise) get rules and a review flag; the concordance CSV has a `keep` column for hand-checking.
* **"I think"** count and rate.
* **Vocabulary profile** (sub-tab 5): each word is reduced to its headword and checked against NGSL 1.01 + TOEIC Service List 1.1 (bands 1–500, 501–1000, 1001–2000, 2001–2801, supplementary, TSL) or the CEFR-J Wordlist 1.5 + Octanove C1/C2 (A1–C2). You get % of tokens per level, off-list words, a colour-coded paragraph, a mean CEFR-J level, and a word-by-word CSV. Any other list (e.g. a licensed JACET8000 or SVL12000) can be uploaded as a CSV with `headword, level`.

## AUT scoring tab
Load a CSV with **one idea per row** (`student_id, item, response_order, condition, response`). Items such as 新聞, レンガ, 封筒, ハンガー map to the Wahbeh et al. codebook; other objects get a custom codebook typed on the Code tab. Code with the keyboard (1–9/0/q… category, i not possible, a/s/d elaboration, j/k/l originality, Enter next, n next uncoded), download the coded file, and reload it to continue. The Scores tab gives fluency, flexibility, elaboration and originality per participant × item × condition. The Two raters tab gives percent agreement, Krippendorff's α and a disagreement list for a third rater.

Semantic distance: either compute it in the browser (a multilingual sentence model, ~120 MB, downloaded once from Hugging Face; responses stay on the computer) or export a SemDis-ready CSV for https://cap.ist.psu.edu/semdis and load the results back. Japanese was not among the languages validated for multilingual SemDis (Patterson et al., 2023), so the tab reports how semantic distance correlates with your own originality codes and with response length.

## Credits
Design summaries are paraphrased, with page references, from Nygren, T. (2026). *Experimental methods and statistics for educational research: With examples from teaching and learning in history and social studies.* Uppsala University Publications (CC BY-NC-ND). The PRISMA 2020 flow diagram template is from Page, M. J., et al. (2021). *BMJ, 372*, n71 (CC BY 4.0). The visual style is inspired by explorable explanations such as Seeing Theory, R Psychologist and mfviz.com. Spurious correlations are from Tyler Vigen, [tylervigen.com](https://www.tylervigen.com/spurious-correlations) (CC BY 4.0). The R² circles follow Heiss, A. (2021), [Visualizing the explained variance of a regression with Euler diagrams](https://www.andrewheiss.com/blog/2021/08/21/r2-euler/). Reading data: Siegelman, N., et al. (2024). Rethinking first language–second language similarities and differences in English proficiency: Insights from the ENglish Reading Online (ENRO) project. *Language Learning, 74*(1), 249–294. https://doi.org/10.1111/lang.12586. AUT codebook: Wahbeh, H., Cannard, C., Yount, G., Delorme, A., & Radin, D. (2024). Creative self-belief responses versus manual and automated alternate use task scoring. *Journal of Creativity, 34*(3), 100088 (Supplemental Data). Causal connectives: Halliday, M. A. K., & Hasan, R. (1976). *Cohesion in English*. Longman. Word lists: Browne, C., Culligan, B., & Phillips, J. (2013). *The New General Service List* (1.01) and Browne & Culligan, *The TOEIC Service List* (1.1), CC BY-SA 4.0, https://www.newgeneralservicelist.com; *The CEFR-J Wordlist Version 1.5*, compiled by Yukio Tono, Tokyo University of Foreign Studies (via Open Language Profiles); Octanove Vocabulary Profile C1/C2 1.0, CC BY-SA 4.0; English lemmatization list by Michal Boleslav Měchura, ODbL. The files in `app/data/wordlists/` keep those licences. Other data, including the demo paragraphs and AUT responses, are simulated or illustrative.
