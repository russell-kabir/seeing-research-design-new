# Seeing Research Design

An interactive companion for teaching educational research methods. It covers the nine research designs in Nygren (2026), a PRISMA 2020 flow diagram builder, Cohen's d and power, p-values, chi-square, probability distributions, the central limit theorem, correlation and R², and a Research clinic where students self-assess their thesis projects.

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

## Credits
Design summaries are paraphrased, with page references, from Nygren, T. (2026). *Experimental methods and statistics for educational research: With examples from teaching and learning in history and social studies.* Uppsala University Publications (CC BY-NC-ND). The PRISMA 2020 flow diagram template is from Page, M. J., et al. (2021). *BMJ, 372*, n71 (CC BY 4.0). The visual style is inspired by explorable explanations such as Seeing Theory, R Psychologist and mfviz.com. Spurious correlations are from Tyler Vigen, [tylervigen.com](https://www.tylervigen.com/spurious-correlations) (CC BY 4.0). The R² circles follow Heiss, A. (2021), [Visualizing the explained variance of a regression with Euler diagrams](https://www.andrewheiss.com/blog/2021/08/21/r2-euler/). Reading data: Siegelman, N., et al. (2024). Rethinking first language–second language similarities and differences in English proficiency: Insights from the ENglish Reading Online (ENRO) project. *Language Learning, 74*(1), 249–294. https://doi.org/10.1111/lang.12586. Other data are simulated or illustrative.
