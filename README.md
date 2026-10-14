# Seeing Research Design

An interactive companion for teaching educational research methods. It covers the nine research designs in Nygren (2026), a PRISMA 2020 flow diagram builder, Cohen's d and power, p-values, chi-square, probability distributions, and the central limit theorem.

**▶ Open the app:** https://YOUR-USERNAME.github.io/seeing-research-design/

The app runs entirely in your browser, so there's nothing to install. The first visit takes 10–30 seconds to load.

## Run it in R instead
```r
install.packages(c("shiny", "bslib"))
shiny::runGitHub("seeing-research-design", "YOUR-USERNAME", subdir = "app")
```

## Files
| | |
|---|---|
| `app/app.R` | The whole app: UI, simulations, statistics, PRISMA diagram |
| `app/viz.js` | D3 drawing and animation (R sends it data with `session$sendCustomMessage()`) |
| `app/style.css` | Styling |
| `app/d3.min.js` | D3 v7.9.0 |
| `.github/workflows/deploy.yml` | Rebuilds and republishes the website whenever `main` changes |

## Credits
Design summaries are paraphrased, with page references, from Nygren, T. (2026). *Experimental methods and statistics for educational research: With examples from teaching and learning in history and social studies.* Uppsala University Publications (CC BY-NC-ND). The PRISMA 2020 flow diagram template is from Page, M. J., et al. (2021). *BMJ, 372*, n71 (CC BY 4.0). The visual style is inspired by explorable explanations such as Seeing Theory, R Psychologist and mfviz.com. All data are simulated or illustrative.
