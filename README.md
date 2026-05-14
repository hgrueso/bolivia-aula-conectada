# Bolivia Investment Case — Aulas Conectadas

This repository contains the R code and methodology to replicate an investment case study projecting the expected effectiveness and return on investment of the **Aulas Conectadas** programme in Bolivia — a school-based digital ecosystem designed to promote gender equality and improve digital skills among adolescent girls 10–17 through teacher training, digital infrastructure, connectivity, and robotics-based learning content.

The analysis combines descriptive evidence on gender gaps in education from the Bolivian Encuesta de Hogares (EH) 2024, multivariate regression analysis with intersectional cuts, and Bolivia-adjusted projections of effects from comparable international anchor programmes.

---

## Repository structure

```
analysis/
├── .here                          # project root sentinel
├── README.md                      # this file
├── setup_git.sh                   # one-shot GitHub init/push helper
├── run_all.R                      # full pipeline driver
├── render_pdf.sh                  # render slide decks to PDF
│
├── R/                             # shared utilities and reference data
│   ├── utils.R                    # helper functions
│   ├── theme.R                    # ggplot theme + UNICEF colours
│   ├── variable_mapping.R         # EH 2024 variable dictionary
│   ├── anchors_demographics.R     # anchor study metadata
│   └── lit_review_data.R          # literature review loader (52 papers)
│
├── code/                          # numbered analytical stages
│   ├── 01_clean_data.R            # data cleaning + harmonisation
│   ├── 02_descriptive.R           # weighted descriptive statistics
│   ├── 03_figures.R               # Spanish-language figures
│   ├── 03b_figures_en.R           # English-language figures
│   ├── 04_econometrics.R          # LPM with intersectional interactions
│   ├── 04b_heterogeneity.R        # regression forest heterogeneity diagnostics
│   ├── 05_projections.R           # 5-factor φ Bolivia-adjusted projections
│   ├── 06_slides_es.qmd           # Spanish slide deck (Quarto/reveal.js)
│   ├── 06_slides_en.qmd           # English slide deck
│   ├── 07_toc_projection.R        # ToC-arrow forward projection stub
│   └── slides_theme.scss          # reveal.js theme
│
├── data/                          # input data (raw microdata gitignored)
│   ├── BD_EH2024/                 # raw EH 2024 microdata (NOT committed)
│   ├── lit_review_papers.csv      # 52-paper literature review
│   └── lit_review_effect_modifiers.csv
│
└── output/                        # generated artefacts (gitignored)
    ├── figures/                   # PNGs for slides
    ├── models/                    # regression coefficients + diagnostics
    ├── projections/               # anchor-adjusted projections
    └── tables/                    # descriptive tables
```

---

## Methodology summary

### Data

**Encuesta de Hogares (EH) 2024 — INE Bolivia.** National household survey, ~6,254 adolescents 10–17 (expanded to ~2M nationally; ~976K girls). Survey design implemented with `survey` + `srvyr`, using primary sampling units, strata, and expansion factors.

### Descriptive layer

Gender gaps in three core education outcomes — **enrolment**, **attendance**, **age-for-grade delay** — disaggregated by sex, then intersectionally by rural/urban, Indigenous heritage, poverty, and disability. The disability subgroup is small (~0.4%) and interpreted with caution.

### Mechanism layer

Digital access patterns at the household level (internet, computer, smartphone, any device). Framed as **mechanisms that motivate the intervention**, not as outcomes.

### Multivariate analysis

Linear probability models with department fixed effects, sampling weights, and PSU-clustered standard errors (`fixest::feols`). Two-way intersectional interactions with sex; three-way interactions reported as a robustness check.

### Heterogeneity diagnostics

Regression forest (`grf::regression_forest`, Athey & Wager 2019) with honest splitting. Variable importance and predicted subgroup gaps are extracted as a **non-causal** diagnostic for population targeting.

### Transportability projections

External effects from anchor studies are adjusted to the Bolivian context using a **five-factor multiplicative framework**:

```
β̂_T ≈ β̂_S · φ_pop · φ_baseline · φ_geo · φ_econ · φ_delivery
```

Each φ ∈ [0, 1] is computed from observable variables:

- `φ_pop` — population similarity (standardised age, % rural)
- `φ_baseline` — headroom for improvement given Bolivia's current level
- `φ_geo` — geographic distance from La Paz (Haversine, banded)
- `φ_econ` — GDP per capita ratio Bolivia/anchor country
- `φ_delivery` — government implementation (1.0) vs. NGO/research (0.7)

The 95% confidence interval is propagated from the anchor's reported SE without synthetic noise. Identifying assumptions (sufficiency of observables, separability, monotonicity, SE independence) are stated explicitly in the slide deck appendix.

### Structural Equation Model (proposed)

A SEM is pre-registered to integrate all anchors into a single model of the Theory of Change, with parenting effects propagating to educational outcomes via the home-environment latent construct. Estimation pending MICS Bolivia 2019 + pilot data.

---

## Reproducing the analysis

### Prerequisites

```r
install.packages(c(
  "haven", "labelled", "dplyr", "tidyr", "purrr", "stringr",
  "readr", "tibble", "here", "fs", "glue",
  "survey", "srvyr", "fixest", "broom", "marginaleffects",
  "Hmisc", "ggplot2", "scales", "ggtext", "patchwork",
  "openxlsx", "grf"
))
```

[Quarto](https://quarto.org) and a recent pandoc are required to render the slide decks.

### Run the full pipeline

```bash
# From the analysis/ root:
Rscript run_all.R
bash render_pdf.sh both     # Spanish + English decks
```

Or run individual stages — see the `code/` folder for the numbered sequence.

---

## Data access

Raw EH 2024 microdata is distributed by INE Bolivia and is **not redistributed in this repository** (see `.gitignore`). To reproduce the analysis end-to-end, place the .sav files in `data/BD_EH2024/`. The pipeline reads from there.

---

## Contact

Hernando Grueso, PhD · `hgrueso@unicef.org`
