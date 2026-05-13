# Bolivia Investment Case — Aulas Conectadas

**Evidence Review, Programme Design, and Investment Case for Gender-Responsive Digital Education, Skills and Parenting for Adolescent Girls in Bolivia.**

This repository contains the analytical pipeline and slide decks (Spanish + English) supporting UNICEF Bolivia's investment case for the **Aulas Conectadas** programme — a school-based digital ecosystem designed to promote gender equality and improve digital skills through teacher training, digital infrastructure, connectivity, and robotics-based learning content. The pilot is being developed with the Ministry of Education for selected public schools, with the aim of generating evidence to inform potential national scale-up.

The central organising frame across all components is **the digital gender gap — what it concretely costs, and what it takes to close it**.

---

## Background

Bolivia is undergoing its first major education reform in more than a decade, with the Government declaring an education emergency and digital education set to become a key pillar of the new Education Law. Within this policy window, gender disparities remain stark: only four out of ten new STEM students are women, reflecting persistent structural barriers — gender stereotypes, unequal access to technology, and limited integration of digital skills into the national education system.

These barriers compound for adolescent girls from **low-income and Indigenous communities**, where intersecting inequalities — early pregnancy, domestic responsibilities, limited connectivity, safety concerns around device use — restrict participation in education and digital opportunities. Any credible programme design must account for these dimensions so that girls not only have access to digital tools but **meaningfully use and benefit from them**.

The Aulas Conectadas initiative builds on the broader Skills4Girls Bolivia programme, integrating digital and emerging technology skills (robotics, coding, AI), socioemotional learning, and leadership development, with a **parenting component** planned for the next phase to address household-level gender norms.

---

## Assignment objectives

Per the Terms of Reference, this assignment supports UNICEF Bolivia and the Ministry of Education in developing an evidence-based investment case across six specific objectives:

1. **Evidence synthesis** — Identify and synthesise robust international, regional, and national evidence on interventions that improve digital skills, socioemotional skills, gender equality in classrooms, parental engagement, and girls' meaningful technology use.
2. **Programme design guidance** — Translate global evidence into a context-specific design framework for Bolivia.
3. **Teacher training strategies** — Evidence-based models of teacher professional development for gender-responsive digital education.
4. **Costing and investment case** — Cost estimates for implementation and scaling, disaggregated to reflect the gender dimensions of the intervention.
5. **Impact modelling** — Feasibility assessment of using national datasets (Encuesta de Hogares 2023–24) to model expected impacts, adapting ATE coefficients from comparable programmes.
6. **Advocacy support** — Investment case to inform discussions with the Ministry of Education, World Bank, IDB, CAF, and GPE.

---

## What's in this repository

This repository covers **objective 5 (impact modelling) and the analytical underpinnings** of objectives 1, 2, and 6. It produces:

- A diagnostic of gender gaps in education and digital access from EH 2024
- Multivariate analysis with intersectional interactions (girl × rural × Indigenous × poverty × disability)
- Heterogeneity diagnostics via regression forests for population targeting
- A transparent transportability framework for adapting external anchor effects (RCTs and quasi-experimental studies) to the Bolivian context — the **5-factor φ approach**
- Two parallel slide decks (Spanish and English) at the level of senior policymakers
- A pre-registered Structural Equation Model integrating the parenting layer into the ToC

**Out of scope per the ToR** (and therefore not in this repository):

- The Theory of Change itself — UNICEF Bolivia provides the ToC as input
- A monitoring and evaluation framework or results matrix — developed by UNICEF Bolivia with partners
- A full cost-benefit analysis monetising education outcomes
- An IFI-grade financial model with depreciation schedules
- Costing data integration — placeholder slides are in the deck pending Ministry data

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
│   ├── anchors_demographics.R     # anchor study metadata (10 anchors)
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

### Descriptive layer (Section 1 of the deck)

Gender gaps in three core education outcomes — **enrolment**, **attendance**, **age-for-grade delay** — disaggregated by sex, then intersectionally by rural/urban, Indigenous heritage, poverty, and disability. The disability subgroup is small (~0.4%) and interpreted with caution.

### Mechanism layer (Section 2)

Digital access patterns at the household level (internet, computer, smartphone, any device). Framed as **mechanisms that motivate the intervention**, not as outcomes.

### Multivariate analysis (Section 3 + Appendix A3–A4)

Linear probability models with department fixed effects, sampling weights, and PSU-clustered standard errors (`fixest::feols`). Three-way interactions reported as a robustness check (Appendix A9).

### Heterogeneity diagnostics (Appendix A5)

Regression forest (`grf::regression_forest`, Athey & Wager 2019) with honest splitting. Variable importance and predicted subgroup gaps are extracted. This is a **non-causal** diagnostic used to identify which features explain the largest gaps for targeting purposes.

### Transportability projections (Section 6 + Appendix A6–A8)

External effects from 10 anchor studies are adjusted to the Bolivian context using a five-factor multiplicative framework:

```
β̂_T ≈ β̂_S · φ_pop · φ_baseline · φ_geo · φ_econ · φ_delivery
```

Each φ ∈ [0, 1] is computed from observable variables — population composition (age, % rural), Bolivia's baseline level vs. headroom, geographic distance from La Paz, GDP per capita ratio, and government vs. NGO/research delivery. The framework draws on Pearl & Bareinboim (2014), Stuart et al. (2011), Dahabreh et al. (2020), and Vivalt (2020). Identifying assumptions are stated explicitly in Appendix A7.

### Structural Equation Model (Appendix A10 — proposed)

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

The literature review CSVs (`data/lit_review_papers.csv`, `data/lit_review_effect_modifiers.csv`) are committed as curated derived artefacts.

---

## Key stakeholders (per ToR)

- UNICEF Bolivia Country Office
- Ministry of Education (Vice-Ministry of Regular Education)
- UNICEF Adolescent Girls Hub
- Potential financing partners: World Bank, Inter-American Development Bank, CAF, Global Partnership for Education

---

## Citation

> Grueso, H. (2026). *Aulas Conectadas — Bolivia Investment Case: pipeline and methodology*. UNICEF Bolivia Country Office (forthcoming).

A methodological paper formalising the five-factor transportability framework as a practical protocol for development investment cases is in preparation.

---

## Contact

Hernando Grueso, PhD · `hgrueso@unicef.org`

AImpact Lab · UNICEF HQ-supported consultancy
