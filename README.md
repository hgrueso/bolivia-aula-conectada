# Bolivia Investment Case — Aula Conectada

This repository contains the R code and methodology to replicate an investment case for the **Aula Conectada** programme in Bolivia, designed and costed **through a gender-inclusive evidence lens**. The case asks not "should Bolivia buy devices?" but "what, on the evidence, actually keeps marginalised adolescent girls learning and in school?" — and matches each diagnosed problem to the intervention whose evidence speaks to it.

The analysis combines descriptive evidence on gender gaps in education from the **Encuesta de Hogares (EH) 2024**, a home-environment diagnosis (violence, discipline, early pregnancy) from the **Encuesta de Demografía y Salud (EDSA) 2023**, multivariate regression with intersectional cuts, Bolivia-adjusted projections of effects from comparable international anchor programmes, and a transparent three-layer cost-benefit analysis.

The population of interest throughout is **adolescent girls aged 10–19** (with 15–19 used where the two surveys overlap and can be read together).

---

## What the diagnosis finds

- **The gap is not access.** Enrolment is near-universal in the early teens (~98% at 10–14) and falls to **~86% by 15–19** — losses concentrate at the **senior-secondary transition**, not in primary. The binding problems are learning quality and staying in school.
- **The home environment matters.** Harsh physical discipline of children is concentrated in **rural** (52% vs 32% urban) and **indigenous** (46% vs 35%) homes, and girls in harsh-discipline homes average **~2 fewer years of schooling**.
- **Early pregnancy is the single strongest schooling correlate** (**−2.9 years**), a distinct pathway from the violence pathway.

These map to three evidence-matched layers: a **learning layer** (pedagogy + teacher training), an **empowerment layer** (ELA-style clubs, for retention and the early-pregnancy pathway), and a **home-environment layer** (low-cost digital parenting support, for the violence pathway). Hardware is treated as an enabler, not the intervention.

---

## Repository structure

```
analysis/
├── .here                          # project root sentinel
├── README.md                      # this file
├── run_all.R                      # full pipeline driver
├── render_pdf.sh                  # render slide decks to PDF (en | es | both)
│
├── R/                             # shared utilities and reference data
│   ├── utils.R                    # helper functions
│   ├── theme.R                    # ggplot theme + UNICEF colours
│   ├── variable_mapping.R         # EH 2024 / EDSA 2023 variable dictionaries
│   ├── anchors_demographics.R     # anchor study metadata (effects, SEs, demographics)
│   └── lit_review_data.R          # literature review loader
│
├── code/                          # numbered analytical stages + helpers
│   ├── 01_clean_data.R            # data cleaning + harmonisation (both surveys)
│   ├── 02_descriptive.R           # weighted descriptive statistics
│   ├── 03_figures.R               # Spanish-language figures
│   ├── 03b_figures_en.R           # English-language figures
│   ├── 03e_diagnosis_panel.R      # schooling-by-discipline panel (EN)
│   ├── 03e_diagnosis_panel_es.R   # schooling-by-discipline panel (ES)
│   ├── 04_econometrics.R          # LPM / OLS with intersectional interactions
│   ├── 05_projections.R           # 5-factor φ Bolivia-adjusted projections
│   ├── 08_bcr_estimation.R        # Bolivia Mincer return + discounted lifetime-earnings PV
│   ├── cba_three_layers.R         # three-layer cost-effectiveness + φ-sensitivity BCR
│   ├── f7_violence_2panel_en.R    # harsh-discipline 2-panel figure (EN)
│   ├── f7_violence_2panel_es.R    # harsh-discipline 2-panel figure (ES)
│   ├── make_toc.R                 # Theory-of-Change diagram → f_toc_en/es.png
│   ├── 06_slides_en.qmd           # English slide deck (Quarto/reveal.js)
│   ├── 06_slides_es.qmd           # Spanish slide deck
│   └── slides_theme.scss          # reveal.js theme
│
├── data/                          # input data (raw microdata gitignored)
│   ├── BD_EH2024/                 # raw EH 2024 microdata (NOT committed)
│   ├── EDS_2023/                  # raw EDSA 2023 microdata (NOT committed)
│   ├── lit_review_papers.csv      # literature-review database
│   └── lit_review_effect_modifiers.csv
│
├── slides/                        # rendered investment-case decks (committed)
│   ├── aula_conectada_es.pdf
│   └── aula_conectada_en.pdf
│
└── output/                        # generated artefacts (gitignored)
    ├── figures/                   # PNGs for slides (incl. f_toc_en/es.png)
    ├── models/                    # regression coefficients + diagnostics
    ├── projections/               # anchor-adjusted projections
    └── tables/                    # descriptive tables
```

---

## Methodology summary

### Data — two independent INE surveys

**Encuesta de Hogares (EH) 2024 — INE Bolivia.** National household survey; analytical subsample of **adolescents 10–19** (≈ 2.4M adolescents nationally when weighted, of whom **≈ 1.19M are girls**). Carries the education, access, and Mincer-earnings evidence. Survey design implemented with `survey` + `srvyr` (primary sampling units, strata, expansion factors).

**Encuesta de Demografía y Salud (EDSA) 2023 — INE Bolivia (DHS round).** Carries the **home-environment diagnosis**: violent discipline, intimate-partner violence, agency, and adolescent pregnancy. **14,545 women 12–49**; the child-discipline module is asked of **mothers (4,678)**. Design `upm` / `estrato` / `ponderadorm`, Taylor linearisation via `srvyr`.

**Why two surveys.** EH carries the education/earnings case; EDSA carries the home-environment diagnosis. They are **independent and not individually linkable**, so EDSA is used to calibrate the diagnosis and the violence→attendance association — never merged into EH at the unit level. At the overlapping 15–19 band the two agree within ~1pp on attendance, schooling, % rural, and % indigenous, supporting joint use.

> Note on the discipline measure: it is a **household-level** indicator reported by women about discipline of children in the home; EDSA does not disaggregate it by the child's sex. The argument is therefore about the household as a risk environment, which girls in it experience or witness.

### Descriptive layer

Gender gaps in core education outcomes — **enrolment**, **attendance** (incl. the 10–14 vs 15–19 transition), and **age-for-grade delay** — disaggregated by sex, then intersectionally by rural/urban, Indigenous heritage, poverty, and disability. The disability subgroup is small (~0.4%) and interpreted with caution. Household digital-access patterns are reported as **context that motivates the intervention**, not as outcomes.

### Multivariate analysis

Weighted OLS / linear probability models with department fixed effects and PSU-clustered standard errors (`fixest::feols`). Two-way intersectional interactions with sex; **three-way interactions reported as a robustness check** (they are not significant — vulnerabilities accumulate roughly additively, which supports single-axis targeting). The harsh-discipline and early-pregnancy associations are estimated on EDSA and treated as **associational, not causal**.

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

The form is conservative (each φ ≤ 1, so projection only attenuates). 95% CIs propagate from the anchor's reported SE without synthetic noise. The three anchors in use:

| Layer | Anchor | Measured effect | Used for |
|---|---|---|---|
| Learning | Evans & Yuan (2022), 73+ RCTs | +0.07 SD learning | learning benefit (reported in SD) |
| Empowerment | Bandiera et al. (2018), ELA | +8.5 pp enrolment | attendance → earnings (φ-adjusted) |
| Home environment | Cluver et al. (2017) + Janowski et al. (2025) delivery | IRR 0.55 on child abuse | violence ↓ → attendance |

### Cost-benefit analysis

Benefits are monetised **only** through the attendance → earnings channel (a conservative floor), using a **Bolivia-estimated Mincer return of 7.5%/year** (EH 2024, women; 95% CI [6.6%, 8.4%]) over a 35-year working life discounted at 3% → **~$3,520 present value per attended school-year**. Each layer is reported in **the unit its evidence measured** ("Route A" — no cross-unit conversion): the learning layer in SD, the ELA and parenting layers in percentage points of attendance.

- **Per-layer cost-effectiveness** (the robust headline): ELA ≈ $6 per +1pp attendance ($26/girl), parenting ≈ $5 per +1pp ($10/caregiver), learning ≈ $25 per 0.01 SD ($152/girl).
- **Combined illustrative BCR ≈ 4.1–6.1** (φ_ELA range, central ~5.1) through attendance→earnings alone — presented as a range, not a point, because it rests on the ELA transport factor and the violence→attendance assumption.
- **National scale.** At the recommended full-priority scale (~568,000 girls), the ~$970 present-value gain per girl aggregates to **≈ $551M** in discounted lifetime earnings for ≈ $107M invested. Broader benefits (safer homes, intergenerational effects) are documented but **not** monetised, so the true return is higher.

ELA/parenting unit costs are international benchmarks pending a Bolivia-specific costing; all constants and their sources are listed on the CBA-assumptions slide.

### Structural Equation Model (proposed)

A SEM is pre-registered to integrate all anchors into a single model of the Theory of Change, with parenting effects propagating to educational outcomes via a home-environment latent construct. Estimation pending MICS Bolivia + pilot data.

---

## Reproducing the analysis

### Prerequisites

```r
install.packages(c(
  "haven", "labelled", "dplyr", "tidyr", "purrr", "stringr",
  "readr", "tibble", "here", "fs", "glue",
  "survey", "srvyr", "fixest", "broom", "marginaleffects",
  "Hmisc", "ggplot2", "scales", "ggtext", "patchwork",
  "openxlsx", "rsvg"
))
```

[Quarto](https://quarto.org) and a recent pandoc are required to render the slide decks. `rsvg` is used by `make_toc.R` to render the Theory-of-Change diagram.

### Run the full pipeline

```bash
# From the analysis/ root:
Rscript run_all.R
Rscript code/make_toc.R          # Theory-of-Change diagram (EN + ES)
bash render_pdf.sh both          # Spanish + English decks
```

Or run individual stages — see the `code/` folder for the numbered sequence.

---

## Data access

Raw microdata are distributed by **INE Bolivia** and are **not redistributed in this repository** (see `.gitignore`). To reproduce the analysis end-to-end, place the `.sav` files in `data/BD_EH2024/` (EH 2024) and `data/EDS_2023/` (EDSA 2023). The pipeline reads from there. Catalogue references: EH 2024 — `anda.ine.gob.bo/catalog/163`; EDSA 2023 — `anda.ine.gob.bo/catalog/119`.

---

## Contact

Hernando Grueso, PhD · `hgrueso@unicef.org`
