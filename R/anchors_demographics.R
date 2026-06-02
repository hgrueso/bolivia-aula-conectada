# R/anchors_demographics.R
# -----------------------------------------------------------------------------
# Demographic profile, outcome metadata, and contextual covariates for each
# programme anchor. Hand-coded from the Lit Review Comprehensive document
# (Grueso, May 2026).
#
# Used by 05_projections.R to compute four adjustment factors:
#   φ_pop      : population alignment over (age, rural_share)
#                NOTE: indig_share is intentionally NOT used. "Indigenous"
#                means different things across contexts (Quechua/Aymara
#                Bolivia ≠ Scheduled Tribes India ≠ unspecified Sierra Leone)
#                and is not apples-to-apples.
#   φ_baseline : headroom on the matched outcome
#   φ_geo      : great-circle distance from La Paz, decayed
#   φ_econ     : GDP per capita ratio anchor↔Bolivia (capped at 1)
#   φ_delivery : 1.0 if government-implemented, 0.7 otherwise (Vivalt 2020)
# -----------------------------------------------------------------------------
# Field conventions
#   anchor_id          : short identifier (matches 05_projections.R)
#   programme          : human-readable name
#   country            : country name (Spanish)
#   country_iso        : ISO3 code
#   country_lat,lng    : capital-city coordinates (used for φ_geo)
#   gdp_pc_usd         : GDP per capita USD at year of study (used for φ_econ)
#   gov_implementer    : TRUE if government-implemented, FALSE otherwise
#   programme_type     : ToC arrow this anchor primarily serves
#                        (digital_education, parenting, gender_norms,
#                         labor_market, teacher_training)
#   age_lo, age_hi     : age range of study sample (years)
#   rural_share        : proportion rural (0 to 1). NA if not reported.
#   baseline_y         : pre-treatment outcome level (control). NA if NR.
#   baseline_y_max     : theoretical max of the outcome scale.
#   effect             : reported effect size (point estimate).
#   se                 : reported standard error.
#   unit               : "SD" | "ppts" | "score_pts"
#   pop_match_outcome  : the Bolivia EH 2024 variable used to compute
#                        baseline headroom for this anchor.
# -----------------------------------------------------------------------------

# Bolivia reference for distance: La Paz (administrative capital)
BOLIVIA_REF_LAT <-  -16.4897
BOLIVIA_REF_LNG <-  -68.1193
BOLIVIA_GDP_PC_USD <- 3500   # approximate, 2024 World Bank

anchors_demo <- tibble::tribble(
  ~anchor_id,   ~programme,                                                ~country,       ~country_iso, ~country_lat, ~country_lng, ~gdp_pc_usd, ~gov_implementer, ~programme_type,        ~age_lo, ~age_hi, ~rural_share, ~baseline_y, ~baseline_y_max, ~effect, ~se,    ~unit,        ~pop_match_outcome,
  "A0_EY",      "Girls' education meta-analysis (Evans & Yuan 2022)",       "LMIC (pooled)","BOL",        -16.2902,    -63.5887,     3600,        TRUE,             "digital_education",     10,      19,      0.30,         NA,          NA,              0.07,     0.04,   "SD",         "attending",
  "A1_OS_full", "Growth Mindset (Outes & Sanchez, full)",        "Perú",         "PER",        -12.0464,    -77.0428,     7000,        TRUE,             "digital_education",     12,      14,      0.00,         NA,          NA,              0.054,    0.030,  "SD",         "attending",
  "A1_OS_reg",  "Growth Mindset (Outes & Sanchez, regional)",    "Perú",         "PER",        -12.0464,    -77.0428,     7000,        TRUE,             "digital_education",     12,      14,      0.00,         NA,          NA,              0.135,    0.051,  "SD",         "attending",
  "A1b_Porter", "Growth Mindset (Porter et al.)",                "Sudáfrica",    "ZAF",        -25.7479,     28.2293,     6500,        FALSE,            "digital_education",     13,      16,      0.00,         10.02,       NA,              -0.04,    0.10,   "SD",         "attending",
  "A2_ELA",     "ELA (Bandiera et al., 12-17 cohort)",                      "Sierra Leona", "SLE",          8.4657,    -13.2317,      500,        FALSE,            "gender_norms",          12,      17,      1.00,         42.4,        100,             7.25,     2.70,   "score_pts",  "attending",
  "A3_Irumi",   "Irûmi — pensamiento computacional (Näslund-Hadley)",       "Paraguay",     "PRY",        -25.2637,    -57.5759,     5800,        TRUE,             "digital_education",      7,       8,      0.40,         0.00,        NA,              0.089,    0.052,  "SD",         "hh_computer",
  "A6_caut",    "Plan Ceibal SOLO HARDWARE (De Melo)",                      "Uruguay",      "URY",        -34.9011,    -56.1645,    20800,        TRUE,             "digital_education",      8,      12,      0.05,         0.00,        NA,              0.00,     0.05,   "SD",         "hh_computer",
  "A6_ped",     "Plan Ceibal + programación (Gómez-Ruiz)",                  "Uruguay",      "URY",        -34.9011,    -56.1645,    20800,        TRUE,             "digital_education",     14,      17,      0.05,         NA,          NA,              0.13,     0.05,   "SD",         "attending",
  "A5_ProJ",    "ProJoven (Ñopo et al.)",                                    "Perú",         "PER",        -12.0464,    -77.0428,     7000,        TRUE,             "labor_market",          16,      25,      0.00,         NA,          NA,              0.152,    0.060,  "ppts",       "attending",
  # Parenting layer — kept here as parameters for the SEM (parenting → school-engagement arrow).
  # Excluded from the digital-education projection table (programme_type filter).
  # NOTE on Cluver 2017: original outcome is IRR 0.55 on physical/emotional abuse —
  # we encode a downstream-engagement proxy effect (NOT the IRR) as a placeholder
  # until the SEM measurement model defines how parenting effects propagate.
  "A7_ParApp",  "ParentApp Tanzania (Janowski 2025)",                       "Tanzania",     "TZA",         -6.7924,     39.2083,     1200,        FALSE,            "parenting",              0,      17,      0.50,         NA,          NA,              0.13,     0.06,   "SD",         "attending",
  "A7_PLH",     "Parenting for Lifelong Health (Cluver et al. 2017)",       "Sudáfrica",    "ZAF",        -25.7479,     28.2293,     6500,        FALSE,            "parenting",             10,      17,      0.50,         NA,          NA,              0.10,     0.05,   "ppts",       "attending"
)

# Bolivia subgroup profiles — computed at run time in 05_projections.R from
# the analysis_ready.rds dataset. Reference structure:
bolivia_profile_template <- list(
  subgroup_id      = NA_character_,
  subgroup_label   = NA_character_,
  age_mid          = NA_real_,
  rural_share      = NA_real_,
  n_girls_weighted = NA_real_,
  baseline_attending     = NA_real_,
  baseline_hh_internet   = NA_real_,
  baseline_hh_computer   = NA_real_,
  baseline_hh_any_device = NA_real_
)
