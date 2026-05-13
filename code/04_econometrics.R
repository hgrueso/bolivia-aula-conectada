# 04_econometrics.R — Stage 4: LPMs & intersectional risk profiles -------
# Baseline gaps and risk profiles ONLY — NO causal claim about programme impact.
# Survey weights via `weights`; SE clustered at PSU; department fixed effects.
# Outputs: output/models/*.csv  +  output/models/main_lpm_table.tex
# ------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(readr); library(tibble); library(here); library(glue)
  library(fixest); library(broom); library(marginaleffects)
})

source(here::here("R", "utils.R"))
source(here::here("R", "variable_mapping.R"))
ensure_dirs()

ado <- readRDS(here::here("output", "analysis_ready.rds"))

# Pre-process: ensure factor types for regressors, drop incomplete cases ----
reg_dat <- ado |>
  mutate(
    age_c       = age - 13,
    rural_f     = factor(rural,      levels = c(0, 1),
                         labels = c("Urban", "Rural")),
    indig_f     = factor(indigenous, levels = c(0, 1),
                         labels = c("Non-Indigenous", "Indigenous")),
    disab_f     = factor(disab_any,  levels = c(0, 1),
                         labels = c("No disab", "Disab")),
    poor_f      = factor(poor_d,     levels = c(0, 1),
                         labels = c("Non-poor", "Poor")),
    female_f    = factor(female,     levels = c(0, 1),
                         labels = c("Boys", "Girls")),
    department  = droplevels(department)
  )

outcomes <- c(
  attending      = "Currently attending",
  enrolled       = "Currently enrolled",
  grade_delay    = "Age-for-grade delay",
  hh_internet    = "HH has internet",
  hh_computer    = "HH has computer",
  hh_any_device  = "HH any digital device"
)

# 1. Main LPM: full set of demographic regressors -------------------------
# y = a + b1 female + b2 rural + b3 indig + b4 disab + b5 poor + age + dept FE
formula_main <- function(y) {
  as.formula(glue(
    "{y} ~ female + rural + indigenous + disab_any + poor_d + age_c | department"
  ))
}

main_models <- map(names(outcomes), function(y) {
  feols(formula_main(y), data = reg_dat,
        weights = ~weight, cluster = ~psu)
}) |> setNames(names(outcomes))

# Save tidy coefficients (with N and R² per model)
get_r2 <- function(m) {
  tryCatch(as.numeric(fixest::r2(m, "r2")), error = function(e) NA_real_)
}
main_tidy <- imap_dfr(main_models, function(m, y) {
  tidy_pp(m, label = y) |>
    mutate(model = "main",
           nobs  = stats::nobs(m),
           r2    = get_r2(m))
})
write_csv(main_tidy, here::here("output", "models", "main_lpm_coefs.csv"))

# 2. Intersectional models: female × {rural, indig, disab, poor} ---------
# Pooled sample with sex interactions
inter_models <- list(
  female_rural = map(names(outcomes), function(y) {
    f <- as.formula(glue("{y} ~ female * rural + indigenous + disab_any + poor_d + age_c | department"))
    feols(f, data = reg_dat, weights = ~weight, cluster = ~psu)
  }) |> setNames(names(outcomes)),
  female_indig = map(names(outcomes), function(y) {
    f <- as.formula(glue("{y} ~ female * indigenous + rural + disab_any + poor_d + age_c | department"))
    feols(f, data = reg_dat, weights = ~weight, cluster = ~psu)
  }) |> setNames(names(outcomes)),
  female_disab = map(names(outcomes), function(y) {
    f <- as.formula(glue("{y} ~ female * disab_any + rural + indigenous + poor_d + age_c | department"))
    feols(f, data = reg_dat, weights = ~weight, cluster = ~psu)
  }) |> setNames(names(outcomes)),
  female_poor  = map(names(outcomes), function(y) {
    f <- as.formula(glue("{y} ~ female * poor_d + rural + indigenous + disab_any + age_c | department"))
    feols(f, data = reg_dat, weights = ~weight, cluster = ~psu)
  }) |> setNames(names(outcomes))
)

# Save interaction-term coefficients only
inter_tidy <- imap_dfr(inter_models, function(by_y, inter_type) {
  imap_dfr(by_y, function(m, y) {
    td <- tidy_pp(m, label = y) |>
      filter(str_detect(term, ":")) |>
      mutate(interaction = inter_type,
             nobs        = stats::nobs(m),
             r2          = get_r2(m))
    td
  })
})
write_csv(inter_tidy, here::here("output", "models", "intersectional_coefs.csv"))

# 2b. Three-way interaction models --------------------------------------
# OLS-based replication of the heterogeneity story. Tests whether being a
# girl in a multiply-disadvantaged context (rural × indígena, etc.) compounds
# the gap beyond the sum of two-way effects.
#
# Three combinations:
#   female × rural × indigenous
#   female × rural × poor
#   female × indigenous × poor
threeway_models <- list(
  fri = map(names(outcomes), function(y) {
    f <- as.formula(glue("{y} ~ female * rural * indigenous + disab_any + poor_d + age_c | department"))
    feols(f, data = reg_dat, weights = ~weight, cluster = ~psu)
  }) |> setNames(names(outcomes)),
  frp = map(names(outcomes), function(y) {
    f <- as.formula(glue("{y} ~ female * rural * poor_d + indigenous + disab_any + age_c | department"))
    feols(f, data = reg_dat, weights = ~weight, cluster = ~psu)
  }) |> setNames(names(outcomes)),
  fip = map(names(outcomes), function(y) {
    f <- as.formula(glue("{y} ~ female * indigenous * poor_d + rural + disab_any + age_c | department"))
    feols(f, data = reg_dat, weights = ~weight, cluster = ~psu)
  }) |> setNames(names(outcomes))
)

# Keep only the three-way terms (e.g. "female:rural:indigenous")
threeway_tidy <- imap_dfr(threeway_models, function(by_y, inter_type) {
  imap_dfr(by_y, function(m, y) {
    td <- tidy_pp(m, label = y)
    td |>
      filter(str_count(term, ":") == 2L) |>          # three-way only
      mutate(interaction_set = inter_type,
             nobs            = stats::nobs(m))
  })
})
write_csv(threeway_tidy, here::here("output", "models", "threeway_coefs.csv"))

# 3. Profile predictions: predicted probabilities by risk profile --------
profiles <- crossing(
  female      = c(0, 1),
  rural       = c(0, 1),
  indigenous  = c(0, 1),
  poor_d      = c(0, 1),
  disab_any   = 0,                            # focus on baseline disab=0
  age_c       = 0,                            # age 13
  department  = unique(reg_dat$department)[1] # hold dept constant
)

profile_preds <- map_dfr(names(outcomes), function(y) {
  m <- main_models[[y]]
  # vcov = FALSE: marginaleffects refuses to compute SEs for fixest with FE.
  # We only need point estimates for the profile table on slides.
  pp <- predictions(m, newdata = profiles, type = "response", vcov = FALSE)
  as_tibble(pp) |>
    select(female, rural, indigenous, poor_d, disab_any, estimate) |>
    mutate(outcome = y)
}) |>
  mutate(
    sex     = ifelse(female == 1, "Girls", "Boys"),
    profile = paste(
      ifelse(female == 1, "Girls", "Boys"), "·",
      ifelse(rural == 1, "Rural", "Urban"), "·",
      ifelse(indigenous == 1, "Indig.", "Non-Indig."), "·",
      ifelse(poor_d == 1, "Poor", "Non-poor")
    )
  )
write_csv(profile_preds, here::here("output", "models", "profile_predictions.csv"))

# 4. Compact LaTeX & markdown table of main coefficients -----------------
keep_terms <- c("female", "rural", "indigenous", "disab_any", "poor_d", "age_c")
dict <- c(
  female      = "Girl",
  rural       = "Rural",
  indigenous  = "Indigenous",
  disab_any   = "Disability (WG 3+)",
  poor_d      = "Poor (INE)",
  age_c       = "Age (centred at 13)"
)

# LaTeX
try({
  etable(main_models,
         keep      = keep_terms,
         dict      = dict,
         fitstat   = c("n", "r2"),
         tex       = TRUE,
         file      = here::here("output", "models", "main_lpm_table.tex"),
         title     = "Baseline LPMs — Adolescents 10–17, Bolivia EH 2024",
         label     = "tab:main_lpm",
         notes     = "Survey-weighted OLS with department fixed effects; SE clustered at PSU.",
         replace   = TRUE)
}, silent = TRUE)

# Markdown / plain text for the slide deck
main_table_md <- imap_dfr(main_models, function(m, y) {
  td <- broom::tidy(m, conf.int = TRUE) |>
    filter(term %in% keep_terms) |>
    transmute(term, label = dict[term],
              est = sprintf("%.3f", estimate),
              ci  = sprintf("[%.3f, %.3f]", conf.low, conf.high),
              p   = sprintf("%.3f", p.value),
              outcome = y)
  td
}) |>
  pivot_wider(id_cols = c(term, label),
              names_from = outcome,
              values_from = est)
write_csv(main_table_md, here::here("output", "models", "main_lpm_table_wide.csv"))

# 5. Summary message ------------------------------------------------------
message("Stage 4 complete.")
message(" → output/models/main_lpm_coefs.csv")
message(" → output/models/intersectional_coefs.csv")
message(" → output/models/profile_predictions.csv")
message(" → output/models/main_lpm_table.tex")
message(" → output/models/main_lpm_table_wide.csv")

# Print headline gaps to console for quick visual check
message("\nHeadline female coefficients (pp gap, girl vs boy, controlling for rest):")
main_tidy |>
  filter(term == "female") |>
  transmute(outcome, gap_pp = round(estimate_pp, 2),
            ci = sprintf("[%.2f, %.2f]", conf.low_pp, conf.high_pp)) |>
  print()
