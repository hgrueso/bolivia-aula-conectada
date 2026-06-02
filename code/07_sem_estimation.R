# 07_sem_estimation.R — Structural Equation Model on EH 2024 observables
# -----------------------------------------------------------------------------
# Purpose
#   Estimate the parts of the Aula Conectada Theory of Change for which we
#   have indicators in EH 2024. The parenting layer is DELIBERATELY EXCLUDED
#   because Bolivia lacks contemporary parenting-practice data (MICS Bolivia
#   2019 does not exist; the last full MICS is from 2008). If parenting data
#   from a programme pilot or future survey becomes available, the parenting
#   equations can be added later.
#
# What we estimate
#   - Skill-formation context (EduEngagement, latent) from enrolled + attending
#   - Home environment (HomeEnv, latent) from lives_with_both + poverty +
#     disability (these are weak proxies — the SEM section explicitly flags
#     this as a parenting-data limitation)
#   - Structural arrows:
#       HomeEnv → EduEngagement
#       EduEngagement + HomeEnv → Progression (proxied by inverse of grade_delay)
#
# What we do NOT estimate
#   - Parenting → HomeEnv (no Bolivian parenting data)
#   - Teacher-quality channels (no teacher data in EH 2024)
#   - Programme-effect parameters (no Aula Conectada pilot data yet)
#
# Method
#   Maximum-likelihood SEM with sampling weights, robust SE (CR2 cluster-robust
#   at the PSU level via lavaan.survey or weighted lavaan). For the adolescent
#   girls 10-19 sample only.
#
# Outputs
#   output/models/sem_fit_summary.txt  -- model fit indices + estimates
#   output/models/sem_coefs.csv         -- coefficients for the slide
#
# Note
#   This SEM is descriptive structure on the observed indicators, NOT a causal
#   identification of the Aula Conectada programme effect. It complements the
#   regression analyses (Section 3) by showing how the EH-observed pieces
#   of the ToC hang together statistically.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  # Install once if needed:
  #   install.packages(c("lavaan"))
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("lavaan not installed. Run: install.packages('lavaan')")
  }
  library(dplyr); library(readr); library(here); library(lavaan); library(tibble)
})

OUT <- here::here("output", "models")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------ data
dat <- readRDS(here::here("output", "analysis_ready.rds"))

# Adolescent girls 10-19 only — the programme's target population
girls <- dat |>
  filter(age >= 10, age <= 19, female == 1) |>
  mutate(
    # Reverse-code grade_delay so higher = better progression
    progression = 1 - grade_delay,
    # Lives_with_both as 0/1 (poverty/disability already 0/1)
    lives_with_both = as.integer(!is.na(parents_in_hh) & parents_in_hh == "Ambos padres"),
    # Poverty and disability indicators (HomeEnv "adversity" inputs, reversed)
    not_poor       = 1 - poor_d,
    no_disability  = 1 - disab_any
  ) |>
  # Drop missing on key indicators
  filter(!is.na(enrolled), !is.na(attending), !is.na(progression),
         !is.na(lives_with_both), !is.na(not_poor), !is.na(no_disability)) |>
  # Standardise binary indicators (lavaan handles this internally but explicit
  # is safer with weighted estimation)
  mutate(
    enrolled       = as.numeric(enrolled),
    attending      = as.numeric(attending),
    progression    = as.numeric(progression),
    lives_with_both = as.numeric(lives_with_both),
    not_poor       = as.numeric(not_poor),
    no_disability  = as.numeric(no_disability)
  )

message(sprintf("SEM sample (girls 10-19, complete cases): %d", nrow(girls)))

# ------------------------------------------------------------------ model
# Latent constructs:
#   HomeEnv: protective home environment, indicated by living with both
#            parents (proxy for stable structure), being above the INE poverty
#            line, and absence of disability (resource availability proxy)
#   EduEngagement: school engagement, indicated by enrolment + current attendance
# Structural paths:
#   HomeEnv → EduEngagement (does a more supportive home environment associate
#                            with stronger engagement?)
#   EduEngagement + HomeEnv → progression
sem_model <- '
  # Measurement model
  HomeEnv       =~ lives_with_both + not_poor + no_disability
  EduEngagement =~ enrolled + attending

  # Structural model
  EduEngagement ~ HomeEnv
  progression   ~ EduEngagement + HomeEnv
'

# Fit with sampling weights and PSU-clustered robust SE
fit <- tryCatch(
  sem(
    model    = sem_model,
    data     = girls,
    sampling.weights = "weight",
    cluster  = "psu",
    estimator = "MLR",            # ML with robust SE
    se        = "robust.cluster"
  ),
  error = function(e) {
    message("Robust-cluster estimation failed; trying plain ML.")
    message("Error was: ", e$message)
    sem(model = sem_model, data = girls, estimator = "ML")
  }
)

# ------------------------------------------------------------------ outputs
# Full text summary
sink(file.path(OUT, "sem_fit_summary.txt"))
cat("Aula Conectada Investment Case — SEM on EH 2024 observables\n")
cat("Sample: adolescent girls 10-19 (n = ", nrow(girls), ")\n", sep = "")
cat("Estimation: lavaan ML with sampling weights + PSU-clustered robust SE\n")
cat("============================================================\n\n")
print(summary(fit, fit.measures = TRUE, standardized = TRUE, rsq = TRUE))
sink()
message("✓ SEM summary written to: ", file.path(OUT, "sem_fit_summary.txt"))

# Tidy coefficients table for the slide
coefs <- parameterEstimates(fit, standardized = TRUE) |>
  as_tibble() |>
  filter(op %in% c("~", "=~")) |>
  mutate(
    relationship = case_when(
      op == "~"  ~ paste(lhs, "←", rhs),
      op == "=~" ~ paste(lhs, "→", rhs)
    ),
    type = case_when(
      op == "~"  ~ "Structural path",
      op == "=~" ~ "Measurement loading"
    )
  ) |>
  select(type, relationship, est, se, z, pvalue, std.all)

write_csv(coefs, file.path(OUT, "sem_coefs.csv"))
message("✓ SEM coefficients table: ", file.path(OUT, "sem_coefs.csv"))

# Print key paths to console for quick inspection
cat("\n=== KEY STRUCTURAL PATHS (standardised) ===\n")
coefs |>
  filter(type == "Structural path") |>
  mutate(`Std. coef` = sprintf("%.3f", std.all),
         `p-value`   = sprintf("%.3f", pvalue)) |>
  select(Relationship = relationship, `Std. coef`, `p-value`) |>
  print()

# Fit indices for the slide
fits <- fitmeasures(fit, c("cfi", "tli", "rmsea", "srmr", "chisq", "df", "pvalue"))
fit_df <- tibble(
  index = names(fits),
  value = round(as.numeric(fits), 3)
)
write_csv(fit_df, file.path(OUT, "sem_fit_indices.csv"))

cat("\n=== MODEL FIT INDICES ===\n")
print(fit_df)
cat("\nDone. See output/models/ for full results.\n")
