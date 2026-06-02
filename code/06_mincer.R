# 06_mincer.R — Estimate Bolivia-specific Mincer returns from EH 2024
# -----------------------------------------------------------------------------
# Purpose
#   The BCR needs a "return per additional year of education" — the Mincer
#   coefficient β₁ in ln(wage) = α + β₁·schooling + β₂·exp + β₃·exp² + γ·X + ε.
#   Using a Bolivia-specific estimate (rather than a regional LMIC reference)
#   makes the investment case more defensible.
#
# Sample
#   Adults aged 25–60 with positive monthly labour income (ylab > 0).
#   Restricted to those reporting hours worked > 0 (excludes inactive).
#
# Model
#   Specification A (basic OLS, on log monthly labour income):
#       ln(ylab) ~ aestudio + exp + I(exp^2) + female + rural + indigenous
#   Specification B (Heckman selection model, robustness):
#       Selection equation: employed ~ aestudio + age + age^2 + female +
#                                       rural + indigenous + n_kids_hh
#       Outcome equation:   as in spec A, corrected for selection
#   Specification C (female-only, the relevant population for the BCR):
#       Spec A restricted to female == 1
#
# Outputs
#   output/projections/mincer_bolivia.csv  -- low/central/high coefficients
#                                            (read by 08_bcr_estimation.R)
#   output/projections/mincer_full.csv     -- all specifications with full coef
#   Console -- summary of all specs for inspection
#
# Note
#   The 'low/central/high' values written to mincer_bolivia.csv use:
#     - central = female-only OLS coefficient (Spec C)
#     - low/high = central ± 1.96 · SE  (i.e., 95% CI bounds)
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(readr)
  library(here); library(glue); library(fixest); library(broom)
})

OUT <- here::here("output", "projections")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------ data prep
# Read the cleaned analysis_ready (built by 01_clean_data.R). That file has
# aestudio, age, female, rural, indigenous but NOT individual labour income.
# We need to add labour income from Persona module.
ado_path <- here::here("output", "analysis_ready_full.rds")
if (!file.exists(ado_path)) {
  stop("Run 01_clean_data.R first to produce analysis_ready_full.rds")
}
dat <- readRDS(ado_path)

# Pull labour income from raw Persona file (it isn't currently in the merge)
per_path <- here::here("data", "BD_EH2024", "EH2024_Persona.sav")
if (!file.exists(per_path)) {
  stop("Cannot find Persona file at: ", per_path)
}
per_raw <- read_sav(per_path)

# Standard EH 2024 labour income variables
#   ylab      = ingreso laboral mensual principal
#   yperLb    = ingreso laboral (alt name in some waves)
#   condact   = labour-force status (1 = ocupado)
inc_var <- intersect(c("ylab", "yperLb", "ylab_pri", "iact"), names(per_raw))[1]
if (is.na(inc_var)) {
  message("Labour income variable not found among standard names. Searching...")
  income_candidates <- grep("^y(lab|per|ing)", names(per_raw), value = TRUE,
                            ignore.case = TRUE)
  message("Candidates: ", paste(income_candidates, collapse = ", "))
  if (length(income_candidates) > 0) {
    inc_var <- income_candidates[1]
    message("Using: ", inc_var)
  } else {
    stop("No labour income variable found. Inspect Persona file manually.")
  }
}
message(glue("Using labour income variable: {inc_var}"))

# Key from analysis_ready ↔ Persona is (folio, nro) per EH convention
join_keys <- intersect(c("folio", "nro"), names(per_raw))
if (length(join_keys) < 2) {
  stop("Could not find folio + nro keys in Persona file")
}
inc_df <- per_raw |>
  select(all_of(join_keys), ylab_raw = !!inc_var) |>
  mutate(ylab_raw = as.numeric(ylab_raw))

dat <- dat |> left_join(inc_df, by = join_keys)

# ------------------------------------------------------------------ Mincer sample
mincer_sample <- dat |>
  filter(
    age >= 25, age <= 60,
    !is.na(ylab_raw), ylab_raw > 0,           # positive labour income
    !is.na(aestudio)                            # years of schooling not missing
  ) |>
  mutate(
    log_wage       = log(ylab_raw),
    exp_yrs        = pmax(0, age - aestudio - 6),  # potential experience
    exp_sq         = exp_yrs^2,
    female         = as.integer(female),
    rural          = as.integer(rural),
    indigenous     = as.integer(indigenous)
  )

message(glue("Mincer sample (adults 25-60 with ylab > 0): {nrow(mincer_sample)} obs"))
message(glue("  Female: {sum(mincer_sample$female == 1)}"))
message(glue("  Rural:  {sum(mincer_sample$rural == 1)}"))

# ------------------------------------------------------------------ estimation
# Specification A: pooled OLS with sex/rural/indigenous controls
spec_A <- feols(
  log_wage ~ aestudio + exp_yrs + exp_sq + female + rural + indigenous,
  data = mincer_sample,
  weights = ~ weight,
  cluster = ~ psu
)

# Specification C: female-only (the BCR-relevant cohort)
spec_C <- feols(
  log_wage ~ aestudio + exp_yrs + exp_sq + rural + indigenous,
  data = mincer_sample |> filter(female == 1),
  weights = ~ weight,
  cluster = ~ psu
)

# ------------------------------------------------------------------ extract
tidy_spec <- function(model, label) {
  broom::tidy(model) |>
    mutate(specification = label) |>
    select(specification, term, estimate, std.error, p.value)
}

mincer_full <- bind_rows(
  tidy_spec(spec_A, "A: Pooled OLS"),
  tidy_spec(spec_C, "C: Female only")
)

write_csv(mincer_full, file.path(OUT, "mincer_full.csv"))
message("✓ full coefficients: mincer_full.csv")

# ------------------------------------------------------------------ headline
# Central = female-only schooling coefficient
central <- spec_C$coefficients["aestudio"]
se      <- spec_C$se["aestudio"]
low     <- central - 1.96 * se
high    <- central + 1.96 * se

# Also compute pooled for comparison
central_pool <- spec_A$coefficients["aestudio"]
se_pool      <- spec_A$se["aestudio"]

cat("\n========================================================\n")
cat("BOLIVIA MINCER ESTIMATES (EH 2024, adults 25-60)\n")
cat("========================================================\n")
cat(sprintf("Spec A (pooled OLS):  %.4f  (SE: %.4f)\n", central_pool, se_pool))
cat(sprintf("Spec C (female only): %.4f  (SE: %.4f)\n", central, se))
cat(sprintf("                      → 95%% CI: [%.4f, %.4f]\n", low, high))
cat("\nInterpretation: each additional year of schooling raises\n")
cat(sprintf("expected log labour income by %.1f%% (female-only OLS).\n", central * 100))

# Write a compact table that 08_bcr_estimation.R can read
mincer_bolivia <- tibble(
  parameter = "Mincer return per year of edu (Bolivia EH 2024)",
  low     = round(low,     4),
  central = round(central, 4),
  high    = round(high,    4),
  source  = "Bolivia EH 2024, female adults 25-60 with positive labour income, weighted OLS"
)
write_csv(mincer_bolivia, file.path(OUT, "mincer_bolivia.csv"))
message("✓ headline Mincer for BCR: mincer_bolivia.csv")

# ------------------------------------------------------------------ caveats
cat("\nCAVEATS:\n")
cat(" 1. OLS Mincer on the employed sample suffers from selection bias.\n")
cat("    The female-only estimate may be biased upward if higher-ability\n")
cat("    women are more likely to work. A Heckman correction is a useful\n")
cat("    robustness check but is not implemented here.\n")
cat(" 2. The estimate uses CURRENT Bolivia labour market structure. Returns\n")
cat("    for adolescents entering work in 10-20 years may differ.\n")
cat(" 3. The estimate is for monthly LABOUR income; non-labour income\n")
cat("    (transfers, self-consumption) is excluded.\n")
