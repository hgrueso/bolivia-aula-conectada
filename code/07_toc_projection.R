# 07_toc_projection.R — compounded forward projection of Aula Conectada
# -----------------------------------------------------------------------------
# Stub for the SEM that will replace this once MICS + pilot + costing data
# arrive. Takes the φ-adjusted anchor effects and propagates them through a
# simple ToC chain to produce population-level expected impacts.
#
# Inputs:  output/projections/anchor_projections_long.csv
# Outputs: output/projections/toc_compound_projection.csv
#          console summary table
#
# ToC arrows (simplified):
#   parenting    → home_environment  (Cluver + Janowski)
#   home_env     → school_attendance (assumed multiplier; calibrate w/ MICS)
#   teacher_cap  → learning          (Outes-Sanchez + Gómez-Ruiz)
#   gender_norms → dropout_prevention (ELA Bandiera)
#
# Each arrow has:
#   - source_anchors: which anchors contribute
#   - effect_unit:    natural units of the effect
#   - bolivia_pop:    affected population
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(here); library(glue)
  library(tibble)
})

source(here::here("R", "utils.R"))
ensure_dirs()

proj <- read_csv(here::here("output", "projections", "anchor_projections_long.csv"),
                 show_col_types = FALSE)

# Priority population for downstream calculations
N_PRIORITY_GIRLS <- proj |>
  filter(subgroup_id == "priority") |>
  pull(n_girls_weighted) |>
  unique()

cat(sprintf("Priority girls (rural ∨ indígena ∨ pobre ∨ disab): %s\n",
            format(round(N_PRIORITY_GIRLS), big.mark = ",")))

# =============================================================================
# 1. Group anchor effects by ToC arrow
# =============================================================================
arrows <- tibble::tribble(
  ~arrow,                    ~source_anchor_id, ~weight,
  "learning_math_sd",        "A1_OS_full",      0.5,
  "learning_math_sd",        "A1_OS_reg",       0.5,
  "learning_coding_sd",      "A6_ped",          1.0,
  "dropout_prevention_pp",   "A2_ELA",          1.0,
  "parenting_home_env_sd",   "A7_ParApp",       0.5,
  "parenting_home_env_sd",   "A7_PLH",          0.5
)

# =============================================================================
# 2. Compute weighted Bolivia-adjusted effect per arrow
# =============================================================================
arrow_effects <- proj |>
  filter(subgroup_id == "priority") |>
  inner_join(arrows, by = c("anchor_id" = "source_anchor_id")) |>
  group_by(arrow) |>
  summarise(
    avg_adj_effect = sum(adj_effect * weight, na.rm = TRUE) /
                     sum(weight * !is.na(adj_effect)),
    avg_lower      = sum(lower_95   * weight, na.rm = TRUE) /
                     sum(weight * !is.na(lower_95)),
    avg_upper      = sum(upper_95   * weight, na.rm = TRUE) /
                     sum(weight * !is.na(upper_95)),
    n_anchors_used = n(),
    units          = first(unit),
    .groups = "drop"
  ) |>
  mutate(
    n_priority_girls = N_PRIORITY_GIRLS,
    population_change = case_when(
      grepl("_pp$", arrow)  ~ avg_adj_effect * N_PRIORITY_GIRLS,
      grepl("_sd$", arrow)  ~ NA_real_,         # SD units don't translate to N
      TRUE                  ~ NA_real_
    )
  )

cat("\n=== Forward projection — ToC arrows ===\n")
print(arrow_effects)

# =============================================================================
# 3. Save
# =============================================================================
write_csv(arrow_effects,
          here::here("output", "projections", "toc_compound_projection.csv"))

cat(sprintf("\nWrote: output/projections/toc_compound_projection.csv\n"))

# =============================================================================
# 4. Suggested next-step calculations (commented out until data arrives)
# =============================================================================
# When MICS Bolivia 2019 is loaded, replace the heuristic weights above with
# Bolivia-specific moderators:
#   - moderator_home_env_to_attendance: from MICS regression of school
#     attendance on intrahousehold violence indicators, controlling for poverty
#   - moderator_parenting_attendance: from MICS gender-norm scale × attendance
# These become the missing connectors:
#   compound_effect_on_attendance =
#     parenting_home_env_sd × moderator_home_env_to_attendance +
#     direct_attendance_anchors (if any)
#
# When costing data arrives:
#   cost_per_girl = total_cost / N_PRIORITY_GIRLS
#   USD_per_pp_attendance = cost_per_girl / compound_effect_on_attendance
#   USD_per_SD_learning   = cost_per_girl / avg_learning_effect
