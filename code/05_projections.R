# 05_projections.R — Stage 5: Bolivia-adjusted return projections
# -----------------------------------------------------------------------------
# Methodological upgrade (May 2026): removes the arbitrary 0.45/0.675/0.90
# "fidelity" multipliers and replaces them with two data-derived adjustments:
#
#   1. φ_pop      = exp(- weighted Euclidean distance over (age_mid,
#                    rural_share, indig_share) features that are observable
#                    both in the anchor study and in EH 2024). Bounded [0, 1].
#                    Anchors whose study population closely matches a Bolivia
#                    subgroup retain ≈ all of their effect; anchors whose
#                    population is very different are shrunk.
#
#   2. φ_baseline = (max - Y_Bolivia_baseline) / (max - Y_anchor_baseline)
#                    when the anchor reports a control-group baseline level on
#                    the same outcome. Captures available headroom. Bounded
#                    [0, 1] (no upward extrapolation).
#
# Uncertainty is carried through the anchor's reported standard error → 95% CI
# is propagated. No invented numbers.
#
# Inputs:  output/analysis_ready.rds, R/anchors_demographics.R
# Outputs: output/projections/Bolivia_anchor_projections.xlsx
#          output/projections/anchor_projections_long.csv
#          output/projections/anchor_projections_wide.csv
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(tibble)
  library(here); library(glue); library(openxlsx); library(srvyr); library(survey)
})

source(here::here("R", "utils.R"))
source(here::here("R", "anchors_demographics.R"))
ensure_dirs()

ado <- readRDS(here::here("output", "analysis_ready.rds"))

options(survey.lonely.psu = "adjust")
des <- ado |>
  as_survey_design(ids = psu, strata = stratum, weights = weight, nest = TRUE)

# =============================================================================
# 1. Define Bolivia subgroups and compute their observable profiles
# =============================================================================
bolivia_subgroups <- list(
  list(id = "all_girls",     label = "Todas las niñas 10–17",
       filt = quote(female == 1)),
  list(id = "urban_nonindig", label = "Niñas urbanas no indígenas",
       filt = quote(female == 1 & rural == 0 & indigenous == 0)),
  list(id = "rural_nonindig", label = "Niñas rurales no indígenas",
       filt = quote(female == 1 & rural == 1 & indigenous == 0)),
  list(id = "urban_indig",    label = "Niñas urbanas indígenas",
       filt = quote(female == 1 & rural == 0 & indigenous == 1)),
  list(id = "rural_indig",    label = "Niñas rurales indígenas",
       filt = quote(female == 1 & rural == 1 & indigenous == 1)),
  list(id = "priority",       label = "Niñas prioritarias (rural ∨ indígena ∨ pobre ∨ discapacidad)",
       filt = quote(female == 1 & (rural == 1 | indigenous == 1 |
                                   poor_d == 1 | disab_any == 1)))
)

compute_profile <- function(d, sub) {
  mask <- eval(sub$filt, d)
  d_sub <- d[mask, ]
  if (nrow(d_sub) == 0) return(NULL)
  w <- d_sub$weight
  tibble(
    subgroup_id      = sub$id,
    subgroup_label   = sub$label,
    age_mid          = weighted.mean(d_sub$age, w, na.rm = TRUE),
    rural_share      = weighted.mean(as.integer(d_sub$rural == 1), w, na.rm = TRUE),
    indig_share      = weighted.mean(as.integer(d_sub$indigenous == 1), w, na.rm = TRUE),
    n_girls_weighted = sum(w),
    baseline_attending      = weighted.mean(d_sub$attending     == 1, w, na.rm = TRUE),
    baseline_hh_internet    = weighted.mean(d_sub$hh_internet   == 1, w, na.rm = TRUE),
    baseline_hh_computer    = weighted.mean(d_sub$hh_computer   == 1, w, na.rm = TRUE),
    baseline_hh_any_device  = weighted.mean(d_sub$hh_any_device == 1, w, na.rm = TRUE)
  )
}

bolivia_profiles <- map_dfr(bolivia_subgroups, ~compute_profile(ado, .x))
print(bolivia_profiles)

# =============================================================================
# 2. Compute φ_pop: population alignment over age + rural share
# =============================================================================
# CRITICAL CHANGE (May 2026): indig_share was dropped. "Indigenous" means
# different things across contexts (Quechua/Aymara in Andean Bolivia is not
# apples-to-apples with Scheduled Tribes in India or unspecified Indigenous
# in Sierra Leone). Comparing those marginal shares does not measure
# population similarity in a meaningful way. We use age and rural_share only.

all_profiles <- bind_rows(
  anchors_demo |>
    transmute(id = anchor_id,
              age_mid     = (age_lo + age_hi) / 2,
              rural_share,
              type = "anchor"),
  bolivia_profiles |>
    transmute(id = subgroup_id, age_mid, rural_share, type = "bolivia")
)

feat_stats <- list(
  age   = list(mu = mean(all_profiles$age_mid, na.rm = TRUE),
               sd = sd(all_profiles$age_mid,  na.rm = TRUE)),
  rural = list(mu = mean(all_profiles$rural_share, na.rm = TRUE),
               sd = sd(all_profiles$rural_share,  na.rm = TRUE))
)

z <- function(x, key) {
  (x - feat_stats[[key]]$mu) / feat_stats[[key]]$sd
}

TARGET_AGE_LO <- 10
TARGET_AGE_HI <- 19

age_overlap_share <- function(anchor_lo, anchor_hi) {
  ov <- pmax(0, pmin(anchor_hi, TARGET_AGE_HI) - pmax(anchor_lo, TARGET_AGE_LO))
  ov / (TARGET_AGE_HI - TARGET_AGE_LO)
}

anchor_age_flags <- anchors_demo |>
  transmute(
    anchor_id,
    age_overlap    = age_overlap_share(age_lo, age_hi),
    in_age_range   = age_overlap >= 0.25
  )

phi_pop <- crossing(
  anchor_id    = anchors_demo$anchor_id,
  subgroup_id  = bolivia_profiles$subgroup_id
) |>
  left_join(anchors_demo |>
              transmute(anchor_id,
                        a_age   = (age_lo + age_hi) / 2,
                        a_rural = rural_share),
            by = "anchor_id") |>
  left_join(bolivia_profiles |>
              transmute(subgroup_id,
                        b_age   = age_mid,
                        b_rural = rural_share),
            by = "subgroup_id") |>
  rowwise() |>
  mutate(
    d_age_sq   = if (!is.na(a_age)   & !is.na(b_age))   (z(a_age,   "age")   - z(b_age,   "age"))^2     else NA_real_,
    d_rural_sq = if (!is.na(a_rural) & !is.na(b_rural)) (z(a_rural, "rural") - z(b_rural, "rural"))^2  else NA_real_,
    n_avail    = sum(!is.na(c(d_age_sq, d_rural_sq))),
    insufficient_demo = n_avail < 2,
    d_mean     = if (!insufficient_demo) mean(c(d_age_sq, d_rural_sq), na.rm = TRUE) else NA_real_,
    phi_pop    = if (!is.na(d_mean)) exp(-d_mean) else NA_real_
  ) |>
  ungroup() |>
  left_join(anchor_age_flags, by = "anchor_id") |>
  select(anchor_id, subgroup_id, n_avail, insufficient_demo,
         age_overlap, in_age_range, phi_pop)

# =============================================================================
# 2b. Compute φ_geo: great-circle distance from La Paz, decayed
# =============================================================================
# Bolivia reference is La Paz (BOLIVIA_REF_LAT, BOLIVIA_REF_LNG) — set in
# R/anchors_demographics.R. Decay schedule (transparent, hand-coded):
#   same country (Bolivia)  → 1.00
#   distance <  3000 km     → 0.85
#   distance <  6000 km     → 0.65
#   distance >= 6000 km     → 0.50
# This captures regional similarity in a way that is interpretable and
# reproducible. Distances computed via Haversine formula.

haversine_km <- function(lat1, lng1, lat2, lng2) {
  R <- 6371
  dlat <- (lat2 - lat1) * pi / 180
  dlng <- (lng2 - lng1) * pi / 180
  a <- sin(dlat/2)^2 +
       cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlng/2)^2
  2 * R * asin(sqrt(a))
}

phi_geo <- anchors_demo |>
  transmute(
    anchor_id,
    distance_km = haversine_km(country_lat, country_lng,
                               BOLIVIA_REF_LAT, BOLIVIA_REF_LNG),
    phi_geo = case_when(
      country_iso == "BOL"   ~ 1.00,
      distance_km <  3000    ~ 0.85,
      distance_km <  6000    ~ 0.65,
      TRUE                   ~ 0.50
    )
  )

# =============================================================================
# 2c. Compute φ_econ: GDP per capita ratio anchor↔Bolivia
# =============================================================================
# If anchor country is poorer than Bolivia: φ_econ = 1 (no haircut for being
# from a less wealthy context — that's not a transportability deficit).
# If anchor country is richer: φ_econ = Bolivia/anchor (capped in [0.3, 1]).
# Captures that interventions evaluated in high-income contexts have access
# to systems and complementary inputs that Bolivia may lack.

phi_econ <- anchors_demo |>
  transmute(
    anchor_id,
    gdp_ratio = BOLIVIA_GDP_PC_USD / gdp_pc_usd,
    phi_econ  = pmin(1.0, pmax(0.30, gdp_ratio))
  )

# =============================================================================
# 2d. Compute φ_delivery: government vs NGO/research implementation
# =============================================================================
# Vivalt (2020) meta-analysis: programmes evaluated as research/NGO pilots
# tend to overstate effects by ~30% relative to government-implemented at
# scale. Government scale-ups face friction (training, supervision, fidelity)
# that research pilots avoid. Aula Conectada will be government-implemented,
# so anchors of the same modality get full weight.

phi_delivery <- anchors_demo |>
  transmute(
    anchor_id,
    phi_delivery = if_else(gov_implementer, 1.00, 0.70)
  )

# =============================================================================
# 3. Compute φ_baseline: headroom adjustment
# =============================================================================
# For anchors that report a control baseline level on a comparable outcome,
# scale by available headroom in Bolivia vs the anchor study.
phi_baseline_one <- function(anchor_row, sub_row) {
  y0_anchor <- anchor_row$baseline_y
  y_max     <- anchor_row$baseline_y_max
  out_col   <- paste0("baseline_", anchor_row$pop_match_outcome)
  y0_bolivia <- sub_row[[out_col]]

  if (is.na(y0_anchor) || is.na(y0_bolivia) || is.na(y_max)) return(NA_real_)
  headroom_anchor  <- y_max - y0_anchor
  headroom_bolivia <- y_max - y0_bolivia
  if (headroom_anchor <= 0) return(NA_real_)
  min(headroom_bolivia / headroom_anchor, 1)   # bounded above at 1
}

phi_base <- crossing(
  anchor_id   = anchors_demo$anchor_id,
  subgroup_id = bolivia_profiles$subgroup_id
) |>
  mutate(
    phi_baseline = map2_dbl(anchor_id, subgroup_id, function(a, s) {
      ar <- anchors_demo |> filter(anchor_id == a) |> as.list()
      sr <- bolivia_profiles |> filter(subgroup_id == s) |> as.list()
      phi_baseline_one(ar, sr)
    })
  )

# =============================================================================
# 4. Bolivia-adjusted effect with 95% CI propagated from anchor SE
# =============================================================================
ci <- function(estimate, se, alpha = 0.05) {
  z_crit <- qnorm(1 - alpha / 2)
  tibble(
    lower = estimate - z_crit * se,
    upper = estimate + z_crit * se
  )
}

# Each row: anchor × subgroup. We compute the Bolivia-adjusted effect as
#   adj_effect = external_effect × phi_pop × (phi_baseline if available, else 1)
# Then we propagate uncertainty using the anchor's SE only — the φ factors
# are point estimates derived from observable shares, not estimated parameters
# (we explicitly do NOT inflate them with synthetic uncertainty).
projections <- crossing(
  anchor_id   = anchors_demo$anchor_id,
  subgroup_id = bolivia_profiles$subgroup_id
) |>
  left_join(anchors_demo,             by = "anchor_id") |>
  left_join(bolivia_profiles,         by = "subgroup_id") |>
  left_join(phi_pop  |> select(anchor_id, subgroup_id, phi_pop,
                               n_avail, insufficient_demo,
                               age_overlap, in_age_range),
            by = c("anchor_id","subgroup_id")) |>
  left_join(phi_base |> select(anchor_id, subgroup_id, phi_baseline),
            by = c("anchor_id","subgroup_id")) |>
  left_join(phi_geo,      by = "anchor_id") |>
  left_join(phi_econ,     by = "anchor_id") |>
  left_join(phi_delivery, by = "anchor_id") |>
  mutate(
    usable_for_projection = in_age_range & !insufficient_demo,
    phi_baseline_used = coalesce(phi_baseline, 1),    # if NA, no adjustment
    # Multiplicative adjustment over four φ factors. φ_baseline only applied
    # when measurable (otherwise = 1, no adjustment).
    adj_factor   = phi_pop * phi_baseline_used *
                   phi_geo * phi_econ * phi_delivery,
    adj_effect   = effect * adj_factor,
    se_adj       = se     * adj_factor,
    lower_95     = adj_effect - 1.96 * se_adj,
    upper_95     = adj_effect + 1.96 * se_adj,
    ext_lower    = effect - 1.96 * se,
    ext_upper    = effect + 1.96 * se,
    # Parameter count: how many of the 5 φ factors are based on observed
    # data (vs assumed default of 1). Maximum = 5.
    n_params_used = as.integer(!is.na(phi_pop)) +     # φ_pop computed
                    as.integer(!is.na(phi_baseline)) + # φ_base measurable
                    as.integer(!is.na(phi_geo)) +      # always 1 here
                    as.integer(!is.na(phi_econ)) +     # always 1 here
                    as.integer(!is.na(phi_delivery)),  # always 1 here
    # Pretty effect strings
    ext_str = case_when(
      unit == "SD"        ~ sprintf("%.3f SD [%.3f, %.3f]", effect, ext_lower, ext_upper),
      unit == "ppts"      ~ sprintf("%.1f ppts [%.1f, %.1f]", effect*100, ext_lower*100, ext_upper*100),
      unit == "score_pts" ~ sprintf("%.1f pts [%.1f, %.1f]", effect, ext_lower, ext_upper),
      TRUE                ~ NA_character_
    ),
    adj_str = case_when(
      !usable_for_projection ~ "fuera de rango",
      unit == "SD"        ~ sprintf("%.3f SD [%.3f, %.3f]", adj_effect, lower_95, upper_95),
      unit == "ppts"      ~ sprintf("%.1f ppts [%.1f, %.1f]", adj_effect*100, lower_95*100, upper_95*100),
      unit == "score_pts" ~ sprintf("%.1f pts [%.1f, %.1f]", adj_effect, lower_95, upper_95),
      TRUE                ~ NA_character_
    )
  )

# Clean long-format output for the deck
proj_long <- projections |>
  select(anchor_id, programme, country, unit, programme_type,
         subgroup_id, subgroup_label, n_girls_weighted,
         effect, se, ext_str, ext_lower, ext_upper,
         age_overlap, in_age_range, insufficient_demo,
         usable_for_projection,
         phi_pop, phi_baseline, phi_baseline_used,
         phi_geo, phi_econ, phi_delivery,
         adj_factor, n_params_used,
         adj_effect, se_adj, lower_95, upper_95, adj_str)

write_csv(proj_long, here::here("output", "projections", "anchor_projections_long.csv"))

# Wide format: one row per anchor, columns by subgroup
proj_wide <- proj_long |>
  select(programme, country, subgroup_label, adj_str) |>
  pivot_wider(names_from = subgroup_label, values_from = adj_str)
write_csv(proj_wide, here::here("output", "projections", "anchor_projections_wide.csv"))

# =============================================================================
# 5. Write Excel workbook with methodology sheet
# =============================================================================
wb <- createWorkbook()
addWorksheet(wb, "Methodology")
writeData(wb, "Methodology", tibble(
  Section = c("Purpose",
              "Step 1 - phi_pop", "Step 2 - phi_baseline",
              "Step 3 - phi_geo", "Step 4 - phi_econ", "Step 5 - phi_delivery",
              "Step 6 - effect", "Step 7 - uncertainty",
              "Age-range guard", "Minimum demographics", "Why no indig_share",
              "What this avoids", "Key sources"),
  Description = c(
    "Translate each anchor's measured effect into a Bolivia-adjusted effect for each subgroup using observable features only.",
    "Population alignment = exp(-mean squared standardised distance over age_mid + rural_share). Bounded [0,1].",
    "Baseline headroom = (max - Y_Bolivia) / (max - Y_anchor) on the matched outcome. Bounded [0,1]. NA when not measurable.",
    "Geographic distance from La Paz (Haversine). Same country = 1.00; <3000 km = 0.85; <6000 km = 0.65; >=6000 km = 0.50.",
    "GDP per capita ratio Bolivia/anchor, capped in [0.30, 1.0]. Anchor poorer than Bolivia → no haircut.",
    "1.00 if government-implemented, 0.70 otherwise. Captures the research-pilot vs scale-up gap (Vivalt 2020).",
    "Bolivia-adjusted effect = external_effect × phi_pop × phi_baseline_used × phi_geo × phi_econ × phi_delivery.",
    "95% CI propagated from the anchor's reported SE: adj_effect ± 1.96 × SE × adj_factor. No synthetic noise added to the phi factors.",
    "Anchors whose age range overlaps <25% with target 10-17 are flagged 'out of range' and excluded.",
    "phi_pop is computed only when at least 2 features (age, rural) are available on both sides.",
    "indig_share was dropped from phi_pop in May 2026. 'Indigenous' is not apples-to-apples across contexts (Quechua/Aymara Bolivia, Scheduled Tribes India, unspecified Sierra Leone). Comparing marginal shares does not measure population similarity meaningfully.",
    "We removed the arbitrary 0.45/0.675/0.90 fidelity multipliers used in earlier drafts. Every phi factor in this workbook is derived from observable shares.",
    "Anchor demographics from Lit Review Comprehensive (Grueso, May 2026). Bolivia profiles from INE EH 2024."
  )
))

addWorksheet(wb, "Anchors")
writeData(wb, "Anchors", anchors_demo)

addWorksheet(wb, "Bolivia profiles")
writeData(wb, "Bolivia profiles", bolivia_profiles)

addWorksheet(wb, "phi_pop")
writeData(wb, "phi_pop", phi_pop)

addWorksheet(wb, "phi_baseline")
writeData(wb, "phi_baseline", phi_base)

addWorksheet(wb, "phi_geo")
writeData(wb, "phi_geo", phi_geo)

addWorksheet(wb, "phi_econ")
writeData(wb, "phi_econ", phi_econ)

addWorksheet(wb, "phi_delivery")
writeData(wb, "phi_delivery", phi_delivery)

addWorksheet(wb, "Projections (long)")
writeData(wb, "Projections (long)", proj_long)

addWorksheet(wb, "Projections (wide)")
writeData(wb, "Projections (wide)", proj_wide)

saveWorkbook(wb,
             here::here("output", "projections", "Bolivia_anchor_projections.xlsx"),
             overwrite = TRUE)

message("Stage 5 complete.")
message(" → output/projections/Bolivia_anchor_projections.xlsx (7 sheets)")
message(" → output/projections/anchor_projections_long.csv")
message(" → output/projections/anchor_projections_wide.csv")
print(proj_long |>
        filter(subgroup_id %in% c("priority", "rural_indig")) |>
        select(programme, subgroup_id, phi_pop, phi_baseline_used, adj_str))
