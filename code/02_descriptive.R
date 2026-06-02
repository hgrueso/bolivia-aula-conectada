# 02_descriptive.R — Stage 2: weighted descriptives -----------------------
# Inputs:  output/analysis_ready.rds
# Outputs: output/tables/t1_…csv … t8_…csv
# ------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(readr); library(tibble); library(here); library(glue)
  library(survey); library(srvyr); library(scales)
})

source(here::here("R", "utils.R"))
source(here::here("R", "variable_mapping.R"))
ensure_dirs()

ado <- readRDS(here::here("output", "analysis_ready.rds"))

# 1. Survey design --------------------------------------------------------
options(survey.lonely.psu = "adjust")
des <- ado |>
  as_survey_design(ids = psu, strata = stratum, weights = weight, nest = TRUE)

# Outcomes / indicators to summarise
KEY_BIN <- c("attending", "enrolled", "matric_no_asiste", "grade_delay",
             "public_school", "bono_juancito", "meal_school",
             "hh_internet", "hh_computer", "hh_tablet", "hh_smartphone",
             "hh_any_device",
             "disab_any", "indigenous", "poor_d", "rural")

# Helper: weighted mean by group(s), with N (weighted population) ---------
# svyby returns: <by-cols>, <var-name>, se   (for single-response calls)
# We standardise to: <by-cols>, variable, estimate, se
wt_summary <- function(design, vars, by = NULL) {
  if (!is.null(by)) vars <- setdiff(vars, by)   # never group by var on itself
  if (length(vars) == 0) {
    return(tibble(variable = character(), estimate = numeric(), se = numeric()))
  }
  res <- map_dfr(vars, function(v) {
    fml <- as.formula(paste0("~", v))
    if (is.null(by)) {
      m <- svymean(fml, design, na.rm = TRUE)
      n <- sum(weights(design)[!is.na(design$variables[[v]])])
      tibble(variable = v,
             estimate = as.numeric(m),
             se       = sqrt(diag(attr(m, "var"))) |> as.numeric(),
             n_pop    = n)
    } else {
      by_fml <- as.formula(paste0("~", paste(by, collapse = "+")))
      df <- svyby(fml, by_fml, design, svymean, na.rm = TRUE) |>
        as_tibble()
      # The estimate column is named after v; the SE column is `se` for a
      # single-response call (or `se.<v>` for multi-response variants).
      se_col <- intersect(c("se", paste0("se.", v)), names(df))[1]
      if (is.na(se_col)) {
        # Defensive: keep going even if se columns aren't where expected
        df$se <- NA_real_
        se_col <- "se"
      }
      df |>
        rename(estimate = !!sym(v), se = !!sym(se_col)) |>
        mutate(variable = v)
    }
  })
  res
}

# Table 1 — overall (adolescents 10–19) -----------------------------------
t1 <- wt_summary(des, KEY_BIN) |>
  mutate(pct = scales::percent(estimate, accuracy = 0.1)) |>
  select(variable, estimate, se, pct, n_pop) |>
  arrange(variable)
write_table(t1, "t1_overall_adolescents")

# Table 2 — by sex --------------------------------------------------------
t2 <- wt_summary(des, KEY_BIN, by = "female")
t2 <- t2 |>
  mutate(sex = ifelse(female == 1, "Girls", "Boys")) |>
  select(variable, sex, estimate, se) |>
  pivot_wider(names_from = sex, values_from = c(estimate, se)) |>
  mutate(
    gap_girls_minus_boys = estimate_Girls - estimate_Boys,
    gap_pct_pts = gap_girls_minus_boys * 100
  )
write_table(t2, "t2_by_sex")

# Table 3 — by area (urban/rural) within girls only ----------------------
des_girls <- des |> filter(female == 1)
t3 <- wt_summary(des_girls, KEY_BIN, by = "rural")
t3 <- t3 |>
  mutate(area = ifelse(rural == 1, "Rural", "Urban")) |>
  select(variable, area, estimate, se) |>
  pivot_wider(names_from = area, values_from = c(estimate, se)) |>
  mutate(gap_rural_minus_urban = estimate_Rural - estimate_Urban)
write_table(t3, "t3_girls_by_area")

# Table 4 — Indigenous girls vs non-Indigenous girls ----------------------
t4 <- wt_summary(des_girls, KEY_BIN, by = "indigenous")
t4 <- t4 |>
  mutate(group = ifelse(indigenous == 1, "Indigenous", "Non-Indigenous")) |>
  select(variable, group, estimate, se) |>
  pivot_wider(names_from = group, values_from = c(estimate, se)) |>
  mutate(gap_indig_minus_nonindig = estimate_Indigenous - `estimate_Non-Indigenous`)
write_table(t4, "t4_girls_by_indigenous")

# Table 5 — Girls with vs without disability ------------------------------
t5 <- wt_summary(des_girls, KEY_BIN, by = "disab_any")
t5 <- t5 |>
  mutate(group = ifelse(disab_any == 1, "Disability", "No disability")) |>
  select(variable, group, estimate, se) |>
  pivot_wider(names_from = group, values_from = c(estimate, se)) |>
  mutate(gap_disab_minus_nondisab =
           estimate_Disability - `estimate_No disability`)
write_table(t5, "t5_girls_by_disability")

# Table 6 — Girls by income quintile --------------------------------------
t6 <- wt_summary(des_girls, KEY_BIN, by = "inc_quintile")
t6 <- t6 |>
  select(variable, inc_quintile, estimate, se) |>
  arrange(variable, inc_quintile)
write_table(t6, "t6_girls_by_income_quintile")

# Table 7 — Girls by department --------------------------------------------
t7 <- wt_summary(des_girls, KEY_BIN, by = "department")
t7 <- t7 |>
  select(variable, department, estimate, se) |>
  arrange(variable, department)
write_table(t7, "t7_girls_by_department")

# Table 8 — Target population funnel (weighted girl counts) ---------------
girl_n <- function(design, expr) {
  e <- rlang::enquo(expr)
  d <- design |> filter(!!e)
  sum(weights(d))
}
t8 <- tibble(
  segment = c(
    "All adolescents 10–19 (weighted)",
    "All girls 10–19 (weighted)",
    "Girls 10–19 — rural",
    "Girls 10–19 — Indigenous",
    "Girls 10–19 — poor (INE)",
    "Girls 10–19 — disability (WG 3+)",
    "Girls 10–19 — no HH internet",
    "Girls 10–19 — priority (rural OR Indigenous OR poor OR disability)"
  ),
  n_pop = c(
    sum(weights(des)),
    girl_n(des, female == 1),
    girl_n(des, female == 1 & rural == 1),
    girl_n(des, female == 1 & indigenous == 1),
    girl_n(des, female == 1 & poor_d == 1),
    girl_n(des, female == 1 & disab_any == 1),
    girl_n(des, female == 1 & hh_internet == 0),
    girl_n(des, female == 1 & (rural == 1 | indigenous == 1 |
                               poor_d == 1 | disab_any == 1))
  )
) |>
  mutate(share_of_all_girls = n_pop / n_pop[2])
write_table(t8, "t8_target_funnel")

message("Stage 2 complete. → output/tables/t1_… through t8_…")
print(t8)
