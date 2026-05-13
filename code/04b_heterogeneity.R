# 04b_heterogeneity.R — Stage 4b: heterogeneity via regression forests
# -----------------------------------------------------------------------------
# Inputs:  output/analysis_ready.rds
# Outputs: output/models/forest_var_importance.csv
#          output/models/forest_subgroup_preds.csv
#          output/figures/f8_var_importance.png
#          output/figures/f9_subgroup_exclusion.png
#
# NOTE: This is *not* causal forest in the Athey & Wager (2019) sense.
#       There is no treatment. We use `grf::regression_forest` to estimate
#       conditional expected outcomes E[Y | X] non-parametrically, then
#       compare predictions across observable subgroups. This identifies
#       which combinations of characteristics drive the largest baseline
#       exclusion gaps — a targeting question, not a treatment-effect one.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(readr); library(tibble); library(here); library(glue)
  library(ggplot2); library(scales); library(grf)
})

source(here::here("R", "utils.R"))
source(here::here("R", "theme.R"))
source(here::here("R", "variable_mapping.R"))
ensure_dirs()

ado <- readRDS(here::here("output", "analysis_ready.rds"))

# 1. Build the design matrix --------------------------------------------------
# Forest training sample = all adolescents 10–17 (both sexes). This lets the
# forest learn how the female effect varies across subgroups — restricting to
# girls would prevent measuring the gender × subgroup interaction.
X <- ado |>
  transmute(
    female      = as.integer(female == 1),
    age         = as.integer(age),
    rural       = as.integer(rural == 1),
    indigenous  = as.integer(indigenous == 1),
    disab_any   = coalesce(disab_any, 0L),
    poor_d      = as.integer(poor_d == 1),
    department  = as.integer(as.numeric(department))
  ) |>
  filter(complete.cases(female, age, rural, indigenous, disab_any, poor_d, department))

# Keep the same row mask for outcomes (drop incomplete-X rows)
ok <- complete.cases(ado |>
  select(female, age, rural, indigenous, disab_any, poor_d, department))
ado_ok <- ado[ok, ]
weights_vec <- ado_ok$weight

OUTCOMES <- list(
  attending     = "Asiste a la escuela",
  hh_internet   = "Internet en hogar",
  hh_any_device = "Cualquier dispositivo",
  hh_computer   = "Computadora en hogar"
)

X_pretty_names <- c(
  female      = "Niña (1=sí)",
  age         = "Edad",
  rural       = "Rural",
  indigenous  = "Indígena",
  disab_any   = "Discapacidad",
  poor_d      = "Pobre (INE)",
  department  = "Departamento"
)

set.seed(20260512)

# 2. Fit one regression forest per outcome ------------------------------------
fit_one_forest <- function(outcome_var) {
  Y <- ado_ok[[outcome_var]]
  ok2 <- !is.na(Y)
  forest <- regression_forest(
    X            = as.matrix(X[ok2, ]),
    Y            = Y[ok2],
    sample.weights = weights_vec[ok2],
    honesty      = TRUE,
    honesty.fraction = 0.5,
    num.trees    = 2000,
    seed         = 20260512
  )
  attr(forest, "ok_mask") <- ok2
  forest
}

message("Fitting forests …")
forests <- map(names(OUTCOMES), function(v) {
  message("  · ", v)
  fit_one_forest(v)
}) |> setNames(names(OUTCOMES))

# 3. Variable importance -----------------------------------------------------
vi_long <- imap_dfr(forests, function(f, v) {
  vi <- variable_importance(f) |> as.numeric()
  tibble(
    outcome   = v,
    outcome_label = OUTCOMES[[v]],
    variable  = colnames(X),
    var_label = X_pretty_names[colnames(X)],
    importance = vi / sum(vi)            # normalise to share
  )
})
write_csv(vi_long, here::here("output", "models", "forest_var_importance.csv"))

# 4. Predicted outcome by interpretable subgroup ------------------------------
# Define subgroup grid: female × rural × indigenous × poverty, holding
# disability=0, age=13 (centre), department=La Paz (modal value 2).
grid <- expand_grid(
  female      = c(0, 1),
  rural       = c(0, 1),
  indigenous  = c(0, 1),
  poor_d      = c(0, 1)
) |>
  mutate(
    age        = 13L,
    disab_any  = 0L,
    department = 2L                       # La Paz as reference
  ) |>
  select(female, age, rural, indigenous, disab_any, poor_d, department)

subgroup_preds <- imap_dfr(forests, function(f, v) {
  pred <- predict(f, newdata = as.matrix(grid))
  grid |>
    mutate(
      outcome      = v,
      outcome_label = OUTCOMES[[v]],
      pred         = pred$predictions
    )
})
write_csv(subgroup_preds, here::here("output", "models", "forest_subgroup_preds.csv"))

# 5. Build readable subgroup labels & rank by predicted exclusion -----------
subgroup_ranked <- subgroup_preds |>
  mutate(
    sex        = ifelse(female == 1, "Niñas", "Niños"),
    zona       = ifelse(rural  == 1, "Rural", "Urbano"),
    etnia      = ifelse(indigenous == 1, "Indígena", "No indígena"),
    pobreza    = ifelse(poor_d == 1, "Pobre", "No pobre"),
    profile    = paste(sex, zona, etnia, pobreza, sep = " · "),
    girl_only  = female == 1
  )

# 6. Figure F8: variable importance heatmap (outcome × variable) -------------
vi_plot <- vi_long |>
  mutate(
    var_label     = factor(var_label, levels = unname(X_pretty_names)),
    outcome_label = factor(outcome_label, levels = unname(unlist(OUTCOMES)))
  )

f8 <- ggplot(vi_plot, aes(x = outcome_label, y = var_label, fill = importance)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = scales::percent(importance, accuracy = 1)),
            size = 3.4, colour = "white", fontface = "bold") +
  scale_fill_gradient(
    low = "#9DD4EC", high = UNICEF_DARK,
    labels = scales::percent_format(accuracy = 1),
    breaks = c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60),
    limits = c(0, max(0.65, max(vi_plot$importance))),
    guide  = guide_colorbar(
      barheight = unit(3.0, "in"),         # tall legend bar
      barwidth  = unit(0.30, "in"),
      ticks     = TRUE,
      frame.colour = "grey60",
      ticks.colour = "grey60"
    )
  ) +
  labs(
    title    = "Variables que más explican la heterogeneidad",
    subtitle = "Importancia relativa de cada característica (regression forest, normalizada)",
    x = NULL, y = NULL, fill = "Importancia",
    caption = "Fuente: INE Bolivia – Encuesta de Hogares 2024. Forest de regresión sobre adolescentes 10–17."
  ) +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.title = element_text(size = 11, face = "bold"),
        legend.text  = element_text(size = 10))
save_fig(f8, "f8_var_importance", width = 11, height = 6)

# 7. Figure F9: predicted outcome by subgroup — niñas only, top-bottom -----
# For each outcome, show predicted level for the 8 girl-only profiles
# (rural × indigenous × poor), highlighting which combinations are worst.
girl_preds <- subgroup_ranked |>
  filter(girl_only) |>
  mutate(
    profile_short = paste(zona, etnia, pobreza, sep = " · "),
    profile_short = factor(profile_short)
  )

# Rank profiles by mean predicted level across the 4 outcomes (worst first)
profile_rank <- girl_preds |>
  group_by(profile_short) |>
  summarise(mean_pred = mean(pred), .groups = "drop") |>
  arrange(mean_pred) |>
  mutate(profile_short = factor(profile_short, levels = profile_short))

girl_preds <- girl_preds |>
  mutate(profile_short = factor(profile_short,
                                levels = levels(profile_rank$profile_short)))

f9 <- ggplot(girl_preds,
             aes(x = pred, y = profile_short, fill = outcome_label)) +
  geom_col(position = position_dodge(0.75), width = 0.65) +
  geom_text(aes(label = scales::percent(pred, accuracy = 1)),
            position = position_dodge(0.75), hjust = -0.15, size = 2.9,
            colour = GREY_DARK) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1.08),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = c("Asiste a la escuela"    = ACCENT_GIRL,
                               "Internet en hogar"      = UNICEF_DARK,
                               "Cualquier dispositivo"  = UNICEF_BLUE,
                               "Computadora en hogar"   = "#9DD4EC")) +
  labs(
    title    = "Acceso predicho por subgrupo — niñas adolescentes 10–17",
    subtitle = "Promedio condicional sobre la EH 2024 (regression forest, edad = 13)",
    x = NULL, y = NULL, fill = NULL,
    caption = "Fuente: INE Bolivia – Encuesta de Hogares 2024. Predicciones del bosque de regresión por subgrupo."
  ) +
  theme(panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_blank(),
        legend.position = "top")
save_fig(f9, "f9_subgroup_exclusion", width = 11, height = 6)

# 8. Save a small "headline gap" table for the slide -------------------------
headline_gaps <- girl_preds |>
  group_by(outcome, outcome_label) |>
  summarise(
    best_profile  = profile_short[which.max(pred)],
    best_pred     = max(pred),
    worst_profile = profile_short[which.min(pred)],
    worst_pred    = min(pred),
    gap_ppts      = (max(pred) - min(pred)) * 100,
    .groups = "drop"
  )
write_csv(headline_gaps, here::here("output", "models", "forest_headline_gaps.csv"))

message("Stage 4b complete.")
message(" → output/models/forest_var_importance.csv")
message(" → output/models/forest_subgroup_preds.csv")
message(" → output/models/forest_headline_gaps.csv")
message(" → output/figures/f8_var_importance.png")
message(" → output/figures/f9_subgroup_exclusion.png")

print(headline_gaps)
