# 03b_figures_en.R — Stage 3: figures in English for the investment case ----
# Inputs:  output/analysis_ready.rds
# Outputs: output/figures/f0_…png (ToC) y f1_…png through f7_…png
# ------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(readr); library(tibble); library(here); library(glue)
  library(ggplot2); library(scales); library(survey); library(srvyr)
  library(ggtext); library(patchwork)
})

source(here::here("R", "utils.R"))
source(here::here("R", "theme.R"))
source(here::here("R", "variable_mapping.R"))
ensure_dirs()

ado <- readRDS(here::here("output", "analysis_ready.rds"))

options(survey.lonely.psu = "adjust")
des <- ado |>
  as_survey_design(ids = psu, strata = stratum, weights = weight, nest = TRUE)

# Helper: weighted % for one indicator, optionally by group(s) ------------
wt_pct <- function(design, var, by = NULL) {
  fml <- as.formula(paste0("~", var))
  if (is.null(by)) {
    tibble(estimate = as.numeric(svymean(fml, design, na.rm = TRUE)))
  } else {
    by_fml <- as.formula(paste0("~", paste(by, collapse = "+")))
    svyby(fml, by_fml, design, svymean, na.rm = TRUE) |>
      as_tibble() |>
      rename(estimate = !!sym(var))
  }
}

cap_src <- "Source: INE Bolivia – Household Survey 2024. Weighted estimates, adolescents 10–19."

# ========================================================================
# Figura 0: Teoría de Cambio (diagrama horizontal en 6 columnas)
# ========================================================================
toc_cols <- tribble(
  ~stage,    ~label,                ~content,
  1, "Inputs",            "Teachers\nConnectivity\nDevices\nDigital content\nInclusive materials\nSTEM modules",
  2, "Activities",        "Teacher training\nDigital pedagogy\nCollaborative learning\nCurriculum integration\nGirls' participation\nFamily engagement",
  3, "Outputs",          "Teachers trained\nClassrooms equipped\nContent in use\nGirls in digital activities\nPriority groups identified",
  4, "Short-term\noutcomes", "Teaching practice\nEquitable participation\nDigital confidence\nDigital skills\nHigher attendance",
  5, "Medium-term\noutcomes","Better learning\nLess dropout\nBetter progression\nSTEM interest\nLess digital exclusion",
  6, "Expected\nImpact",  "Reduction of gender\nand intersectional\ngaps in digital\nlearning and STEM\ntrajectories"
) |>
  mutate(
    x      = stage,
    fill_c = c(PAL_SEQ[1:5], UNICEF_DARK)[stage]
  )

f0 <- ggplot(toc_cols, aes(x = x, y = 1)) +
  geom_tile(aes(fill = I(fill_c)), height = 0.6, width = 0.92) +
  geom_text(aes(label = label), y = 1.42, fontface = "bold",
            colour = GREY_DARK, size = 4.6) +
  geom_text(aes(label = content), size = 3.4,
            colour = "white", lineheight = 1.05) +
  geom_segment(data = tibble(x = 1:5),
               aes(x = x + 0.48, xend = x + 0.52, y = 1, yend = 1),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),
               colour = GREY_DARK, inherit.aes = FALSE) +
  scale_x_continuous(limits = c(0.5, 6.5)) +
  scale_y_continuous(limits = c(0.55, 1.55)) +
  labs(title    = NULL,
       subtitle = "From inputs to impact: technology as means, pedagogy as engine",
       x = NULL, y = NULL) +
  theme_void(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", size = 16, colour = UNICEF_DARK,
                                     margin = margin(b = 4)),
        plot.subtitle = element_text(size = 12, colour = GREY_DARK,
                                     margin = margin(b = 14)),
        plot.caption  = element_text(size = 9, colour = "grey40", hjust = 0,
                                     margin = margin(t = 10)),
        plot.margin   = margin(18, 18, 14, 18))
save_fig(f0, "f0_theory_of_change_en", width = 12, height = 5.5)

# ========================================================================
# NUEVAS Figuras 1a-1d: Outcomes educativos por sexo + cortes intersec.
# These are the "descriptive-first" figures for the investment case.
# Outcomes: enrolment, attendance, age-for-grade delay.
# ========================================================================
EDU_LEVELS <- c("Enrolled", "Attending school",
                "Age-for-grade delay",
                "Dropout",
                "NEET (15-19)")
fill_sx    <- c("Boys" = ACCENT_BOY, "Girls" = ACCENT_GIRL)

# Helper to build education dataset by (sex × group)
# Five indicators: enrolment, attendance, age-for-grade delay, dropout, NEET.
# NEET is computed only for 15–19 (conventional adolescent range).
edu_outcomes_by <- function(design, by_var = NULL) {
  fr_filter <- if (!is.null(by_var)) c("female", by_var) else "female"
  # Sub-design for NEET: only 15-19 (older adolescents)
  design_1519 <- subset(design, age >= 15 & age <= 19)
  bind_rows(
    wt_pct(design, "enrolled",     by = fr_filter) |>
      mutate(indicator = "Enrolled"),
    wt_pct(design, "attending",    by = fr_filter) |>
      mutate(indicator = "Attending school"),
    wt_pct(design, "grade_delay",  by = fr_filter) |>
      mutate(indicator = "Age-for-grade delay"),
    wt_pct(design, "dropout",      by = fr_filter) |>
      mutate(indicator = "Dropout"),
    wt_pct(design_1519, "neet",    by = fr_filter) |>
      mutate(indicator = "NEET (15-19)")
  ) |>
    mutate(
      sexo      = ifelse(female == 1, "Girls", "Boys"),
      indicator = factor(indicator, levels = EDU_LEVELS)
    )
}

# Tema compartido: centra facet strips y leyenda
edu_theme <- function() {
  theme(
    strip.background = element_rect(fill = "white", colour = NA),
    strip.text = element_text(hjust = 0.5, face = "bold", size = 12,
                              colour = GREY_DARK,
                              margin = margin(b = 6, t = 4)),
    strip.placement = "outside",
    legend.position = "top",
    legend.justification = "center",
    legend.box.just  = "center",
    panel.spacing.x  = unit(1.2, "lines")
  )
}

# --- f1a: outcomes educativos por sexo (global) -------------------------
f1a_dat <- edu_outcomes_by(des)

f1a <- ggplot(f1a_dat, aes(x = sexo, y = estimate, fill = sexo)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            vjust = -0.4, size = 4.0, colour = GREY_DARK) +
  facet_wrap(~indicator, nrow = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx, name = NULL) +
  labs(title    = NULL,
       subtitle = "Adolescents 10–19 in Bolivia",
       x = NULL, y = NULL, caption = cap_src) +
  edu_theme()
save_fig(f1a, "f1a_edu_overall_by_sex_en", height = 6.5)

# --- f1b: education outcomes by sex × rural --------------------------
f1b_dat <- edu_outcomes_by(des, "rural") |>
  mutate(zona = ifelse(rural == 1, "Rural", "Urban"))

f1b <- ggplot(f1b_dat, aes(x = zona, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator, nrow = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = NULL,
       subtitle = "Adolescents 10–19, by sex and rural/urban area",
       x = NULL, y = NULL, fill = NULL, caption = cap_src) +
  edu_theme()
save_fig(f1b, "f1b_edu_by_sex_rural_en", height = 6.5)

# --- f1c: education outcomes by sex × indigenous -----------------------
f1c_dat <- edu_outcomes_by(des, "indigenous") |>
  mutate(grupo = ifelse(indigenous == 1, "Indigenous", "Non-indigenous"))

f1c <- ggplot(f1c_dat, aes(x = grupo, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator, nrow = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = NULL,
       subtitle = "Adolescents 10–19, by sex",
       x = NULL, y = NULL, fill = NULL, caption = cap_src) +
  edu_theme()
save_fig(f1c, "f1c_edu_by_sex_indig_en", height = 6.5)

# --- f1d: education outcomes by sex × poverty ------------------------
f1d_dat <- edu_outcomes_by(des, "poor_d") |>
  filter(!is.na(poor_d)) |>
  mutate(grupo = ifelse(poor_d == 1, "Poor (INE)", "Non-poor"))

f1d <- ggplot(f1d_dat, aes(x = grupo, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator, nrow = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = NULL,
       subtitle = "Adolescents 10–19, by sex and poverty status (INE)",
       x = NULL, y = NULL, fill = NULL, caption = cap_src) +
  edu_theme()
save_fig(f1d, "f1d_edu_by_sex_poverty_en", height = 6.5)

# ========================================================================
# Age-band figures (appendix): same partitions as f1a-f1d / f5
# but crossed by early (10-14) vs late (15-19) adolescence.
# For readability we use a helper returning 5 outcomes ×
# sex × age_group_adol (NEET only meaningful for 15-19, so
# in 10-14 it will be ≈ 0 — made explicit in the caption).
# ========================================================================

edu_outcomes_by_ageband <- function(design, extra_var = NULL) {
  group_cols <- c("female", "age_group_adol", extra_var)
  bind_rows(
    wt_pct(design, "enrolled",     by = group_cols) |>
      mutate(indicator = "Enrolled"),
    wt_pct(design, "attending",    by = group_cols) |>
      mutate(indicator = "Attending school"),
    wt_pct(design, "grade_delay",  by = group_cols) |>
      mutate(indicator = "Age-for-grade delay"),
    wt_pct(design, "dropout",      by = group_cols) |>
      mutate(indicator = "Dropout"),
    wt_pct(design, "neet",         by = group_cols) |>
      mutate(indicator = "NEET (15-19)")
  ) |>
    filter(!is.na(age_group_adol)) |>
    mutate(
      sexo = ifelse(female == 1, "Girls", "Boys"),
      indicator = factor(indicator, levels = EDU_LEVELS),
      # Short axis labels
      banda = case_when(
        stringr::str_detect(age_group_adol, "temprana") ~ "10-14",
        stringr::str_detect(age_group_adol, "tardía")   ~ "15-19",
        TRUE                                            ~ NA_character_
      ),
      banda = factor(banda, levels = c("10-14", "15-19"))
    )
}

cap_age <- "Source: INE Bolivia – Household Survey 2024. Weighted estimates."

# --- f1a_age: by sex × age band ---
f1a_age_dat <- edu_outcomes_by_ageband(des)

f1a_age <- ggplot(f1a_age_dat, aes(x = banda, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.0,
            colour = GREY_DARK) +
  facet_wrap(~indicator, nrow = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = NULL,
       subtitle = "Early (10-14) vs late (15-19) adolescence",
       x = NULL, y = NULL, fill = NULL, caption = cap_age) +
  edu_theme()
save_fig(f1a_age, "f1a_age_edu_by_sex_ageband_en", height = 6.5)

# --- f1b_age: sex × age band × rural ---
f1b_age_dat <- edu_outcomes_by_ageband(des, "rural") |>
  filter(!is.na(rural)) |>
  mutate(zona = ifelse(rural == 1, "Rural", "Urban"))

f1b_age <- ggplot(f1b_age_dat,
                  aes(x = banda, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 2.6,
            colour = GREY_DARK) +
  facet_grid(zona ~ indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = NULL,
       subtitle = "By sex, age (10-14 / 15-19) and area of residence",
       x = NULL, y = NULL, fill = NULL, caption = cap_age) +
  edu_theme()
save_fig(f1b_age, "f1b_age_edu_by_sex_rural_ageband_en", width = 12, height = 5.5)

# --- f1c_age: sex × age band × indigenous ---
f1c_age_dat <- edu_outcomes_by_ageband(des, "indigenous") |>
  filter(!is.na(indigenous)) |>
  mutate(grupo = ifelse(indigenous == 1, "Indigenous", "Non-indigenous"))

f1c_age <- ggplot(f1c_age_dat,
                  aes(x = banda, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 2.6,
            colour = GREY_DARK) +
  facet_grid(grupo ~ indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = NULL,
       subtitle = "By sex, age (10-14 / 15-19) and indigenous status",
       x = NULL, y = NULL, fill = NULL, caption = cap_age) +
  edu_theme()
save_fig(f1c_age, "f1c_age_edu_by_sex_indig_ageband_en", width = 12, height = 5.5)

# --- f1d_age: sex × age band × poverty ---
f1d_age_dat <- edu_outcomes_by_ageband(des, "poor_d") |>
  filter(!is.na(poor_d)) |>
  mutate(grupo = ifelse(poor_d == 1, "Poor (INE)", "Non-poor"))

f1d_age <- ggplot(f1d_age_dat,
                  aes(x = banda, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 2.6,
            colour = GREY_DARK) +
  facet_grid(grupo ~ indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = NULL,
       subtitle = "By sex, age (10-14 / 15-19) and poverty status",
       x = NULL, y = NULL, fill = NULL, caption = cap_age) +
  edu_theme()
save_fig(f1d_age, "f1d_age_edu_by_sex_poverty_ageband_en", width = 12, height = 5.5)

# --- f5_age: sex × age band × disability ---
f5_age_dat <- edu_outcomes_by_ageband(des, "disab_any") |>
  filter(!is.na(disab_any)) |>
  mutate(grupo = ifelse(disab_any == 1, "Disability (WG 3+)", "No disability"))

f5_age <- ggplot(f5_age_dat,
                 aes(x = banda, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 2.6,
            colour = GREY_DARK) +
  facet_grid(grupo ~ indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = NULL,
       subtitle = "By sex, age (10-14 / 15-19) and disability",
       x = NULL, y = NULL, fill = NULL, caption = cap_age) +
  edu_theme()
save_fig(f5_age, "f5_age_edu_by_sex_disab_ageband_en", width = 12, height = 5.5)


# ========================================================================
# Figure 1: Acceso digital del hogar por sexo y grupo etario
# ========================================================================
f1_dat <- bind_rows(
  wt_pct(des, "hh_internet",   by = c("female", "age_group")) |>
    mutate(indicator = "Internet at home"),
  wt_pct(des, "hh_computer",   by = c("female", "age_group")) |>
    mutate(indicator = "Computadora en el hogar"),
  wt_pct(des, "hh_smartphone", by = c("female", "age_group")) |>
    mutate(indicator = "Smartphone en el hogar")
) |>
  mutate(
    sexo = ifelse(female == 1, "Girls", "Boys"),
    indicator = factor(indicator,
                       levels = c("Internet at home",
                                  "Computadora en el hogar",
                                  "Smartphone en el hogar"))
  )

f1 <- ggplot(f1_dat, aes(x = age_group, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator, nrow = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Boys" = ACCENT_BOY, "Girls" = ACCENT_GIRL)) +
  labs(title    = NULL,
       subtitle = "Share of girls and boys in households with each digital asset, by age group",
       x = NULL, y = NULL, fill = NULL, caption = cap_src) +
  edu_theme()
save_fig(f1, "f1_digital_access_sex_age_en")

# ========================================================================
# Figure 2: Brecha digital territorial (urbano vs rural × sexo)
# ========================================================================
f2_dat <- bind_rows(
  wt_pct(des, "hh_internet",   by = c("female", "rural")) |>
    mutate(indicator = "Internet at home"),
  wt_pct(des, "hh_computer",   by = c("female", "rural")) |>
    mutate(indicator = "Computadora en el hogar"),
  wt_pct(des, "hh_smartphone", by = c("female", "rural")) |>
    mutate(indicator = "Smartphone en el hogar")
) |>
  mutate(
    sexo = ifelse(female == 1, "Girls", "Boys"),
    zona = ifelse(rural  == 1, "Rural", "Urban"),
    indicator = factor(indicator,
                       levels = c("Internet at home",
                                  "Computadora en el hogar",
                                  "Smartphone en el hogar"))
  )

f2 <- ggplot(f2_dat, aes(x = zona, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator, nrow = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Boys" = ACCENT_BOY, "Girls" = ACCENT_GIRL)) +
  labs(title    = NULL,
       subtitle = "Adolescents 10–19, by sex and area of residence",
       x = NULL, y = NULL, fill = NULL, caption = cap_src) +
  edu_theme()
save_fig(f2, "f2_digital_access_area_sex_en")

# ========================================================================
# Figure 3: Indigenous girls vs no indígenas — brechas digitales y escolares
# ========================================================================
f3_dat <- des |>
  filter(female == 1) |>
  group_by(indigenous) |>
  summarise(
    `Smartphone en el hogar` = survey_mean(hh_smartphone == 1, na.rm = TRUE, vartype = NULL),
    `Internet en el hogar`   = survey_mean(hh_internet   == 1, na.rm = TRUE, vartype = NULL),
    `Computadora en el hogar`= survey_mean(hh_computer   == 1, na.rm = TRUE, vartype = NULL),
    `Asiste a la escuela`    = survey_mean(attending     == 1, na.rm = TRUE, vartype = NULL)
  ) |>
  pivot_longer(-indigenous, names_to = "indicator", values_to = "estimate") |>
  mutate(group = ifelse(indigenous == 1, "Indigenous girls", "Non-indigenous girls"))

f3 <- ggplot(f3_dat, aes(x = estimate, y = indicator, fill = group)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), hjust = -0.15, size = 3.2,
            colour = GREY_DARK) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1.05),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = c("Indigenous girls"     = ACCENT_GIRL,
                               "Non-indigenous girls"  = UNICEF_BLUE)) +
  labs(title    = NULL,
       subtitle = "Adolescent girls 10–19 in Bolivia, by indigenous status",
       x = NULL, y = NULL, fill = NULL, caption = cap_src) +
  theme(panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_blank())
save_fig(f3, "f3_indigenous_girls_gaps_en")

# ========================================================================
# Figure 4: School attendance × internet at home
# Lollipop with zoomed axis to make the gap visible (attendance ~90%+)
# ========================================================================
f4_dat <- wt_pct(des, "attending", by = c("female", "hh_internet")) |>
  mutate(
    sexo  = ifelse(female == 1, "Girls", "Boys"),
    hogar = ifelse(hh_internet == 1, "With internet", "Without internet"),
    hogar = factor(hogar, levels = c("Without internet", "With internet"))
  )

ymin <- floor((min(f4_dat$estimate) - 0.02) * 20) / 20

f4 <- ggplot(f4_dat, aes(x = hogar, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 0.1)),
            position = position_dodge(width = 0.7),
            vjust = -0.6, size = 4, fontface = "bold", show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1.0),
                     expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(values = c("Boys" = ACCENT_BOY, "Girls" = ACCENT_GIRL)) +
  labs(title    = NULL,
       subtitle = "School attendance rate of adolescents 10–19, by household connectivity",
       x = NULL, y = "Currently attending", fill = NULL, caption = cap_src) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
        legend.position = "top")
save_fig(f4, "f4_attendance_by_internet_en")

# ========================================================================
# Figure 5: Discapacidad × sexo — mismo formato que f1a–f1d
# Compares girls and boys WITH vs WITHOUT disability across the three outcomes
# education indicators. Small sample size → interpret with caution.
# ========================================================================
f5_dat <- edu_outcomes_by(des, "disab_any") |>
  filter(!is.na(disab_any)) |>
  mutate(grupo = ifelse(disab_any == 1,
                        "Con discapacidad",
                        "No disability"),
         grupo = factor(grupo, levels = c("No disability",
                                          "Con discapacidad")))

f5 <- ggplot(f5_dat, aes(x = grupo, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator, nrow = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = NULL,
       subtitle = "Adolescents 10–19, by sex · Washington Group severidad ≥ 3",
       x = NULL, y = NULL, fill = NULL,
       caption = paste0(cap_src,
                        " Small sample size (~0.4%) — interpret with caution.")) +
  edu_theme()
save_fig(f5, "f5_disability_gap_en", height = 6.5)

# ========================================================================
# Figure 6: Departmental variation — household internet (girls)
# ========================================================================
f6_dat <- wt_pct(des |> filter(female == 1),
                 "hh_internet", by = "department") |>
  arrange(estimate)

f6 <- ggplot(f6_dat,
             aes(x = estimate, y = reorder(department, estimate))) +
  geom_col(fill = UNICEF_BLUE, width = 0.7) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            hjust = -0.2, size = 3.4, colour = GREY_DARK) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, max(f6_dat$estimate) * 1.18),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title    = NULL,
       subtitle = "Household internet, adolescent girls 10–19, by department",
       x = NULL, y = NULL, caption = cap_src) +
  theme(panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_blank())
save_fig(f6, "f6_internet_by_department_en")

# ========================================================================
# Figure 7: Población objetivo (funnel) — Aula Conectada
# ========================================================================
funnel <- read_csv(here::here("output", "tables", "t8_target_funnel.csv"),
                   show_col_types = FALSE) |>
  filter(segment != "All adolescents 10–19 (weighted)") |>
  mutate(
    # Recode segment labels for the figure
    segment_en = recode(segment,
      "All girls 10–19 (weighted)"                                              = "All girls 10–19",
      "Girls 10–19 — rural"                                                     = "Rural girls 10–19",
      "Girls 10–19 — Indigenous"                                                = "Indigenous girls 10–19",
      "Girls 10–19 — poor (INE)"                                                = "Girls in poverty 10–19",
      "Girls 10–19 — disability (WG 3+)"                                        = "Girls with disability 10–19",
      "Girls 10–19 — no HH internet"                                            = "Girls without HH internet 10–19",
      "Girls 10–19 — priority (rural OR Indigenous OR poor OR disability)"      = "Priority girls (rural ∨ Indigenous ∨ poor ∨ disability)"
    ),
    segment_en = factor(segment_en, levels = rev(segment_en))
  )

f7 <- ggplot(funnel, aes(x = n_pop, y = segment_en)) +
  geom_col(fill = UNICEF_DARK, width = 0.7) +
  geom_text(aes(label = scales::comma(round(n_pop))),
            hjust = -0.1, size = 3.4, colour = GREY_DARK) +
  scale_x_continuous(labels = scales::comma_format(),
                     expand = expansion(mult = c(0, 0.18))) +
  labs(title    = NULL,
       subtitle = "Weighted adolescent girls 10–19 in Bolivia, by segment",
       x = NULL, y = NULL, caption = cap_src) +
  theme(panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_blank())
save_fig(f7, "f7_target_funnel_en", width = 10, height = 6)

message("Stage 3 complete. → output/figures/f0_…png + f1_…png through f7_…png")
