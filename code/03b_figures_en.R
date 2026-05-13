# 03b_figures_en.R — English-language figures for the parallel deck
# Inputs:  output/analysis_ready.rds
# Outputs: output/figures/f0_..._en.png, f1_..._en.png, ..., f7_..._en.png
# ------------------------------------------------------------------------
# Mirrors 03_figures.R one-for-one, with all titles, subtitles, axis labels,
# legend labels, and captions in English. Filenames carry an `_en` suffix.

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

cap_src <- "Source: INE Bolivia – Household Survey 2024. Weighted estimates, adolescents 10–17."

# ========================================================================
# Figure 0: Theory of Change (horizontal 6-column diagram)
# ========================================================================
toc_cols <- tribble(
  ~stage, ~label,                  ~content,
  1, "Inputs",                "Teachers\nConnectivity\nDevices\nDigital content\nInclusive materials\nSTEM modules",
  2, "Activities",            "Teacher training\nDigital pedagogy\nCollaborative learning\nCurriculum integration\nGirls' participation\nFamily engagement",
  3, "Outputs",               "Trained teachers\nEquipped classrooms\nContent in use\nGirls in digital activities\nPriority groups identified",
  4, "Short-term\noutcomes",  "Teacher practice\nEquitable participation\nDigital confidence\nDigital skills\nHigher attendance",
  5, "Medium-term\noutcomes", "Better learning\nLower dropout\nBetter progression\nSTEM interest\nLower digital exclusion",
  6, "Expected\nImpact",      "Reduction of gender\nand intersectional\ngaps in digital\nlearning and STEM\npathways"
) |>
  mutate(x = stage, fill_c = c(PAL_SEQ[1:5], UNICEF_DARK)[stage])

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
  labs(title    = "Theory of Change — Aula Conectada",
       subtitle = "From inputs to impact: technology as means, pedagogy as engine",
       x = NULL, y = NULL) +
  theme_void(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", size = 16, colour = UNICEF_DARK,
                                     margin = margin(b = 4)),
        plot.subtitle = element_text(size = 12, colour = GREY_DARK,
                                     margin = margin(b = 14)),
        plot.margin   = margin(18, 18, 14, 18))
save_fig(f0, "f0_theory_of_change_en", width = 12, height = 5.5)

# ========================================================================
# NEW Figures 1a-1d: Education outcomes by sex + intersectional cuts
# (English versions, mirror the Spanish f1a/b/c/d in 03_figures.R)
# Outcomes: enrolment, attendance, age-for-grade delay.
# ========================================================================
EDU_LEVELS_EN <- c("Enrolled", "Attending School", "Age-for-Grade Delay")
fill_sx       <- c("Boys" = ACCENT_BOY, "Girls" = ACCENT_GIRL)

edu_outcomes_by_en <- function(design, by_var = NULL) {
  fr_filter <- if (!is.null(by_var)) c("female", by_var) else "female"
  bind_rows(
    wt_pct(design, "enrolled",     by = fr_filter) |>
      mutate(indicator = "Enrolled"),
    wt_pct(design, "attending",    by = fr_filter) |>
      mutate(indicator = "Attending School"),
    wt_pct(design, "grade_delay",  by = fr_filter) |>
      mutate(indicator = "Age-for-Grade Delay")
  ) |>
    mutate(
      sex       = ifelse(female == 1, "Girls", "Boys"),
      indicator = factor(indicator, levels = EDU_LEVELS_EN)
    )
}

# Shared theme: centred facet strips + top legend
edu_theme_en <- function() {
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

# --- f1a: education outcomes by sex (overall) ---------------------------
f1a_dat <- edu_outcomes_by_en(des)

f1a <- ggplot(f1a_dat, aes(x = sex, y = estimate, fill = sex)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            vjust = -0.4, size = 4.0, colour = GREY_DARK) +
  facet_wrap(~indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx, name = NULL) +
  labs(title    = "Gender gaps in education outcomes",
       subtitle = "Adolescents 10–17 in Bolivia",
       x = NULL, y = NULL, caption = cap_src) +
  edu_theme_en()
save_fig(f1a, "f1a_edu_overall_by_sex_en")

# --- f1b: education outcomes by sex × rural -----------------------------
f1b_dat <- edu_outcomes_by_en(des, "rural") |>
  mutate(zone = ifelse(rural == 1, "Rural", "Urban"))

f1b <- ggplot(f1b_dat, aes(x = zone, y = estimate, fill = sex)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = "Education outcomes by area of residence",
       subtitle = "Adolescents 10–17, by sex and rural/urban",
       x = NULL, y = NULL, fill = NULL, caption = cap_src) +
  edu_theme_en()
save_fig(f1b, "f1b_edu_by_sex_rural_en")

# --- f1c: education outcomes by sex × Indigenous ------------------------
f1c_dat <- edu_outcomes_by_en(des, "indigenous") |>
  mutate(group = ifelse(indigenous == 1, "Indigenous", "Non-Indigenous"))

f1c <- ggplot(f1c_dat, aes(x = group, y = estimate, fill = sex)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = "Education outcomes by Indigenous heritage",
       subtitle = "Adolescents 10–17, by sex",
       x = NULL, y = NULL, fill = NULL, caption = cap_src) +
  edu_theme_en()
save_fig(f1c, "f1c_edu_by_sex_indig_en")

# --- f1d: education outcomes by sex × poverty ---------------------------
f1d_dat <- edu_outcomes_by_en(des, "poor_d") |>
  filter(!is.na(poor_d)) |>
  mutate(group = ifelse(poor_d == 1, "Poor (INE)", "Non-poor"))

f1d <- ggplot(f1d_dat, aes(x = group, y = estimate, fill = sex)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = "Education outcomes by poverty status",
       subtitle = "Adolescents 10–17, by sex (INE)",
       x = NULL, y = NULL, fill = NULL, caption = cap_src) +
  edu_theme_en()
save_fig(f1d, "f1d_edu_by_sex_poverty_en")

# ========================================================================
# Figure 1: Household digital access by sex and age group
# ========================================================================
f1_dat <- bind_rows(
  wt_pct(des, "hh_internet",   by = c("female", "age_group")) |>
    mutate(indicator = "HH internet"),
  wt_pct(des, "hh_computer",   by = c("female", "age_group")) |>
    mutate(indicator = "HH computer"),
  wt_pct(des, "hh_smartphone", by = c("female", "age_group")) |>
    mutate(indicator = "HH smartphone")
) |>
  mutate(sex = ifelse(female == 1, "Girls", "Boys"),
         indicator = factor(indicator,
                            levels = c("HH internet", "HH computer", "HH smartphone")))

f1 <- ggplot(f1_dat, aes(x = age_group, y = estimate, fill = sex)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Boys" = ACCENT_BOY, "Girls" = ACCENT_GIRL)) +
  labs(title    = "Household digital access for adolescents in Bolivia",
       subtitle = "Share of girls and boys in households with each digital asset, by age group",
       x = NULL, y = NULL, fill = NULL, caption = cap_src)
save_fig(f1, "f1_digital_access_sex_age_en")

# ========================================================================
# Figure 2: Territorial digital divide (urban vs rural × sex)
# ========================================================================
f2_dat <- bind_rows(
  wt_pct(des, "hh_internet",   by = c("female", "rural")) |>
    mutate(indicator = "HH internet"),
  wt_pct(des, "hh_computer",   by = c("female", "rural")) |>
    mutate(indicator = "HH computer"),
  wt_pct(des, "hh_smartphone", by = c("female", "rural")) |>
    mutate(indicator = "HH smartphone")
) |>
  mutate(sex  = ifelse(female == 1, "Girls", "Boys"),
         zone = ifelse(rural  == 1, "Rural", "Urban"),
         indicator = factor(indicator,
                            levels = c("HH internet", "HH computer", "HH smartphone")))

f2 <- ggplot(f2_dat, aes(x = zone, y = estimate, fill = sex)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Boys" = ACCENT_BOY, "Girls" = ACCENT_GIRL)) +
  labs(title    = "The largest digital divide is territorial, not just gender",
       subtitle = "Adolescents 10–17, by sex and area",
       x = NULL, y = NULL, fill = NULL, caption = cap_src)
save_fig(f2, "f2_digital_access_area_sex_en")

# ========================================================================
# Figure 3: Indigenous vs non-Indigenous girls — digital and school gaps
# ========================================================================
f3_dat <- des |>
  filter(female == 1) |>
  group_by(indigenous) |>
  summarise(
    `HH smartphone`   = survey_mean(hh_smartphone == 1, na.rm = TRUE, vartype = NULL),
    `HH internet`     = survey_mean(hh_internet   == 1, na.rm = TRUE, vartype = NULL),
    `HH computer`     = survey_mean(hh_computer   == 1, na.rm = TRUE, vartype = NULL),
    `Currently attending` = survey_mean(attending == 1, na.rm = TRUE, vartype = NULL)
  ) |>
  pivot_longer(-indigenous, names_to = "indicator", values_to = "estimate") |>
  mutate(group = ifelse(indigenous == 1, "Indigenous girls", "Non-Indigenous girls"))

f3 <- ggplot(f3_dat, aes(x = estimate, y = indicator, fill = group)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), hjust = -0.15, size = 3.2,
            colour = GREY_DARK) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1.05),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = c("Indigenous girls"     = ACCENT_GIRL,
                               "Non-Indigenous girls" = UNICEF_BLUE)) +
  labs(title    = "Indigenous girls face significant digital access gaps",
       subtitle = "Adolescent girls 10–17 in Bolivia, by Indigenous heritage",
       x = NULL, y = NULL, fill = NULL, caption = cap_src) +
  theme(panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_blank())
save_fig(f3, "f3_indigenous_girls_gaps_en")

# ========================================================================
# Figure 4: School attendance × HH internet
# ========================================================================
f4_dat <- wt_pct(des, "attending", by = c("female", "hh_internet")) |>
  mutate(sex  = ifelse(female == 1, "Girls", "Boys"),
         hh   = ifelse(hh_internet == 1, "HH with internet", "HH without internet"))

f4 <- ggplot(f4_dat, aes(x = hh, y = estimate, fill = sex)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.4,
            colour = GREY_DARK) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Boys" = ACCENT_BOY, "Girls" = ACCENT_GIRL)) +
  labs(title    = "Connected households show higher school attendance",
       subtitle = "Share of adolescents 10–17 currently attending school",
       x = NULL, y = NULL, fill = NULL, caption = cap_src)
save_fig(f4, "f4_attendance_by_internet_en")

# ========================================================================
# Figure 5: Disability × sex — panel format matching f1a-f1d
# Compares boys and girls WITH vs WITHOUT disability across three
# education outcomes. Small sample → interpret with caution.
# ========================================================================
f5_dat <- edu_outcomes_by_en(des, "disab_any") |>
  filter(!is.na(disab_any)) |>
  mutate(group = ifelse(disab_any == 1, "With disability", "Without disability"),
         group = factor(group, levels = c("Without disability", "With disability")))

f5 <- ggplot(f5_dat, aes(x = group, y = estimate, fill = sex)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = "Education outcomes by disability status",
       subtitle = "Adolescents 10–17, by sex · Washington Group severity ≥ 3",
       x = NULL, y = NULL, fill = NULL,
       caption = paste0(cap_src,
                        " Small sample (~0.4%) — interpret with caution.")) +
  edu_theme_en()
save_fig(f5, "f5_disability_gap_en")

# ========================================================================
# Figure 6: Departmental variation — HH internet (girls)
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
  labs(title    = "Territorial variation — input for prioritisation and sequencing",
       subtitle = "HH internet, adolescent girls 10–17, by department",
       x = NULL, y = NULL, caption = cap_src) +
  theme(panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_blank())
save_fig(f6, "f6_internet_by_department_en")

# ========================================================================
# Figure 7: Target population (funnel) — Aula Conectada
# ========================================================================
funnel <- read_csv(here::here("output", "tables", "t8_target_funnel.csv"),
                   show_col_types = FALSE) |>
  filter(segment != "All adolescents 10–17 (weighted)") |>
  mutate(
    segment_en = recode(segment,
      "All girls 10–17 (weighted)"                                              = "All girls 10–17",
      "Girls 10–17 — rural"                                                     = "Rural girls 10–17",
      "Girls 10–17 — Indigenous"                                                = "Indigenous girls 10–17",
      "Girls 10–17 — poor (INE)"                                                = "Girls in poverty (INE) 10–17",
      "Girls 10–17 — disability (WG 3+)"                                        = "Girls with disability (WG 3+) 10–17",
      "Girls 10–17 — no HH internet"                                            = "Girls without HH internet 10–17",
      "Girls 10–17 — priority (rural OR Indigenous OR poor OR disability)"      = "Priority girls (rural ∨ Indigenous ∨ poor ∨ disability)"
    ),
    segment_en = factor(segment_en, levels = rev(segment_en))
  )

f7 <- ggplot(funnel, aes(x = n_pop, y = segment_en)) +
  geom_col(fill = UNICEF_DARK, width = 0.7) +
  geom_text(aes(label = scales::comma(round(n_pop))),
            hjust = -0.1, size = 3.4, colour = GREY_DARK) +
  scale_x_continuous(labels = scales::comma_format(),
                     expand = expansion(mult = c(0, 0.18))) +
  labs(title    = "Target population and priority groups — Aula Conectada",
       subtitle = "Weighted adolescent girls 10–17 in Bolivia, by segment",
       x = NULL, y = NULL, caption = cap_src) +
  theme(panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_blank())
save_fig(f7, "f7_target_funnel_en", width = 10, height = 6)

message("Stage 3b complete. → English-language figures saved as f*_en.png")
