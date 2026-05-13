# 03_figures.R — Stage 3: figuras en español para el caso de inversión ----
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

cap_src <- "Fuente: INE Bolivia – Encuesta de Hogares 2024. Estimaciones ponderadas, adolescentes 10–17."

# ========================================================================
# Figura 0: Teoría de Cambio (diagrama horizontal en 6 columnas)
# ========================================================================
toc_cols <- tribble(
  ~stage,    ~label,                ~content,
  1, "Insumos",            "Docentes\nConectividad\nDispositivos\nContenidos digitales\nMateriales inclusivos\nMódulos STEM",
  2, "Actividades",        "Formación docente\nPedagogía digital\nAprendizaje colaborativo\nContenido en currículo\nParticipación de niñas\nApoyo familiar",
  3, "Productos",          "Docentes formados\nAulas equipadas\nContenido en uso\nNiñas en actividades digitales\nGrupos prioritarios identificados",
  4, "Resultados\ncorto plazo", "Práctica docente\nParticipación equitativa\nConfianza digital\nHabilidades digitales\nMayor asistencia",
  5, "Resultados\nmediano plazo","Mejores aprendizajes\nMenor deserción\nMejor progresión\nInterés en STEM\nMenor exclusión digital",
  6, "Impacto\nEsperado",  "Reducción de brechas\nde género e\ninterseccionales en\naprendizaje digital y\ntrayectorias STEM"
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
  labs(title    = "Teoría de Cambio — Aula Conectada",
       subtitle = "De insumos a impacto: la tecnología como medio, la pedagogía como motor",
       x = NULL, y = NULL) +
  theme_void(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", size = 16, colour = UNICEF_DARK,
                                     margin = margin(b = 4)),
        plot.subtitle = element_text(size = 12, colour = GREY_DARK,
                                     margin = margin(b = 14)),
        plot.caption  = element_text(size = 9, colour = "grey40", hjust = 0,
                                     margin = margin(t = 10)),
        plot.margin   = margin(18, 18, 14, 18))
save_fig(f0, "f0_theory_of_change", width = 12, height = 5.5)

# ========================================================================
# NUEVAS Figuras 1a-1d: Resultados educativos por sexo + cortes intersec.
# Estas son las figuras "descriptivas-primero" para el caso de inversión.
# Outcomes: matriculación, asistencia, rezago escolar.
# ========================================================================
EDU_LEVELS <- c("Matriculada/o", "Asiste a la Escuela",
                "Rezago Escolar (Edad/Grado)")
fill_sx    <- c("Niños" = ACCENT_BOY, "Niñas" = ACCENT_GIRL)

# Helper para construir el dataset educativo por (sexo × grupo)
edu_outcomes_by <- function(design, by_var = NULL) {
  fr_filter <- if (!is.null(by_var)) c("female", by_var) else "female"
  bind_rows(
    wt_pct(design, "enrolled",     by = fr_filter) |>
      mutate(indicator = "Matriculada/o"),
    wt_pct(design, "attending",    by = fr_filter) |>
      mutate(indicator = "Asiste a la Escuela"),
    wt_pct(design, "grade_delay",  by = fr_filter) |>
      mutate(indicator = "Rezago Escolar (Edad/Grado)")
  ) |>
    mutate(
      sexo      = ifelse(female == 1, "Niñas", "Niños"),
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
  facet_wrap(~indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx, name = NULL) +
  labs(title    = "Brechas de género en resultados educativos",
       subtitle = "Adolescentes 10–17 en Bolivia",
       x = NULL, y = NULL, caption = cap_src) +
  edu_theme()
save_fig(f1a, "f1a_edu_overall_by_sex")

# --- f1b: outcomes educativos por sexo × rural --------------------------
f1b_dat <- edu_outcomes_by(des, "rural") |>
  mutate(zona = ifelse(rural == 1, "Rural", "Urbano"))

f1b <- ggplot(f1b_dat, aes(x = zona, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = "Resultados educativos por zona de residencia",
       subtitle = "Adolescentes 10–17, por sexo y rural/urbano",
       x = NULL, y = NULL, fill = NULL, caption = cap_src) +
  edu_theme()
save_fig(f1b, "f1b_edu_by_sex_rural")

# --- f1c: outcomes educativos por sexo × indígena -----------------------
f1c_dat <- edu_outcomes_by(des, "indigenous") |>
  mutate(grupo = ifelse(indigenous == 1, "Indígena", "No indígena"))

f1c <- ggplot(f1c_dat, aes(x = grupo, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = "Resultados educativos por pertenencia indígena",
       subtitle = "Adolescentes 10–17, por sexo",
       x = NULL, y = NULL, fill = NULL, caption = cap_src) +
  edu_theme()
save_fig(f1c, "f1c_edu_by_sex_indig")

# --- f1d: outcomes educativos por sexo × pobreza ------------------------
f1d_dat <- edu_outcomes_by(des, "poor_d") |>
  filter(!is.na(poor_d)) |>
  mutate(grupo = ifelse(poor_d == 1, "Pobre (INE)", "No pobre"))

f1d <- ggplot(f1d_dat, aes(x = grupo, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = "Resultados educativos por situación de pobreza",
       subtitle = "Adolescentes 10–17, por sexo (INE)",
       x = NULL, y = NULL, fill = NULL, caption = cap_src) +
  edu_theme()
save_fig(f1d, "f1d_edu_by_sex_poverty")

# ========================================================================
# Figura 1: Acceso digital del hogar por sexo y grupo etario
# ========================================================================
f1_dat <- bind_rows(
  wt_pct(des, "hh_internet",   by = c("female", "age_group")) |>
    mutate(indicator = "Internet en el hogar"),
  wt_pct(des, "hh_computer",   by = c("female", "age_group")) |>
    mutate(indicator = "Computadora en el hogar"),
  wt_pct(des, "hh_smartphone", by = c("female", "age_group")) |>
    mutate(indicator = "Smartphone en el hogar")
) |>
  mutate(
    sexo = ifelse(female == 1, "Niñas", "Niños"),
    indicator = factor(indicator,
                       levels = c("Internet en el hogar",
                                  "Computadora en el hogar",
                                  "Smartphone en el hogar"))
  )

f1 <- ggplot(f1_dat, aes(x = age_group, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Niños" = ACCENT_BOY, "Niñas" = ACCENT_GIRL)) +
  labs(title    = "Acceso digital del hogar para adolescentes en Bolivia",
       subtitle = "Proporción de niñas y niños en hogares con cada activo digital, por grupo etario",
       x = NULL, y = NULL, fill = NULL, caption = cap_src)
save_fig(f1, "f1_digital_access_sex_age")

# ========================================================================
# Figura 2: Brecha digital territorial (urbano vs rural × sexo)
# ========================================================================
f2_dat <- bind_rows(
  wt_pct(des, "hh_internet",   by = c("female", "rural")) |>
    mutate(indicator = "Internet en el hogar"),
  wt_pct(des, "hh_computer",   by = c("female", "rural")) |>
    mutate(indicator = "Computadora en el hogar"),
  wt_pct(des, "hh_smartphone", by = c("female", "rural")) |>
    mutate(indicator = "Smartphone en el hogar")
) |>
  mutate(
    sexo = ifelse(female == 1, "Niñas", "Niños"),
    zona = ifelse(rural  == 1, "Rural", "Urbano"),
    indicator = factor(indicator,
                       levels = c("Internet en el hogar",
                                  "Computadora en el hogar",
                                  "Smartphone en el hogar"))
  )

f2 <- ggplot(f2_dat, aes(x = zona, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Niños" = ACCENT_BOY, "Niñas" = ACCENT_GIRL)) +
  labs(title    = "La mayor brecha digital es territorial, no solo de género",
       subtitle = "Adolescentes 10–17, por sexo y zona de residencia",
       x = NULL, y = NULL, fill = NULL, caption = cap_src)
save_fig(f2, "f2_digital_access_area_sex")

# ========================================================================
# Figura 3: Niñas indígenas vs no indígenas — brechas digitales y escolares
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
  mutate(group = ifelse(indigenous == 1, "Niñas indígenas", "Niñas no indígenas"))

f3 <- ggplot(f3_dat, aes(x = estimate, y = indicator, fill = group)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), hjust = -0.15, size = 3.2,
            colour = GREY_DARK) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1.05),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = c("Niñas indígenas"     = ACCENT_GIRL,
                               "Niñas no indígenas"  = UNICEF_BLUE)) +
  labs(title    = "Niñas indígenas enfrentan brechas importantes de acceso digital",
       subtitle = "Niñas adolescentes 10–17 en Bolivia, por pertenencia indígena",
       x = NULL, y = NULL, fill = NULL, caption = cap_src) +
  theme(panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_blank())
save_fig(f3, "f3_indigenous_girls_gaps")

# ========================================================================
# Figura 4: Asistencia escolar × internet en el hogar
# ========================================================================
f4_dat <- wt_pct(des, "attending", by = c("female", "hh_internet")) |>
  mutate(
    sexo  = ifelse(female == 1, "Niñas", "Niños"),
    hogar = ifelse(hh_internet == 1, "Hogar con internet", "Hogar sin internet")
  )

f4 <- ggplot(f4_dat, aes(x = hogar, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.4,
            colour = GREY_DARK) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Niños" = ACCENT_BOY, "Niñas" = ACCENT_GIRL)) +
  labs(title    = "Hogares conectados muestran mayor asistencia escolar",
       subtitle = "Adolescentes 10–17 que asisten actualmente a la escuela",
       x = NULL, y = NULL, fill = NULL, caption = cap_src)
save_fig(f4, "f4_attendance_by_internet")

# ========================================================================
# Figura 5: Discapacidad × sexo — mismo formato que f1a–f1d
# Compara niñas y niños CON vs SIN discapacidad sobre los tres outcomes
# educativos. Tamaño muestral pequeño → interpretar con cautela.
# ========================================================================
f5_dat <- edu_outcomes_by(des, "disab_any") |>
  filter(!is.na(disab_any)) |>
  mutate(grupo = ifelse(disab_any == 1,
                        "Con discapacidad",
                        "Sin discapacidad"),
         grupo = factor(grupo, levels = c("Sin discapacidad",
                                          "Con discapacidad")))

f5 <- ggplot(f5_dat, aes(x = grupo, y = estimate, fill = sexo)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            colour = GREY_DARK) +
  facet_wrap(~indicator) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
  scale_fill_manual(values = fill_sx) +
  labs(title    = "Resultados educativos por situación de discapacidad",
       subtitle = "Adolescentes 10–17, por sexo · Washington Group severidad ≥ 3",
       x = NULL, y = NULL, fill = NULL,
       caption = paste0(cap_src,
                        " Tamaño muestral pequeño (~0.4%) — interpretar con cautela.")) +
  edu_theme()
save_fig(f5, "f5_disability_gap")

# ========================================================================
# Figura 6: Variación departamental — internet en el hogar (niñas)
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
  labs(title    = "Variación territorial para orientar priorización y secuenciación",
       subtitle = "Internet en el hogar, niñas adolescentes 10–17, por departamento",
       x = NULL, y = NULL, caption = cap_src) +
  theme(panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_blank())
save_fig(f6, "f6_internet_by_department")

# ========================================================================
# Figura 7: Población objetivo (funnel) — Aula Conectada
# ========================================================================
funnel <- read_csv(here::here("output", "tables", "t8_target_funnel.csv"),
                   show_col_types = FALSE) |>
  filter(segment != "All adolescents 10–17 (weighted)") |>
  mutate(
    # Traducir etiquetas al español para la figura
    segment_es = recode(segment,
      "All girls 10–17 (weighted)"                                              = "Todas las niñas 10–17",
      "Girls 10–17 — rural"                                                     = "Niñas rurales 10–17",
      "Girls 10–17 — Indigenous"                                                = "Niñas indígenas 10–17",
      "Girls 10–17 — poor (INE)"                                                = "Niñas en pobreza 10–17",
      "Girls 10–17 — disability (WG 3+)"                                        = "Niñas con discapacidad 10–17",
      "Girls 10–17 — no HH internet"                                            = "Niñas sin internet en hogar 10–17",
      "Girls 10–17 — priority (rural OR Indigenous OR poor OR disability)"      = "Niñas prioritarias (rural ∨ indígena ∨ pobre ∨ discapacidad)"
    ),
    segment_es = factor(segment_es, levels = rev(segment_es))
  )

f7 <- ggplot(funnel, aes(x = n_pop, y = segment_es)) +
  geom_col(fill = UNICEF_DARK, width = 0.7) +
  geom_text(aes(label = scales::comma(round(n_pop))),
            hjust = -0.1, size = 3.4, colour = GREY_DARK) +
  scale_x_continuous(labels = scales::comma_format(),
                     expand = expansion(mult = c(0, 0.18))) +
  labs(title    = "Población objetivo y grupos prioritarios — Aula Conectada",
       subtitle = "Niñas adolescentes 10–17 ponderadas en Bolivia, por segmento",
       x = NULL, y = NULL, caption = cap_src) +
  theme(panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_blank())
save_fig(f7, "f7_target_funnel", width = 10, height = 6)

message("Stage 3 complete. → output/figures/f0_…png + f1_…png through f7_…png")
