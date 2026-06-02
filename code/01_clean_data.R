# 01_clean_data.R — Stage 1: load, merge, clean adolescent sample
# ------------------------------------------------------------------------
# Inputs:  data/BD_EH2024/*.sav   (Persona, Vivienda, Equipamiento)
# Outputs: output/analysis_ready.rds   (adolescents 10–19)
#          output/analysis_ready_full.rds  (all persons, for HH-level stats)
#          output/data_dictionary.csv
# ------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(haven); library(labelled); library(dplyr); library(tidyr)
  library(purrr); library(stringr); library(readr); library(tibble)
  library(here); library(fs); library(glue); library(Hmisc)
})

# Project root — `here` resolves via the .here sentinel in `analysis/`
source(here::here("R", "utils.R"))
source(here::here("R", "variable_mapping.R"))

DATA_DIR <- here::here("data", "BD_EH2024")
OUT      <- ensure_dirs()

check_mapping()

# 1. Load raw files --------------------------------------------------------
message("Reading Persona…")
persona      <- read_sav(file.path(DATA_DIR, "EH2024_Persona.sav"))
message("Reading Vivienda…")
vivienda     <- read_sav(file.path(DATA_DIR, "EH2024_Vivienda.sav"))
message("Reading Equipamiento…")
equipamiento <- read_sav(file.path(DATA_DIR, "EH2024_Equipamiento.sav"))

message(glue("Persona:      {nrow(persona)} rows × {ncol(persona)} cols"))
message(glue("Vivienda:     {nrow(vivienda)} rows × {ncol(vivienda)} cols"))
message(glue("Equipamiento: {nrow(equipamiento)} rows × {ncol(equipamiento)} cols"))

# 2. Select & rename per-file ---------------------------------------------
p <- pick_vars(persona,  DESIGN, PERSONA) |> mutate(across(where(is.labelled), to_plain))
v <- pick_vars(vivienda, list(folio = "folio"), VIVIENDA) |>
  mutate(across(where(is.labelled), to_plain))

# 3. Equipamiento is LONG (item × HH). Pivot to wide. ---------------------
# The dictionary names vary by year; auto-detect the item-code column and
# the tenencia (has it) column.
eq_raw <- equipamiento |> mutate(across(where(is.labelled), to_plain))

# Survey-design / housekeeping columns we never want as item or tenencia
EQ_EXCLUDE <- c("folio", "nro", "upm", "estrato", "factor",
                "area", "depto", "depart", "municipio", "id")

# Item-code candidate: numeric col with several distinct values 1–50
candidate_code_cols <- names(eq_raw)[map_lgl(eq_raw, function(x) {
  is.numeric(x) && all(x %in% c(NA, 1:50)) && length(unique(na.omit(x))) >= 10
})]
# Yes/no tenencia candidate: numeric col with exactly two values in {1,2}
candidate_yes_cols <- names(eq_raw)[map_lgl(eq_raw, function(x) {
  is.numeric(x) && all(na.omit(x) %in% c(1, 2)) && length(unique(na.omit(x))) == 2
})]
candidate_code_cols <- setdiff(candidate_code_cols, EQ_EXCLUDE)
candidate_yes_cols  <- setdiff(candidate_yes_cols,  EQ_EXCLUDE)

# Among the code candidates, score by share of values that fall in our
# target item codes — the true item-code column scores 1.0, while a
# "cantidad" column (which mostly has values 1–5) scores much lower.
score_code <- function(col) {
  vals <- eq_raw[[col]]
  mean(vals %in% EQUIP_ITEM_CODES, na.rm = TRUE)
}
if (length(candidate_code_cols) > 1) {
  scores <- vapply(candidate_code_cols, score_code, numeric(1))
  candidate_code_cols <- candidate_code_cols[order(-scores)]
  message("Code col scores: ",
          paste(sprintf("%s=%.2f", names(scores), scores), collapse = ", "))
}

# Promote commonly-used names to the front of each candidate list.
prefer_names <- function(x, prefer) c(intersect(prefer, x),
                                       setdiff(x, prefer))
candidate_code_cols <- prefer_names(candidate_code_cols,
                                    c("item", "codigo", "cod_item"))
candidate_yes_cols  <- prefer_names(candidate_yes_cols,
                                    c("s08b_1", "tiene", "tenencia"))

message("Equipamiento candidate item-code cols: ",
        paste(candidate_code_cols, collapse = ", "))
message("Equipamiento candidate yes/no cols:    ",
        paste(candidate_yes_cols, collapse = ", "))

# Pick the first surviving candidate, with hard fallbacks
item_col <- if (length(candidate_code_cols)) candidate_code_cols[1] else "item"
yes_col  <- if (length(candidate_yes_cols))  candidate_yes_cols[1]  else "s08b_1"
stopifnot(item_col %in% names(eq_raw), yes_col %in% names(eq_raw))
message(glue("Using item code col = '{item_col}', yes/no col = '{yes_col}'"))

eq_long <- eq_raw |>
  filter(.data[[item_col]] %in% EQUIP_ITEM_CODES) |>
  transmute(
    folio = folio,
    item  = factor(.data[[item_col]], levels = EQUIP_ITEM_CODES,
                   labels = names(EQUIP_ITEM_CODES)),
    has   = as.integer(.data[[yes_col]] == 1)
  )

# Sanity check: (folio, item) should be unique. If not, columns were
# probably mis-detected — abort with a clear message.
dup_n <- eq_long |> count(folio, item) |> filter(n > 1) |> nrow()
if (dup_n > 0) {
  stop(glue(
    "Equipamiento pivot: {dup_n} duplicate (folio, item) pairs.\n",
    "  item_col = '{item_col}' and yes_col = '{yes_col}' are probably wrong.\n",
    "  Inspect equipamiento with `glimpse()` and set them by hand at the\n",
    "  top of 01_clean_data.R."
  ))
}

eq_wide <- eq_long |>
  pivot_wider(names_from = item, values_from = has, values_fill = 0L,
              names_prefix = "hh_")

# 4. Merge: Persona ← Vivienda ← Equipamiento -----------------------------
hh <- v |> left_join(eq_wide, by = "folio")

dat <- p |> left_join(hh, by = "folio", suffix = c("", "_hh"))
message(glue("Merged: {nrow(dat)} persons in {n_distinct(dat$folio)} households"))

# 5. Construct analysis variables -----------------------------------------
# Pre-flag whether condact was extracted from this EH wave; NEET requires it
HAS_CONDACT <- "condact" %in% names(dat)
if (!HAS_CONDACT) {
  message("⚠ condact (labour status) not found in persona file — NEET will be NA")
}

dat <- dat |>
  mutate(
    # --- Identity ---
    female = case_when(sex_raw == 2 ~ 1L, sex_raw == 1 ~ 0L, TRUE ~ NA_integer_),
    age    = as.integer(age),

    # --- Adolescent sample 10–19 (extended from 10–19 to capture late
    #     adolescence, where dropout and NEET cliffs are concentrated) ---
    adolescent = age >= 10 & age <= 19,

    # Two age-band variables:
    #  - age_group        : narrower 4-yr bands (used in older figures)
    #  - age_group_adol   : early (10-14) vs late (15-19) adolescence
    age_group  = case_when(
      age >= 10 & age <= 13 ~ "10-13",
      age >= 14 & age <= 17 ~ "14-17",
      age >= 18 & age <= 19 ~ "18-19",
      TRUE                  ~ NA_character_
    ),
    age_group_adol = case_when(
      age >= 10 & age <= 14 ~ "Adolescencia temprana (10-14)",
      age >= 15 & age <= 19 ~ "Adolescencia tardía (15-19)",
      TRUE                  ~ NA_character_
    ),
    # School-cycle proxy (primary vs secondary by typical Bolivian system age)
    cycle_proxy = case_when(
      age >= 10 & age <= 11 ~ "Primaria (10-11)",
      age >= 12 & age <= 17 ~ "Secundaria (12-17)",
      age >= 18 & age <= 19 ~ "Post-secundaria/joven (18-19)",
      TRUE                  ~ NA_character_
    ),

    # --- Geography ---
    rural      = case_when(area == 2 ~ 1L, area == 1 ~ 0L, TRUE ~ NA_integer_),
    department = factor(depto, levels = names(DEPTO_LABELS), labels = DEPTO_LABELS),

    # --- Indigenous proxy ---
    indig_first  = lang_first %in% INDIG_LANG_CODES,
    indig_spoken = (lang_spoken1 %in% INDIG_LANG_CODES) |
                   (lang_spoken2 %in% INDIG_LANG_CODES) |
                   (lang_spoken3 %in% INDIG_LANG_CODES),
    indigenous = case_when(
      indig_first  ~ 1L,
      indig_spoken ~ 1L,                # fallback: speaks Indigenous lang
      !is.na(lang_first) | !is.na(lang_spoken1) ~ 0L,
      TRUE         ~ NA_integer_
    ),

    # --- Disability (WG short set: severity 3 or 4 on ANY domain) ---
    disab_any = pmax(
      coalesce(as.integer(disab_seeing  >= 3), 0L),
      coalesce(as.integer(disab_hearing >= 3), 0L),
      coalesce(as.integer(disab_walking >= 3), 0L),
      coalesce(as.integer(disab_cogn    >= 3), 0L)
    ),
    disab_any = if_else(
      is.na(disab_seeing) & is.na(disab_hearing) &
        is.na(disab_walking) & is.na(disab_cogn),
      NA_integer_, disab_any
    ),

    # --- Enrolment (INE-derived cmasi) ---
    # IMPORTANT: 'enrolled' is FORMAL REGISTRATION for the school year
    # — it does NOT mean the adolescent is actually attending classes.
    # In Bolivia formal enrolment is high; the harder tests of inclusion
    # are 'attending', 'dropout' (matriculated-not-attending) and
    # 'grade_delay'. Lead with these in the descriptive narrative.
    enrolled = case_when(
      cmasi %in% c(2, 3) ~ 1L,   # matriculado (asiste OR no asiste)
      cmasi == 1         ~ 0L,
      TRUE               ~ NA_integer_
    ),
    attending = case_when(            # currently attending
      cmasi == 2 ~ 1L,
      cmasi %in% c(1, 3) ~ 0L,
      TRUE       ~ NA_integer_
    ),
    # De facto dropout signal: matriculated but not attending.
    # This is the strongest 'left school during the year' indicator
    # available cross-sectionally in EH 2024.
    dropout         = as.integer(cmasi == 3),
    matric_no_asiste = as.integer(cmasi == 3),  # legacy name; same content

    # Age-for-grade delay: years of schooling < age - 6  (rough proxy)
    grade_delay = case_when(
      age >= 10 & !is.na(aestudio) ~ as.integer(aestudio < (age - 6)),
      TRUE                          ~ NA_integer_
    ),

    # --- NEET (Not in Education, Employment, or Training) ---
    # Built from cmasi (education) and condact (labour status).
    # condact codes (EH 2024): 1=ocupado, 2=cesante, 3=aspirante, 4=inactivo
    # NEET = (not attending school) AND (not employed = not ocupado)
    # 'cesante' (laid off) and 'aspirante' (seeking work) are NOT employed,
    # so they ARE NEET when out of school.
    # If condact column is missing from this EH wave, NEET will be NA.
    in_school  = as.integer(cmasi == 2),     # actively attending
    employed   = if (HAS_CONDACT) as.integer(condact == 1) else NA_integer_,
    neet = case_when(
      is.na(in_school) | is.na(employed) ~ NA_integer_,
      in_school == 0 & employed == 0     ~ 1L,
      TRUE                                ~ 0L
    ),

    # School type
    public_school = case_when(
      school_type == 1 ~ 1L, school_type == 2 ~ 0L, TRUE ~ NA_integer_
    ),
    # Bono Juancito Pinto (1=yes, 2=no → 1/0)
    bono_juancito = case_when(
      bono_juancito == 1 ~ 1L, bono_juancito == 2 ~ 0L, TRUE ~ NA_integer_
    ),
    # Receives school meal (1=yes, 2=no → 1/0)
    meal_school = case_when(
      meal_school == 1 ~ 1L, meal_school == 2 ~ 0L, TRUE ~ NA_integer_
    ),

    # --- Poverty (INE-derived) ---
    poor_d        = as.integer(poor == 1),
    poor_ext_d    = as.integer(poor_extreme == 1),

    # --- HH internet (from Vivienda) ---
    hh_internet = case_when(
      hh_internet == 1 ~ 1L, hh_internet == 2 ~ 0L, TRUE ~ NA_integer_
    ),

    # Equipamiento devices (already 0/1 from pivot)
    hh_computer   = coalesce(hh_computer, 0L),
    hh_tablet     = coalesce(hh_tablet, 0L),
    hh_smartphone = coalesce(hh_smartphone, 0L),
    hh_smart_tv   = coalesce(hh_smart_tv, 0L),
    hh_any_device = as.integer(
      hh_computer == 1 | hh_tablet == 1 | hh_smartphone == 1
    ),

    # Digital index: count of (internet, computer, tablet, smartphone)
    digital_index = coalesce(hh_internet, 0L) + hh_computer +
                    hh_tablet + hh_smartphone
  ) |>
  # Cleanup: drop interim helper cols
  select(-indig_first, -indig_spoken)

# 6. Income quintiles (HH-level, weighted) --------------------------------
hh_inc <- dat |>
  distinct(folio, yhogpc, weight) |>
  filter(!is.na(yhogpc))
qs <- wquantile(hh_inc$yhogpc, hh_inc$weight,
                probs = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
dat <- dat |>
  mutate(
    inc_quintile = cut(yhogpc, breaks = qs, include.lowest = TRUE,
                       labels = paste0("Q", 1:5))
  )

# 7. Priority girl flags --------------------------------------------------
dat <- dat |>
  mutate(
    girl                 = as.integer(female == 1 & adolescent),
    girl_rural           = as.integer(girl == 1 & rural == 1),
    girl_indigenous      = as.integer(girl == 1 & indigenous == 1),
    girl_poor            = as.integer(girl == 1 & poor_d == 1),
    girl_disab           = as.integer(girl == 1 & disab_any == 1),
    girl_no_hh_internet  = as.integer(girl == 1 & hh_internet == 0),
    girl_priority        = as.integer(
      girl == 1 & (rural == 1 | indigenous == 1 |
                   poor_d == 1 | disab_any == 1)
    )
  )

# 7b. Parenting / agency indicators ---------------------------------------
# Household composition: father / mother present in the HH roster.
# In EH 2024, father_id / mother_id is 0 if parent not in HH, >0 otherwise.
# Adolescent pregnancy / motherhood: girls only (module is not asked of males).
dat <- dat |>
  mutate(
    has_father_hh   = as.integer(!is.na(father_id) & father_id > 0),
    has_mother_hh   = as.integer(!is.na(mother_id) & mother_id > 0),
    parents_in_hh   = case_when(
      has_father_hh == 1 & has_mother_hh == 1 ~ "Ambos padres",
      has_father_hh == 1 & has_mother_hh == 0 ~ "Sólo padre",
      has_father_hh == 0 & has_mother_hh == 1 ~ "Sólo madre",
      has_father_hh == 0 & has_mother_hh == 0 ~ "Ninguno",
      TRUE                                     ~ NA_character_
    ),
    # Adolescent pregnancy: ever pregnant (currently or in the past).
    # Defined only for females; NA for males.
    ever_pregnant   = case_when(
      female == 1 & ever_preg %in% c(1, 2) ~ 1L,
      female == 1 & ever_preg == 3         ~ 0L,
      TRUE                                  ~ NA_integer_
    ),
    # Restrict to the analytical 15-19 girl subsample
    ever_pregnant_1519 = ifelse(female == 1 & age >= 15 & age <= 19,
                                 ever_pregnant, NA_integer_),
    is_mother_1519     = ifelse(female == 1 & age >= 15 & age <= 19 &
                                  !is.na(num_births),
                                as.integer(num_births >= 1), NA_integer_)
  )

# 8. Save full + adolescent-only datasets ---------------------------------
saveRDS(dat, file.path(OUT, "analysis_ready_full.rds"))

ado <- dat |> filter(adolescent)
message(glue("Adolescents 10–19: {nrow(ado)} ({n_distinct(ado$folio)} HHs)"))
saveRDS(ado, file.path(OUT, "analysis_ready.rds"))

# 9. Quick QA summary -----------------------------------------------------
qa <- ado |>
  summarise(
    n            = n(),
    pct_female   = mean(female == 1, na.rm = TRUE),
    pct_rural    = mean(rural  == 1, na.rm = TRUE),
    pct_indig    = mean(indigenous == 1, na.rm = TRUE),
    pct_poor     = mean(poor_d == 1, na.rm = TRUE),
    pct_disab    = mean(disab_any == 1, na.rm = TRUE),
    pct_attend   = mean(attending == 1, na.rm = TRUE),
    pct_hh_int   = mean(hh_internet == 1, na.rm = TRUE),
    pct_hh_comp  = mean(hh_computer == 1, na.rm = TRUE),
    pct_smartph  = mean(hh_smartphone == 1, na.rm = TRUE)
  ) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value")
write_table(qa, "qa_unweighted_summary")
print(qa)

# 10. Save analysis dictionary --------------------------------------------
analysis_dict <- tibble(
  var = names(ado),
  type = vapply(ado, function(x) class(x)[1], character(1)),
  n_miss = vapply(ado, function(x) sum(is.na(x)), integer(1)),
  example = vapply(ado, function(x) paste(head(na.omit(unique(x)), 3),
                                          collapse = " | "), character(1))
)
write_table(analysis_dict, "analysis_dictionary")

message("Stage 1 complete. → output/analysis_ready.rds")
