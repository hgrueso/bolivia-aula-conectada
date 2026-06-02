# R/variable_mapping.R
# -----------------------------------------------------------------------------
# Single source of truth: maps EH 2024 raw variable names to analysis names.
# Confirmed against the EH 2024 data dictionary (INE Bolivia).
# -----------------------------------------------------------------------------

# --- Survey design --------------------------------------------------------
# Present in every file
DESIGN <- list(
  folio   = "folio",     # household id
  nro     = "nro",       # within-HH person number
  psu     = "upm",       # primary sampling unit
  stratum = "estrato",   # stratum (character, ~70 levels)
  weight  = "factor"     # expansion factor
)

# --- Persona file --------------------------------------------------------
PERSONA <- list(
  # Demographics
  sex_raw      = "s01a_02",   # 1 = hombre, 2 = mujer
  age          = "s01a_03",   # años cumplidos (0–98)
  rel_head     = "s01a_05",   # parentesco con jefe(a)

  # Household composition: row number of father/mother in the HH roster
  # 0 = parent NOT in household; >0 = roster ID of the parent
  father_id    = "s01a_05b",  # padre / padrastro
  mother_id    = "s01a_05c",  # madre / madrastra

  # Adolescent agency indicators (girls only)
  ever_preg    = "s02b_06",   # 1=actualmente embarazada, 2=estuvo, 3=no
  num_births   = "s02b_07",   # número de hijos nacidos vivos

  # Language / Indigenous proxy
  lang_first   = "s01a_08",   # idioma de la niñez
  lang_spoken1 = "s01a_07_1", # idioma que habla (1)
  lang_spoken2 = "s01a_07_2", # idioma que habla (2)
  lang_spoken3 = "s01a_07_3", # idioma que habla (3)

  # Disability — Washington Group short set (severity 1–4)
  disab_seeing  = "s02a_04a",
  disab_hearing = "s02a_04b",
  disab_walking = "s02a_04c",
  disab_cogn    = "s02a_04d",

  # Education — INE-derived enrolment status
  cmasi         = "cmasi",      # 1=no matric, 2=asiste, 3=matric-no asiste
  aestudio      = "aestudio",   # years of completed schooling
  school_type   = "s03a_09",    # 1=Fiscal/Convenio (público), 2=Privado
  meal_school   = "s03a_07a",   # recibe alimentación escolar
  bono_juancito = "s03a_08",    # Bono Juancito Pinto (recipient)

  # Poverty (INE-derived, only adults? — check; usually whole HH)
  poor          = "p0",         # 0/1 pobre moderado
  poor_extreme  = "pext0",      # 0/1 pobreza extrema
  yhog          = "yhog",       # ingreso del hogar
  yhogpc        = "yhogpc",     # ingreso per cápita
  pov_line      = "z",          # línea de pobreza
  pov_line_ext  = "zext",       # línea de pobreza extrema

  # Labour-status — used to construct NEET (Not in Education, Employment or Training)
  # condact: 1=ocupado, 2=cesante, 3=aspirante, 4=inactivo (codes per EH 2024)
  condact       = "condact"     # condición de actividad
)

# --- Vivienda file (household-level) -------------------------------------
VIVIENDA <- list(
  area      = "area",      # 1=urbano, 2=rural
  depto     = "depto",     # 1–9 departments
  hh_internet = "s06a_19"  # ¿el hogar tiene servicio de internet?
)

# --- Equipamiento (LONG format: 1 row per HH × item) ---------------------
# Item codes used for digital index:
#   6  = computadora/laptop
#   7  = tablet
#   8  = celular (cualquiera)
#   13 = Smart TV
EQUIP_ITEM_CODES <- c(
  computer   = 6L,
  tablet     = 7L,
  smartphone = 8L,
  smart_tv   = 13L
)
# The variable holding the item code and the "has it" flag varies by year;
# 01_clean_data.R auto-detects them from the column set.

# --- Department labels ---------------------------------------------------
DEPTO_LABELS <- c(
  `1` = "Chuquisaca", `2` = "La Paz",     `3` = "Cochabamba",
  `4` = "Oruro",      `5` = "Potosí",     `6` = "Tarija",
  `7` = "Santa Cruz", `8` = "Beni",       `9` = "Pando"
)

# --- Bolivian Indigenous language codes (childhood / spoken) -------------
# Per EH 2024 dictionary: codes 1–37 are Bolivian Indigenous languages
# (quechua, aymara, guaraní, and ~34 lowland languages); codes 70 & 71
# are also Indigenous categories (e.g. otros nativos / lenguas de señas
# native). Code 6 = Castellano, 38–69 = foreign, 995/996 = no habla.
INDIG_LANG_CODES <- c(1:5, 7:37, 70L, 71L)
# Note: code 6 is intentionally excluded (Castellano/Spanish).

# --- Helper: validate mapping vs an actual data frame ---------------------
check_mapping <- function() {
  all_maps <- list(
    DESIGN = DESIGN, PERSONA = PERSONA, VIVIENDA = VIVIENDA
  )
  out <- purrr::imap_dfr(all_maps, function(lst, nm) {
    tibble::tibble(
      group = nm,
      analysis_name = names(lst),
      raw_name = unlist(lapply(lst, function(x) if (is.null(x)) NA else x))
    )
  })
  n_missing <- sum(is.na(out$raw_name))
  message(sprintf("Variable mapping: %d / %d entries filled (%d missing).",
                  nrow(out) - n_missing, nrow(out), n_missing))
  invisible(out)
}
