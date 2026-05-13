# 00_explore_data.R — Stage 0: dump variable metadata for all SAV files ---
# Run ONCE to inspect the EH 2024 variable inventory.
# Outputs three CSVs in output/ for offline reading.
# ------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(haven); library(labelled); library(dplyr); library(purrr)
  library(stringr); library(readr); library(tibble); library(here); library(fs)
})

source(here::here("R", "utils.R"))
OUT <- ensure_dirs()

DATA_DIR <- here::here("data", "BD_EH2024")
files <- dir_ls(DATA_DIR, glob = "*.sav")

extract_metadata <- function(path) {
  message("Reading: ", basename(path))
  df <- read_sav(path)
  tibble(
    file       = basename(path),
    var        = names(df),
    label      = vapply(df, function(x) var_label(x) %||% "", character(1)),
    type       = vapply(df, function(x) class(x)[1], character(1)),
    n_obs      = nrow(df),
    n_miss     = vapply(df, function(x) sum(is.na(x)), integer(1)),
    n_unique   = vapply(df, function(x) length(unique(x)), integer(1)),
    min_val    = vapply(df, function(x) {
      if (is.numeric(x)) suppressWarnings(min(x, na.rm = TRUE)) else NA_real_
    }, numeric(1)),
    max_val    = vapply(df, function(x) {
      if (is.numeric(x)) suppressWarnings(max(x, na.rm = TRUE)) else NA_real_
    }, numeric(1)),
    example    = vapply(df, function(x) paste(head(na.omit(unique(x)), 3),
                                              collapse = " | "), character(1))
  )
}

dict <- map_dfr(files, extract_metadata)

# File-level summary
file_summary <- dict |>
  group_by(file) |>
  summarise(n_variables = n(),
            n_obs       = first(n_obs),
            avg_pct_missing = round(mean(n_miss / n_obs * 100), 2),
            .groups = "drop")

cat("\n=== FILE SUMMARY ===\n")
print(file_summary)

# Keyword filter: things likely relevant to the analysis
hits <- dict |>
  filter(str_detect(tolower(paste(var, label)),
                    "edad|sexo|hombre|mujer|asist|matric|escuel|grado|nivel|educac|indigen|lengua|idioma|pueblo|disca|ver|oir|caminar|recordar|trabaj|inter|comput|tablet|celul|telefon|tecnolog|hogar|jefe|pobre|ingreso|gasto|area|depar|munic|psu|estrato|factor"))

readr::write_csv(dict,         file.path(OUT, "EH2024_data_dictionary_raw.csv"))
readr::write_csv(file_summary, file.path(OUT, "EH2024_file_summary.csv"))
readr::write_csv(hits,         file.path(OUT, "EH2024_keyword_hits.csv"))

cat("\nSaved:\n",
    "  ", file.path(OUT, "EH2024_data_dictionary_raw.csv"), "\n",
    "  ", file.path(OUT, "EH2024_file_summary.csv"), "\n",
    "  ", file.path(OUT, "EH2024_keyword_hits.csv"), "\n")
