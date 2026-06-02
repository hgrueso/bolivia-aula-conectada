# 00_explore_eh2024.R — variable inventory of all EH 2024 SAV files
# Purpose: find which variables (if any) can populate the parenting layer of the ToC.
# Output: output/data_exploration/eh2024_variable_catalogue.csv
#         output/data_exploration/eh2024_variable_summaries.txt
# Run:    Rscript code/00_explore_eh2024.R

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(purrr); library(stringr); library(readr); library(here)
})

DATA_DIR <- here::here("data", "BD_EH2024")
OUT_DIR  <- here::here("output", "data_exploration")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

files <- list.files(DATA_DIR, pattern = "\\.sav$", full.names = TRUE)
stopifnot(length(files) > 0)

# Keywords that might flag a parenting / family / violence / gender-norms variable
PARENTING_KEYWORDS <- c(
  # English
  "parent", "child", "discipline", "punish", "hit", "beat", "violence",
  "abuse", "norm", "gender", "communicat", "monitor", "supervis",
  "autonomy", "decision", "freedom", "permission", "support",
  "household_head", "head_of_household", "married", "marriage", "spouse",
  # Spanish
  "padre", "madre", "hijo", "hija", "disciplin", "castig", "golpe", "violen",
  "abus", "norma", "genero", "g\u00E9nero", "comunic", "supervis",
  "permiso", "decis", "autonom\u00EDa", "matrimon", "casad", "c\u00F3nyuge",
  "discrimina", "trato", "respeto", "embaraz", "edad_matrim"
)
PARENTING_RX <- paste(PARENTING_KEYWORDS, collapse = "|")

cat("Found", length(files), "SAV files in", DATA_DIR, "\n\n")

catalogue <- map_dfr(files, function(f) {
  cat("Reading", basename(f), "... ")
  df <- tryCatch(read_sav(f, n_max = 50), error = function(e) NULL)
  if (is.null(df)) { cat("FAILED\n"); return(NULL) }
  cat(ncol(df), "variables\n")
  tibble(
    file       = basename(f),
    variable   = names(df),
    label      = map_chr(df, ~ attr(.x, "label") %||% ""),
    class      = map_chr(df, ~ paste(class(.x), collapse = "/")),
    has_values = map_chr(df, ~ if (!is.null(attr(.x, "labels"))) "yes" else "")
  )
})

# Tag candidates: variable name or label hits keyword
catalogue <- catalogue |>
  mutate(
    name_hit  = str_detect(tolower(variable), PARENTING_RX),
    label_hit = str_detect(tolower(label),    PARENTING_RX),
    candidate = name_hit | label_hit
  )

# Write full catalogue
write_csv(catalogue, file.path(OUT_DIR, "eh2024_variable_catalogue.csv"))
cat("\nFull catalogue: ", file.path(OUT_DIR, "eh2024_variable_catalogue.csv"), "\n")
cat("Total variables across all files:", nrow(catalogue), "\n")
cat("Parenting-keyword hits           :", sum(catalogue$candidate), "\n\n")

# Print the candidates to console for immediate visual scan
cands <- catalogue |> filter(candidate)
if (nrow(cands) > 0) {
  cat("=== CANDIDATE VARIABLES (potentially relevant to parenting layer) ===\n\n")
  cands |>
    arrange(file, variable) |>
    select(file, variable, label) |>
    print(n = Inf)
} else {
  cat("No parenting-keyword hits found.\n")
}

# For each candidate, dump value labels and a frequency table (n_max = 5000 for speed)
sink(file.path(OUT_DIR, "eh2024_variable_summaries.txt"))
cat("EH 2024 — frequency summaries of parenting-candidate variables\n")
cat("Generated:", format(Sys.time()), "\n")
cat(strrep("=", 70), "\n\n")

for (f in unique(cands$file)) {
  cat("\n### ", f, "\n", strrep("-", 70), "\n", sep = "")
  full_df <- read_sav(file.path(DATA_DIR, f), n_max = 5000)
  vs <- cands |> filter(file == f) |> pull(variable)
  for (v in vs) {
    cat("\n--- ", v, " ---\n")
    cat("Label  : ", attr(full_df[[v]], "label") %||% "(none)", "\n")
    cat("Class  : ", paste(class(full_df[[v]]), collapse = "/"), "\n")
    vls <- attr(full_df[[v]], "labels")
    if (!is.null(vls)) {
      cat("Value labels:\n")
      print(vls)
    }
    cat("Frequency (top 15):\n")
    print(head(sort(table(full_df[[v]], useNA = "always"), decreasing = TRUE), 15))
    cat("\n")
  }
}
sink()

cat("\nDetailed summaries: ", file.path(OUT_DIR, "eh2024_variable_summaries.txt"), "\n")
cat("\nNext step: review the candidates and tell me which look usable.\n")
