# 00b_inspect_parenting_vars.R
# Targeted inspection of the candidates we plan to use.
# Run: Rscript code/00b_inspect_parenting_vars.R

suppressPackageStartupMessages({
  library(haven); library(here); library(dplyr); library(stringr)
})

cat("\n=== EH 2024: targeted inspection for parenting-layer + agency indicators ===\n\n")

per_path <- here::here("data", "BD_EH2024", "EH2024_Persona.sav")
per <- read_sav(per_path)
cat("Persona file: ", nrow(per), " rows, ", ncol(per), " variables\n\n", sep = "")

# Helper: print label + value labels + frequency for a variable
inspect_var <- function(df, v, age_filter = NULL) {
  if (!v %in% names(df)) {
    cat("  [VARIABLE NOT FOUND: ", v, "]\n\n", sep = "")
    return(invisible(NULL))
  }
  x <- df[[v]]
  cat("\n--- ", v, " ---\n", sep = "")
  cat("Label : ", attr(x, "label") %||% "(none)", "\n", sep = "")
  vls <- attr(x, "labels")
  if (!is.null(vls)) {
    cat("Value labels:\n"); print(vls)
  }
  cat("Frequency (full sample):\n")
  print(table(x, useNA = "always"))
  if (!is.null(age_filter) && "s01a_03" %in% names(df)) {
    cat("Frequency (adolescents 10-19):\n")
    print(table(x[df$s01a_03 >= 10 & df$s01a_03 <= 19], useNA = "always"))
    if (!is.null(age_filter$min_age)) {
      cat("Frequency (", age_filter$min_age, "-", age_filter$max_age, " only):\n", sep = "")
      print(table(
        x[df$s01a_03 >= age_filter$min_age & df$s01a_03 <= age_filter$max_age],
        useNA = "always"
      ))
    }
  }
  cat("\n")
}

# ---- 1. HOUSEHOLD COMPOSITION (s01a_05b, s01a_05c) ----
cat("================================================================\n")
cat("1. HOUSEHOLD COMPOSITION — who is the adolescent's father/mother?\n")
cat("================================================================\n")
inspect_var(per, "s01a_05b")  # Padre / padrastro
inspect_var(per, "s01a_05c")  # Madre / madrastra

# Cross-tab for adolescents 10-19: does each have a father / mother present?
if (all(c("s01a_03", "s01a_05b", "s01a_05c") %in% names(per))) {
  ado <- per |> filter(s01a_03 >= 10, s01a_03 <= 19)
  cat("\nAmong adolescents 10-19 (n =", nrow(ado), "):\n")
  ado <- ado |>
    mutate(
      has_father = !is.na(s01a_05b) & s01a_05b != 0,
      has_mother = !is.na(s01a_05c) & s01a_05c != 0
    )
  cat("  Has father in HH roster: ", sum(ado$has_father), "\n", sep = "")
  cat("  Has mother in HH roster: ", sum(ado$has_mother), "\n", sep = "")
  cat("  Has both                 : ", sum(ado$has_father & ado$has_mother), "\n", sep = "")
  cat("  Has neither              : ", sum(!ado$has_father & !ado$has_mother), "\n", sep = "")
}

# ---- 2. ADOLESCENT PREGNANCY / MOTHERHOOD ----
cat("\n================================================================\n")
cat("2. ADOLESCENT PREGNANCY / MOTHERHOOD (girls only)\n")
cat("================================================================\n")
# s02b_06: Ever been pregnant?
# s02b_07: Number of children ever born alive
inspect_var(per, "s02b_06", age_filter = list(min_age = 15, max_age = 19))
inspect_var(per, "s02b_07", age_filter = list(min_age = 15, max_age = 19))

# Cross-tab: among girls 15-19, what share have ever been pregnant?
if (all(c("s01a_03", "s01a_02", "s02b_06") %in% names(per))) {
  girls_15_19 <- per |> filter(s01a_03 >= 15, s01a_03 <= 19, s01a_02 == 2)
  cat("\nAmong girls 15-19 (n =", nrow(girls_15_19), "):\n")
  cat("  Ever pregnant (s02b_06 == 1): ", sum(girls_15_19$s02b_06 == 1, na.rm = TRUE), "\n", sep = "")
  cat("  Never pregnant (s02b_06 == 2): ", sum(girls_15_19$s02b_06 == 2, na.rm = TRUE), "\n", sep = "")
  cat("  NA                            : ", sum(is.na(girls_15_19$s02b_06)), "\n", sep = "")
}

cat("\n=== END inspection ===\n\n")
