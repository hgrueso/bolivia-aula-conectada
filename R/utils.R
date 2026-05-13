# R/utils.R — helpers used across stages ----------------------------------

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

# Strip haven labels (keep numeric codes)
to_plain <- function(x) {
  if (inherits(x, "haven_labelled")) haven::zap_labels(x) else x
}

# Pick & rename variables from a list of mappings (analysis_name = "raw_name").
# Warns about missing raw names but does not fail.
pick_vars <- function(df, ...) {
  maps <- c(...)
  keep <- maps[!vapply(maps, function(x) is.null(x) || (length(x) == 1 && is.na(x)),
                       logical(1))]
  raw      <- unlist(keep, use.names = FALSE)
  analysis <- names(keep)
  in_df    <- raw %in% names(df)
  if (any(!in_df)) {
    warning(deparse(substitute(df)), " missing: ",
            paste(raw[!in_df], collapse = ", "), call. = FALSE)
  }
  out <- df[, raw[in_df], drop = FALSE]
  names(out) <- analysis[in_df]
  out
}

# Weighted proportion + SE + 95 % CI using a survey design.
weighted_prop <- function(design, var, by = NULL, na.rm = TRUE) {
  fml <- as.formula(paste0("~", var))
  if (is.null(by)) {
    out <- survey::svymean(fml, design, na.rm = na.rm)
    tibble::tibble(
      variable = var,
      estimate = as.numeric(out),
      se       = as.numeric(sqrt(diag(attr(out, "var")))),
      n        = sum(!is.na(design$variables[[var]]))
    )
  } else {
    by_fml <- as.formula(paste0("~", paste(by, collapse = "+")))
    out <- survey::svyby(fml, by_fml, design, survey::svymean, na.rm = na.rm)
    tibble::as_tibble(out)
  }
}

# Tidy fixest/lm output → percentage-point gaps.
tidy_pp <- function(model, label = NULL) {
  td <- broom::tidy(model, conf.int = TRUE)
  td$estimate_pp  <- td$estimate * 100
  td$conf.low_pp  <- td$conf.low  * 100
  td$conf.high_pp <- td$conf.high * 100
  if (!is.null(label)) td$outcome <- label
  td
}

# Ensure output dirs exist.
ensure_dirs <- function() {
  out <- here::here("output")
  for (sub in c("tables", "figures", "models", "projections")) {
    fs::dir_create(file.path(out, sub))
  }
  invisible(out)
}

# Save a tibble as CSV.
write_table <- function(df, name) {
  out <- here::here("output", "tables")
  fs::dir_create(out)
  readr::write_csv(df, file.path(out, paste0(name, ".csv")))
  invisible(df)
}

# Save a ggplot as PNG (always). PDF is opt-in; if cairo_pdf is unavailable
# the call is swallowed silently so the script never breaks on a missing
# XQuartz / Cairo install.
save_fig <- function(plot, name, width = 9, height = 5.5, dpi = 300, pdf = FALSE) {
  out <- here::here("output", "figures")
  fs::dir_create(out)
  ggplot2::ggsave(file.path(out, paste0(name, ".png")),
                  plot, width = width, height = height, dpi = dpi, bg = "white")
  if (pdf) {
    suppressWarnings(try(
      ggplot2::ggsave(file.path(out, paste0(name, ".pdf")),
                      plot, width = width, height = height, device = cairo_pdf),
      silent = TRUE
    ))
  }
  invisible(plot)
}

# Weighted quantile (income quintiles etc.) using Hmisc.
wquantile <- function(x, w, probs) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  Hmisc::wtd.quantile(x[ok], weights = w[ok], probs = probs, normwt = TRUE)
}

# Format numbers with thousands separators
fmt_n <- function(x, digits = 0) {
  format(round(x, digits), big.mark = ",", scientific = FALSE)
}
fmt_pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), x * 100)
