# run_all.R — execute every stage of the pipeline in order ---------------
# Usage:  Rscript run_all.R    (from the analysis root)
#
# Each stage saves its outputs to output/ and the next stage picks them up.
# ------------------------------------------------------------------------

stages <- c(
  "code/01_clean_data.R",
  "code/02_descriptive.R",
  "code/03_figures.R",
  "code/03b_figures_en.R",
  "code/04_econometrics.R",
  "code/04b_heterogeneity.R",
  "code/05_projections.R"
)

t0 <- Sys.time()
for (s in stages) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("→ Running ", s, "\n", sep = "")
  cat(strrep("=", 70), "\n", sep = "")
  t_stage <- Sys.time()
  source(s, echo = FALSE)
  cat(sprintf("✓ Finished %s in %.1f s\n", s,
              as.numeric(difftime(Sys.time(), t_stage, units = "secs"))))
}

cat(sprintf("\nAll stages complete in %.1f s.\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cat("→ Render slides with:  bash render_pdf.sh both\n")
cat("   or individually:    bash render_pdf.sh es | bash render_pdf.sh en\n")
