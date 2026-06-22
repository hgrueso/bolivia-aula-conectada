# f7_violence_2panel_en.R
# Slide 8 figure — harsh physical discipline by AREA and by ETHNICITY.
# Replaces the old 4-way f7_violence_by_group chart (items 1 + 3).
#
# Self-contained: point estimates and SEs are hard-coded from the EDSA 2023
# weighted means computed in two_quick_checks.R, so this script does NOT need
# the microdata. If you later want it data-driven, swap `dat` for the survey
# means and keep the plotting block.
#
# Run from $ANALYSIS:  Rscript code/f7_violence_2panel_en.R

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(patchwork)
})

# UNICEF palette
unicef_cyan <- "#1CABE2"
unicef_blue <- "#00377C"
unicef_mag  <- "#E2007A"
grey_lo     <- "#9AA7B2"

# --- Estimates from EDSA 2023 (weighted, women with children) ---------------
dat <- tibble::tribble(
  ~panel,       ~group,            ~est,   ~se,
  "Area",       "Urbano",           0.322,  0.0108,
  "Area",       "Rural",           0.523,  0.0187,
  "Ethnicity",  "No indígena",  0.350,  0.0107,
  "Ethnicity",  "Indígena",      0.459,  0.0182
) |>
  mutate(
    lo  = est - 1.96 * se,
    hi  = est + 1.96 * se,
    lab = sprintf("%.1f%%", 100 * est)
  )

# keep the order we want on the x-axis
dat$group <- factor(dat$group,
  levels = c("Urbano", "Rural", "No indígena", "Indígena"))

make_panel <- function(d, title, fill_lo, fill_hi) {
  d$fillcol <- ifelse(d$est == max(d$est), fill_hi, fill_lo)
  ggplot(d, aes(group, est)) +
    geom_col(aes(fill = fillcol), width = 0.62) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.16,
                  linewidth = 0.6, colour = "grey25") +
    # label lifted ABOVE the upper CI whisker (fixes item 1)
    geom_text(aes(y = hi + 0.035, label = lab),
              fontface = "bold", size = 5.2, colour = "grey15") +
    scale_fill_identity() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 0.66),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title = title, x = NULL, y = "Disciplina física severa") +
    theme_minimal(base_size = 15) +
    theme(
      plot.title   = element_text(face = "bold", colour = unicef_blue,
                                  size = 16, margin = margin(b = 8)),
      axis.title.y = element_text(size = 12, colour = "grey30"),
      axis.text.x  = element_text(face = "bold", size = 13, colour = "grey15"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank()
    )
}

pA <- make_panel(filter(dat, panel == "Area"),
                 "Por zona de residencia", grey_lo, unicef_cyan)
pB <- make_panel(filter(dat, panel == "Ethnicity"),
                 "Por etnicidad", grey_lo, unicef_mag)

fig <- pA + pB +
  plot_annotation(
    caption = "EDSA 2023, INE Bolivia · ponderado · mujeres con hijos · bigotes = IC 95%",
    theme = theme(plot.caption = element_text(size = 10, colour = "grey45",
                                              hjust = 0))
  )

out_dir <- if (requireNamespace("here", quietly = TRUE) &&
               file.exists(here::here(".here"))) {
  here::here("output", "figures")
} else "output/figures"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(out_dir, "f7_violence_2panel_es.png"),
       fig, width = 11, height = 4.6, dpi = 200, bg = "white")

cat("wrote", file.path(out_dir, "f7_violence_2panel_es.png"), "\n")
