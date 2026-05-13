# R/theme.R — UNICEF-friendly clean theme for ggplot2 ---------------------

suppressPackageStartupMessages(library(ggplot2))

UNICEF_BLUE   <- "#1CABE2"
UNICEF_DARK   <- "#00377C"
ACCENT_GIRL   <- "#E2007A"
ACCENT_BOY    <- "#374EA2"
ACCENT_GOLD   <- "#FFC20E"
GREY_DARK     <- "#374649"
GREY_MID      <- "#7A8487"
GREY_LIGHT    <- "#D9D9D9"

PAL_GENDER <- c("Boys" = ACCENT_BOY, "Girls" = ACCENT_GIRL)
PAL_AREA   <- c("Urban" = UNICEF_BLUE, "Rural" = UNICEF_DARK)
PAL_SEQ    <- c("#D7EFF8", "#9DD4EC", "#1CABE2", "#005A8B", "#00377C")
PAL_DIV    <- c("#E2007A", "#F5A4C2", "#F2F2F2", "#9DD4EC", "#1CABE2")

theme_unicef <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title           = element_text(face = "bold", size = base_size + 4,
                                          colour = GREY_DARK, margin = margin(b = 6)),
      plot.subtitle        = element_text(size = base_size, colour = GREY_DARK,
                                          margin = margin(b = 12)),
      plot.caption         = element_text(size = base_size - 2, colour = "grey40",
                                          hjust = 0, margin = margin(t = 10)),
      plot.title.position  = "plot",
      plot.caption.position = "plot",
      panel.grid.minor     = element_blank(),
      panel.grid.major.x   = element_blank(),
      panel.grid.major.y   = element_line(colour = "grey90", linewidth = 0.3),
      axis.title           = element_text(colour = GREY_DARK, size = base_size - 1),
      axis.text            = element_text(colour = GREY_DARK),
      axis.ticks           = element_blank(),
      legend.position      = "top",
      legend.title         = element_text(face = "bold", size = base_size - 1),
      legend.key.height    = unit(0.6, "lines"),
      strip.text           = element_text(face = "bold", colour = GREY_DARK,
                                          hjust = 0, margin = margin(b = 4)),
      plot.margin          = margin(14, 18, 12, 14)
    )
}

theme_set(theme_unicef())
