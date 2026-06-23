# make_toc.R — regenerate the Theory-of-Change diagram (EN + ES).
# Run this as a figure step (like the other f*.R scripts) so the ToC is always
# present in output/figures and never has to be hand-copied.
#
#   Rscript code/make_toc.R
#
# One-time dependency: the 'rsvg' package (lightweight, CRAN).
#   install.packages("rsvg")

suppressPackageStartupMessages(library(here))
if (!requireNamespace("rsvg", quietly = TRUE)) {
  stop("Please install it once:  install.packages(\"rsvg\")", call. = FALSE)
}

out_dir <- if (file.exists(here::here(".here"))) here::here("output", "figures") else "output/figures"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

svg_en <- '<svg viewBox="0 0 1080 470" width="1080" height="470" xmlns="http://www.w3.org/2000/svg">
  <defs><marker id="arrow" markerWidth="10" markerHeight="10" refX="8" refY="3" orient="auto" markerUnits="strokeWidth"><path d="M0,0 L8,3 L0,6 Z" fill="#8a8a8a"/></marker></defs>
  <rect x="0" y="0" width="1080" height="470" fill="white"/>
  <path d="M320,80 C400,80 400,205 466,210" fill="none" stroke="#8a8a8a" stroke-width="2" marker-end="url(#arrow)"/>
  <path d="M320,235 L462,235" fill="none" stroke="#8a8a8a" stroke-width="2" marker-end="url(#arrow)"/>
  <path d="M320,390 C400,390 400,265 466,260" fill="none" stroke="#8a8a8a" stroke-width="2" marker-end="url(#arrow)"/>
  <path d="M650,235 L786,235" fill="none" stroke="#8a8a8a" stroke-width="2" marker-end="url(#arrow)"/>
  <rect x="30" y="20" width="290" height="120" rx="8" fill="#fde9f2" stroke="#E2007A" stroke-width="1.5"/>
  <rect x="30" y="175" width="290" height="120" rx="8" fill="#fff0f8" stroke="#E2007A" stroke-width="1.5"/>
  <rect x="30" y="330" width="290" height="120" rx="8" fill="#eaf6fc" stroke="#1CABE2" stroke-width="1.5"/>
  <rect x="470" y="175" width="180" height="120" rx="8" fill="#fff7e6" stroke="#FFC20E" stroke-width="1.5"/>
  <rect x="790" y="185" width="240" height="100" rx="8" fill="#FFC20E" stroke="#d99e00" stroke-width="2"/>
  <text x="48" y="60" font-family="Helvetica,Arial,sans-serif" font-size="20" font-weight="700" fill="#a3005a">&#9312; Learning layer<tspan x="48" dy="30" font-weight="400">pedagogy &amp;</tspan><tspan x="48" dy="30" font-weight="400">teacher training</tspan></text>
  <text x="48" y="215" font-family="Helvetica,Arial,sans-serif" font-size="20" font-weight="700" fill="#a3005a">&#9313; Empowerment<tspan x="48" dy="30" font-weight="400">ELA-style clubs</tspan><tspan x="48" dy="30" font-weight="400">(staying in school)</tspan></text>
  <text x="48" y="370" font-family="Helvetica,Arial,sans-serif" font-size="20" font-weight="700" fill="#00377C">&#9314; Home environment<tspan x="48" dy="30" font-weight="400">parenting support</tspan><tspan x="48" dy="30" font-weight="400">(violence)</tspan></text>
  <text x="486" y="213" font-family="Helvetica,Arial,sans-serif" font-size="19" font-weight="600" fill="#7a5b00">Girls stay,<tspan x="486" dy="28" font-weight="400">learn &amp;</tspan><tspan x="486" dy="28" font-weight="400">transition</tspan></text>
  <text x="808" y="223" font-family="Helvetica,Arial,sans-serif" font-size="19" font-weight="700" fill="#3d2e00">Narrower gaps<tspan x="808" dy="28" font-weight="600">in learning</tspan><tspan x="808" dy="28" font-weight="600">&amp; work</tspan></text>
</svg>
'
svg_es <- '<svg viewBox="0 0 1080 470" width="1080" height="470" xmlns="http://www.w3.org/2000/svg">
  <defs><marker id="arrow" markerWidth="10" markerHeight="10" refX="8" refY="3" orient="auto" markerUnits="strokeWidth"><path d="M0,0 L8,3 L0,6 Z" fill="#8a8a8a"/></marker></defs>
  <rect x="0" y="0" width="1080" height="470" fill="white"/>
  <path d="M320,80 C400,80 400,205 466,210" fill="none" stroke="#8a8a8a" stroke-width="2" marker-end="url(#arrow)"/>
  <path d="M320,235 L462,235" fill="none" stroke="#8a8a8a" stroke-width="2" marker-end="url(#arrow)"/>
  <path d="M320,390 C400,390 400,265 466,260" fill="none" stroke="#8a8a8a" stroke-width="2" marker-end="url(#arrow)"/>
  <path d="M650,235 L786,235" fill="none" stroke="#8a8a8a" stroke-width="2" marker-end="url(#arrow)"/>
  <rect x="30" y="20" width="290" height="120" rx="8" fill="#fde9f2" stroke="#E2007A" stroke-width="1.5"/>
  <rect x="30" y="175" width="290" height="120" rx="8" fill="#fff0f8" stroke="#E2007A" stroke-width="1.5"/>
  <rect x="30" y="330" width="290" height="120" rx="8" fill="#eaf6fc" stroke="#1CABE2" stroke-width="1.5"/>
  <rect x="470" y="175" width="180" height="120" rx="8" fill="#fff7e6" stroke="#FFC20E" stroke-width="1.5"/>
  <rect x="790" y="185" width="240" height="100" rx="8" fill="#FFC20E" stroke="#d99e00" stroke-width="2"/>
  <text x="48" y="58" font-family="Helvetica,Arial,sans-serif" font-size="18" font-weight="700" fill="#a3005a">&#9312; Capa de aprendizaje<tspan x="48" dy="30" font-weight="400">pedagog&#237;a y</tspan><tspan x="48" dy="30" font-weight="400">formaci&#243;n docente</tspan></text>
  <text x="48" y="213" font-family="Helvetica,Arial,sans-serif" font-size="18" font-weight="700" fill="#a3005a">&#9313; Empoderamiento<tspan x="48" dy="30" font-weight="400">clubes tipo ELA</tspan><tspan x="48" dy="30" font-weight="400">(permanencia escolar)</tspan></text>
  <text x="48" y="368" font-family="Helvetica,Arial,sans-serif" font-size="18" font-weight="700" fill="#00377C">&#9314; Entorno del hogar<tspan x="48" dy="30" font-weight="400">apoyo a la crianza</tspan><tspan x="48" dy="30" font-weight="400">(violencia)</tspan></text>
  <text x="486" y="213" font-family="Helvetica,Arial,sans-serif" font-size="18" font-weight="600" fill="#7a5b00">Permanencia,<tspan x="486" dy="28" font-weight="400">aprendizaje</tspan><tspan x="486" dy="28" font-weight="400">y transici&#243;n</tspan></text>
  <text x="808" y="223" font-family="Helvetica,Arial,sans-serif" font-size="18" font-weight="700" fill="#3d2e00">Menores brechas<tspan x="808" dy="28" font-weight="600">en aprendizaje</tspan><tspan x="808" dy="28" font-weight="600">y empleo</tspan></text>
</svg>
'

rsvg::rsvg_png(charToRaw(svg_en), file.path(out_dir, "f_toc_en.png"), width = 2200)
rsvg::rsvg_png(charToRaw(svg_es), file.path(out_dir, "f_toc_es.png"), width = 2200)

cat("wrote f_toc_en.png and f_toc_es.png to", out_dir, "\n")
