# R/lit_review_data.R
# -----------------------------------------------------------------------------
# Loads the 52-paper consolidated lit review and tags each paper by its
# relevance for the Aula Conectada theory of change. Used by:
#   - 06_slides.qmd        (consolidated evidence table on appendix)
#   - 05_projections.R     (filtering which papers feed into Bolivia projections)
#   - Future SEM code      (selecting anchors per ToC arrow)
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(here); library(tibble)
})

# 1. Load -----------------------------------------------------------------
lit_papers <- read_csv(
  here::here("data", "lit_review_papers.csv"),
  show_col_types = FALSE,
  na = c("", "not_reported", "NA")
)

lit_modifiers <- read_csv(
  here::here("data", "lit_review_effect_modifiers.csv"),
  show_col_types = FALSE,
  na = c("", "not_reported", "NA")
)

# 2. Tag each paper by its ToC arrow ---------------------------------------
# Each paper can map to one or more arrows in the Aula Conectada ToC. We tag
# by keyword search over the paper title + one_sentence_description.
# Arrows:
#   - digital_education : technology / digital learning / ICT-based programmes
#   - teacher_training  : teacher-side capacity building
#   - parenting         : parent/caregiver-targeted interventions
#   - gender_norms      : interventions targeting attitudes, marriage, agency
#   - cash_transfer     : CCTs and UCTs
#   - mentoring         : peer / adult mentoring, life-skills, ELA-style
#   - labor_market      : post-schooling job/training programmes
#   - learning_focus    : pure school-based learning interventions
#   - health_nutrition  : health/nutrition-side interventions (mostly out-of-scope)

classify_paper <- function(name, desc) {
  txt <- tolower(paste(name, desc, sep = " "))
  list(
    digital_education = str_detect(txt, "digital|ict|technolog|laptop|computer|tablet|olpc|ceibal|computational|stem|sms|app|online|tutor"),
    teacher_training  = str_detect(txt, "teacher train|docente|pedagog|teach.*method|teach.*practice|professional develop"),
    parenting         = str_detect(txt, "parent|caregiver|family|household.*norm|mother.*train"),
    gender_norms      = str_detect(txt, "gender norm|empower|agency|negotiat|self.efficacy|child marriage|adolescent girl|safe space"),
    cash_transfer     = str_detect(txt, "cash transfer|cct|uct|conditional cash|stipend|grant|cash incentive"),
    mentoring         = str_detect(txt, "mentor|ela |life skill|safe space|club|peer support"),
    labor_market      = str_detect(txt, "labor|labour|employ|earning|wage|projoven|job training|vocational"),
    learning_focus    = str_detect(txt, "learning|literacy|numeracy|test score|achievement|cognitive"),
    health_nutrition  = str_detect(txt, "vaccin|health|nutrit|mental health|hiv|disease|food security|deworm")
  )
}

# Apply classification and add columns
classifications <- mapply(
  classify_paper,
  lit_papers$paper_name,
  lit_papers$one_sentence_description,
  SIMPLIFY = FALSE
)
class_df <- bind_rows(classifications) |>
  mutate(across(everything(), as.integer))

lit_papers <- bind_cols(lit_papers, class_df)

# 3. Eligibility for Aula Conectada projection pipeline -------------------
# A paper qualifies as a *projection anchor* (gets a phi-adjusted Bolivia
# estimate in the main table) if:
#   - It has measurable effect_size AND standard_error
#   - It maps to one of the core ToC arrows (digital_education, teacher_training,
#     parenting, mentoring, learning_focus, gender_norms)
#   - It is an RCT or quasi-experiment (study_design)
#   - Age range overlaps reasonably with 10-17 (computed in 05_projections.R)

lit_papers <- lit_papers |>
  mutate(
    has_effect      = !is.na(effect_size) & !is.na(standard_error),
    rigorous_design = study_design %in% c("RCT", "quasi-experiment"),
    in_scope_arrows = digital_education + teacher_training + parenting +
                      mentoring + learning_focus + gender_norms,
    eligible_anchor = has_effect & rigorous_design & in_scope_arrows >= 1
  )

# 4. Diagnostic summary ----------------------------------------------------
if (interactive() || identical(Sys.getenv("DEBUG_LIT"), "1")) {
  message("Lit review loaded: ", nrow(lit_papers), " papers")
  message("Eligible projection anchors: ",
          sum(lit_papers$eligible_anchor), " of ", nrow(lit_papers))
  message("\nBy region:")
  print(lit_papers |> count(region) |> arrange(desc(n)))
  message("\nBy study design:")
  print(lit_papers |> count(study_design))
  message("\nByToC arrow (any paper can match multiple):")
  arrow_counts <- lit_papers |>
    summarise(across(c(digital_education, teacher_training, parenting,
                       gender_norms, cash_transfer, mentoring,
                       labor_market, learning_focus, health_nutrition),
                     sum, na.rm = TRUE))
  print(arrow_counts)
}
