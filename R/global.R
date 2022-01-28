global <- new.env()

addResourcePath(
    "climate_narrative",
    system.file("www", package = "climate.narrative")
  )

# defining the questionnaire using list of QuestionTab objects
global$ordered_tabs <- c(
  "title", "auth", "type",
  "bank_re", "bank_c", "bank_sov",
  "ins_l", "ins_nl", "ins_c", "ins_sov",
  "am_c", "am_sov", "am_re",
  "report"
)

# defining possible report versions
global$report_versions <- 1:3
names(global$report_versions) <- c(
  # the names will be displayed in selectInput (dev mode only)
  "v1 = base version",
  "v2 = v1 + added links to top (HTML only)",
  "v3 = v2 + interim executive summary (both formats)"
)