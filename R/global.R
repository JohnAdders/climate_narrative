global <- new.env()

# defining the questionnaire using list of QuestionTab objects
global$ordered_tabs <- c(
  "title", "auth", "type",
  "bank_re", "bank_c", "bank_sov",
  "ins_l", "ins_nl", "ins_c", "ins_sov",
  "am_c", "am_sov", "am_re",
  "report"
)

# defining possible report versions
global$report_versions <- c(1, 2)
names(global$report_versions) <- c(
  "v1 base version",
  "v2 added links to top (HTML only)"
)