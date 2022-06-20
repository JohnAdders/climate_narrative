global <- new.env()

global$exposure_classes <- read_dir("exposure_class")
global$exposures <- read_dir("exposure")
global$scenarios <- read_dir("scenario")
global$sections <- read_dir("section")
global$products <- read_dir("product")

# defining the questionnaire using list of QuestionTab objects
global$ordered_tabs <- c(
  "title", "auth", "instruction", "rep_type", "inst_type",
  "bank_re", "bank_c", "bank_retail", "bank_sov",
  "ins_l", "ins_nl", "ins_c", "ins_sov", "ins_re",
  "am_c", "am_sov", "am_re",
  "report"
)

# defining possible report versions
global$report_versions <- 5:7
names(global$report_versions) <- c(
  # the names will be displayed in selectInput (dev mode only)
  "v5 = base version",
  "v6 = v5 with sidebar table of content in HTML report",
  "v7 = v6 with async report production"
)
