global <- new.env()


global$exposures <- read_dir("exposure")
global$scenarios <- read_dir("scenario")
global$products <- read_dir("product")
global$exposure_classes <- read_dir("exposure_class")

# ordering the scenarios
global$scenarios <- global$scenarios[order(sapply(global$scenarios, `[[`, i = "position"))]

# defining the questionnaire using list of QuestionTab objects
global$ordered_tabs <- c(
  "title", "auth", "type",
  "bank_re", "bank_c", "bank_sov",
  "ins_l", "ins_nl", "ins_c", "ins_sov",
  "am_c", "am_sov", "am_re",
  "report"
)

if (file.exists("secret.yml")) {
  secret_pars <- yaml::read_yaml("secret.yml")
  global$dev <- FALSE
  for (i in 1:length(secret_pars)) global[[names(secret_pars)[i]]] <- secret_pars[[i]]
} else {
  global$dev <- TRUE
}
