#' Main function that runs the shiny app.
#'
#' @param secrets_file Location of yaml file containing secrets
#' @param ... Additional parameters to shiny server function
#'
#' @import shiny
#' @import R6
#' @importFrom yaml read_yaml
#' @export
run_shiny_app <- function(secrets_file="secret.yml", ...) {
  load_secrets(secrets_file)
  global$exposures <- read_dir(paste0(global$report_version, "/exposure"))
  global$scenarios <- read_dir(paste0(global$report_version, "/scenario"))
  global$products <- read_dir(paste0(global$report_version, "/product"))
  global$exposure_classes <- read_dir(paste0(global$report_version, "/exposure_class"))

  # ordering the scenarios
  global$scenarios <- global$scenarios[order(sapply(global$scenarios, `[[`, i = "position"))]

  global$tabs <- list(
    QuestionTab$new("title", NULL, NULL, "auth", FALSE, FALSE),
    QuestionTab$new("auth", NULL, "title", NULL, ui_settings = list(captcha_code = global$captcha_code)),
    QuestionTab$new("type", NULL, "auth", "ins_l"),
    QuestionTab$new("bank_re", "Bank: Real Estate Exposures", "type", "bank_c", TRUE, TRUE, global$exposures$bankRe, "bank", "R"),
    QuestionTab$new("bank_c", "Bank: Company Exposures", "bank_re", "bank_sov", TRUE, TRUE, global$exposures$bankCorporate, "bank", "C"),
    QuestionTab$new("bank_sov", "Bank: Sovereign Exposures", "bank_c", "report", TRUE, TRUE, global$exposures$sovereign, "bank", "S"),
    QuestionTab$new("ins_l", "Insurance: Life and Health Lines of Business", "type", "ins_nl", TRUE, TRUE, global$exposures$insuranceLife, "insurance", "L"),
    QuestionTab$new("ins_nl", "Insurance: Property and Casualty Lines of Business", "ins_l", "ins_c", TRUE, TRUE, global$exposures$insuranceNonlife, "insurance", "N"),
    QuestionTab$new("ins_c", "Insurance: Corporate and Real Estate Assets", "ins_nl", "ins_sov", TRUE, TRUE, global$exposures$insuranceCorporate, "insurance", "C"),
    QuestionTab$new("ins_sov", "Insurance: Sovereign Assets", "ins_c", "report", TRUE, TRUE, global$exposures$sovereign, "insurance", "S"),
    QuestionTab$new("am_c", "Asset Manager: Corporate Assets under management", "type", "am_sov", TRUE, TRUE, global$exposures$amCorporate, "asset", "C"),
    QuestionTab$new("am_sov", "Asset Manager: Sovereign Assets under management", "am_c", "am_re", TRUE, TRUE, global$exposures$sovereign, "asset", "S"),
    QuestionTab$new("am_re", "Asset Manager: Real Estate Assets under management", "am_sov", "report", TRUE, TRUE, global$exposures$amRe, "asset", "R"),
    QuestionTab$new("report", NULL, "type", NULL)
  )
  # Tab names validation check
  # the global$ordered_tabs must be defined first (the QuestionTab constructor relies on this
  # to assign tab numbers), so check consistency of names post hoc
  if (identical(global$ordered_tabs, sapply(global$tabs, function(x) x$tab_name))){
    # OK
  } else if (setequal(global$ordered_tabs, sapply(global$tabs, function(x) x$tab_name))){
    warning('Order of global$tabs overwritten by global$ordered_tabs')
  } else {
    stop('Names of global$tabs do not match global$ordered_tabs')
  }
  shinyApp(ui = ui(), server = server, ...)
}

load_secrets <- function(secrets_file="secret.yml") {
  if (file.exists(secrets_file)) {
    secret_pars <- yaml::read_yaml(secrets_file)
    global$dev <- FALSE
    for (i in 1:length(secret_pars)) global[[names(secret_pars)[i]]] <- secret_pars[[i]]
    if (is.null(global$report_version)){
      default_version = global$report_versions[1]
      global$report_version = default_version
      warning(paste0("Report version not found in the settings file, defaulting to ", default_version))
    } else if (!global$report_version %in% global$report_versions){
      default_version = global$report_versions[1]
      global$report_version = default_version
      warning(paste0("Invalid report version in the settings file, defaulting to ", default_version))
    }
  } else {
    global$dev <- TRUE
  }
}
