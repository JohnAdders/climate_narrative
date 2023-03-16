#' Main function that runs the shiny app.
#'
#' @param secrets_file Location of yaml file containing secrets
#' @param ... Additional parameters to shiny server function
#'
#' @import shiny
#' @import R6
#' @importFrom yaml read_yaml
#' @importFrom future plan multisession
#' @export
run_shiny_app <- function(secrets_file = "secret.yml", ...) {
  load_secrets(secrets_file)
  initialise_globals()
  shinyApp(ui = ui(), server = server, ...)
}

#' Read the yaml file containing app settings
#'
#' @param secrets_file Path to the file (by default "secret.yml" in the current directory)
#'
load_secrets <- function(secrets_file = "secret.yml") {
  if (file.exists(secrets_file)) {
    secret_pars <- yaml::read_yaml(secrets_file)
    global$dev <- FALSE
    for (i in 1:length(secret_pars)) global[[names(secret_pars)[i]]] <- secret_pars[[i]]
    # simple validation and some default values
    if (is.null(global$progress_bar)) {
      warning("Progress bar setting not found. Defaulting to FALSE")
      global$progress_bar <- FALSE
    }
    if (is.null(global$captcha_threshold)) {
      warning("Captcha threshold setting not found. Defaulting to 0,5")
      global$captcha_threshold <- 0.5
    }
    if (is.null(global$enable_editor)) {
      warning("Enable editor setting not found. Defaulting to FALSE")
      global$enable_editor <- FALSE
    }
    if (is.null(global$ip_whitelist)) {
      warning("IP whitelist setting not found. Defaulting to localhost only")
      global$ip_whitelist <- "127.0.0.1"
    }
    if (is.null(global$report_sleep)) {
      warning("Report sleep setting not found. Defaulting to zero.")
      global$report_sleep <- 0
    }
    global$ip_whitelist <- strsplit(global$ip_whitelist, "\\s+")[[1]]
  } else {
    global$dev <- TRUE
    global$progress_bar <- FALSE
    global$enable_editor <- TRUE
    global$report_sleep <- 0
  }
  return(invisible(NULL))
}

#' All one-time operations to set up necessary objects
#'
initialise_globals <- function() {
  # ordering the scenarios
  global$scenarios <- global$scenarios[order(sapply(global$scenarios, `[[`, i = "position"))]

  # defining tab structure
  global$tabs <- list(
    QuestionTab$new("title", NULL, NULL, "auth", FALSE, FALSE),
    QuestionTab$new("auth", NULL, "title", NULL, add_buttons = FALSE, ui_settings = list(captcha_code = global$captcha_code)),
    QuestionTab$new("instruction", "Introduction to the Tool", "auth", "rep_type"),
    QuestionTab$new("rep_type", "Report Type Selection", "instruction", "inst_type"),
    QuestionTab$new("inst_type", "Institution Type Selection", "rep_type", "ins_l"),
    QuestionTab$new("bank_re", "Bank: Real Estate Exposures", "inst_type", "bank_c", TRUE, TRUE, TRUE, global$exposures$bankRe, "bank", "R"),
    QuestionTab$new("bank_c", "Bank: Company Exposures", "bank_re", "bank_retail", TRUE, TRUE, TRUE, global$exposures$bankCorporate, "bank", "C"),
    QuestionTab$new("bank_retail", "Bank: Individual Exposures", "bank_c", "bank_sov", TRUE, TRUE, TRUE, global$exposures$bankRetail, "bank", "E"),
    QuestionTab$new("bank_sov", "Bank: Sovereign Exposures", "bank_retail", "report", TRUE, TRUE, TRUE, global$exposures$sovereign, "bank", "S"),
    QuestionTab$new("ins_l", "Insurance: Life and Health Lines of Business", "inst_type", "ins_nl", TRUE, TRUE, TRUE, global$exposures$insuranceLife, "insurance", "L"),
    QuestionTab$new("ins_nl", "Insurance: Property and Casualty Lines of Business", "ins_l", "ins_c", TRUE, TRUE, TRUE, global$exposures$insuranceNonlife, "insurance", "N"),
    QuestionTab$new("ins_c", "Insurance: Corporate Assets", "ins_nl", "ins_sov", TRUE, TRUE, TRUE, global$exposures$insuranceCorporate, "insurance", "C"),
    QuestionTab$new("ins_sov", "Insurance: Sovereign Assets", "ins_c", "ins_re", TRUE, TRUE, TRUE, global$exposures$sovereign, "insurance", "S"),
    QuestionTab$new("ins_re", "Insurance: Real Estate Exposures", "ins_sov", "report", TRUE, TRUE, TRUE, global$exposures$insuranceRe, "insurance", "R"),
    QuestionTab$new("am_c", "Asset Manager / Owner / Fund: Corporate Assets", "inst_type", "am_sov", TRUE, TRUE, TRUE, global$exposures$amCorporate, "asset", "C"),
    QuestionTab$new("am_sov", "Asset Manager / Owner / Fund: Sovereign Assets", "am_c", "am_re", TRUE, TRUE, TRUE, global$exposures$sovereign, "asset", "S"),
    QuestionTab$new("am_re", "Asset Manager/ Owner / Fund: Real Estate Assets", "am_sov", "report", TRUE, TRUE, TRUE, global$exposures$amRe, "asset", "R"),
    QuestionTab$new("report", NULL, "rep_type", NULL, FALSE, FALSE, FALSE),
    QuestionTab$new("editor_auth", "Content Edition - authorization", "rep_type", NULL, add_buttons = FALSE),
    QuestionTab$new("editor", "Content Edition", "editor_auth", NULL)
  )
  # Tab names validation check
  # the global$ordered_tabs must be defined first (the QuestionTab constructor relies on this
  # to assign tab numbers), so check consistency of names post hoc
  if (identical(global$ordered_tabs, sapply(global$tabs, function(x) x$tab_name))) {
    # OK
  } else if (setequal(global$ordered_tabs, sapply(global$tabs, function(x) x$tab_name))) {
    warning("Order of global$tabs overwritten by global$ordered_tabs")
  } else {
    stop("Names of global$tabs do not match global$ordered_tabs")
  }

  # combining all the globals required by function get_report_settings in a single list
  # in order to make code more transparent
  global$content_files <- list(
    tabs = global$tabs,
    scenarios = global$scenarios,
    sections = global$sections,
    exposure_classes = global$exposure_classes
  )

  # future plan definition required for async report production
  # forking should work faster and avoid potential export problems, but is not available on windows
  if (.Platform$OS.type == "windows") {
    if (global$dev) {
      future::plan(future::sequential)
    }
    else {
      future::plan(future::multisession)
    }
  } else {
    future::plan(future::multicore)
  }
}
