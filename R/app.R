#' Main funtion that runs the shiny app
#' @import shiny
#' @export
climate_narrative <- function(...){
  addResourcePath(
    "climate_narrative",
    system.file("www", package = "climate.narrative")
  )
  global$tabs <- list(
    QuestionTab$new("title", NULL, "auth", FALSE, FALSE),
    QuestionTab$new("auth", "title", NULL, ui_pars = list(captcha_code=global$captcha_code)),
    QuestionTab$new("type", "auth", "ins_l"),
    QuestionTab$new("bank_re", "type", "bank_c", TRUE, TRUE, global$exposures$bankRe, "bank", "R"),
    QuestionTab$new("bank_c", "bank_re", "bank_sov", TRUE, TRUE, global$exposures$bankCorporate, "bank", "C"),
    QuestionTab$new("bank_sov", "bank_c", "report", TRUE, TRUE, global$exposures$sovereign, "bank", "S"),
    QuestionTab$new("ins_l", "type", "ins_nl", TRUE, TRUE, global$exposures$insuranceLife, "insurance", "L"),
    QuestionTab$new("ins_nl", "ins_l", "ins_c", TRUE, TRUE, global$exposures$insuranceNonlife, "insurance", "N"),
    QuestionTab$new("ins_c", "ins_nl", "ins_sov", TRUE, TRUE, global$exposures$insuranceCorporate, "insurance", "C"),
    QuestionTab$new("ins_sov", "ins_c", "report", TRUE, TRUE, global$exposures$sovereign, "insurance", "S"),
    QuestionTab$new("am_c", "type", "am_sov", TRUE, TRUE, global$exposures$amCorporate, "asset", "C"),
    QuestionTab$new("am_sov", "am_c", "am_re", TRUE, TRUE, global$exposures$sovereign, "asset", "S"),
    QuestionTab$new("am_re", "am_sov", "report", TRUE, TRUE, global$exposures$amRe, "asset", "R"),
    QuestionTab$new("report", "type", NULL)
  )
  shinyApp(ui=ui(), server=server, ...)
}