testthat::test_that("agriculture sector report", {
  shiny::testServer(run_shiny_app(), {

    # Tests whether ...


    # Construct test mock session
    load_secrets(system.file("secret.yml", package = "climate.narrative"))

    # ordering the scenarios
    global$scenarios <- global$scenarios[order(sapply(global$scenarios, `[[`, i = "position"))]

    global$tabs <- list(
      QuestionTab$new("title", NULL, NULL, "auth", FALSE, FALSE),
      QuestionTab$new("auth", NULL, "title", NULL, ui_settings = list(captcha_code = global$captcha_code)),
      QuestionTab$new("intro", "Introduction to the Tool", "auth", "rep_type"),
      QuestionTab$new("rep_type", "Report Type Selection", "intro", "inst_type"),
      QuestionTab$new("inst_type", "Institution Type Selection", "rep_type", "ins_l"),
      QuestionTab$new("bank_re", "Bank: Real Estate Exposures", "inst_type", "bank_c", TRUE, TRUE, global$exposures$bankRe, "bank", "R"),
      QuestionTab$new("bank_c", "Bank: Company Exposures", "bank_re", "bank_sov", TRUE, TRUE, global$exposures$bankCorporate, "bank", "C"),
      QuestionTab$new("bank_sov", "Bank: Sovereign Exposures", "bank_c", "report", TRUE, TRUE, global$exposures$sovereign, "bank", "S"),
      QuestionTab$new("ins_l", "Insurance: Life and Health Lines of Business", "inst_type", "ins_nl", TRUE, TRUE, global$exposures$insuranceLife, "insurance", "L"),
      QuestionTab$new("ins_nl", "Insurance: Property and Casualty Lines of Business", "ins_l", "ins_c", TRUE, TRUE, global$exposures$insuranceNonlife, "insurance", "N"),
      QuestionTab$new("ins_c", "Insurance: Corporate Assets", "ins_nl", "ins_sov", TRUE, TRUE, global$exposures$insuranceCorporate, "insurance", "C"),
      QuestionTab$new("ins_sov", "Insurance: Sovereign Assets", "ins_c", "ins_re", TRUE, TRUE, global$exposures$sovereign, "insurance", "S"),
      QuestionTab$new("ins_re", "Insurance: Real Estate Exposures", "ins_sov", "report", TRUE, TRUE, global$exposures$insuranceRe, "insurance", "R"),
      QuestionTab$new("am_c", "Asset Manager / Owner / Fund: Corporate Assets", "rep_type", "am_sov", TRUE, TRUE, global$exposures$amCorporate, "asset", "C"),
      QuestionTab$new("am_sov", "Asset Manager / Owner / Fund: Sovereign Assets", "am_c", "am_re", TRUE, TRUE, global$exposures$sovereign, "asset", "S"),
      QuestionTab$new("am_re", "Asset Manager/ Owner / Fund: Real Estate Assets", "am_sov", "report", TRUE, TRUE, global$exposures$amRe, "asset", "R"),
      QuestionTab$new("report", NULL, "rep_type", NULL)
    )

    session$setInputs(
      wizard = "page_1",
      rep_type = "sect",
      inst_type = "bank",
      report_scenario_selection = "",
      report_sector_selection = "",
      bank_C_subordinatedDebtAndEquity_bondLike_agriculture_agriculture = "Medium",
      report = 0
    )
    session$setInputs(wizard = paste0("page_", 2))
    session$setInputs(wizard = paste0("page_", tab_name_to_number("report")))
    session$setInputs(report_sector_selection = "agriculture")
    output$report
    session$setInputs(report = input$report + 1)
    
    file_conn_1 <- file(session$userData$temp_rtf)
    rtf_report <- readLines(file_conn_1)
    close(file_conn_1)
    file_conn_2 <- file(session$userData$temp_html)
    html_report <- readLines(file_conn_2)
    close(file_conn_2)
    file_conn_3 <- file(system.file("tests/testthat/test_agriculture.rtf", package = "climate.narrative"))
    rtf_report_comp <- readLines(file_conn_3)
    close(file_conn_3)
    file_conn_4 <- file(system.file("tests/testthat/test_agriculture.html", package = "climate.narrative"))
    html_report_comp <- readLines(file_conn_4)
    close(file_conn_4)
    
    # Compare test data vs function outputs.
    # only one row (title, which is random for temp file) should differ
    expect_equal(sum(html_report != html_report_comp), 1)
    expect_equal(sum(html_report != html_report_comp), 1)
  })
})
