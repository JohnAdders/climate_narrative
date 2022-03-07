testthat::test_that("agriculture sector report", {
  shiny::testServer(run_shiny_app(), {

    # Tests whether the reports are not unintentionally changed or corrupt

    # Construct test mock session
    load_secrets(system.file("secret.yml", package = "climate.narrative"))
    initialise_globals()
  
    session$setInputs(
      wizard = "page_1",
      rep_type = "sect",
      inst_type = "bank",
      report_scenario_selection = "",
      report_sector_selection = "",
      bank_C_subordinatedDebtAndEquity_bondLike_agriculture_agriculture = "Medium",
      report = 0
    )
    # change the further inputs step by step to mock real user actions
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
