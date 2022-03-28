library(climate.narrative)
testthat::test_that("report files", {
  # Tests whether the reports are not unintentionally changed or corrupt
  shiny::testServer(
    # First define a test function - a server function with correct initialisation
    function(input, output, session) {
      initialise_globals()
      # emulate the secret file
      global$dev <- TRUE
      global$report_version <- 6
      global$progress_bar <- TRUE
      # the server function itself
      server(input, output, session)
    },
    {
      # Construct test mock session
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
      output$dev_report
      output$dev_report_2
      session$setInputs(wizard = paste0("page_", 2))
      session$setInputs(wizard = paste0("page_", tab_name_to_number("report")))
      session$setInputs(report_sector_selection = "agriculture")
      output$report
      session$setInputs(report = input$report + 1)

      temp_filenames <- c(
        session$userData$temp_html,
        session$userData$temp_rtf,
        session$userData$temp_rtf_dev_2
      )
      comparison_filenames <- c(
        "report_agriculture.html",
        "report_agriculture.rtf",
        "report_sectors_output.rtf"
      )
      comparison_filenames <- sapply(
        comparison_filenames, function(filename) {
          system.file(paste0("tests/testthat/", filename), package = "climate.narrative")
        }
      )
      report_files <- list()
      comparison_files <- list()
      for (filename in temp_filenames) {
        file_conn <- file(filename)
        report_files <- c(report_files, list(readLines(file_conn)))
        close(file_conn)
      }
      for (filename in comparison_filenames) {
        file_conn <- file(filename)
        comparison_files <- c(comparison_files, list(readLines(file_conn)))
        close(file_conn)
      }

      # Compare test data vs function outputs.
      # at most one row (title, which is random for temp file) should differ
      for (i in 1:length(report_files)) {
        expect_equal(
          max(
            1,
            sum(
              report_files[[i]] != comparison_files[[i]]
            )
          ),
          1
        )
      }
    }
  )
})
