tab_report_ui <- function() {
  valid_options <- c("", unname(unlist(lapply(global$scenarios, function(x) {
    if (x$is_scenario) {
      return(x$name)
    } else {
      return(NULL)
    }
  }))))

  out <- list(
    selectInput(
      "report_scenario_selection",
      "Select the scenario to show",
      valid_options,
      selectize = FALSE
    ),
    selectInput(
      "report_sector_selection",
      "Select the sector to show",
      c(""),
      selectize = FALSE
    ),
    hr()
  )
  if(global$dev){
    out <- c(
      out,
      list(
        selectInput(
          "version_selection",
          "Select the report version",
          global$report_versions,
          global$report_version,
          selectize=FALSE
        )
      )
    )
  }
  if (!rmarkdown::pandoc_available()) {
    warning("Pandoc (required to render rtf) not available, hiding download report button")
    out <- c(out, list(uiOutput("html_report")))
  } else {
    out <- c(out, list(
      downloadButton("report", "Download the selected scenario/sector report as RTF"),
      conditionalPanel(
        'input.report_scenario_selection != "" | input.report_sector_selection != ""',
        hr(),
        actionButton(paste0("page_",tab_name_to_number("report"), "_previous_duplicate"), "prev")
      ),
      uiOutput("html_report")
    ))
  }
}

tab_report_server <- function(input, output, session, tab) {
  if (global$dev == TRUE) {
    updateSelectInput(session, "report_selection", selected = global$scenarios[[2]]$name)
  }
  observeEvent(
    input$type,
    {
      tab$previous_tab <- tab_name_to_number(
        switch(input$type,
          insurance = "ins_sov",
          asset = "am_re",
          bank = "bank_sov"
        )
      )
    }
  )
  observeEvent(
    input[[paste0("page_",  tab_name_to_number("report"), "_previous")]],
    {
      updateSelectInput(session, "report_scenario_selection", selected = "")
      updateSelectInput(session, "report_sector_selection", selected = "")
    }
  )
}
