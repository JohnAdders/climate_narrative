tab_report_ui <- function() {
  valid_options <- c("", unname(unlist(lapply(scenarios, function(x) {
    if (x$is_scenario) {
      return(x$name)
    } else {
      return(NULL)
    }
  }))))

  out <- list(
    selectInput(
      "report_selection",
      "Select the scenario to show",
      valid_options,
      selectize = FALSE
    ),
    hr()
  )
  if (!pandoc_available()) {
    warning("Pandoc (required to render rtf) not available, hiding download report button")
    out <- c(out, list(uiOutput("html_report")))
  } else {
    out <- c(out, list(
      downloadButton("report", "Download the selected scenario report as RTF"),
      uiOutput("html_report")
    ))
  }
}

tab_report_server <- function(input, output, session, tab) {
  if (session$userData$dev == TRUE){
    updateSelectInput(session, "report_selection", selected = scenarios[[2]]$name)
  }
  observeEvent(
    input$type,
    {
      tab$previous_tab <- as.integer(
        factor(
          switch(input$type,
            insurance = "ins_sov",
            asset = "am_re",
            bank = "bank_sov"
          ),
          ordered_tabs
        )
      )
    }
  )
  observeEvent(
    input[[paste0("page_",  as.integer(factor("report", ordered_tabs)), "_previous")]],
    updateSelectInput(session, "report_selection", selected = "")
  )
}
