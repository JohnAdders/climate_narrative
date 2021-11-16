tab_report_ui <- function () {
  out <- list()
  if(!pandoc_available()){
    warning('Pandoc (required to render rtf) not available, hiding download report button')
  } else {
    out <- c(out, list(
      downloadButton("report", "Download the full report as RTF")
    ))
  }
  valid_options <- c('', unname(unlist(lapply(scenarios, function(x) {
    if(x$is_scenario){
      return(x$name)
    } else {
      return(NULL)
    }
  }))))
  out <- c(out, list(
    hr(), 
    selectInput(
      'report_selection',
      'Select the scenario to show',
      valid_options,
      selectize=FALSE
    ),
    uiOutput("rendered_report")
  ))
}

tab_report_server <- function (input, output, session, tab) {
  observeEvent(
    input$type,
    {
      tab$previous_tab <- as.integer(factor(switch(
        input$type,
        insurance = 'ins_l',
        asset = 'am',
        bank = 'bank'
      ), ordered_tabs))
    }
  )
}