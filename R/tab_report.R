tab_report_ui <- function () {
  out <- list()
  if(!pandoc_available()){
    warning('Pandoc (required to render rtf) not available, hiding download report button')
  } else {
    out <- c(out, list(
      downloadButton("report", "Download the report as RTF")
    ))
  }
  out <- c(out, list(
    conditionalPanel("$('html').hasClass('shiny-busy')", 
        tags$div('Rendering the report...')
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