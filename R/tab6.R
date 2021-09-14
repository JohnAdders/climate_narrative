tab6_ui <- function () {
  if(!pandoc_available()){
    warning('Pandoc (required to render rtf) not available, hiding download report button')
    out <- list()
  } else {
    out <- list(
      downloadButton("report", "Download the report as RTF")
    )
  }
  out <- c(out, list(
    # uiOutput('test_output'), # uncomment for debugging
    # uiOutput("show_aggregated_inputs"), # uncomment for debugging
    # uiOutput('show_all_inputs'), # uncomment for debugging
    uiOutput("rendered_report")
  ))
}

tab6_server <- function (input, output, session, tab) {
  observeEvent(
    input$type,
    {
      tab$previous_tab <- switch(
        input$type,
        insurance = 5,
        asset = 4,
        2
      )
    }
  )
}
# the code below is currently not needed (the report is reactive).
# but I keep it, it may be more efficient to produce report only here
# (and not update it reactively if any input changes)
#observeEvent(
#  input$wizard,
#  if(input$wizard=='page_6') update_final_page(input, output, session)
#)