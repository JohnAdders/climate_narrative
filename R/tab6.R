tab6_ui <- function () {
  list(
    # uiOutput('test_output'), # uncomment for debugging
    # uiOutput("show_aggregated_inputs"), # uncomment for debugging
    downloadButton("report", "Download the report as html (TODO: pdf, but this may requires LaTeX)"),
    uiOutput("rendered_report")
  )
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