tab_report_ui <- function () {
  out <- list(textOutput("rendering_info"))
  if(!pandoc_available()){
    warning('Pandoc (required to render rtf) not available, hiding download report button')
  } else {
    out <- c(out, list(
      downloadButton("report", "Download the report as RTF")
    ))
  }
  out <- c(out, list(
    uiOutput("rendered_report")
  ))
}

tab_report_server <- function (input, output, session, tab) {
  observeEvent(
    input$type,
    {
      tab$previous_tab <- switch(
        input$type,
        insurance = 6,
        asset = 5,
        3
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