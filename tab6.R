tab6_ui <- function () {
  list(
    uiOutput("summary")
  )
}

tab6_server <- function (input, output, session, tab) {
  observeEvent(
    input$type,
    {
      tab$previous_tab <- switch(
        input$type,
        insurance = 5,
        asset = 3,
        1
      )
    }
  )
}

