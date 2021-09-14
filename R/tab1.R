tab1_ui <- function () {
  list(
    radioButtons(
      "type",
      "Type of Institution:",
      c(
        "Bank" = "bank",
        "Insurance" = "insurance",
        "Asset Manager" = "asset"
      )
    )
  )
}

tab1_server <- function (input, output, session, tab) {
  observeEvent(
    input$type,
    {
      tab$next_tab <- switch(
        input$type,
        insurance = 3,
        asset = 4,
        2
      )
    }
  )
}