tab_type_ui <- function () {
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

tab_type_server <- function (input, output, session, tab) {
  observeEvent(
    input$type,
    {
      tab$next_tab <- switch(
        input$type,
        insurance = 4,
        asset = 5,
        3
      )
    }
  )
}