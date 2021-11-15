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
        insurance = as.integer(factor('ins_a', ordered_tabs)),
        asset = as.integer(factor('am', ordered_tabs)),
        bank = as.integer(factor('bank', ordered_tabs))
      )
    }
  )
}

tab_ins_l_foot <- function(){
  list(
    helpText('A longer explanatory text regarding types of financial institutions may be put here')
  )
}