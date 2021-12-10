#' some comment
#' @export
tab_type_ui <- function() {
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

tab_type_server <- function(input, output, session, tab) {
  observeEvent(
    input$type,
    {
      tab$next_tab <- switch(input$type,
        insurance = as.integer(factor("ins_l", global$ordered_tabs)),
        asset = as.integer(factor("am_c", global$ordered_tabs)),
        bank = as.integer(factor("bank_re", global$ordered_tabs))
      )
    }
  )
}

tab_type_foot <- function() {
  list(
    helpText("Please select your firm's type")
  )
}
