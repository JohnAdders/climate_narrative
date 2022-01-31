tab_type_ui <- function() {
  list(
    radioButtons(
      "type",
      "Type of Institution:",
      c(
        "Bank" = "bank",
        "Insurance" = "insurance",
        "Asset Manager/Owner" = "asset"
      )
    )
  )
}

tab_type_server <- function(input, output, session, tab) {
  if (global$dev == TRUE){
    tab$previous_tab <- tab_name_to_number("title")
  }
  observeEvent(
    input$type,
    {
      tab$next_tab <- switch(input$type,
        insurance = tab_name_to_number("ins_l"),
        asset = tab_name_to_number("am_c"),
        bank = tab_name_to_number("bank_re")
      )
    }
  )
}

tab_type_foot <- function() {
  list(
    helpText("Please select your firm's type")
  )
}
