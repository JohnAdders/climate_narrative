tab_inst_type_ui <- function() {
  list(
    radioButtons(
      "inst_type",
      "Type of Institution:",
      c(
        "Bank" = "bank",
        "Insurance" = "insurance",
        "Asset Manager / Owner / Fund" = "asset"
      )
    )
  )
}

tab_inst_type_server <- function(input, output, session, tab) {
  observeEvent(
    input$inst_type,
    {
      tab$next_tab <- switch(input$inst_type,
        insurance = tab_name_to_number("ins_l"),
        asset = tab_name_to_number("am_c"),
        bank = tab_name_to_number("bank_re")
      )
    }
  )
}

tab_inst_type_helper <- function() {
  list(
    helpText("Please select your firm's type")
  )
}
