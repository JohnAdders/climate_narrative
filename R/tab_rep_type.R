tab_rep_type_ui <- function() {
  list(
    radioButtons(
      "rep_type",
      "Type of Report:",
      c(
        "Institutional Report" = "inst",
        "Sector Report" = "sect"
      )
    )
  )
}

tab_rep_type_server <- function(input, output, session, tab) {
  observeEvent(
    input$rep_type,
    {
      tab$next_tab <- switch(input$rep_type,
        inst = tab_name_to_number("inst_type"),
        sect = tab_name_to_number("report")
      )
    }
  )
}

tab_rep_type_helper <- function() {
  list(
    helpText("Please select the report type."),
    helpText("Choose \"institutional report\" if you want to generate a report
      depending on your own sectoral and geographical exposure."),
    helpText("If you want to access the complete risk assessment
      for a sector or a country, choose \"sector report\".")
  )
}
