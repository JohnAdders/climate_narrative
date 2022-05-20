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
    helpText("The tool can generate two types of report:"),
    helpText("•	an \"Institutional Report\" which gives a report tailored to your institution"),
    helpText("•	a \"Sector Report\" which allows users to see the content for an individual sector"),
    helpText("Selecting \"Institutional Report\" will take you to a series of screens where you need to input some basic information 
      regarding your firm’s business activities, products, and materiality of exposures to an asset class, sector, or liability class."),
    helpText("Selecting \"Sector Report\" will take you straight to the final report page which will allow you to select the detailed
      narrative by sector and climate scenario."),
    hr()
  )
}
