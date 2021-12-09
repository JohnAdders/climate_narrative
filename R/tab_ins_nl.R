tab_ins_nl_ui <- function() {
  list(
    h2("Insurance: Property and Casualty Lines of Business")
  )
}

tab_ins_nl_foot <- function() {
  p(
    list("Enter your firm's exposures by asset class and sector using the following definitions:",
      tags$ul(
        tags$li("\"High\": more than 10% of total premium income"),
        tags$li("\"Medium\": 5% - 10% of total premium income"),
        tags$li("\"Low\": below 5% of total premium income"),
        tags$li("blank: no exposure")
      )
    )
  )
}
