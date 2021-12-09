tab_am_c_ui <- function() {
  list(
    h2("Asset Manager: Corporate Assets under management")
  )
}

tab_am_c_foot <- function() {
  p(
    list("Enter your firm's exposures by asset class and sector using the following definitions:"),
      tags$ul(
        tags$li("\"High\": more than 10% of total assets under management"),
        tags$li("\"Medium\": 5% - 10% of total assets under management"),
        tags$li("\"Low\": below 5% of total assets under management"),
        tags$li("blank: no exposure")
      )
  )
}
