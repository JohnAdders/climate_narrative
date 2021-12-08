tab_ins_sov_ui <- function() {
  list(
    h2("Insurance: Sovereign Assets")
  )
}

tab_ins_sov_foot <- function() {
  p(
    list("Enter your firm's exposures by asset class and sector using the following definitions:",
      tags$ul(
        tags$li("\"High\": more than 10% of total assets"),
        tags$li("\"Medium\": 5% - 10% of total assets"),
        tags$li("\"Low\": below 5% of total assets")
      )
    )
  )
}
