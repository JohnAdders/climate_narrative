tab_bank_server <- function(input, output, session, tab) {
  exposure_grid_server(
    input,
    output,
    exposures$bankexposures,
    produce_tooltip_matrix(exposures$bankexposures),
    "bank_A",
    session$userData$dev,
    width='6em'
  )
}

tab_bank_ui <- function() {
  list(
    h2("Bank Exposures"),
    exposure_grid_ui("bank_A")
  )
}

tab_bank_foot <- function() {
  list(
    helpText("Enter your firm's exposures by asset class and sector")
  )
}
