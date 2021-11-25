tab_bank_server <- function(input, output, session, tab) {
  exposure_grid_server(
    input,
    output,
    exposures$bankexposures,
    matrix(
      "Placeholder for explanation, help text, remarks regarding this particular input field",
      nrow = nrow(exposures$bankexposures),
      ncol = ncol(exposures$bankexposures) - 2
    ),
    "bank_A",
    session$userData$dev
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
